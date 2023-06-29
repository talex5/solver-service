open Eio.Std
module Worker = Solver_service_api.Worker
module Log = Solver_service_api.Solver.Log
module Selection = Worker.Selection

type t = {
  pool : Solver.stream Lwt_pool.t;
  stores : Stores.t;
}

(* Send [request] to [worker] and read the reply. *)
let process ~log ~id request (worker:Solver.stream) =
  let reply, set_reply = Promise.create () in
  Eio.Stream.add worker (request, set_reply);
  let results, time = Promise.await reply in
  match results with
  | Ok packages ->
    Log.info log "%s: found solution in %f s" id time;
    Ok packages
  | Error msg ->
    Log.info log "%s: eliminated all possibilities in %f s" id time;
    Error msg

let ocaml = OpamPackage.Name.of_string "ocaml"

(* If a local package has a literal constraint on OCaml's version and it doesn't match
   the platform, we just remove that package from the set to test, so other packages
   can still be tested. *)
let compatible_with ~ocaml_version (dep_name, filter) =
  let check_ocaml = function
    | OpamTypes.Constraint (op, OpamTypes.FString v) ->
        let v = OpamPackage.Version.of_string v in
        OpamFormula.eval_relop op ocaml_version v
    | _ -> true
  in
  if OpamPackage.Name.equal dep_name ocaml then
    OpamFormula.eval check_ocaml filter
  else true

(* Handle a request by distributing it among the worker processes and then aggregating their responses. *)
let solve t ~log request =
  let {
    Worker.Solve_request.opam_repository_commits;
    platforms;
    root_pkgs;
    pinned_pkgs;
    lower_bound = _;
  } =
    request
  in
  Stores.fetch_commits t.stores opam_repository_commits;
  let root_pkgs = List.map fst root_pkgs in
  let pinned_pkgs = List.map fst pinned_pkgs in
  let pins =
    root_pkgs @ pinned_pkgs
    |> List.map (fun pkg -> OpamPackage.name (OpamPackage.of_string pkg))
    |> OpamPackage.Name.Set.of_list
  in
  Log.info log "Solving for %a" Fmt.(list ~sep:comma string) root_pkgs;
  platforms
  |> Fiber.List.map (fun p ->
      let id, vars = p in
      let ocaml_version =
        OpamPackage.Version.of_string vars.Worker.Vars.ocaml_version
      in
      let compatible_root_pkgs =
        request.root_pkgs
        |> List.filter (fun (_name, contents) ->
            if String.equal "" contents then true
            else
              let opam = Git_context.opam_read_from_string_threadsafe contents in
              let deps = OpamFile.OPAM.depends opam in
              OpamFormula.eval (compatible_with ~ocaml_version) deps)
      in
      (* If some packages are compatible but some aren't, just solve for the compatible ones.
         Otherwise, try to solve for everything to get a suitable error. *)
      let root_pkgs =
        if compatible_root_pkgs = [] then request.root_pkgs
        else compatible_root_pkgs
      in
      let slice = { request with platforms = [ p ]; root_pkgs } in
      match
        Lwt_eio.run_lwt @@ fun () ->
        Lwt_pool.use t.pool (fun worker ->
            Lwt_eio.run_eio @@ fun () ->
            process ~log ~id slice worker
          )
      with
      | Error _ as e -> (id, e)
      | Ok packages ->
        let repo_packages =
          packages
          |> List.filter_map (fun (pkg : OpamPackage.t) ->
              if OpamPackage.Name.Set.mem pkg.name pins then None
              else Some pkg)
        in
        (* Hack: ocaml-ci sometimes also installs odoc, but doesn't tell us about it.
           Make sure we have at least odoc 2.1.1 available, otherwise it won't work on OCaml 5.0. *)
        let repo_packages =
          OpamPackage.of_string "odoc.2.1.1" :: repo_packages
        in
        let commits = Stores.oldest_commits_with t.stores repo_packages ~from:opam_repository_commits in
        let compat_pkgs = List.map fst compatible_root_pkgs in
        let packages = List.map OpamPackage.to_string packages in
        (id, Ok { Worker.Selection.id; compat_pkgs; packages; commits }))
  |> List.filter_map (fun (id, result) ->
      Log.info log "= %s =" id;
      match result with
      | Ok result ->
        Log.info log "-> @[<hov>%a@]"
          Fmt.(list ~sep:sp string)
          result.Selection.packages;
        Log.info log "(valid since opam-repository commit(s): @[%a@])"
          Fmt.(list ~sep:semi (pair ~sep:comma string string))
          result.Selection.commits;
        Some result
      | Error msg ->
        Log.info log "%s" msg;
        None)

let solve t ~log request =
  try Ok (solve t ~log request)
  with
  | Failure msg -> Error (`Msg msg)
  | ex -> Fmt.error_msg "%a" Fmt.exn ex

let create ~sw ~domain_mgr ~process_mgr ~n_workers =
  let stores = Stores.create ~process_mgr in
  let create_worker _commits =
    Logs.info (fun f -> f "create_worker");
    try
      let stream = Eio.Stream.create n_workers in
      Fiber.fork ~sw (fun () ->
          Eio.Domain_manager.run domain_mgr @@ fun () ->
          Solver.main ~stores stream
        );
      Logs.info (fun f -> f "create_worker done");
      stream
    with ex ->
      let bt = Printexc.get_raw_backtrace () in
      Eio.traceln "service.ml:v: %a" Fmt.exn_backtrace (ex, bt);
      raise ex
  in
  let pool = Lwt_pool.create n_workers (fun () -> Lwt_eio.run_eio create_worker) in
  {
    stores;
    pool;
  }

let capnp_service t =
  let open Capnp_rpc_lwt in
  let module X = Solver_service_api.Raw.Service.Solver in
  X.local
  @@ object
    inherit X.service

    method solve_impl params release_param_caps =
      let open X.Solve in
      let request = Params.request_get params in
      let log = Params.log_get params in
      release_param_caps ();
      match log with
      | None -> Service.fail "Missing log argument!"
      | Some log ->
        Capnp_rpc_lwt.Service.return_lwt @@ fun () ->
        Capability.with_ref log @@ fun log ->
        match
          Worker.Solve_request.of_yojson
            (Yojson.Safe.from_string request)
        with
        | Error msg ->
          Lwt_result.fail
            (`Capnp
               (Capnp_rpc.Error.exn "Bad JSON in request: %s" msg))
        | Ok request ->
          Lwt_eio.run_eio @@ fun () ->
          let selections = solve t ~log request in
          let json =
            Yojson.Safe.to_string
              (Worker.Solve_response.to_yojson selections)
          in
          let response, results =
            Capnp_rpc_lwt.Service.Response.create Results.init_pointer
          in
          Results.response_set results json;
          Ok response
  end
