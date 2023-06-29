open Eio.Std

module Worker = Solver_service_api.Worker
module Solver = Opam_0install.Solver.Make (Git_context)
module Store = Git_unix.Store

type reply = (OpamPackage.t list, string) result * float

type stream = (Solver_service_api.Worker.Solve_request.t * reply Promise.u) Eio.Stream.t

let env (vars : Worker.Vars.t) v =
  match v with
  | "arch" -> Some (OpamTypes.S vars.arch)
  | "os" -> Some (OpamTypes.S vars.os)
  | "os-distribution" -> Some (OpamTypes.S vars.os_distribution)
  | "os-version" -> Some (OpamTypes.S vars.os_version)
  | "os-family" -> Some (OpamTypes.S vars.os_family)
  | "opam-version"  -> Some (OpamVariable.S vars.opam_version)
  | "sys-ocaml-version" -> None
  | "ocaml:native" -> Some (OpamTypes.B true)
  | "enable-ocaml-beta-repository" -> None      (* Fake variable? *)
  | _ ->
    (* Disabled, as not thread-safe! *)
    (* OpamConsole.warning "Unknown variable %S" v; *)
    None

let parse_opam (name, contents) =
  let pkg = OpamPackage.of_string name in
  let opam = Git_context.opam_read_from_string_threadsafe contents in
  (OpamPackage.name pkg, (OpamPackage.version pkg, opam))

let solve ~packages ~pins ~root_pkgs ~lower_bound (vars : Worker.Vars.t) =
  let ocaml_package = OpamPackage.Name.of_string vars.ocaml_package in
  let ocaml_version = OpamPackage.Version.of_string vars.ocaml_version in
  let context =
    Git_context.create () ~packages ~pins ~env:(env vars)
      ~constraints:
        (OpamPackage.Name.Map.singleton ocaml_package (`Eq, ocaml_version))
      ~test:(OpamPackage.Name.Set.of_list root_pkgs)
      ~with_beta_remote:
        Ocaml_version.(Releases.is_dev (of_string_exn vars.ocaml_version))
      ~lower_bound
  in
  let t0 = Unix.gettimeofday () in
  let r = Solver.solve context (ocaml_package :: root_pkgs) in
  let t1 = Unix.gettimeofday () in
  let r =
    match r with
    | Ok sels -> Ok (Solver.packages_of_result sels)
    | Error diagnostics -> Error (Solver.diagnostics diagnostics)
  in
  r, (t1 -. t0)

let last_index = ref None

let main ~stores (stream:stream) =
  Logs.info (fun f -> f "solver.ml:main");
  let packages commits =
    Eio.Mutex.use_ro Stores.git_lock @@ fun () ->
    match !last_index with
    | Some (k, v) when k = commits -> v
    | _ ->
      (* Read all the package from all the given opam-repository repos,
       * and collate them into a single Map. *)
      let v =
        List.fold_left
          (fun acc (repo_url, hash) ->
             let store = Stores.get stores repo_url in
             let hash = Store.Hash.of_hex hash in
             Git_context.read_packages ~acc store hash
          )
          Git_context.empty_index commits
      in
      last_index := Some (commits, v);
      v
  in
  while true do
    let request, reply = Eio.Stream.take stream in
    let {
      Worker.Solve_request.opam_repository_commits;
      root_pkgs;
      pinned_pkgs;
      platforms;
      lower_bound;
    } =
      request
    in
    let packages = packages opam_repository_commits in
    let root_pkgs = List.map parse_opam root_pkgs in
    let pinned_pkgs = List.map parse_opam pinned_pkgs in
    let pins = root_pkgs @ pinned_pkgs |> OpamPackage.Name.Map.of_list in
    let root_pkgs = List.map fst root_pkgs in
    platforms
    |> List.iter (fun (_id, platform) ->
        let r, time = solve ~packages ~pins ~root_pkgs ~lower_bound platform in
        Promise.resolve reply (r, time)
      )
  done
