open Solver_service
open Lwt.Syntax
module Service = Service.Make (Opam_repository)
module Worker_process = Internal_worker.Worker_process

let pp_timestamp f x =
  let open Unix in
  let tm = localtime x in
  Fmt.pf f "%04d-%02d-%02d %02d:%02d.%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let reporter =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stderr
      ("%a %a %a @[" ^^ fmt ^^ "@]@.")
      pp_timestamp (Unix.gettimeofday ())
      Fmt.(styled `Magenta string)
      (Printf.sprintf "%14s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report }

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  (* Disable tls.tracing when logs are set to debug *)
  (* List.iter
     (fun src -> match Logs.Src.name src with "tls.tracing" -> Logs.Src.set_level src (Some Info) | _ -> ())
     @@ Logs.Src.list (); *)
  Logs.set_reporter reporter;
  ()

let export service ~on:socket =
  let restore =
    Capnp_rpc_net.Restorer.single
      (Capnp_rpc_net.Restorer.Id.public "solver")
      service
  in
  let switch = Lwt_switch.create () in
  let stdin =
    Capnp_rpc_unix.Unix_flow.connect socket
    |> Capnp_rpc_net.Endpoint.of_flow
         (module Capnp_rpc_unix.Unix_flow)
         ~peer_id:Capnp_rpc_net.Auth.Digest.insecure ~switch
  in
  let (_ : Capnp_rpc_unix.CapTP.t) =
    Capnp_rpc_unix.CapTP.connect ~restore stdin
  in
  let crashed, set_crashed = Lwt.wait () in
  let* () =
    Lwt_switch.add_hook_or_exec (Some switch) (fun () ->
        Lwt.wakeup_exn set_crashed (Failure "Capnp switch turned off");
        Lwt.return_unit)
  in
  crashed

let start_server address ~n_workers =
  let config =
    Capnp_rpc_unix.Vat_config.create ~secret_key:`Ephemeral address
  in
  let service_id =
    Capnp_rpc_unix.Vat_config.derived_id config "solver-service"
  in
  let create_worker commits =
    let cmd =
      ("", [| Sys.argv.(0); "--worker"; Remote_commit.list_to_string commits |])
    in
    Worker_process.create cmd
  in
  let* service = Service.v ~n_workers ~create_worker in
  let restore = Capnp_rpc_net.Restorer.single service_id service in
  let+ vat = Capnp_rpc_unix.serve config ~restore in
  Capnp_rpc_unix.Vat.sturdy_uri vat service_id

let main () hash address sockpath n_workers =
  match (hash, address) with
  | None, Some address ->
      (* Run with a capnp address as the endpoint *)
      Lwt_main.run
        (let* uri = start_server address ~n_workers in
         Fmt.pr "Solver service running at: %a@." Uri.pp_hum uri;
         fst @@ Lwt.wait ())
  | None, None ->
      let socket =
        match sockpath with
        | Some path ->
            let sock = Unix.(socket PF_UNIX SOCK_STREAM 0) in
            Unix.connect sock (ADDR_UNIX path);
            Lwt_unix.of_unix_file_descr sock
        | None -> Lwt_unix.stdin
      in
      (* Run locally reading from socket *)
      Lwt_main.run
        (let create_worker commits =
           let cmd =
             ( "",
               [|
                 Sys.argv.(0); "--worker"; Remote_commit.list_to_string commits;
               |] )
           in
           Worker_process.create cmd
         in
         let* service = Service.v ~n_workers ~create_worker in
         export service ~on:socket)
  | Some commits_str, _ ->
      Solver.main (Remote_commit.list_of_string_or_fail commits_str)

(* Command-line parsing *)

open Cmdliner

let setup_log =
  let docs = Manpage.s_common_options in
  Term.(
    const setup_log $ Fmt_cli.style_renderer ~docs () $ Logs_cli.level ~docs ())

let worker_commits =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The hash commits of the worker." ~docv:"COMMITS"
       [ "worker" ]

let internal_workers =
  Arg.value
  @@ Arg.opt Arg.int 20
  @@ Arg.info ~doc:"The number of sub-process solving requests in parallel"
       ~docv:"N" [ "internal-workers" ]

let address =
  Arg.value
  @@ Arg.opt Arg.(some Capnp_rpc_unix.Network.Location.cmdliner_conv) None
  @@ Arg.info
       ~doc:
         "The address to read requests from, if not provided will use \
          $(b,--sockpath)."
       ~docv:"ADDRESS" [ "address" ]

let sockpath =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info
       ~doc:
         "The UNIX domain socket path to read requests from, if not provided \
          will use stdin."
       ~docv:"SOCKPATH" [ "sockpath" ]

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let cmd =
  let doc = "Solver for ocaml-ci" in
  let info = Cmd.info "solver-service" ~doc ~version in
  Cmd.v info
    Term.(
      const main
      $ setup_log
      $ worker_commits
      $ address
      $ sockpath
      $ internal_workers)

let () = Cmd.(exit @@ eval cmd)
