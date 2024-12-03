open Eio.Std

let or_die = function
  | Ok x -> x
  | Error `Msg m -> failwith m

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
  (* Disable tls.tracing when logs are set to debug:
     (remove once we have https://github.com/dbuenzli/logs/issues/37) *)
  (* List.iter
     (fun src -> match Logs.Src.name src with "tls.tracing" -> Logs.Src.set_level src (Some Info) | _ -> ())
     @@ Logs.Src.list (); *)
  Logs.set_reporter reporter;
  Logs_threaded.enable ();
  ()

let export service ~on:socket =
  Switch.run @@ fun sw ->
  let restore =
    Capnp_rpc_net.Restorer.single
      (Capnp_rpc_net.Restorer.Id.public "solver")
      service
  in
  Fiber.fork ~sw Capnp_rpc.Leak_handler.run;
  Eio_unix.Net.import_socket_stream ~sw ~close_unix:false socket
  |> Capnp_rpc_net.Endpoint.of_flow ~peer_id:Capnp_rpc_net.Auth.Digest.insecure
  |> Capnp_rpc_unix.CapTP.connect ~sw ~restore
  |> Capnp_rpc_unix.CapTP.run

let start_server ~sw ~service vat_config =
  let service_id =
    Capnp_rpc_unix.Vat_config.derived_id vat_config "solver-service"
  in
  let restore = Capnp_rpc_net.Restorer.single service_id service in
  let vat = Capnp_rpc_unix.serve ~sw vat_config ~restore in
  Capnp_rpc_unix.Vat.sturdy_uri vat service_id

let main_service () solver cap_file vat_config =
  Switch.run @@ fun sw ->
  let uri = start_server ~sw ~service:(Solver_service.Service.v solver) vat_config in
  Capnp_rpc_unix.Cap_file.save_uri uri cap_file |> or_die;
  Fmt.pr "Wrote solver service's address to %S@." cap_file;
  Fiber.await_cancel ()

let main_service_pipe () solver =
  let socket = Unix.stdin in
  (* Run locally reading from socket *)
  export (Solver_service.Service.v solver) ~on:socket

let main_cluster net () solver name capacity register_addr =
  Switch.run @@ fun sw ->
  let vat = Capnp_rpc_unix.client_only_vat ~sw net in
  let sr = Capnp_rpc_unix.Vat.import_exn vat register_addr in
  let `Cancelled =
    Solver_worker.run solver sr
      ~name
      ~capacity
  in
  ()

(* Command-line parsing *)

open Cmdliner

let ( $ ) = Term.( $ )
let ( $$ ) f x = Term.const f $ x

let setup_log =
  let docs = Manpage.s_common_options in
  setup_log $$ Fmt_cli.style_renderer ~docs () $ Logs_cli.level ~docs ()

let internal_workers =
  Arg.value
  @@ Arg.opt Arg.int (Domain.recommended_domain_count () - 1)
  @@ Arg.info ~doc:"The number of sub-process solving requests in parallel"
    ~docv:"N" [ "internal-workers" ]

let worker_name =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"Unique worker name" ~docv:"ID" [ "name" ]

let register_addr =
  Arg.required
  @@ Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None
  @@ Arg.info ~doc:"Path of register.cap from OCluster scheduler" ~docv:"ADDR"
    [ "c"; "connect" ]

let capacity =
  Arg.value
  @@ Arg.opt Arg.int 15
  @@ Arg.info ~doc:"The number of cluster jobs that can run in parallel" ~docv:"N"
    [ "capacity" ]

let cap_file =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"Path for new service.cap" ~docv:"FILE"
    [ "cap-file" ]

let cache_dir =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"Path cached Git clones" ~docv:"DIR"
    [ "cache-dir" ]

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let () =
  let doc = "a service for selecting opam packages" in
  let info = Cmd.info "solver-service" ~doc ~version in
  exit @@
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  let domain_mgr = env#domain_mgr in
  let process_mgr = env#process_mgr in
  Switch.run @@ fun sw ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun () ->
  let solver =
    let make cache_dir n_workers =
      if not (Sys.file_exists cache_dir) then Unix.mkdir cache_dir 0o777;
      Solver_service.Solver.create ~sw ~domain_mgr ~process_mgr ~cache_dir ~n_workers
    in
    make $$ cache_dir $ internal_workers
  in
  let run_service =
    let doc = "run solver as a stand-alone service" in
    let info = Cmd.info "run-service" ~doc ~version in
    Cmd.v info (
      main_service $$ setup_log $ solver $ cap_file $ Capnp_rpc_unix.Vat_config.cmd env
    )
  in
  let run_service_pipe =
    let doc = "run solver as sub-process using stdin as socket" in
    let info = Cmd.info "run-child" ~doc ~version in
    Cmd.v info (main_service_pipe $$ setup_log $ solver)
  in
  let run_agent =
    let doc = "run solver as a cluster worker agent" in
    let info = Cmd.info "run-cluster" ~doc ~version in
    Cmd.v info (
      main_cluster env#net $$ setup_log $ solver $ worker_name $ capacity $ register_addr
    )
  in
  Cmd.eval @@ Cmd.group info [ run_service; run_service_pipe; run_agent ]
