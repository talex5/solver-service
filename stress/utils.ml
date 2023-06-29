open Lwt.Infix
open Lwt.Syntax

let pp_args =
  let sep = Fmt.(const string) " " in
  Fmt.(array ~sep (quote string))

let pp_cmd f = function
  | "", args -> pp_args f args
  | bin, args -> Fmt.pf f "(%S, %a)" bin pp_args args

let pp_status f = function
  | Unix.WEXITED x -> Fmt.pf f "exited with status %d" x
  | Unix.WSIGNALED x -> Fmt.pf f "failed with signal %a" Fmt.Dump.signal x
  | Unix.WSTOPPED x -> Fmt.pf f "stopped with signal %a" Fmt.Dump.signal x

let check_status cmd = function
  | Unix.WEXITED 0 -> ()
  | status -> Fmt.failwith "%a %a" pp_cmd cmd pp_status status

let pread cmd =
  Lwt_process.with_process_in cmd @@ fun proc ->
  Lwt_io.read proc#stdout >>= fun output ->
  proc#status >|= check_status cmd >|= fun () -> output

let opam_template arch =
  let arch = Option.value ~default:"%{arch}%" arch in
  Fmt.str
    {|
  {
    "arch": "%s",
    "os": "%%{os}%%",
    "os_family": "%%{os-family}%%",
    "os_distribution": "%%{os-distribution}%%",
    "os_version": "%%{os-version}%%",
    "opam_version": "%%{opam-version}%%"
  }
|}
    arch

let get_vars ~ocaml_package_name ~ocaml_version ?arch () =
  let+ vars =
    pread
      ("", [| "opam"; "config"; "expand"; opam_template arch |])
  in
  let json =
    match Yojson.Safe.from_string vars with
    | `Assoc items ->
        `Assoc
          (("ocaml_package", `String ocaml_package_name)
          :: ("ocaml_version", `String ocaml_version)
          :: items)
    | json ->
        Fmt.failwith "Unexpected JSON: %a"
          Yojson.Safe.(pretty_print ~std:true)
          json
  in
  Result.get_ok @@ Solver_service_api.Worker.Vars.of_yojson json

let get_opam_file pv =
  pread ("", [| "opam"; "show"; "--raw"; pv |])

let get_opam_packages () =
  let open Lwt.Infix in
  pread
    ("", [| "opam"; "list"; "--short"; "--color=never" |])
  >|= String.split_on_char '\n'
  >|= List.map String.trim
