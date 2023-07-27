module Solver = Opam_0install.Solver.Make (Git_context)

type request = OpamPackage.Name.t list

type reply = OpamPackage.t list
let env v =
  match v with
  | "arch" -> Some (OpamTypes.S "x86_64")
  | "os" -> Some (OpamTypes.S "linux")
  | "os-distribution" -> Some (OpamTypes.S "debian")
  | "os-version" -> Some (OpamTypes.S "12")
  | "os-family" -> Some (OpamTypes.S "debian")
  | "opam-version"  -> Some (OpamVariable.S "2.1.3")
  | "sys-ocaml-version" -> None
  | "ocaml:native" -> Some (OpamTypes.B true)
  | "enable-ocaml-beta-repository" -> None      (* Fake variable? *)
  | _ ->
    (* Disabled, as not thread-safe! *)
    (* OpamConsole.warning "Unknown variable %S" v; *)
    None

let solve packages root_pkgs =
  let context =
    Git_context.create ()
      ~packages
      ~env
  in
  let r = Solver.solve context root_pkgs in
  match r with
  | Ok sels -> Solver.packages_of_result sels
  | Error _ -> assert false
