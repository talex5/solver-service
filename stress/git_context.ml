type rejection = UserConstraint of OpamFormula.atom | Unavailable

type t = {
  env : string -> OpamVariable.variable_contents option;
  packages : Packages.t;
}

let dev = OpamPackage.Version.of_string "dev"

let user_restrictions _t _name = None

let env t pkg v =
  if List.mem v OpamPackageVar.predefined_depends_variables then None
  else
    match OpamVariable.Full.to_string v with
    | "version" -> Some (OpamTypes.S (OpamPackage.version_to_string pkg))
    | x -> t.env x

let filter_deps t pkg f =
  let dev = OpamPackage.Version.compare (OpamPackage.version pkg) dev = 0 in
  f
  |> OpamFilter.partial_filter_formula (env t pkg)
  |> OpamFilter.filter_deps ~build:true ~post:true ~test:false ~doc:false ~dev
       ~default:false

let filter_available t pkg opam =
  let available = OpamFile.OPAM.available opam in
  match OpamFilter.eval ~default:(B false) (env t pkg) available with
  | B true -> Ok opam
  | B false -> Error Unavailable
  | _ ->
      OpamConsole.error "Available expression not a boolean: %s"
        (OpamFilter.to_string available);
      Error Unavailable

let version_compare (v1, opam1) (v2, opam2) =
  let avoid1 =
    List.mem OpamTypes.Pkgflag_AvoidVersion (OpamFile.OPAM.flags opam1)
  in
  let avoid2 =
    List.mem OpamTypes.Pkgflag_AvoidVersion (OpamFile.OPAM.flags opam2)
  in
  if avoid1 = avoid2 then
    OpamPackage.Version.compare v1 v2
  else if avoid1 then -1
  else 1

let candidates t name =
  let versions = Packages.get_versions t.packages name in
  OpamPackage.Version.Map.bindings versions
  |> List.fast_sort version_compare
  |> List.rev_map (fun (v, opam) ->
      let pkg = OpamPackage.create name v in
      (v, filter_available t pkg opam))

let pp_rejection f = function
  | UserConstraint x ->
    Fmt.pf f "Rejected by user-specified constraint %s"
      (OpamFormula.string_of_atom x)
  | Unavailable -> Fmt.string f "Availability condition not satisfied"

let create ~env ~packages () =
  { env; packages }
