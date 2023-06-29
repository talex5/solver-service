include Opam_0install.S.CONTEXT

type index

val empty_index : index

val read_packages :
  ?acc:index ->
  Git_unix.Store.t ->
  Git_unix.Store.Hash.t ->
  index
(** [read_packages store commit] is an index of the opam files in [store] at
    [commit]. *)

val create :
  ?test:OpamPackage.Name.Set.t ->
  ?pins:(OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
  ?lower_bound:bool ->
  constraints:OpamFormula.version_constraint OpamPackage.Name.Map.t ->
  env:(string -> OpamVariable.variable_contents option) ->
  packages:index ->
  with_beta_remote:bool ->
  unit ->
  t

val opam_read_from_string_threadsafe : string -> OpamFile.OPAM.t
