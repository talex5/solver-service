type t
(** A cache of opam files for a particular Git commit (or set of commits). *)

val empty : t

val of_commit :
  string ->
  t

val get_versions : t -> OpamPackage.Name.t -> OpamFile.OPAM.t OpamPackage.Version.Map.t
(** [get_versions t name] returns the versions of [name] in [t].

    This can be called from any domain. *)
