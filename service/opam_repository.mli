val open_store : process_mgr:#Eio.Process.mgr -> ?repo_url:string -> unit -> Git_unix.Store.t
(** Open the local clone of the repo at the given URL. If the local clone does
    not yet exist, this clones it first. If repo_url is unspecified, it
    defaults to ocaml/opam-repository on GitHub. *)

val clone : process_mgr:#Eio.Process.mgr -> ?repo_url:string -> unit -> unit
(** [clone ()] ensures that a local clone of the specified repo exists. If
    not, it clones it. If repo_url is unspecified, it defaults to
    ocaml/opam-repository on GitHub. *)

val oldest_commits_with :
  process_mgr:#Eio.Process.mgr ->
  from:(string * string) list ->
  OpamPackage.t list ->
  (string * string) list
(** Use "git-log" to find the oldest commits with these package versions. This
    avoids invalidating the Docker build cache on every update to
    opam-repository.

    @param from
      The repo_url and commit hash for each opam_repository at which to begin
      the search. *)

val fetch : process_mgr:#Eio.Process.mgr -> ?repo_url:string -> unit -> unit
(* Does a "git fetch origin" to update the store. *)
