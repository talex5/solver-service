(** Solver API types for communication between solver clients and solver
    workers.

    Where solver clients are any CI system that submits solver requests to be
    processed. *)

(** Variables describing a build environment. *)
module Vars = struct
  type t = {
    arch : string;
    os : string;
    os_family : string;
    os_distribution : string;
    os_version : string;
    ocaml_package : string;
    ocaml_version : string;
    opam_version : string;
  }
  [@@deriving yojson]
end

(** A set of packages for a single build. *)
module Selection = struct
  type t = {
    id : string;  (** The platform ID from the request. *)
    compat_pkgs : string list;
        (* Local root packages compatible with the platform. *)
    packages : string list;  (** The selected packages ("name.version"). *)
    commits : (string * string) list; [@deriving yojson]
        (** The commits in each opam-repository to use. A pair of the repo URL
            and the commit hash*)
  }
  [@@deriving yojson, ord]
end

(** A request to select sets of packages for the builds. *)
module Solve_request = struct
  type t = {
    opam_repository_commits : (string * string) list;
        (** Pair of repo URL and commit hash, for each opam-repository to use. *)
    root_pkgs : (string * string) list;
        (** Name and contents of top-level opam files. *)
    pinned_pkgs : (string * string) list;
        (** Name and contents of other pinned opam files. *)
    platforms : (string * Vars.t) list;  (** Possible build platforms, by ID. *)
    lower_bound : bool;
        (** Solve for the oldest possible versions instead of newest. *)
  }
  [@@deriving yojson]
end

(** The response from the solver. *)
module Solve_response = struct
  type ('a, 'b) result = ('a, 'b) Stdlib.result = Ok of 'a | Error of 'b
  [@@deriving yojson]

  type t = (Selection.t list, [ `Cancelled | `Msg of string ]) result
  [@@deriving yojson]
end
