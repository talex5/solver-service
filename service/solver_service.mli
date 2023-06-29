type t

val create :
  sw:Eio.Switch.t ->
  domain_mgr:#Eio.Domain_manager.t ->
  process_mgr:#Eio.Process.mgr ->
  n_workers:int ->
  t
(** [create ~sw ~domain_mgr ~process_mgr ~n_workers] is a solver service that
    distributes work to up to [n_workers] domains.

    @param sw Holds the worker domains.
    @param domain_mgr Used to spawn new domains.
    @param process_mgr Used to run the "git" command.
    @param n_workers Maximum number of worker domains. *)

val solve :
  t ->
  log:Solver_service_api.Solver.Log.t ->
  Solver_service_api.Worker.Solve_request.t ->
  Solver_service_api.Worker.Solve_response.t

val capnp_service : t -> Solver_service_api.Solver.t
(** [capnp_service t] is a Cap'n Proto service that handles requests using [t]. *)
