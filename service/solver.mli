type reply = (OpamPackage.t list, string) result * float

type stream = (Solver_service_api.Worker.Solve_request.t * reply Eio.Promise.u) Eio.Stream.t

val main :
  stores:Stores.t ->
  stream -> unit
(** [main stream] runs a worker process that reads requests from [stream] and solves them. *)
