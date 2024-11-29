open Capnp_rpc.Std

val run :
  name:string ->
  capacity:int ->
  Solver_service.Solver.t ->
  Cluster_api.Registration.X.t Sturdy_ref.t -> 'a
