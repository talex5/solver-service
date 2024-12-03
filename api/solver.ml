(** Solver API for CapnP RPC. *)

open Eio.Std
open Capnp_rpc.Std

(** Logger for writing to a running {!solve} request. *)
module Log = struct
  module X = Raw.Client.Log

  type t = {
    cap : X.t Capability.t;
    sw : Switch.t;
  }

  let pp_timestamp f x =
    let open Unix in
    let tm = gmtime x in
    Fmt.pf f "%04d-%02d-%02d %02d:%02d.%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
      tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

  let write t msg =
    let open X.Write in
    let message_size = 150 + String.length msg in
    let request, params =
      Capability.Request.create ~message_size Params.init_pointer
    in
    Params.msg_set params msg;
    Capability.call_for_unit_exn t.cap method_id request

  let info t fmt =
    let now = Unix.gettimeofday () in
    let k msg =
      Fiber.fork ~sw:t.sw (fun () ->
          try
            write t msg
          with ex ->
            Format.eprintf "Log.info(%S) failed: %a@." msg Fmt.exn ex
        )
    in
    Fmt.kstr k ("%a [INFO] @[" ^^ fmt ^^ "@]@.") pp_timestamp now

  let make ~sw cap = { cap; sw }
end

module X = Raw.Client.Solver

type t = X.t Capability.t
(** CapnP Capability to call {!Raw.Client.Solver}. *)

(** Runs a solve for request {!Worker.Solve_request.t} returning the results as
    {!Worker.Solve_response.t}.

    Errors are reported as [Failure] exceptions. *)
let solve t ~log reqs =
  let open X.Solve in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.request_set params
    (Worker.Solve_request.to_yojson reqs |> Yojson.Safe.to_string);
  Params.log_set params (Some log);
  let result = Capability.call_for_value t method_id request in
  match result with
  | Error (`Capnp e) -> Fmt.failwith "Capnp error: %a" Capnp_rpc.Error.pp e
  | Ok json -> (
      match
        Worker.Solve_response.of_yojson
          (Yojson.Safe.from_string @@ Results.response_get json)
      with
      | Ok x -> x
      | Error ex -> failwith ex)
