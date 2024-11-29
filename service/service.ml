open Eio.Std

module Worker = Solver_service_api.Worker

let v t =
  let open Capnp_rpc.Std in
  let module X = Solver_service_api.Raw.Service.Solver in
  X.local
  @@ object
    inherit X.service

    method solve_impl params release_param_caps =
      let open X.Solve in
      let request = Params.request_get params in
      let log = Params.log_get params in
      release_param_caps ();
      match log with
      | None -> Service.fail "Missing log argument!"
      | Some log ->
        Capability.with_ref log @@ fun log ->
        match
          Worker.Solve_request.of_yojson
            (Yojson.Safe.from_string request)
        with
        | Error msg ->
          Service.error (Capnp_rpc.Error.exn "Bad JSON in request: %s" msg)
        | Ok request ->
          Switch.run @@ fun sw ->
          let log = Solver_service_api.Solver.Log.make ~sw log in
          let selections = Solver.solve t ~log request in
          let json =
            Yojson.Safe.to_string
              (Worker.Solve_response.to_yojson selections)
          in
          let response, results =
            Capnp_rpc.Service.Response.create Results.init_pointer
          in
          Results.response_set results json;
          Service.return response
  end
