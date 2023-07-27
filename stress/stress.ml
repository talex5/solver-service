(* This stress test is for the underlying solver-service library
   that the workers use to solve dependencies. *)

let packages = Packages.of_commit "../opam-repository/packages"

let request = [OpamPackage.Name.of_string "0install-solver"]

let rec run_worker packages todo =
  if Atomic.fetch_and_add todo (-1) > 0 then (
    ignore @@ Domain_worker.solve packages request;
    run_worker packages todo
  )

let main n_workers count =
  let todo = Atomic.make count in
  (* Warm-up *)
  let before = Unix.gettimeofday () in
  let expected = Domain_worker.solve packages request in
  Format.printf "%a@." Fmt.(Dump.list string) (List.map OpamPackage.to_string expected);
  let time = Unix.gettimeofday () -. before in
  Format.printf "@.Solved warm-up request in: %.2fs@." time;
  (* Main run *)
  Format.printf "Running another %d solves...@." count;
  let before = Unix.gettimeofday () in
  let domains = List.init n_workers (fun _ -> Domain.spawn (fun () -> run_worker packages todo)) in
  List.iter Domain.join domains;
  let time = Unix.gettimeofday () -. before in
  let rate = float count /. time in
  Format.printf "@.Solved %d requests in %.2fs (%.2fs/iter) (%.2f solves/s)@."
    count time (time /. float count) rate;
  Fmt.pr "Workers, Rate@.";
  Fmt.pr "%d, %.3f@." n_workers rate

open Cmdliner

let internal_workers =
  Arg.value
  @@ Arg.opt Arg.int (Domain.recommended_domain_count () - 1)
  @@ Arg.info ~doc:"The number of sub-process solving requests in parallel"
    ~docv:"N" [ "internal-workers" ]

let count =
  Arg.value
  @@ Arg.opt Arg.int 3
  @@ Arg.info ~doc:"The number of requests to send" ~docv:"N" [ "count" ]

let stress_local =
  let doc = "Run jobs using an in-process solver" in
  let info = Cmd.info "local" ~doc in
  Cmd.v info Term.(const main $ internal_workers $ count)

let () =
  exit @@ Cmd.eval @@ stress_local
