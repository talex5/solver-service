(* This stress test is for the underlying solver-service library
   that the workers use to solve dependencies. *)

let packages = Packages.of_commit "../opam-repository/packages"

let request = [OpamPackage.Name.of_string "0install-solver"]

let run_worker packages n =
  for _i = 1 to n do
    ignore @@ Domain_worker.solve packages request;
  done

let spawn_child fn =
  match Unix.fork () with
  | 0 -> fn (); exit 0
  | child ->
    fun () ->
      match Unix.waitpid [] child with
      | _, WEXITED 0 -> ()
      | _ -> failwith "Child failed!"

let spawn_domain fn =
  let d = Domain.spawn fn in
  fun () -> Domain.join d

let main n_workers count fork =
  Format.printf "Running in %s mode@." (if fork then "fork" else "domain");
  let requests = count * n_workers in
  (* Warm-up *)
  let before = Unix.gettimeofday () in
  let expected = Domain_worker.solve packages request in
  Format.printf "%a@." Fmt.(Dump.list string) (List.map OpamPackage.to_string expected);
  let time = Unix.gettimeofday () -. before in
  Format.printf "@.Solved warm-up request in: %.2fs@." time;
  (* Main run *)
  Format.printf "Running another %d * %d solves...@." count n_workers;
  let before = Unix.gettimeofday () in
  let domains =
    let spawn = if fork then spawn_child else spawn_domain in
    List.init (n_workers - 1) (fun _ -> spawn (fun () -> run_worker packages count))
  in
  run_worker packages count;    (* Also do work in main domain *)
  List.iter (fun f -> f ()) domains;
  let time = Unix.gettimeofday () -. before in
  let rate = float requests /. time in
  Format.printf "@.Solved %d requests in %.2fs (%.2fs/iter) (%.2f solves/s)@."
    requests time (time /. float requests) rate;
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
  @@ Arg.info ~doc:"The number of requests to send per worker" ~docv:"N" [ "count" ]

let fork =
  Arg.value
  @@ Arg.flag
  @@ Arg.info ~doc:"Fork instead of using domains" [ "fork" ]

let stress_local =
  let doc = "Run jobs using an in-process solver" in
  let info = Cmd.info "local" ~doc in
  Cmd.v info Term.(const main $ internal_workers $ count $ fork)

let () =
  exit @@ Cmd.eval @@ stress_local
