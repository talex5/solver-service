open Eio.Std

module Store = Git_unix.Store
module Store_map = Map.Make(String)

let git_lock = Eio.Mutex.create ()

type t = {
  stores_lock : Eio.Mutex.t;
  mutable stores : Store.t Store_map.t;
  process_mgr : Eio.Process.mgr;
}

let oldest_commit = Eio.Semaphore.make 180
(* we are using at most 360 pipes at the same time and that's enough to keep the current
 * performance and prevent some jobs to fail because of file descriptors exceed the limit.*)

let get t repo_url =
  Eio.Mutex.use_rw ~protect:false t.stores_lock @@ fun () ->
  match Store_map.find_opt repo_url t.stores with
  | Some x -> x
  | None ->
    let store = Opam_repository.open_store ~process_mgr:t.process_mgr ~repo_url () in
    t.stores <- Store_map.add repo_url store t.stores;
    store

let mem store hash = Lwt_eio.run_lwt (fun () -> Store.mem store hash)

let update_opam_repository_to_commit t (repo_url, hash) =
  let store = get t repo_url in
  let hash = Store.Hash.of_hex hash in
  if mem store hash then ()
  else (
    Fmt.pr "Need to update %s to get new commit %a@." repo_url Store.Hash.pp
      hash;
    Opam_repository.fetch ~process_mgr:t.process_mgr ~repo_url ();
    if not (mem store hash) then
      Fmt.failwith "Still missing commit after update!")

let create ~process_mgr =
  {
    process_mgr = (process_mgr :> Eio.Process.mgr);
    stores_lock = Eio.Mutex.create ();
    stores = Store_map.empty;
  }

let oldest_commits_with t ~from repo_packages =
  Eio.Semaphore.acquire oldest_commit;
  Fun.protect ~finally:(fun () -> Eio.Semaphore.release oldest_commit) @@ fun () ->
  Opam_repository.oldest_commits_with repo_packages
    ~process_mgr:t.process_mgr
    ~from

let fetch_commits t commits =
  Eio.Mutex.use_rw ~protect:true git_lock (fun () ->
      Fiber.List.iter (update_opam_repository_to_commit t) commits
    )
