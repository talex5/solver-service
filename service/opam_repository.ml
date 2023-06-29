open Eio.Std
module Log = Solver_service_api.Solver.Log
module Store = Git_unix.Store

let default_repo_url = "https://github.com/ocaml/opam-repository.git"

let replace_special =
  String.map @@ function
  | 'A'..'Z'
  | 'a'..'z'
  | '0'..'9'
  | '-' as c -> c
  | _ -> '_'

let rec mkdir_p path =
  try Unix.mkdir path 0o700 with
  | Unix.Unix_error (EEXIST, _, _) -> ()
  | Unix.Unix_error (ENOENT, _, _) ->
      let parent = Filename.dirname path in
      mkdir_p parent;
      Unix.mkdir path 0o700

let repo_url_to_clone_path repo_url =
  (* The unit tests pass "opam-repository" as repo_url to refer to a local clone *)
  if repo_url = "opam-repository" then Fpath.v "opam-repository"
  else
    let uri = Uri.of_string repo_url in
    let sane_host =
      match Uri.host uri with
      | Some host -> replace_special host
      | None -> "no_host"
    in
    let sane_path =
      Uri.(
        path uri
        |> pct_decode
        |> Filename.chop_extension
        |> replace_special)
    in
    Fpath.(v sane_host / sane_path)

let clone ~process_mgr ?(repo_url = default_repo_url) () =
  let clone_path = repo_url_to_clone_path repo_url in
  let clone_parent = Fpath.parent clone_path |> Fpath.to_string in
  let clone_path_str = Fpath.to_string clone_path in
  match Unix.lstat clone_path_str with
  | Unix.{ st_kind = S_DIR; _ } -> ()
  | _ -> Fmt.failwith "%S is not a directory!" clone_path_str
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      mkdir_p clone_parent;
      Eio.Process.run process_mgr ["git"; "clone"; "--bare"; repo_url; clone_path_str]

let open_store ~process_mgr ?(repo_url = default_repo_url) () =
  clone ~process_mgr ~repo_url ();
  let path = repo_url_to_clone_path repo_url in
  match Lwt_eio.run_lwt (fun () -> Git_unix.Store.v ~dotgit:path path) with
  | Ok x -> x
  | Error e ->
      Fmt.failwith "Failed to open %a: %a" Fpath.pp path Store.pp_error e

let oldest_commit_with ~process_mgr ~repo_url ~from paths =
  let clone_path = repo_url_to_clone_path repo_url |> Fpath.to_string in
  let cmd =
    "git"
    :: "-C" :: clone_path
    :: "log"
    :: "-n" :: "1"
    :: "--format=format:%H"
    :: from
    :: "--"
    :: paths
  in
  Eio.Process.parse_out process_mgr Eio.Buf_read.take_all cmd |> String.trim

let oldest_commits_with ~process_mgr ~from pkgs =
  let paths =
    pkgs
    |> List.map (fun pkg ->
           let name = OpamPackage.name_to_string pkg in
           let version = OpamPackage.version_to_string pkg in
           Printf.sprintf "packages/%s/%s.%s" name name version)
  in
  from
  |> Fiber.List.map (fun (repo_url, hash) ->
      let commit = oldest_commit_with ~process_mgr ~repo_url ~from:hash paths in
      (repo_url, commit)
    )

let fetch ~process_mgr ?(repo_url = default_repo_url) () =
  let clone_path = repo_url_to_clone_path repo_url |> Fpath.to_string in
  Eio.Process.run process_mgr ["git"; "-C"; clone_path; "fetch"; "origin"]
