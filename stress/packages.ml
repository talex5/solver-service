type t = OpamFile.OPAM.t OpamPackage.Version.Map.t Lazy.t OpamPackage.Name.Map.t

let empty = OpamPackage.Name.Map.empty

let opam_lock = Mutex.create ()

let read_dir path =
  Sys.readdir path |> Array.to_list |> List.sort String.compare

let read_package pkg_dir =
  let ch = open_in (Filename.concat pkg_dir "opam") in
  let len = in_channel_length ch in
  let data = really_input_string ch len in
  close_in ch;
  Mutex.lock opam_lock;
  let x = OpamFile.OPAM.read_from_string data in
  Mutex.unlock opam_lock;
  x

(* Get a map of the versions inside [entry] (an entry under "packages") *)
let read_versions package_dir =
  read_dir package_dir
  |> List.fold_left
    (fun acc (entry : string) ->
       match OpamPackage.of_string_opt entry with
       | Some pkg ->
         let opam = read_package (Filename.concat package_dir entry) in
         OpamPackage.Version.Map.add pkg.version opam acc
       | None ->
         OpamConsole.log "opam-0install" "Invalid package name %S" entry;
         acc)
    OpamPackage.Version.Map.empty

let read_packages packages_dir =
  read_dir packages_dir
  |> List.filter_map (fun (entry : string) ->
      let path = Filename.concat packages_dir entry in
      let name = OpamPackage.Name.of_string entry in
      (* We only do one warm-up solve, so lazy is safe here. *)
      Some (name, lazy (read_versions path))
    )
  |> OpamPackage.Name.Map.of_list

let of_commit packages : t =
  read_packages packages

let get_versions (t:t) name =
  match OpamPackage.Name.Map.find_opt name t with
  | None -> OpamPackage.Version.Map.empty
  | Some versions -> Lazy.force versions
