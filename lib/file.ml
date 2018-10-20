open Core
open Async

let build_type_exn () =
  let%bind dune = Deferred.Or_error.try_with (fun () -> Unix.stat "dune") in
  let%map jbuild = Deferred.Or_error.try_with (fun () -> Unix.stat "jbuild") in
  match (dune, jbuild) with
  | Ok _, Ok _ ->
    raise_s [%message "Directory can only contain dune or jbuild. Not both"]
  | Ok _dune, Error _ -> `Dune
  | Error _, Ok _jbuild -> `Jbuild
  | Error _, Error _ ->
    raise_s [%message "Directory must contain either a dune or jbuild file"]

(*   print_s [%sexp (dune : Unix.Stats.t)] *)

let open_buildfile_exn () =
  (* Try loading dune file first. If that doesn't work
   * try loading jbuild. If THAT doesn't work, bail baby *)
  match%bind Reader.load_sexp "dune" Fn.id with
  | Ok sexp -> return sexp
  | Error _ignore -> Reader.load_sexp_exn "jbuild" Fn.id

(* let get_exe_name buildfile = *)
