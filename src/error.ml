(* error.ml — CocoScript error reporting
   Pretty basic for now: just prints "file:line: error: msg" and exits.
   TODO: once the parser tracks source positions properly, wire that in
   here so we can show the offending line and a caret underneath it. *)

exception CocoError of string * int

let report filename line msg =
  Printf.eprintf "%s:%d: error: %s\n" filename line msg;
  exit 1
