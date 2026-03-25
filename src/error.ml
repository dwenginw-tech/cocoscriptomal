exception CocoError of string * int

let report filename line msg =
  Printf.eprintf "%s:%d: error: %s\n" filename line msg;
  exit 1
