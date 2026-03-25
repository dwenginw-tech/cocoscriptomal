(* ast.ml — CocoScript abstract syntax tree
   This is the IR that the parser hands to codegen. Kept intentionally
   flat — no nested scopes in the tree itself, the codegen pass handles
   that with its own environment stack. *)

type expr =
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | BoolLit of bool
  | Nil
  | Ident of string
  | BinOp of binop * expr * expr
  | UnaryOp of unop * expr
  | Call of string * expr list
  | Index of expr * expr        (* table[key] access *)
  | TableLit of expr list       (* {1, 2, 3} style table constructors *)

(* Concat is separate from Add because strings aren't ints — we need
   distinct codegen paths for `..` vs `+`. Same reason Neg lives in
   unop rather than being sugar for Mul by -1. *)
and binop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Gt | Lte | Gte
  | And | Or
  | Concat

and unop = Neg | Not

type stmt =
  | LocalDecl of string * expr       (* local x = ... *)
  | Assign of string * expr          (* x = ... *)
  | IndexAssign of expr * expr * expr (* tbl[key] = val *)
  | IfStmt of expr * stmt list * stmt list
  | WhileStmt of expr * stmt list
  | ForStmt of string * expr * expr * stmt list  (* for i = lo, hi do ... end *)
  | Return of expr
  | ExprStmt of expr                 (* bare expression, usually a call *)
  | Include of string

(* top-level function definition *)
type func_def = {
  name : string;
  params : string list;
  body : stmt list;
}

(* a full .coco program *)
type program = {
  includes : string list;
  functions : func_def list;
}
