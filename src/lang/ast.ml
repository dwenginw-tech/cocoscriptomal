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
  | Index of expr * expr
  | TableLit of expr list
  | FieldGet of expr * string
  | MethodCall of expr * string * expr list
  | NewObj of string * expr list
  | Lambda of string list * stmt list

and binop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Gt | Lte | Gte
  | And | Or
  | Concat

and unop = Neg | Not

and func_def = {
  name : string;
  params : string list;
  body : stmt list;
}

and stmt =
  | LocalDecl of string * expr
  | MultiDecl of (string * expr) list
  | Assign of string * expr
  | IndexAssign of expr * expr * expr
  | IfStmt of expr * stmt list * stmt list
  | WhileStmt of expr * stmt list
  | ForStmt of string * expr * expr * stmt list
  | ForEach of string * expr * stmt list
  | Return of expr
  | Break
  | ExprStmt of expr
  | Include of string
  | FieldSet of expr * string * expr
  | ClassDecl of string * func_def list

type program = {
  includes : string list;
  functions : func_def list;
}
