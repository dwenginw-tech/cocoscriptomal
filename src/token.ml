(* token.ml — CocoScript lexical tokens
   The token set here is deliberately lua-ish: keywords like `local`,
   `then`/`end` blocks, `..` for concat, `#` for length, etc.
   We diverge a little (no `repeat`/`until`, added `include`) but the
   feel should be familiar to anyone who's touched Lua. *)

type t =
  | Include
  | Func
  | Local
  | If | Then | Else | ElseIf | End
  | While | Do
  | For
  | Return
  | And | Or | Not
  | True | False
  | Nil
  (* identifiers and literals *)
  | Ident of string
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  (* arithmetic *)
  | Plus | Minus | Star | Slash | Percent
  (* comparison / equality *)
  | Eq | EqEq | NotEq
  | Lt | Gt | LtEq | GtEq
  (* delimiters *)
  | LParen | RParen
  | LBrace | RBrace
  | LBracket | RBracket
  (* punctuation *)
  | Comma | Dot | Hash | Colon | DotDot
  | Newline
  | Eof
