type t =
  | Include
  | Func
  | Local
  | If | Then | Else | ElseIf | End
  | While | Do
  | For
  | Return
  | Break
  | And | Or | Not | In
  | True | False
  | Nil
  | Class
  | Self
  | Ident of string
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | Plus | Minus | Star | Slash | Percent
  | Eq | EqEq | NotEq
  | Lt | Gt | LtEq | GtEq
  | LParen | RParen
  | LBrace | RBrace
  | LBracket | RBracket
  | Comma | Dot | Hash | Colon | DotDot
  | Newline
  | Eof
