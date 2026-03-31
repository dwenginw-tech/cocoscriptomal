type t =
  | Include
  | Import
  | From
  | As
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

let to_string = function
  | Include -> "include"
  | Import -> "import"
  | From -> "from"
  | As -> "as"
  | Func -> "func"
  | Local -> "local"
  | If -> "if" | Then -> "then" | Else -> "else" | ElseIf -> "elseif" | End -> "end"
  | While -> "while" | Do -> "do"
  | For -> "for"
  | Return -> "return"
  | Break -> "break"
  | And -> "and" | Or -> "or" | Not -> "not" | In -> "in"
  | True -> "true" | False -> "false"
  | Nil -> "nil"
  | Class -> "class"
  | Self -> "self"
  | Ident s -> Printf.sprintf "identifier '%s'" s
  | IntLit n -> Printf.sprintf "number %d" n
  | FloatLit f -> Printf.sprintf "number %f" f
  | StringLit s -> Printf.sprintf "string \"%s\"" s
  | Plus -> "+" | Minus -> "-" | Star -> "*" | Slash -> "/" | Percent -> "%"
  | Eq -> "=" | EqEq -> "==" | NotEq -> "!="
  | Lt -> "<" | Gt -> ">" | LtEq -> "<=" | GtEq -> ">="
  | LParen -> "(" | RParen -> ")"
  | LBrace -> "{" | RBrace -> "}"
  | LBracket -> "[" | RBracket -> "]"
  | Comma -> "," | Dot -> "." | Hash -> "#" | Colon -> ":" | DotDot -> ".."
  | Newline -> "newline"
  | Eof -> "end of file"
