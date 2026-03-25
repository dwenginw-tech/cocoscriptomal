let keywords = [
  "func", Token.Func;
  "local", Token.Local;
  "if", Token.If;
  "then", Token.Then;
  "else", Token.Else;
  "elseif", Token.ElseIf;
  "end", Token.End;
  "while", Token.While;
  "do", Token.Do;
  "for", Token.For;
  "return", Token.Return;
  "and", Token.And;
  "or", Token.Or;
  "not", Token.Not;
  "true", Token.True;
  "false", Token.False;
  "nil", Token.Nil;
]

type state = {
  src : string;
  mutable pos : int;
  mutable line : int;
}

let create src = { src; pos = 0; line = 1 }

let at_end s = s.pos >= String.length s.src

let peek s =
  if at_end s then '\000'
  else s.src.[s.pos]

let advance s =
  let c = s.src.[s.pos] in
  s.pos <- s.pos + 1;
  if c = '\n' then s.line <- s.line + 1;
  c

let skip_whitespace s =
  while not (at_end s) && (peek s = ' ' || peek s = '\t' || peek s = '\r') do
    ignore (advance s)
  done

let skip_comment s =
  while not (at_end s) && peek s <> '\n' do
    ignore (advance s)
  done

let read_string s =
  let buf = Buffer.create 16 in
  ignore (advance s);
  while not (at_end s) && peek s <> '"' do
    Buffer.add_char buf (advance s)
  done;
  if not (at_end s) then ignore (advance s);
  Token.StringLit (Buffer.contents buf)

let read_number s =
  let buf = Buffer.create 8 in
  let is_float = ref false in
  while not (at_end s) && (peek s >= '0' && peek s <= '9' || peek s = '.') do
    if peek s = '.' then is_float := true;
    Buffer.add_char buf (advance s)
  done;
  let str = Buffer.contents buf in
  if !is_float then Token.FloatLit (float_of_string str)
  else Token.IntLit (int_of_string str)

let read_ident s =
  let buf = Buffer.create 8 in
  while not (at_end s) && (let c = peek s in
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
    (c >= '0' && c <= '9') || c = '_') do
    Buffer.add_char buf (advance s)
  done;
  let word = Buffer.contents buf in
  match List.assoc_opt word keywords with
  | Some tok -> tok
  | None -> Token.Ident word

let rec next_token s =
  skip_whitespace s;
  if at_end s then Token.Eof
  else
    let c = peek s in
    match c with
    | '\n' -> ignore (advance s); Token.Newline
    | '#' -> ignore (advance s); Token.Hash
    | '"' -> read_string s
    | '+' -> ignore (advance s); Token.Plus
    | '-' ->
      ignore (advance s);
      if not (at_end s) && peek s = '-' then
        (skip_comment s; Token.Newline)
      else Token.Minus
    | '*' -> ignore (advance s); Token.Star
    | '/' -> ignore (advance s); Token.Slash
    | '%' -> ignore (advance s); Token.Percent
    | '(' -> ignore (advance s); Token.LParen
    | ')' -> ignore (advance s); Token.RParen
    | '{' -> ignore (advance s); Token.LBrace
    | '}' -> ignore (advance s); Token.RBrace
    | '[' -> ignore (advance s); Token.LBracket
    | ']' -> ignore (advance s); Token.RBracket
    | ',' -> ignore (advance s); Token.Comma
    | ':' -> ignore (advance s); Token.Colon
    | '.' ->
      ignore (advance s);
      if not (at_end s) && peek s = '.' then
        (ignore (advance s); Token.DotDot)
      else Token.Dot
    | '=' ->
      ignore (advance s);
      if not (at_end s) && peek s = '=' then
        (ignore (advance s); Token.EqEq)
      else Token.Eq
    | '!' ->
      ignore (advance s);
      if not (at_end s) && peek s = '=' then
        (ignore (advance s); Token.NotEq)
      else Token.Not
    | '<' ->
      ignore (advance s);
      if not (at_end s) && peek s = '=' then
        (ignore (advance s); Token.LtEq)
      else Token.Lt
    | '>' ->
      ignore (advance s);
      if not (at_end s) && peek s = '=' then
        (ignore (advance s); Token.GtEq)
      else Token.Gt
    | c when c >= '0' && c <= '9' -> read_number s
    | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' ->
      read_ident s
    | _ ->
      ignore (advance s);
      next_token s

let tokenize src =
  let s = create src in
  let tokens = ref [] in
  let rec loop () =
    let tok = next_token s in
    tokens := tok :: !tokens;
    if tok <> Token.Eof then loop ()
  in
  loop ();
  List.rev !tokens
