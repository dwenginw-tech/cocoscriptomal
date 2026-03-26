let keywords = [
  "func",   Token.Func;
  "local",  Token.Local;
  "if",     Token.If;
  "then",   Token.Then;
  "else",   Token.Else;
  "elseif", Token.ElseIf;
  "end",    Token.End;
  "while",  Token.While;
  "do",     Token.Do;
  "for",    Token.For;
  "return", Token.Return;
  "break",  Token.Break;
  "and",    Token.And;
  "or",     Token.Or;
  "not",    Token.Not;
  "in",     Token.In;
  "true",   Token.True;
  "false",  Token.False;
  "nil",    Token.Nil;
  "class",  Token.Class;
  "self",   Token.Self;
]

type lexer = {
  input : string;
  mutable pos : int;
  mutable line : int;
}

let create input = { input; pos = 0; line = 1 }

let at_end l = l.pos >= String.length l.input

let peek l =
  if at_end l then '\000'
  else l.input.[l.pos]

let bump l =
  let ch = l.input.[l.pos] in
  l.pos <- l.pos + 1;
  if ch = '\n' then l.line <- l.line + 1;
  ch

let skip_whitespace l =
  while not (at_end l) && (peek l = ' ' || peek l = '\t' || peek l = '\r') do
    ignore (bump l)
  done

let skip_line_comment l =
  while not (at_end l) && peek l <> '\n' do
    ignore (bump l)
  done

let scan_string l =
  let buf = Buffer.create 16 in
  ignore (bump l);
  while not (at_end l) && peek l <> '"' do
    if peek l = '\\' then begin
      ignore (bump l);
      if not (at_end l) then
        let esc = bump l in
        match esc with
        | 'n' -> Buffer.add_char buf '\n'
        | 't' -> Buffer.add_char buf '\t'
        | '\\' -> Buffer.add_char buf '\\'
        | '"' -> Buffer.add_char buf '"'
        | _ -> Buffer.add_char buf '\\'; Buffer.add_char buf esc
    end else
      Buffer.add_char buf (bump l)
  done;
  if not (at_end l) then ignore (bump l);
  Token.StringLit (Buffer.contents buf)

let scan_number l =
  let buf = Buffer.create 8 in
  let saw_dot = ref false in
  while not (at_end l) && (peek l >= '0' && peek l <= '9' || peek l = '.') do
    if peek l = '.' then saw_dot := true;
    Buffer.add_char buf (bump l)
  done;
  let raw = Buffer.contents buf in
  if !saw_dot then Token.FloatLit (float_of_string raw)
  else Token.IntLit (int_of_string raw)

let scan_word l =
  let buf = Buffer.create 8 in
  while not (at_end l) && (let ch = peek l in
    (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') ||
    (ch >= '0' && ch <= '9') || ch = '_') do
    Buffer.add_char buf (bump l)
  done;
  let word = Buffer.contents buf in
  match List.assoc_opt word keywords with
  | Some tok -> tok
  | None -> Token.Ident word

let rec read_token l =
  skip_whitespace l;
  if at_end l then Token.Eof
  else
    let c = peek l in
    match c with
    | '\n' -> ignore (bump l); Token.Newline
    | '#'  -> ignore (bump l); Token.Hash
    | '"'  -> scan_string l
    | '+' -> ignore (bump l); Token.Plus
    (* *// line comment *)
    | '*' ->
      ignore (bump l);
      if not (at_end l) && peek l = '/' then begin
        ignore (bump l);
        if not (at_end l) && peek l = '/' then begin
          skip_line_comment l; Token.Newline
        end else Token.Star
      end else Token.Star
    | '/' -> ignore (bump l); Token.Slash
    | '%' -> ignore (bump l); Token.Percent
    | '(' -> ignore (bump l); Token.LParen
    | ')' -> ignore (bump l); Token.RParen
    | '{' -> ignore (bump l); Token.LBrace
    | '}' -> ignore (bump l); Token.RBrace
    | '[' -> ignore (bump l); Token.LBracket
    | ']' -> ignore (bump l); Token.RBracket
    | ',' -> ignore (bump l); Token.Comma
    | ':' -> ignore (bump l); Token.Colon
    | '-' ->
      ignore (bump l);
      if not (at_end l) && peek l = '-' then
        (skip_line_comment l; Token.Newline)
      else Token.Minus
    | '.' ->
      ignore (bump l);
      if not (at_end l) && peek l = '.' then
        (ignore (bump l); Token.DotDot)
      else Token.Dot
    | '=' ->
      ignore (bump l);
      if not (at_end l) && peek l = '=' then
        (ignore (bump l); Token.EqEq)
      else Token.Eq
    | '!' ->
      ignore (bump l);
      if not (at_end l) && peek l = '=' then
        (ignore (bump l); Token.NotEq)
      else Token.Not
    | '<' ->
      ignore (bump l);
      if not (at_end l) && peek l = '=' then
        (ignore (bump l); Token.LtEq)
      else Token.Lt
    | '>' ->
      ignore (bump l);
      if not (at_end l) && peek l = '=' then
        (ignore (bump l); Token.GtEq)
      else Token.Gt
    | c when c >= '0' && c <= '9' -> scan_number l
    | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' ->
      scan_word l
    | _ ->
      ignore (bump l);
      read_token l

let tokenize source =
  let l = create source in
  let tokens = ref [] in
  let rec loop () =
    let tok = read_token l in
    tokens := tok :: !tokens;
    if tok <> Token.Eof then loop ()
  in
  loop ();
  List.rev !tokens
