(* lexer.ml — tokenizer for CocoScript
   Scans source text into a flat token list.
   Handles: keywords, identifiers, numbers (int + float),
   string literals, operators, and Lua-style -- comments. *)

(* keyword table — checked after we finish scanning an identifier *)
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
  "and",    Token.And;
  "or",     Token.Or;
  "not",    Token.Not;
  "true",   Token.True;
  "false",  Token.False;
  "nil",    Token.Nil;
]

(* The scanner state. We track `line` so we can report meaningful
   error positions later — every '\n' we consume bumps it. *)
type lexer = {
  input : string;
  mutable pos : int;
  mutable line : int;
}

let create input = { input; pos = 0; line = 1 }

let at_end l = l.pos >= String.length l.input

(* Look at current char without consuming it. Returns null byte at EOF
   so callers don't need to bounds-check separately. *)
let peek l =
  if at_end l then '\000'
  else l.input.[l.pos]

(* Consume the current character, advancing the cursor.
   Also handles line tracking — bump line count on newlines. *)
let bump l =
  let ch = l.input.[l.pos] in
  l.pos <- l.pos + 1;
  if ch = '\n' then l.line <- l.line + 1;
  ch

let skip_whitespace l =
  while not (at_end l) && (peek l = ' ' || peek l = '\t' || peek l = '\r') do
    ignore (bump l)
  done

(* -- starts a line comment, same as Lua/Haskell.
   We just eat everything until the next newline. *)
let skip_line_comment l =
  while not (at_end l) && peek l <> '\n' do
    ignore (bump l)
  done

(* Scan a double-quoted string literal. We've already seen the opening
   quote in the caller, so start by consuming it here. *)
let scan_string l =
  let buf = Buffer.create 16 in
  ignore (bump l);
  while not (at_end l) && peek l <> '"' do
    Buffer.add_char buf (bump l)
  done;
  if not (at_end l) then ignore (bump l); (* closing quote *)
  Token.StringLit (Buffer.contents buf)

(* Scan a numeric literal.
   Floats: if we hit a dot after digits, keep going and tag it
   as a float instead of an int. *)
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

(* Scan an identifier or keyword. We grab [a-zA-Z0-9_]+ and then
   check whether the result lives in the keywords table. *)
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

(* Main dispatch: figure out what kind of token starts at the current
   position and return it. Whitespace is skipped beforehand. *)
let rec read_token l =
  skip_whitespace l;
  if at_end l then Token.Eof
  else
    let c = peek l in
    match c with
    | '\n' -> ignore (bump l); Token.Newline
    | '#'  -> ignore (bump l); Token.Hash
    | '"'  -> scan_string l

    (* simple single-char operators *)
    | '+' -> ignore (bump l); Token.Plus
    | '*' -> ignore (bump l); Token.Star
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

    (* minus or line comment *)
    | '-' ->
      ignore (bump l);
      if not (at_end l) && peek l = '-' then
        (skip_line_comment l; Token.Newline)
      else Token.Minus

    (* dot or dot-dot (string concatenation) *)
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

    (* numbers start with a digit *)
    | c when c >= '0' && c <= '9' -> scan_number l
    (* identifiers / keywords start with a letter or underscore *)
    | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' ->
      scan_word l

    (* anything we don't recognise — skip it and try again *)
    | _ ->
      ignore (bump l);
      read_token l

(* Public entry point. Tokenize the entire source string and return
   the token list, terminated by Eof. *)
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
