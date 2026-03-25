type state = {
  tokens : Token.t array;
  mutable pos : int;
  mutable line : int;
}

let create tokens = { tokens = Array.of_list tokens; pos = 0; line = 1 }

let peek s =
  if s.pos >= Array.length s.tokens then Token.Eof
  else s.tokens.(s.pos)

let advance s =
  let t = peek s in
  s.pos <- s.pos + 1;
  t

let expect s tok =
  let t = advance s in
  if t <> tok then
    failwith (Printf.sprintf "line %d: expected token, got something else" s.line)

let skip_newlines s =
  while peek s = Token.Newline do ignore (advance s); s.line <- s.line + 1 done

(* precedence climbing expression parser *)
let rec parse_expr s = parse_or s

and parse_or s =
  let left = parse_and s in
  if peek s = Token.Or then (ignore (advance s); Ast.BinOp (Or, left, parse_or s))
  else left

and parse_and s =
  let left = parse_comparison s in
  if peek s = Token.And then (ignore (advance s); Ast.BinOp (And, left, parse_and s))
  else left

and parse_comparison s =
  let left = parse_addition s in
  match peek s with
  | Token.EqEq -> ignore (advance s); Ast.BinOp (Eq, left, parse_addition s)
  | Token.NotEq -> ignore (advance s); Ast.BinOp (Neq, left, parse_addition s)
  | Token.Lt -> ignore (advance s); Ast.BinOp (Lt, left, parse_addition s)
  | Token.Gt -> ignore (advance s); Ast.BinOp (Gt, left, parse_addition s)
  | Token.LtEq -> ignore (advance s); Ast.BinOp (Lte, left, parse_addition s)
  | Token.GtEq -> ignore (advance s); Ast.BinOp (Gte, left, parse_addition s)
  | _ -> left

and parse_addition s =
  let left = parse_multiplication s in
  match peek s with
  | Token.Plus -> ignore (advance s); Ast.BinOp (Add, left, parse_addition s)
  | Token.Minus -> ignore (advance s); Ast.BinOp (Sub, left, parse_addition s)
  | Token.DotDot -> ignore (advance s); Ast.BinOp (Concat, left, parse_addition s)
  | _ -> left

and parse_multiplication s =
  let left = parse_unary s in
  match peek s with
  | Token.Star -> ignore (advance s); Ast.BinOp (Mul, left, parse_multiplication s)
  | Token.Slash -> ignore (advance s); Ast.BinOp (Div, left, parse_multiplication s)
  | Token.Percent -> ignore (advance s); Ast.BinOp (Mod, left, parse_multiplication s)
  | _ -> left

and parse_unary s =
  match peek s with
  | Token.Minus -> ignore (advance s); Ast.UnaryOp (Neg, parse_unary s)
  | Token.Not -> ignore (advance s); Ast.UnaryOp (Not, parse_unary s)
  | _ -> parse_postfix s

and parse_postfix s =
  let expr = parse_primary s in
  match peek s with
  | Token.LBracket ->
    ignore (advance s);
    let idx = parse_expr s in
    expect s Token.RBracket;
    Ast.Index (expr, idx)
  | _ -> expr

and parse_primary s =
  match peek s with
  | Token.IntLit n -> ignore (advance s); Ast.IntLit n
  | Token.FloatLit f -> ignore (advance s); Ast.FloatLit f
  | Token.StringLit str -> ignore (advance s); Ast.StringLit str
  | Token.True -> ignore (advance s); Ast.BoolLit true
  | Token.False -> ignore (advance s); Ast.BoolLit false
  | Token.Nil -> ignore (advance s); Ast.Nil
  | Token.LBrace -> parse_table s
  | Token.LParen ->
    ignore (advance s);
    let e = parse_expr s in
    expect s Token.RParen;
    e
  | Token.Ident name ->
    ignore (advance s);
    if peek s = Token.LParen then parse_call s name
    else Ast.Ident name
  | _ -> failwith (Printf.sprintf "line %d: unexpected token in expression" s.line)

and parse_call s name =
  expect s Token.LParen;
  let args = ref [] in
  if peek s <> Token.RParen then begin
    args := [parse_expr s];
    while peek s = Token.Comma do
      ignore (advance s);
      args := parse_expr s :: !args
    done
  end;
  expect s Token.RParen;
  Ast.Call (name, List.rev !args)

and parse_table s =
  expect s Token.LBrace;
  let elems = ref [] in
  skip_newlines s;
  if peek s <> Token.RBrace then begin
    elems := [parse_expr s];
    while peek s = Token.Comma do
      ignore (advance s);
      skip_newlines s;
      if peek s <> Token.RBrace then
        elems := parse_expr s :: !elems
    done
  end;
  skip_newlines s;
  expect s Token.RBrace;
  Ast.TableLit (List.rev !elems)

let rec parse_stmt s =
  skip_newlines s;
  match peek s with
  | Token.Local -> parse_local s
  | Token.If -> parse_if s
  | Token.While -> parse_while s
  | Token.For -> parse_for s
  | Token.Return -> parse_return s
  | Token.Ident _ -> parse_assign_or_expr s
  | _ -> parse_expr_stmt s

and parse_local s =
  expect s Token.Local;
  let name = match advance s with Token.Ident n -> n | _ -> failwith (Printf.sprintf "line %d: expected identifier" s.line) in
  expect s Token.Eq;
  let expr = parse_expr s in
  Ast.LocalDecl (name, expr)

and parse_if s =
  expect s Token.If;
  let cond = parse_expr s in
  expect s Token.Then;
  let body = parse_block_if s in
  let else_body =
    if peek s = Token.ElseIf then
      [parse_elseif s]
    else if peek s = Token.Else then begin
      ignore (advance s);
      let b = parse_block_if s in
      expect s Token.End;
      b
    end else begin
      expect s Token.End;
      []
    end
  in
  Ast.IfStmt (cond, body, else_body)

and parse_elseif s =
  expect s Token.ElseIf;
  let cond = parse_expr s in
  expect s Token.Then;
  let body = parse_block_if s in
  let else_body =
    if peek s = Token.ElseIf then
      [parse_elseif s]
    else if peek s = Token.Else then begin
      ignore (advance s);
      let b = parse_block_if s in
      expect s Token.End;
      b
    end else begin
      expect s Token.End;
      []
    end
  in
  Ast.IfStmt (cond, body, else_body)

and parse_block_if s =
  let stmts = ref [] in
  skip_newlines s;
  while peek s <> Token.End && peek s <> Token.Else && peek s <> Token.ElseIf && peek s <> Token.Eof do
    stmts := parse_stmt s :: !stmts;
    skip_newlines s
  done;
  List.rev !stmts

and parse_while s =
  expect s Token.While;
  let cond = parse_expr s in
  expect s Token.Do;
  let body = parse_block s in
  expect s Token.End;
  Ast.WhileStmt (cond, body)

and parse_for s =
  expect s Token.For;
  let name = match advance s with Token.Ident n -> n | _ -> failwith (Printf.sprintf "line %d: expected identifier" s.line) in
  expect s Token.Eq;
  let start = parse_expr s in
  expect s Token.Comma;
  let finish = parse_expr s in
  expect s Token.Do;
  let body = parse_block s in
  expect s Token.End;
  Ast.ForStmt (name, start, finish, body)

and parse_return s =
  expect s Token.Return;
  let expr = parse_expr s in
  Ast.Return expr

and parse_assign_or_expr s =
  let name = match advance s with Token.Ident n -> n | _ -> failwith (Printf.sprintf "line %d: expected identifier" s.line) in
  if peek s = Token.Eq then begin
    ignore (advance s);
    let expr = parse_expr s in
    Ast.Assign (name, expr)
  end else if peek s = Token.LBracket then begin
    ignore (advance s);
    let idx = parse_expr s in
    expect s Token.RBracket;
    expect s Token.Eq;
    let expr = parse_expr s in
    Ast.IndexAssign (Ast.Ident name, idx, expr)
  end else if peek s = Token.LParen then begin
    let call = parse_call s name in
    Ast.ExprStmt call
  end else
    Ast.ExprStmt (Ast.Ident name)

and parse_expr_stmt s =
  let e = parse_expr s in
  Ast.ExprStmt e

and parse_block s =
  let stmts = ref [] in
  skip_newlines s;
  while peek s <> Token.End && peek s <> Token.Else && peek s <> Token.Eof do
    stmts := parse_stmt s :: !stmts;
    skip_newlines s
  done;
  List.rev !stmts

let parse_include s =
  expect s Token.Hash;
  match advance s with
  | Token.Ident "include" ->
    let path = match advance s with Token.StringLit p -> p | _ -> failwith (Printf.sprintf "line %d: expected string after #include" s.line) in
    path
  | _ -> failwith (Printf.sprintf "line %d: expected 'include' after #" s.line)

let parse_func s =
  expect s Token.Func;
  let name = match advance s with Token.Ident n -> n | _ -> failwith (Printf.sprintf "line %d: expected function name" s.line) in
  expect s Token.LParen;
  let params = ref [] in
  if peek s <> Token.RParen then begin
    params := [(match advance s with Token.Ident n -> n | _ -> failwith (Printf.sprintf "line %d: expected param name" s.line))];
    while peek s = Token.Comma do
      ignore (advance s);
      params := (match advance s with Token.Ident n -> n | _ -> failwith (Printf.sprintf "line %d: expected param name" s.line)) :: !params
    done
  end;
  expect s Token.RParen;
  skip_newlines s;
  let body = parse_block s in
  expect s Token.End;
  { Ast.name; params = List.rev !params; body }

let parse tokens : Ast.program =
  let s = create tokens in
  let includes = ref [] in
  let functions = ref [] in
  skip_newlines s;
  while peek s <> Token.Eof do
    skip_newlines s;
    if peek s = Token.Eof then ()
    else if peek s = Token.Hash then
      includes := parse_include s :: !includes
    else if peek s = Token.Func then
      functions := parse_func s :: !functions
    else
      failwith (Printf.sprintf "line %d: expected #include or func at top level" s.line);
    skip_newlines s
  done;
  { includes = List.rev !includes; functions = List.rev !functions }
