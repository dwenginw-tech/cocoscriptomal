(* parser.ml — recursive descent parser for CocoScript
   Loosely based on Lua's grammar with C-style includes.
   Precedence climbing: or < and < cmp < add < mul < unary < postfix
   Supports if/elseif/else, while/do, for, func definitions. *)

type pstate = {
  tokens : Token.t array;
  mutable pos : int;
  mutable line : int;
}

let make_parser tokens =
  { tokens = Array.of_list tokens; pos = 0; line = 1 }

(* Return whichever token we're sitting on without consuming it. *)
let current p =
  if p.pos >= Array.length p.tokens then Token.Eof
  else p.tokens.(p.pos)

let eat p =
  let tok = current p in
  p.pos <- p.pos + 1;
  tok

let expect p tok =
  let got = eat p in
  if got <> tok then
    failwith (Printf.sprintf "line %d: expected token, got something else" p.line)

(* Burn through any newlines, bumping the line counter as we go. *)
let skip_nl p =
  while current p = Token.Newline do ignore (eat p); p.line <- p.line + 1 done

(* ---- Expression parsing (Pratt-style / precedence climbing) ----
   Each level calls the next-higher-precedence level for its operands,
   then checks if the current token belongs to its own precedence tier. *)

let rec expr p = or_expr p

(* right-associative for now, might change later *)
and or_expr p =
  let lhs = and_expr p in
  if current p = Token.Or then
    (ignore (eat p); Ast.BinOp (Or, lhs, or_expr p))
  else lhs

and and_expr p =
  let lhs = cmp_expr p in
  if current p = Token.And then
    (ignore (eat p); Ast.BinOp (And, lhs, and_expr p))
  else lhs

(* Comparisons are non-associative — we only parse one operator. *)
and cmp_expr p =
  let lhs = add_expr p in
  match current p with
  | Token.EqEq  -> ignore (eat p); Ast.BinOp (Eq,  lhs, add_expr p)
  | Token.NotEq -> ignore (eat p); Ast.BinOp (Neq, lhs, add_expr p)
  | Token.Lt    -> ignore (eat p); Ast.BinOp (Lt,  lhs, add_expr p)
  | Token.Gt    -> ignore (eat p); Ast.BinOp (Gt,  lhs, add_expr p)
  | Token.LtEq  -> ignore (eat p); Ast.BinOp (Lte, lhs, add_expr p)
  | Token.GtEq  -> ignore (eat p); Ast.BinOp (Gte, lhs, add_expr p)
  | _ -> lhs

and add_expr p =
  let lhs = mul_expr p in
  match current p with
  | Token.Plus   -> ignore (eat p); Ast.BinOp (Add,    lhs, add_expr p)
  | Token.Minus  -> ignore (eat p); Ast.BinOp (Sub,    lhs, add_expr p)
  | Token.DotDot -> ignore (eat p); Ast.BinOp (Concat, lhs, add_expr p)
  | _ -> lhs

and mul_expr p =
  let lhs = unary_expr p in
  match current p with
  | Token.Star    -> ignore (eat p); Ast.BinOp (Mul, lhs, mul_expr p)
  | Token.Slash   -> ignore (eat p); Ast.BinOp (Div, lhs, mul_expr p)
  | Token.Percent -> ignore (eat p); Ast.BinOp (Mod, lhs, mul_expr p)
  | _ -> lhs

and unary_expr p =
  match current p with
  | Token.Minus -> ignore (eat p); Ast.UnaryOp (Neg, unary_expr p)
  | Token.Not   -> ignore (eat p); Ast.UnaryOp (Not, unary_expr p)
  | _ -> postfix_expr p

(* Postfix: table indexing with brackets *)
and postfix_expr p =
  let e = atom p in
  match current p with
  | Token.LBracket ->
    ignore (eat p);
    let idx = expr p in
    expect p Token.RBracket;
    Ast.Index (e, idx)
  | _ -> e

and atom p =
  match current p with
  | Token.IntLit n    -> ignore (eat p); Ast.IntLit n
  | Token.FloatLit f  -> ignore (eat p); Ast.FloatLit f
  | Token.StringLit s -> ignore (eat p); Ast.StringLit s
  | Token.True  -> ignore (eat p); Ast.BoolLit true
  | Token.False -> ignore (eat p); Ast.BoolLit false
  | Token.Nil   -> ignore (eat p); Ast.Nil
  | Token.LBrace -> table_lit p
  | Token.LParen ->
    ignore (eat p);
    let inner = expr p in
    expect p Token.RParen;
    inner
  | Token.Ident name ->
    ignore (eat p);
    if current p = Token.LParen then call_args p name
    else Ast.Ident name
  | _ -> failwith (Printf.sprintf "line %d: unexpected token in expression" p.line)

and call_args p fname =
  expect p Token.LParen;
  let args = ref [] in
  if current p <> Token.RParen then begin
    args := [expr p];
    while current p = Token.Comma do
      ignore (eat p);
      args := expr p :: !args
    done
  end;
  expect p Token.RParen;
  Ast.Call (fname, List.rev !args)

and table_lit p =
  expect p Token.LBrace;
  let elems = ref [] in
  skip_nl p;
  if current p <> Token.RBrace then begin
    elems := [expr p];
    while current p = Token.Comma do
      ignore (eat p);
      skip_nl p;
      if current p <> Token.RBrace then
        elems := expr p :: !elems
    done
  end;
  skip_nl p;
  expect p Token.RBrace;
  Ast.TableLit (List.rev !elems)

(* ---- Statement parsing ---- *)

let rec stmt p =
  skip_nl p;
  match current p with
  | Token.Local  -> local_decl p
  | Token.If     -> if_stmt p
  | Token.While  -> while_stmt p
  | Token.For    -> for_stmt p
  | Token.Return -> return_stmt p
  | Token.Ident _ -> ident_stmt p
  | _ -> expr_stmt p

and local_decl p =
  expect p Token.Local;
  let name = match eat p with
    | Token.Ident n -> n
    | _ -> failwith (Printf.sprintf "line %d: expected identifier" p.line)
  in
  expect p Token.Eq;
  let value = expr p in
  Ast.LocalDecl (name, value)

(* elseif desugars into nested if — same as lua *)
and if_stmt p =
  expect p Token.If;
  let cond = expr p in
  expect p Token.Then;
  let body = if_block p in
  let else_branch =
    if current p = Token.ElseIf then
      [elseif_chain p]
    else if current p = Token.Else then begin
      ignore (eat p);
      let b = if_block p in
      expect p Token.End;
      b
    end else begin
      expect p Token.End;
      []
    end
  in
  Ast.IfStmt (cond, body, else_branch)

and elseif_chain p =
  expect p Token.ElseIf;
  let cond = expr p in
  expect p Token.Then;
  let body = if_block p in
  let else_branch =
    if current p = Token.ElseIf then
      [elseif_chain p]
    else if current p = Token.Else then begin
      ignore (eat p);
      let b = if_block p in
      expect p Token.End;
      b
    end else begin
      expect p Token.End;
      []
    end
  in
  Ast.IfStmt (cond, body, else_branch)

(* block ends at `end`, `else`, or `elseif` token *)
and if_block p =
  let acc = ref [] in
  skip_nl p;
  while current p <> Token.End && current p <> Token.Else && current p <> Token.ElseIf && current p <> Token.Eof do
    acc := stmt p :: !acc;
    skip_nl p
  done;
  List.rev !acc

and while_stmt p =
  expect p Token.While;
  let cond = expr p in
  expect p Token.Do;
  let body = block p in
  expect p Token.End;
  Ast.WhileStmt (cond, body)

and for_stmt p =
  expect p Token.For;
  let var = match eat p with
    | Token.Ident n -> n
    | _ -> failwith (Printf.sprintf "line %d: expected identifier" p.line)
  in
  expect p Token.Eq;
  let lo = expr p in
  expect p Token.Comma;
  let hi = expr p in
  expect p Token.Do;
  let body = block p in
  expect p Token.End;
  Ast.ForStmt (var, lo, hi, body)

and return_stmt p =
  expect p Token.Return;
  let value = expr p in
  Ast.Return value

(* Starts with an identifier — could be assignment, index-assign, call, or bare ident *)
and ident_stmt p =
  let name = match eat p with
    | Token.Ident n -> n
    | _ -> failwith (Printf.sprintf "line %d: expected identifier" p.line)
  in
  if current p = Token.Eq then begin
    ignore (eat p);
    let rhs = expr p in
    Ast.Assign (name, rhs)
  end else if current p = Token.LBracket then begin
    ignore (eat p);
    let idx = expr p in
    expect p Token.RBracket;
    expect p Token.Eq;
    let rhs = expr p in
    Ast.IndexAssign (Ast.Ident name, idx, rhs)
  end else if current p = Token.LParen then begin
    let c = call_args p name in
    Ast.ExprStmt c
  end else
    Ast.ExprStmt (Ast.Ident name)

and expr_stmt p =
  let e = expr p in
  Ast.ExprStmt e

and block p =
  let acc = ref [] in
  skip_nl p;
  while current p <> Token.End && current p <> Token.Else && current p <> Token.Eof do
    acc := stmt p :: !acc;
    skip_nl p
  done;
  List.rev !acc

(* ---- Top-level constructs ---- *)

let include_directive p =
  expect p Token.Hash;
  match eat p with
  | Token.Ident "include" ->
    let path = match eat p with
      | Token.StringLit s -> s
      | _ -> failwith (Printf.sprintf "line %d: expected string after #include" p.line)
    in
    path
  | _ -> failwith (Printf.sprintf "line %d: expected 'include' after #" p.line)

let func_def p =
  expect p Token.Func;
  let name = match eat p with
    | Token.Ident n -> n
    | _ -> failwith (Printf.sprintf "line %d: expected function name" p.line)
  in
  expect p Token.LParen;
  let params = ref [] in
  if current p <> Token.RParen then begin
    params := [(match eat p with Token.Ident n -> n | _ -> failwith (Printf.sprintf "line %d: expected param name" p.line))];
    while current p = Token.Comma do
      ignore (eat p);
      params := (match eat p with Token.Ident n -> n | _ -> failwith (Printf.sprintf "line %d: expected param name" p.line)) :: !params
    done
  end;
  expect p Token.RParen;
  skip_nl p;
  let body = block p in
  expect p Token.End;
  { Ast.name; params = List.rev !params; body }

(* Main entry point — called from compiler.ml as Parser.parse.
   Walks the top-level token stream looking for #include directives
   and function definitions, nothing else is allowed at file scope. *)
let parse tokens : Ast.program =
  let p = make_parser tokens in
  let includes = ref [] in
  let funcs = ref [] in
  skip_nl p;
  while current p <> Token.Eof do
    skip_nl p;
    if current p = Token.Eof then ()
    else if current p = Token.Hash then
      includes := include_directive p :: !includes
    else if current p = Token.Func then
      funcs := func_def p :: !funcs
    else
      failwith (Printf.sprintf "line %d: expected #include or func at top level" p.line);
    skip_nl p
  done;
  { includes = List.rev !includes; functions = List.rev !funcs }
