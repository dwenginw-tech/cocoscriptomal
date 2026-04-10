type pstate = {
  tokens : Token.t array;
  mutable pos : int;
  mutable line : int;
  mutable column : int;
  filename : string;
}

let make_parser ?(filename="<input>") tokens =
  { tokens = Array.of_list tokens; pos = 0; line = 1; column = 1; filename }

let current p =
  if p.pos >= Array.length p.tokens then Token.Eof
  else p.tokens.(p.pos)

let eat p =
  let tok = current p in
  p.pos <- p.pos + 1;
  (match tok with
   | Token.Newline -> p.line <- p.line + 1; p.column <- 1
   | _ -> p.column <- p.column + 1);
  tok

let expect p tok =
  let got = eat p in
  if got <> tok then
    let loc = Error.make_location p.filename p.line p.column in
    Error.syntax_error
      ~location:loc
      ~hint:(Printf.sprintf "Expected %s" (Token.to_string tok))
      (Printf.sprintf "Unexpected token: %s" (Token.to_string got))

let skip_nl p =
  while current p = Token.Newline do ignore (eat p) done

let block_ref : (pstate -> Ast.stmt list) ref = ref (fun _ -> [])

let rec expr p = or_expr p

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

and postfix_expr p =
  let e = ref (atom p) in
  let continue = ref true in
  while !continue do
    match current p with
    | Token.LBracket ->
      ignore (eat p);
      let idx = expr p in
      expect p Token.RBracket;
      e := Ast.Index (!e, idx)
    | Token.Dot ->
      ignore (eat p);
      let field = match eat p with
        | Token.Ident n -> n
        | _ -> failwith (Printf.sprintf "line %d: expected field name after dot" p.line)
      in
      if current p = Token.LParen then begin
        ignore (eat p);
        let args = ref [] in
        if current p <> Token.RParen then begin
          args := [expr p];
          while current p = Token.Comma do
            ignore (eat p);
            args := expr p :: !args
          done
        end;
        expect p Token.RParen;
        e := Ast.MethodCall (!e, field, List.rev !args)
      end else
        e := Ast.FieldGet (!e, field)
    | _ -> continue := false
  done;
  !e

and lambda_expr p =
  ignore (eat p);
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
  let body = !block_ref p in
  expect p Token.End;
  Ast.Lambda (List.rev !params, body)

and atom p =
  match current p with
  | Token.IntLit n    -> ignore (eat p); Ast.IntLit n
  | Token.FloatLit f  -> ignore (eat p); Ast.FloatLit f
  | Token.StringLit s -> ignore (eat p); Ast.StringLit s
  | Token.True  -> ignore (eat p); Ast.BoolLit true
  | Token.False -> ignore (eat p); Ast.BoolLit false
  | Token.Nil   -> ignore (eat p); Ast.Nil
  | Token.Func -> lambda_expr p
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
  | Token.Self ->
    ignore (eat p);
    Ast.Ident "self"
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

let rec stmt p =
  skip_nl p;
  match current p with
  | Token.Local  -> local_decl p
  | Token.If     -> if_stmt p
  | Token.While  -> while_stmt p
  | Token.For    -> for_stmt p
  | Token.Return -> return_stmt p
  | Token.Break -> ignore (eat p); Ast.Break
  | Token.Ident _ -> ident_stmt p
  | Token.Self -> self_stmt p
  | _ -> expr_stmt p

and local_decl p =
  expect p Token.Local;
  let name = match eat p with
    | Token.Ident n -> n
    | _ -> failwith (Printf.sprintf "line %d: expected identifier" p.line)
  in
  if current p = Token.Comma then begin
    let names = ref [name] in
    while current p = Token.Comma do
      ignore (eat p);
      names := (match eat p with Token.Ident n -> n | _ -> failwith (Printf.sprintf "line %d: expected identifier" p.line)) :: !names
    done;
    expect p Token.Eq;
    let exprs = ref [expr p] in
    while current p = Token.Comma do
      ignore (eat p);
      exprs := expr p :: !exprs
    done;
    let pairs = List.combine (List.rev !names) (List.rev !exprs) in
    Ast.MultiDecl pairs
  end else begin
    expect p Token.Eq;
    let e = expr p in
    Ast.LocalDecl (name, e)
  end

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
  if current p = Token.In then begin
    ignore (eat p);
    let collection = expr p in
    expect p Token.Do;
    let body = block p in
    expect p Token.End;
    Ast.ForEach (var, collection, body)
  end else begin
    expect p Token.Eq;
    let lo = expr p in
    expect p Token.Comma;
    let hi = expr p in
    expect p Token.Do;
    let body = block p in
    expect p Token.End;
    Ast.ForStmt (var, lo, hi, body)
  end

and return_stmt p =
  expect p Token.Return;
  let value = expr p in
  Ast.Return value

and ident_stmt p =
  let name = match eat p with
    | Token.Ident n -> n
    | _ -> failwith (Printf.sprintf "line %d: expected identifier" p.line)
  in
  if current p = Token.Dot then begin
    ignore (eat p);
    let field = match eat p with
      | Token.Ident n -> n
      | _ -> failwith (Printf.sprintf "line %d: expected field name after dot" p.line)
    in
    if current p = Token.Eq then begin
      ignore (eat p);
      let value = expr p in
      Ast.FieldSet (Ast.Ident name, field, value)
    end else if current p = Token.LParen then begin
      ignore (eat p);
      let args = ref [] in
      if current p <> Token.RParen then begin
        args := [expr p];
        while current p = Token.Comma do
          ignore (eat p);
          args := expr p :: !args
        done
      end;
      expect p Token.RParen;
      Ast.ExprStmt (Ast.MethodCall (Ast.Ident name, field, List.rev !args))
    end else
      Ast.ExprStmt (Ast.FieldGet (Ast.Ident name, field))
  end else if current p = Token.Eq then begin
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

and self_stmt p =
  ignore (eat p);
  if current p = Token.Dot then begin
    ignore (eat p);
    let field = match eat p with
      | Token.Ident n -> n
      | _ -> failwith (Printf.sprintf "line %d: expected field name after self." p.line)
    in
    if current p = Token.Eq then begin
      ignore (eat p);
      let value = expr p in
      Ast.FieldSet (Ast.Ident "self", field, value)
    end else if current p = Token.LParen then begin
      ignore (eat p);
      let args = ref [] in
      if current p <> Token.RParen then begin
        args := [expr p];
        while current p = Token.Comma do
          ignore (eat p);
          args := expr p :: !args
        done
      end;
      expect p Token.RParen;
      Ast.ExprStmt (Ast.MethodCall (Ast.Ident "self", field, List.rev !args))
    end else
      Ast.ExprStmt (Ast.FieldGet (Ast.Ident "self", field))
  end else
    Ast.ExprStmt (Ast.Ident "self")

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

let () = block_ref := block

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

(* methods get mangled names (ClassName_method) with self as first param *)
let parse_class p =
  expect p Token.Class;
  let name = match eat p with
    | Token.Ident n -> n
    | _ -> failwith (Printf.sprintf "line %d: expected class name" p.line)
  in
  skip_nl p;
  let methods = ref [] in
  while current p <> Token.End && current p <> Token.Eof do
    skip_nl p;
    if current p = Token.Func then
      methods := func_def p :: !methods
    else
      skip_nl p
  done;
  expect p Token.End;
  let method_infos = List.rev_map (fun (f : Ast.func_def) ->
    { Class.mname = f.name; params = f.params; body = f.body }
  ) (List.rev !methods) in
  let _cls = Class.register_class name method_infos in
  List.rev_map (fun (f : Ast.func_def) ->
    { f with
      name = name ^ "_" ^ f.name;
      params = if List.mem "self" f.params then f.params else "self" :: f.params }
  ) (List.rev !methods)

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
    else if current p = Token.Class then
      funcs := parse_class p @ !funcs
    else
      failwith (Printf.sprintf "line %d: expected #include, func, or class at top level" p.line);
    skip_nl p
  done;
  { includes = List.rev !includes; functions = List.rev !funcs }
