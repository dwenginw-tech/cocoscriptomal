let rec constant_fold (expr : Ast.expr) : Ast.expr =
  match expr with
  | Ast.BinOp (op, Ast.IntLit a, Ast.IntLit b) ->
    (match op with
     | Ast.Add -> Ast.IntLit (a + b)
     | Ast.Sub -> Ast.IntLit (a - b)
     | Ast.Mul -> Ast.IntLit (a * b)
     | Ast.Div when b <> 0 -> Ast.IntLit (a / b)
     | Ast.Mod when b <> 0 -> Ast.IntLit (a mod b)
     | Ast.Eq -> Ast.BoolLit (a = b)
     | Ast.Neq -> Ast.BoolLit (a <> b)
     | Ast.Lt -> Ast.BoolLit (a < b)
     | Ast.Gt -> Ast.BoolLit (a > b)
     | Ast.Lte -> Ast.BoolLit (a <= b)
     | Ast.Gte -> Ast.BoolLit (a >= b)
     | _ -> Ast.BinOp (op, Ast.IntLit a, Ast.IntLit b))
  | Ast.BinOp (op, Ast.FloatLit a, Ast.FloatLit b) ->
    (match op with
     | Ast.Add -> Ast.FloatLit (a +. b)
     | Ast.Sub -> Ast.FloatLit (a -. b)
     | Ast.Mul -> Ast.FloatLit (a *. b)
     | Ast.Div -> Ast.FloatLit (a /. b)
     | _ -> Ast.BinOp (op, Ast.FloatLit a, Ast.FloatLit b))
  | Ast.BinOp (Ast.Concat, Ast.StringLit a, Ast.StringLit b) ->
    Ast.StringLit (a ^ b)
  | Ast.BinOp (op, l, r) ->
    Ast.BinOp (op, constant_fold l, constant_fold r)
  | Ast.UnaryOp (Ast.Neg, Ast.IntLit n) -> Ast.IntLit (-n)
  | Ast.UnaryOp (Ast.Neg, Ast.FloatLit f) -> Ast.FloatLit (-.f)
  | Ast.UnaryOp (Ast.Not, Ast.BoolLit b) -> Ast.BoolLit (not b)
  | Ast.UnaryOp (op, e) -> Ast.UnaryOp (op, constant_fold e)
  | Ast.Call (name, args) -> Ast.Call (name, List.map constant_fold args)
  | Ast.Index (arr, idx) -> Ast.Index (constant_fold arr, constant_fold idx)
  | Ast.TableLit elems -> Ast.TableLit (List.map constant_fold elems)
  | Ast.FieldGet (obj, field) -> Ast.FieldGet (constant_fold obj, field)
  | Ast.MethodCall (obj, meth, args) ->
    Ast.MethodCall (constant_fold obj, meth, List.map constant_fold args)
  | Ast.NewObj (cls, args) -> Ast.NewObj (cls, List.map constant_fold args)
  | Ast.Lambda (params, body) -> Ast.Lambda (params, List.map optimize_stmt body)
  | e -> e

and optimize_stmt (stmt : Ast.stmt) : Ast.stmt =
  match stmt with
  | Ast.LocalDecl (name, expr) -> Ast.LocalDecl (name, constant_fold expr)
  | Ast.Assign (name, expr) -> Ast.Assign (name, constant_fold expr)
  | Ast.ExprStmt expr -> Ast.ExprStmt (constant_fold expr)
  | Ast.Return expr -> Ast.Return (constant_fold expr)
  | Ast.IfStmt (Ast.BoolLit true, _then_body, _) ->
    Ast.ExprStmt (Ast.IntLit 0)
  | Ast.IfStmt (Ast.BoolLit false, _, _else_body) ->
    Ast.ExprStmt (Ast.IntLit 0)
  | Ast.IfStmt (cond, then_body, else_body) ->
    Ast.IfStmt (constant_fold cond,
                List.map optimize_stmt then_body,
                List.map optimize_stmt else_body)
  | Ast.WhileStmt (cond, body) ->
    Ast.WhileStmt (constant_fold cond, List.map optimize_stmt body)
  | Ast.ForStmt (var, start, finish, body) ->
    Ast.ForStmt (var, constant_fold start, constant_fold finish,
                 List.map optimize_stmt body)
  | Ast.ForEach (var, col, body) ->
    Ast.ForEach (var, constant_fold col, List.map optimize_stmt body)
  | Ast.FieldSet (obj, field, value) ->
    Ast.FieldSet (constant_fold obj, field, constant_fold value)
  | Ast.IndexAssign (arr, idx, value) ->
    Ast.IndexAssign (constant_fold arr, constant_fold idx, constant_fold value)
  | Ast.MultiDecl pairs ->
    Ast.MultiDecl (List.map (fun (n, e) -> (n, constant_fold e)) pairs)
  | s -> s

let optimize_function (f : Ast.func_def) : Ast.func_def =
  { f with body = List.map optimize_stmt f.body }

let optimize_program (prog : Ast.program) : Ast.program =
  { prog with functions = List.map optimize_function prog.functions }
