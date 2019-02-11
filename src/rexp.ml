type t =
    Lit of Value.t
  | Stmts of t list
  | VarAssign of string * t
  | VarRef of string
  | If of t * t * t
  | While of t * t
  | FuncDef of string * string list * t
  | FuncCall of string * t list
  | AryNew of t list
  | AryRef of t * t
  | AryAssign of t * t * t
  | HashNew of t list
  | And of t * t
  | Or of t * t

let rec show = function
    Lit l -> Value.show l
  | Stmts ss -> ss |> List.map show |> String.concat "\n"
  | VarAssign (v, e) ->
    let e = show e in
    {j|$(v) = $(e)|j}
  | VarRef v -> v
  | If (c, a, b) ->
    let c = show c and a = show a and b = show b in
    {j|(if $(c) then $(a) else $(b))|j}
  | While (c, e) ->
    let c = show c and e = show e in
    {j|while $(c) do $(e) end|j}
  | FuncDef (name, args, e) ->
    let args = match args with
        [] -> ""
      | arg :: args -> List.fold_left (fun acc a -> acc ^ ", " ^ a) arg args
    and e = show e in
    {j|def $(name)($(args)) do $(e) end|j}
  | FuncCall (name, params) ->
    let params =
      params
      |> List.map show
      |> String.concat ", " in
    {j|$(name)($(params))|j}
  | AryNew es ->
    let es =
      es
      |> List.map show
      |> String.concat ", " in
    {j|[$(es)]|j}
  | AryRef (arr, idx) ->
    let arr = show arr and idx = show idx in
    {j|$(arr)[$(idx)]|j}
  | AryAssign (arr, idx, e) ->
    let arr = show arr and idx = show idx and v = show e in
    {j|$(arr)[$(idx)] = $(v)|j}
  | HashNew es ->
    let rec go = function
        [] -> []
      | k :: v :: rest ->
        let k = show k and v = show v in
        {j|$(k) => $(v)|j} :: go rest
      | _ -> [] in
    let es = go es |> String.concat ", " in
    {j|{$(es)}|j}
  | And (l, r) ->
    let l = show l and r = show r in
    {j|$(l) && $(r)|j}
  | Or (l, r) ->
    let l = show l and r = show r in
    {j|$(l) || $(r)|j}