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

let show tree =
  let indent nest str = String.make nest ' ' ^ str in
  let rec show nest = function
      Lit l -> Value.show l
    | Stmts ss -> ss |> List.map (show (nest + 2)) |> String.concat "\n"
    | VarAssign (v, e) ->
      let e = show (nest + 2) e |> indent nest in
{j|(let $(v)
$(e))|j}
    | VarRef v -> v
    | If (c, a, b) ->
      let c = show (nest + 2) c
      and a = show (nest + 2) a
      and b = show (nest + 2) b in
{j|(if
($(c))
($(a))
($(b))|j}
    | While (c, e) ->
      let c = show (nest + 2) c
      and e = show (nest + 2) e in
{j|(while
$(c)
$(e))|j}
    | FuncDef (name, args, e) ->
      let args = match args with
          [] -> ""
        | arg :: args -> List.fold_left (fun acc a -> acc ^ ", " ^ a) arg args
      and e = show (nest + 2) e in
{j|(def $(name) ($(args))
$(e))|j}
    | FuncCall (name, params) ->
      let params =
        params
        |> List.map (show (nest + 2))
        |> String.concat ", " in
{j|($(name)
  ($(params)))|j}
    | AryNew es ->
      let es =
        es
        |> List.map (show (nest + 2))
        |> String.concat ", " in
      {j|[$(es)]|j}
    | AryRef (arr, idx) ->
      let arr = show (nest + 2) arr
      and idx = show (nest + 2) idx in
      {j|$(arr)[$(idx)]|j}
    | AryAssign (arr, idx, e) ->
      let arr = show (nest + 2) arr
      and idx = show (nest + 2) idx
      and v = show (nest + 2) e in
{j|(let $(arr)[$(idx)]
  ($(v)))|j}
    | HashNew es ->
      let rec go = function
          [] -> []
        | k :: v :: rest ->
          let k = show (nest + 2) k
          and v = show (nest + 2) v in
          {j|$(k) => $(v)|j} :: go rest
        | _ -> [] in
      let es = go es |> String.concat ", " in
      {j|{$(es)}|j}
    | And (l, r) ->
      let l = show (nest + 2) l
      and r = show (nest + 2) r in
{j|(and
  ($(l))
  ($(r))|j}
    | Or (l, r) ->
      let l = show (nest + 2) l
      and r = show (nest + 2) r in
{j|(or
  ($(l))
  ($(r))|j} in
    show 0 tree
