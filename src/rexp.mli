type t =
    Lit of Value.t
  | Stmts of t list
  | VarAssign of string * t
  | VarRef of string
  | If of t * t * t
  | While of t * t
  | FuncDef of string * string list * t (* function name * parameters * expression *)
  | FuncCall of string * t list
  | AryNew of t list
  | AryRef of t * t (* array * index *)
  | AryAssign of t * t * t (* array * index * value *)
  | HashNew of t list (* (key ; value)* *)
  | And of t * t
  | Or of t * t

val show : t -> string