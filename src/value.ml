module MMap = Belt.MutableMap

module rec Value : sig

  type t =
      Nil
    | False
    | True
    | Int of int
    | Float of float
    | String of string
    | Array of t array
    | Hash of (Value.t, Value.t, Cmp.identity) MMap.t
  
  val show : t -> string
end = struct

  type t =
      Nil
    | False
    | True
    | Int of int
    | Float of float
    | String of string
    | Array of t array
    | Hash of (Value.t, Value.t, Cmp.identity) MMap.t

  let rec show = function
      Nil -> "nil"
    | False -> "false"
    | True -> "true"
    | Int i -> string_of_int i
    | Float f -> string_of_float f
    | String s -> {j|"$(s)"|j}
    | Array vs ->
      let v =
        vs
        |> Array.map show
        |> Array.to_list
        |> String.concat ", " in
      {j|[$(v)]|j}
    | Hash h ->
      let v =
        h
        |> MMap.toList
        |> List.map (fun (k, v) ->
          let k = show k and v = show v in
          {j|$(k) => $(v)|j}
        )
        |> String.concat ", " in
      {j|{$(v)}|j}
end
and Cmp : Belt.Id.Comparable with type t = Value.t = Belt.Id.MakeComparable(struct
  open Value

  type t = Value.t

  let cmp a b = match a, b with
      Nil, Nil -> 0
    | Nil, _ -> -1
    | _, Nil -> 1
    | False, False -> 0
    | False, _ -> -1
    | _, False -> 1
    | True, True -> 0
    | True, _ -> -1
    | _, True -> 1
    | Int a, Int b -> compare a b
    | Int _, _ -> -1
    | _, Int _ -> 1
    | Float a, Float b -> compare a b
    | Float _, _ -> -1
    | _, Float _ -> 1
    | String a, String b -> compare a b
    | String _, _ -> -1
    | _, String _ -> 1
    | Array a, Array b -> compare a b
    | Array _, _ -> -1
    | _, Array _ -> 1
    | Hash a, Hash b -> compare a b
end)

include Value