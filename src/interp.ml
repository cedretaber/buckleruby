open Rexp
open Value

module Env = Belt.MutableMap.String
module Hash = Belt.MutableMap

module Array = struct
  include Array

  let expand = [%raw fun size init arr -> {|
    let i = arr.length;
    arr.length = size;
    for (; i < size; ++i) {
      arr[i] = init;
    }
    return arr;
  |}]
end

exception Invalid_VarRef of string
exception No_such_function of string
exception Invalid_argument_number of int * int * string
exception Invalid_slice_operation of Value.t
exception Invalid_array_index of Value.t
exception Invalid_func_call of string

type func_u = {
  args : string list;
  args_count : int;
  proc : Rexp.t
}

type ('input, 'output) func_b = 'input -> 'output -> Value.t list -> Value.t

let rec eval genv lenv input output = function
    Lit v -> v
  | Stmts [] -> Nil
  | Stmts [e] -> eval genv lenv input output e
  | Stmts (e :: es) ->
    begin
      eval genv lenv input output e |> ignore;
      eval genv lenv input output (Stmts es)
    end
  | VarAssign (var, e) ->
    let v = eval genv lenv input output e in
    begin
      Env.set lenv var v;
      v
    end
  | VarRef var ->
    begin match Env.get lenv var with
      None -> raise (Invalid_VarRef var)
    | Some v -> v
    end
  | If (c, a, b) ->
    begin match eval genv lenv input output c with
      Nil | False -> eval genv lenv input output b
    | _ -> eval genv lenv input output a
    end
  | While (c, s) ->
    begin match eval genv lenv input output c with
      Nil | False -> Nil
    | _ ->
      begin
        eval genv lenv input output s |> ignore;
        eval genv lenv input output (While (c, s))
      end
    end
  | FuncDef (name, args, proc) ->
    let args_count = List.length args in
    Env.set genv name (`UFunc { args; args_count; proc });
    Nil
  | FuncCall (name, params) ->
    begin match Env.get genv name with
      None -> raise (No_such_function name)
    | Some (`UFunc {args; args_count; proc}) ->
      let params_count = List.length params in
      if args_count <> params_count then
        raise (Invalid_argument_number (params_count, args_count, name))
      else
        let lenv' = Env.make () in
        begin
          Belt.List.zip args params
          |. Belt.List.forEach (fun (arg, param) -> Env.set lenv' arg @@ eval genv lenv input output param);
          eval genv lenv' input output proc
        end
    | Some (`BFunc proc) ->
      proc input output @@ List.map (eval genv lenv input output) params
    | _ -> raise (Invalid_func_call name)
    end
  | AryNew vs ->
    Array (vs |> List.map (eval genv lenv input output) |> Array.of_list)
  | AryRef (arr, idx) ->
    begin match eval genv lenv input output arr with
      Array arr ->
      begin match eval genv lenv input output idx with
        Int i ->
        if i < Array.length arr then
          arr.(i)
        else
          Nil
      | v -> raise (Invalid_array_index v)
      end
    | Hash hash ->
      let key = eval genv lenv input output idx in
      begin match Hash.get hash key with
        Some v -> v
      | None -> Nil
      end
    | v -> raise (Invalid_slice_operation v)
    end
  | AryAssign (arr, idx, e) ->
    begin match eval genv lenv input output arr with
      Array arr ->
      begin match eval genv lenv input output idx with
        Int i ->
        let v = eval genv lenv input output e in 
        if i < Array.length arr then begin
          arr.(i) <- v;
          v
        end else begin
          [@warning "-20"] Array.expand (i + 1) Nil arr |> ignore;
          arr.(i) <- v;
          v
        end
      | v -> raise (Invalid_array_index v)
      end
    | Hash hash ->
      let key = eval genv lenv input output idx
      and v = eval genv lenv input output e in
      begin
        Hash.set hash key v;
        v
      end
    | v -> raise (Invalid_slice_operation v)
    end
  | HashNew es ->
    let hash = Hash.make ~id:(module Cmp) in
    let rec go = function
        [] -> ()
      | k :: v :: rest ->
        Hash.set hash (eval genv lenv input output k) (eval genv lenv input output v);
        go rest
      | _ -> () in
    go es;
    Hash hash
  | And (l, r) ->
    begin match eval genv lenv input output l with
      (Nil | False) as falsy -> falsy
    | _ -> eval genv lenv input output r
    end
  | Or (l, r) ->
    begin match eval genv lenv input output l with
      Nil | False -> eval genv lenv input output r
    | truthy -> truthy
    end

module Builtin = struct

  exception Invalid_argument_number of string * int
  exception Invalid_argument_type of string * Value.t list
  exception Unexpected_type_exception of string * Value.t

  let p _ output args =
    let rec go = function
        [] -> Nil
      | v :: vs ->
        output#p (Value.show v ^ "\n");
        go vs in
    go args

  let gets input _ args =
    let params_length = List.length args in
    if params_length <> 0 then
      raise (Invalid_argument_number ("gets", params_length))
    else
      String (input#gets ())

  let plus _ _ = function
      [Int _ as i] -> i
    | [Float _ as f] -> f
    | [Int a; Int b] -> Int (a + b)
    | [Float a; Float b] -> Float (a +. b)
    | [Float a; Int b] -> Float (a +. float_of_int b)
    | [Int a; Float b] -> Float (float_of_int a +. b)
    | [String a; String b] -> String (a ^ b)
    | [Array a; Array b] -> Array (Array.append a b)
    | [a; b] -> raise (Invalid_argument_type ("+", [a; b]))
    | args -> raise (Invalid_argument_number ("+", List.length args))

  let minus _ _ = function
      [Int n] -> Int (-n)
    | [Float f] -> Float (-.f)
    | [Int a; Int b] -> Int (a - b)
    | [Float a; Float b] -> Float (a -. b)
    | [Float a; Int b] -> Float (a -. float_of_int b)
    | [Int a; Float b] -> Float (float_of_int a -. b)
    | [a; b] -> raise (Invalid_argument_type ("-", [a; b]))
    | args -> raise (Invalid_argument_number ("-", List.length args))
  
  let times _ _ = function
      [Int a; Int b] -> Int (a * b)
    | [Float a; Float b] -> Float (a *. b)
    | [Float a; Int b] -> Float (a *. float_of_int b)
    | [Int a; Float b] -> Float (float_of_int a *. b)
    | [String s; Int n] ->
      let strs = Belt.List.make n s in
      String (String.concat "" strs)
    | [a; b] -> raise (Invalid_argument_type ("*", [a; b]))
    | args -> raise (Invalid_argument_number ("*", List.length args))
  
  let div _ _ = function
      [Int a; Int b] -> Int (a / b)
    | [Float a; Float b] -> Float (a /. b)
    | [Float a; Int b] -> Float (a /. float_of_int b)
    | [Int a; Float b] -> Float (float_of_int a /. b)
    | [a; b] -> raise (Invalid_argument_type ("/", [a; b]))
    | args -> raise (Invalid_argument_number ("/", List.length args))
  
  let mod' _ _ = function
      [Int a; Int b] -> Int (a mod b)
    | [Float a; Float b] -> Float (mod_float a b)
    | [Float a; Int b] -> Float (mod_float a @@ float_of_int b)
    | [Int a; Float b] -> Float (mod_float (float_of_int a) b)
    | [a; b] -> raise (Invalid_argument_type ("%", [a; b]))
    | args -> raise (Invalid_argument_number ("%", List.length args))
  
  let expt _ _ = function
      [Int a; Int b] -> Int (int_of_float (float_of_int a ** float_of_int b))
    | [Float a; Float b] -> Float (a ** b)
    | [Float a; Int b] -> Float (a ** float_of_int b)
    | [Int a; Float b] -> Float (float_of_int a ** b)
    | [a; b] -> raise (Invalid_argument_type ("**", [a; b]))
    | args -> raise (Invalid_argument_number ("**", List.length args))

  let vbool_of_bool = function
      true -> True
    | false -> False

  let equal _ _ = function
      [a; b] ->
      let rec impl = function
          Nil, Nil
        | False, False
        | True, True -> true
        | Int a, Int b -> a = b
        | Int a, Float b -> float_of_int a = b
        | Float a, Float b -> a = b
        | Float a, Int b -> a = float_of_int b
        | String a, String b -> a = b
        | Array a, Array b ->
          if Array.length a <> Array.length b then
            false
          else
            Belt.Array.zip a b
            |. Belt.Array.every impl
        | Hash _a, Hash _b ->
          false (* TODO: impl *)
        | _, _ -> false in
      vbool_of_bool @@ impl (a, b)
    | args -> raise (Invalid_argument_number ("==", List.length args))

  let not_equal input output args =
    try
      match equal input output args with
        True -> False
      | False -> True
      | other -> raise (Unexpected_type_exception ("!=", other))
    with
      Invalid_argument_type (_, _) -> raise (Invalid_argument_type ("!=", args))
    | Invalid_argument_number (_, len) -> raise (Invalid_argument_number ("!=", len))

  let less_than _ _ = function
      [a; b] ->
      let rec impl = function
          Int a, Int b -> a < b
        | Int a, (Float _ as b) -> impl (Float (float_of_int a), b)
        | (Float _ as a), Int b -> impl (a, Float (float_of_int b))
        | Float a, Float b -> a < b
        | String a, String b -> a < b
        | a, b -> raise (Invalid_argument_type ("<", [a; b])) in
      vbool_of_bool @@ impl (a, b)
    | args -> raise (Invalid_argument_number ("<", List.length args))

  let less_equal input output args =
    try
      match equal input output args with
        True -> True
      | _ -> less_than input output args
    with
      Invalid_argument_type (_, _) -> raise (Invalid_argument_type ("<=", args))
    | Invalid_argument_number (_, len) -> raise (Invalid_argument_number ("<=", len))


  let greater_than input output args =
    try
      match less_equal input output args with
        True -> False
      | False -> True
      | other -> raise (Unexpected_type_exception (">", other))
    with
      Invalid_argument_type (_, _) -> raise (Invalid_argument_type (">", args))
    | Invalid_argument_number (_, len) -> raise (Invalid_argument_number (">", len))

  let greater_equal input output args =
    try
      match less_than input output args with
        True -> False
      | False -> True
      | other -> raise (Unexpected_type_exception (">=", other))
    with
      Invalid_argument_type (_, _) -> raise (Invalid_argument_type (">=", args))
    | Invalid_argument_number (_, len) -> raise (Invalid_argument_number (">=", len))
end

let default_genv () =
  let genv = Env.make () in
  let open Builtin in
  Env.(
    set genv "p" (`BFunc p);
    set genv "gets" (`BFunc gets);
    set genv "+" (`BFunc plus);
    set genv "-" (`BFunc minus);
    set genv "*" (`BFunc times);
    set genv "/" (`BFunc div);
    set genv "%" (`BFunc mod');
    set genv "**" (`BFunc expt);
    set genv "==" (`BFunc equal);
    set genv "!=" (`BFunc not_equal);
    set genv "<" (`BFunc less_than);
    set genv "<=" (`BFunc less_equal);
    set genv ">" (`BFunc greater_than);
    set genv ">=" (`BFunc greater_equal)
  );
  genv

let eval_with_default input output e =
  let genv = default_genv ()
  and lenv = Env.make () in
  eval genv lenv input output e