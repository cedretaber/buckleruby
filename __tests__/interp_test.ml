open Jest

let parse str = Parser.main Lexer.token @@ Lexing.from_string str

exception Empty_input

let setup () =
  let input = object
    val mutable inputs = []
    method gets () = match inputs with
        [] -> raise Empty_input
      | s :: ss ->
        inputs <- ss;
        s
    method set_inputs ss = inputs <- ss
    method get_rest () = inputs
  end
  and output = object
    val mutable outs = []
    method p str = outs <- str :: outs
    method flush () =
      let output = outs |> List.rev |> String.concat "" in
      outs <- [];
      output
  end in
  (Interp.eval_with_default input output, input, output)

let () =

describe "All" (fun () ->
  let open Expect in
  let open! Operators in
  let open Value in

  let interp, _, _ = setup () in
  let run str = interp @@ parse str in

  test "func and var" (fun () ->
    let str =
{|
def add(a, b)
  a + b
end

x = 42
y = 99

add(x, y)
|} in
    expect (run str) = Int (42 + 99)
  );

  test "fib" (fun () ->
    let str =
{|
def fib(x)
  if x <= 1
    x
  else
    fib(x-1) + fib(x-2)
  end
end

fib(13)
|} in
    expect (run str) = Int 233
  );
);

describe "Operations" (fun () ->
  let open Expect in
  let open! Operators in
  let open Value in

  let interp, _, _ = setup () in
  let run str = interp @@ parse str in

  describe "+" (fun () ->
    describe "Unary" (fun () ->
      test "Int" (fun () ->
        expect (run "+42") = Int 42);
      test "Float" (fun () ->
        expect (run "+3.14") = Float 3.14);
    );
    test "Int" (fun () ->
      expect (run "1 + 2 + 3") = Int 6);
    test "Float" (fun () ->
      expect (run "3.14 + 2.22") = Float (3.14 +. 2.22));
    test "Int + Float" (fun () ->
      expect (run "9 + 3.14") = Float (9. +. 3.14));
    test "Float + Int" (fun () ->
      expect (run "1.14514 + 893") = Float (1.14514 +. 893.));
    test "String" (fun () ->
      expect (run {| "hoge" + "fuga" |}) = String "hogefuga");
  );

  describe "-" (fun () ->
    describe "Unary" (fun () ->
      test "Int" (fun () ->
        expect (run "-42") = Int (-42));
      test "Float" (fun () ->
        expect (run "-3.14") = Float (-3.14));
    );
    test "Int" (fun () ->
      expect (run "7 - 1 - 3") = Int 3);
    test "Float" (fun () ->
      expect (run "3.14 - 3.1") = Float (3.14 -. 3.1));
    test "Int - Float" (fun () ->
      expect (run "5 - 3.14") = Float (5. -. 3.14));
    test "Float - Int" (fun () ->
      expect (run "7.77 - 7") = Float (7.77 -. 7.));
  );

  describe "*" (fun () ->
    test "Int" (fun () ->
      expect (run "1 + 2 * 3 + 4") = Int 11);
    test "Float" (fun () ->
      expect (run "3.14 * 2.7") = Float (3.14 *. 2.7));
    test "Int * Float" (fun () ->
      expect (run "9 * 3.14") = Float (9. *. 3.14));
    test "Float * Int" (fun () ->
      expect (run "2.7 * 6") = Float (2.7 *. 6.));
    test "String * Int" (fun () ->
      expect (run {| "hoge" * 3 |}) = String "hogehogehoge");
  );
    
  describe "/" (fun () ->
    test "Int" (fun () ->
      expect (run "12 / 3") = Int 4);
    test "Float" (fun () ->
      expect (run "3.14 / 2.7") = Float (3.14 /. 2.7));
    test "Int / Float" (fun () ->
      expect (run "3.14 / 9") = Float (3.14 /. 9.));
    test "Float / Int" (fun () ->
      expect (run "5 / 2.7") = Float (5. /. 2.7));
  );

  describe "%" (fun () ->
    test "Int" (fun () ->
      expect (run "43 % 7") = Int 1);
    test "Float" (fun () ->
      expect (run "3.14 % 2.7") = Float (mod_float 3.14 2.7));
    test "Int % Float" (fun () ->
      expect (run "99 % 3.14") = Float (mod_float 99. 3.14));
    test "Float % Int" (fun () ->
      expect (run "2.7 % 2") = Float (mod_float 2.7 2.));
  );

  describe "**" (fun () ->
    test "Int" (fun () ->
      expect (run "2 ** 3") = Int 8);
    test "Float" (fun () ->
      expect (run "3.14 ** 2.7") = Float (3.14 ** 2.7));
    test "Int ** Float" (fun () ->
      expect (run "7 ** 2.7") = Float (7. ** 2.7));
    test "Float ** Int" (fun () ->
      expect (run "3.14 ** 3") = Float (3.14 ** 3.));
  );

  describe "==" (fun () ->
    test "Int" (fun () ->
      expect (run "42 == 42") = True);
  );
);

describe "AndOr" (fun () ->
  let open Expect in
  let open! Operators in
  let open Value in

  let interp, _, _ = setup () in
  let run str = interp @@ parse str in

  describe "and" (fun () ->
    test "true && true" (fun () ->
      expect (run "true && true") = True);  
    test "true && false" (fun () ->
      expect (run "true && false") = False);  
    test "false && true" (fun () ->
      expect (run "false && true") = False);  
    test "false && false" (fun () ->
      expect (run "false && false") = False);  
  );

  describe "or" (fun () ->
    test "true || true" (fun () ->
      expect (run "true || true") = True);  
    test "true || false" (fun () ->
      expect (run "true || false") = True);  
    test "false || true" (fun () ->
      expect (run "false || true") = True);  
    test "false || false" (fun () ->
      expect (run "false || false") = False);  
  );

  describe "short-circuit" (fun () ->
    let interp, _, output = setup () in

    test "true &&" (fun () ->
      let Nil = interp @@ parse {| true && p("test") |} in
      expect (output#flush ()) = ({|"test"|} ^ "\n")
    );

    test "false &&" (fun () ->
      let False = interp @@ parse {| false && p("test") |} in
      expect (output#flush ()) = ""
    );

    test "true ||" (fun () ->
      let True = interp @@ parse {| true || p("test") |} in
      expect (output#flush ()) = ""
    );

    test "false ||" (fun () ->
      let Nil = interp @@ parse {| false || p("test") |} in
      expect (output#flush ()) = ({|"test"|} ^ "\n")
    );
  ) [@warning "-8"];
);

describe "Variable" (fun () ->
  let open Expect in
  let open! Operators in
  let open Value in

  let interp, _, _ = setup () in
  let run str = interp @@ parse str in

  test "set and get" (fun () ->
    let str =
{|
x = 42
y = 99
x + y
|} in
    expect (run str) = Int (42 + 99));
);

describe "Array" (fun () ->
  let open Expect in
  let open! Operators in
  let open Value in

  let interp, _, _ = setup () in
  let run str = interp @@ parse str in

  test "create" (fun () ->
    expect (run {|[42, "hoge", true]|}) = Array [|Int 42; String "hoge"; True|]);
  test "ref" (fun () ->
    let str =
{|
arr = [1, 2, 3]
arr[1]
|} in
    expect (run str) = Int 2
  );
  test "assign" (fun () ->
    let str =
{|
arr = [1,2,3]
arr[1] = 42
arr
|} in
    expect (run str) = Array [|Int 1; Int 42; Int 3|]
  );
  test "expand" (fun () ->
    let str =
{|
arr = [1,2,3]
arr[5] = 42
arr
|} in
    expect (run str) = Array [|Int 1; Int 2; Int 3; Nil; Nil; Int 42|]
  );
);

describe "IO" (fun () ->
  let open Expect in
  let open! Operators in
  let open Value in

  describe "Input" (fun () ->
    let interp, input, _ = setup () in
    let input_str = "test input" in
    input#set_inputs [input_str];
    let res = interp @@ parse "gets()" in

    test "gets" (fun () ->
      expect res = String input_str);
    
    test "rest inputs" (fun () ->
      expect (input#get_rest ()) = []);
  );

  describe "Output" (fun () ->
    let interp, _, output = setup () in
    let output_str1 = "test output"
    and output_str2 = "hogehoge" in
    let str =
{j|
p("$(output_str1)")
p("$(output_str2)")
|j} in
    let res = interp @@ parse str in

    test "p" (fun () ->
      expect res = Nil);

    test "outputs" (fun () ->
      let expected =
{j|"$(output_str1)"
"$(output_str2)"
|j} in
      expect (output#flush ()) = expected);
  );
);