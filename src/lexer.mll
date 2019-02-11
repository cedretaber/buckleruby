{
open Parser
}

let num = ['0'-'9']
let nums = num+

let char = ['a'-'z' 'A'-'Z']
let chars = char+

let space = [' ' '\t']
let new_line = '\r'? '\n'

rule token = parse
      space { token lexbuf }  (* skip blanks *)

    | nums '.' num* as lxm { FLOAT (float_of_string lxm) }
    | nums as lxm          { INT (int_of_string lxm) }
    | '"' ([^ '"']* as lxm) '"' { STRING lxm }

    | "=>" { FATARROW }

    | "**" { EXP }
    | '+'  { PLUS }
    | '-'  { MINUS }
    | '*'  { TIMES }
    | '/'  { DIV }
    | '%'  { MOD }

    | "==" { EQ }
    | "!=" { NEQ }
    | "<=" { LE }
    | '<'  { LT }
    | ">=" { GE }
    | '>'  { GT }
    | "&&" { ANDAND }
    | "||" { OROR }

    | '=' { EQUAL }

    | '(' { LPAREN }
    | ')' { RPAREN }

    | '[' { LBRACKET }
    | ']' { RBRACKET }

    | '{' { LBRACE }
    | '}' { RBRACE }

    | "nil"   { NIL }
    | "false" { FALSE }
    | "true"  { TRUE }
    | "if"    { IF }
    | "then"  { THEN }
    | "else"  { ELSE }
    | "elsif" { ELSIF }
    | "end"   { END }
    | "while" { WHILE }
    | "def"   { DEF }
    | "begin" { BEGIN }

    | ',' { COMMA }

    | ';'      { SEMICOLON }
    | new_line { EOL }

    | ('_' | char) ('_' | char | num)* as lxm { IDENTIFY lxm }

    | eof { EOF }