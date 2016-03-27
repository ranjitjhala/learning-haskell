{
open Parser
}

rule token = parse
        ['\t''\n''\r'' ']+      { token lexbuf }
| ['A'-'Z''a'-'z']+ as lxm      { IDENTIFIER lxm }
| ('-')?['0'-'9']+(['.']['0'-'9']*)? as lxm { NUMBER (float_of_string lxm) }
| "->" { IMPLIES } 
| "=>" { IMPLIES } 
| "<=>" { IFF } 
| "<->" { IFF } 

| "<=" { LE }
| "<" { LT }
| ">=" { GE } 
| ">" { GT } 
| "==" { EQ } 
| "=" { EQ } 
| "!=" { NEQ } 
| "<>" { NEQ } 
| '+' { PLUS } 
| '-' { MINUS } 
| '*' { TIMES } 
| '(' { LPAREN } 
| ')' { RPAREN } 
| '!' { NOT } 
| '~' { NOT } 

| "&&" { AND }
| "||" { OR } 
| "."
| eof   { EOF } 

