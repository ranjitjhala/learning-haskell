type token =
  | NUMBER of (float)
  | IDENTIFIER of (string)
  | PLUS
  | MINUS
  | TIMES
  | LPAREN
  | RPAREN
  | NOT
  | IMPLIES
  | IFF
  | AND
  | OR
  | LT
  | LE
  | GT
  | GE
  | EQ
  | NEQ
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Exp.exp
