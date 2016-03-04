{
module Parser where
import Lexer
}

%name imp
%monad { Alex }
%lexer { lexwrap } { TEOF }
%tokentype {Token}
-- Without this we get a type error
%error { happyError }

%token
'+' {TPlus}
'-' {TMinus}
'*' {TTimes}
'(' {TLparen}
')' {TRparen}
int {TInt $$}
var {TIdentifier $$}
'true' {TTrue}
'false' {TFalse}
'!' {TNot}
'&&' {TAnd}
'/\\' {TAnd}
'||' {TOr}
'\\/' {TOr}
'skip' {TSkip}
':=' {TSet}
';' {TSemicolon}
'if' {TIf}
'then' {TThen}
'else' {TElse}
'while' {TWhile}
'do' {TDo}
'let' {TLet}
'in' {TIn}
'print' {TPrint}
'{' {TLbrace}
'}' {TRbrace}
'=' {TEq}
'==' {TEqtest}
'<=' {TLeq}

%left '&&' '/\\'
%left '||' '\\/'
%left '+' '-'
%left '*'
%left NEG
%left '<=' '==' '='
%nonassoc '!'


%%
Com:
'skip' {Skip} |
var ':=' AExp {Set $1 $3} |
Com ';' Com {Seq $1 $3} |
'if' BExp 'then' Com 'else' Com {If $2 $4 $6} |
'while' BExp 'do' Com {While $2 $4} |
'print' AExp {Print $2} |
'{' Com '}' {Brace $2} |
'let' var '=' AExp 'in' Com {Let $2 $4 $6}


AExp   :
int  {Constant $1}
| var {Variable $1}
| AExp '+' AExp {Add $1 $3}
| AExp '-' AExp {Minus $1 $3}
| AExp '*' AExp {Times $1 $3}
| '(' AExp ')' {Paren $2}
| '-' AExp %prec NEG { Negate $2 }

BExp:
'true' {Btrue}
|'false' {Bfalse}
| '!' BExp {Not $2}
| BExp '&&' BExp {And $1 $3}
| BExp '/\\' BExp {And $1 $3}
| BExp '||' BExp {Or $1 $3}
| BExp '\\/' BExp {Or $1 $3}
| AExp '<=' AExp {Le $1 $3}
| AExp '==' AExp {Eqtest $1 $3}

{

data AExp =
  Constant Int |
  Variable String |
  Add AExp AExp |
  Minus AExp AExp |
  Times AExp AExp |
  Paren AExp |
  Negate AExp
 
data BExp =
  Btrue |
  Bfalse |
  Not BExp |
  And BExp BExp |
  Or BExp BExp |
  Le AExp AExp |
  Eqtest AExp AExp

data Com =
  Skip |
  Set String AExp |
  Seq Com Com |
  If BExp Com Com |
  While BExp Com |
  Print AExp |
  Brace Com |
  Let String AExp Com
  
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

getLineNumber :: Alex Int
getLineNumber = do
  (AlexPn _ l c, _,_,_) <- alexGetInput
  return l
  
happyError :: Token -> Alex a
happyError t = do
    l <- getLineNumber
    alexError ((show l) ++ ": parse error at token " ++ (show t))

}
