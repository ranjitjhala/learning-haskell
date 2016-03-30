{
module Parser where
import Lexer
import Exp
}

%name solver
%monad { Alex }
%lexer { lexwrap } { TEOF }
%tokentype {Token}
-- Without this we get a type error
%error { happyError }

%token
'->' {TImplies}
'=>' {TImplies}
'<->' {TIff}
'<=>' {TIff}

'<=' {TLeq}
'<' {TLt}
'>=' {TGeq}
'>' {TGt}

'=' {TEq}
'==' {TEq}

'!=' {TNeq}
'<>' {TNeq}

'+' {TPlus}
'-' {TMinus}
'*' {TTimes}

'(' {TLparen}
')' {TRparen}

'!' {TNot}
'~' {TNot}

'&&' {TAnd}
'||' {TOr}

float {TFloat $$}
var {TIdentifier $$}

%left '<->' '<=>'
%left '->' '=>'
%left '||' 
%left '&&'
%left '==' '=' '!=' '<>'
%left '<=' '>=' '<' '>'
%left '+' '-'
%left '*'
%left '!' '~'

%%

Exp:
var {Variable $1}
| Exp '&&' Exp {And $1 $3}
| Exp '||' Exp {Or $1 $3}
| Exp '=>' Exp {Implies $1 $3}
| Exp '->' Exp {Implies $1 $3}
| Exp '<->' Exp {Iff $1 $3}
| Exp '<=>' Exp {Iff $1 $3}
|'!' Exp {Not $2}
| '~' Exp {Not $2}
| '(' Exp ')' {$2}
| AExp {$1}

AExp:
Anum '<=' Anum {Arithmetic (Le $1 $3)}
| Anum '>=' Anum {Arithmetic (Ge $1 $3)}
| Anum '=' Anum {Arithmetic (Eq $1 $3)}
| Anum '==' Anum {Arithmetic (Eq $1 $3)}
| Anum '!=' Anum {Not (Arithmetic (Eq $1 $3))}
| Anum '<>' Anum {Not (Arithmetic (Eq $1 $3))}
| Anum '<' Anum {And (Arithmetic (Le $1 $3))
      (Not (Arithmetic (Eq $1 $3)))}
| Anum '>' Anum {And (Arithmetic (Ge $1 $3))
      (Not (Arithmetic (Eq $1 $3)))}

Anum:
float  {Constant $1}
| var {ArithmeticVariable $1}
| Anum '+' Anum {Add $1 $3}
| Anum '-' Anum {Minus $1 $3}
| Anum '*' Anum {Times $1 $3}
-- TODO
--| '(' Anum ')' {$2} -- reduce/reduce conflict

{

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
