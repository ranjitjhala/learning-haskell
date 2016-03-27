{
module Lexer  where
import Control.Monad.State
}

%wrapper "monad"

@constant = (\-)?[0-9]+([.][0-9]*)?
@identifier = [A-Za-z]+

tokens :-

$white+				;
@constant			{makeFloat}
@identifier			{makeIdentifier}
"->"				{mkT TImplies}
"=>"				{mkT TImplies}
"<->"				{mkT TIff}
"<=>"				{mkT TIff}

"<="				{mkT TLeq}
"<"				{mkT TLt}
">="				{mkT TGeq}
">"				{mkT TGt}

"=="				{mkT TEq}
"="				{mkT TEq}

"!="				{mkT TNeq}
"<>"				{mkT TNeq}   

\+				{mkT TPlus}
\-				{mkT TMinus}
\*				{mkT TTimes}

"("				{mkT TLparen}
")"				{mkT TRparen}
  	
  	  	
"!"				{mkT TNot}
"~"				{mkT TNot}
	
"&&"				{mkT TAnd}
"||" 				{mkT TOr}
	
"."				{makeEOF}  
  
{

-- The token type:
data Token =
     	TImplies		|
	TIff			|
	TLeq			|
	TLt			|
	TGeq			|
	TGt			|
	TEq			|
	TNeq			|

     	TPlus			|
	TMinus			|
	TTimes			|

	TLparen			|	
	TRparen			|	

	TNot			|
	TAnd			|
	TOr			|

	TIdentifier String	|
	TFloat Float		|
	
	TEOF
	deriving (Eq,Show)

alexEOF = return TEOF

mkT::Token -> AlexInput -> Int -> Alex Token
mkT t (_,_,_,_) _ = return t

makeEOF:: AlexInput -> Int -> Alex Token
makeEOF _ _ = return TEOF

makeFloat::AlexInput-> Int -> Alex Token
makeFloat (_,_,_,inp) len = return $ TFloat (read (take len inp))

makeIdentifier::AlexInput->Int-> Alex Token
makeIdentifier (_,_,_,inp) len = return $ TIdentifier (take len inp)

}
