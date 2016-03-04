{
module Lexer  where
import Control.Monad.State
import Control.Monad.Error
}

%wrapper "monad"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

@constant = (0x)?[0-9]+
@identifier = [A-Za-z_][0-9A-Za-z_]*

tokens :-

  <0>{
	$white+				;
  	\+				{mkT TPlus}
  	\-				{mkT TMinus}
  	\*				{mkT TTimes}
  	true				{mkT TTrue}
  	false				{mkT TFalse}
  	"="				{mkT TEq}
	"=="				{mkT TEqtest}
  	"<="				{mkT TLeq}
  	"!"				{mkT TNot}
  	"&&"|"/\\"			{mkT TAnd}
  	"||" | "\\/"			{mkT TOr}
  	skip				{mkT TSkip}
  	":="				{mkT TSet}
  	";"				{mkT TSemicolon}
  	if				{mkT TIf}
  	then				{mkT TThen}
  	else				{mkT TElse}
  	while				{mkT TWhile}
  	do				{mkT TDo}
  	let				{mkT TLet}
  	in				{mkT TIn}
  	print				{mkT TPrint}
  	"("				{mkT TLparen}
  	")"				{mkT TRparen}
  	"{"				{mkT TLbrace}
  	"}"				{mkT TRbrace}
  	@constant			{makeInt}
  	@identifier			{makeIdentifier}
  }
  
  <0>"/*"				{begin comment}
  <comment>"*/"				{begin 0}
  <comment>[.\n]            		;
  <0>"(*"  				{begin comment1}
  <comment1>"*)"			{begin 0}
  <comment1>[.\n]           		;
  
{

-- The token type:
data Token =
     	TPlus			|
	TMinus			|
	TTimes			|
	TTrue			|
	TFalse			|
	TEq			|
	TEqtest			|
	TLeq			|
	TNot			|
	TAnd			|
	TOr			|
	TSkip			|
	TSet			|
	TSemicolon		|
	TIf			|
	TThen			|
	TElse			|
	TWhile			|
	TDo			|
	TLet 			|
	TIn  			|
	TPrint			|
	TLparen			|	
	TRparen			|	
	TLbrace			|
	TRbrace			|
	TIdentifier String	|
	TInt Int		|
	TEOF
	deriving (Eq,Show)

alexEOF = return TEOF

mkT::Token -> AlexInput -> Int -> Alex Token
mkT t (_,_,_,_) _ = return t

makeInt::AlexInput->Int-> Alex Token
makeInt (_,_,_,inp) len = return $ TInt (read (take len inp))

makeIdentifier::AlexInput->Int-> Alex Token
makeIdentifier (_,_,_,inp) len = return $ TIdentifier (take len inp)

}
