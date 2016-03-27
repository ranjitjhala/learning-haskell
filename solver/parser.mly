%{
open Exp
%} 
%token <float> NUMBER
%token <string> IDENTIFIER
%token PLUS MINUS TIMES 
%token LPAREN RPAREN
%token NOT 
%token IMPLIES
%token IFF
%token AND OR 
%token LT LE GT GE EQ NEQ
%token EOF 

/* lowest precedence */ 
%left IFF
%left IMPLIES 
%left OR
%left AND
%left EQ NEQ
%left LE GE LT GT 
%left PLUS MINUS 
%left TIMES 
%right NOT

%start main
%type <Exp.exp> main 

%%

main: exp EOF  { $1 }
; 

exp : IDENTIFIER        { Variable($1) }
    | exp AND exp       { And($1,$3) } 
    | exp OR exp        { Or($1,$3) } 
    | exp IMPLIES exp   { Implies($1,$3) } 
    | exp IFF exp       { Iff($1,$3) } 
    | NOT exp           { Not($2) }
    | LPAREN exp RPAREN { $2 }
    | aexp              { $1 } 
;

aexp : anum LE anum     { Arithmetic(Le($1,$3)) } 
     | anum GE anum     { Arithmetic(Ge($1,$3)) } 
     | anum EQ anum     { Arithmetic(Eq($1,$3)) } 
     | anum NEQ anum    { Not(Arithmetic(Eq($1,$3))) } 
     | anum LT anum     { And((Arithmetic(Le($1,$3))),
                              Not(Arithmetic(Eq($1,$3)))) } 
     | anum GT anum     { And((Arithmetic(Ge($1,$3))),
                              Not(Arithmetic(Eq($1,$3)))) } 
;
anum : NUMBER           { Number($1) }
     | IDENTIFIER       { Arithmetic_Variable($1) } 
     | anum PLUS anum   { Plus($1,$3) } 
     | anum MINUS anum  { Minus($1,$3) } 
     | anum TIMES anum  { Times($1,$3) } 
     | LPAREN anum RPAREN { $2 } 
;
