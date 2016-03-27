# 

.PHONY : all test

all : clean solver 

lexer: lex.x
	alex lex.x

parser: lex.hs parser.y
	happy -a parser.y

SOLVER_SRCS = \
	hw2.hs \
	lex.hs \
	parser.hs

compile: $(SOLVER_SRCS)
	ghc -o solver $(SOLVER_SRCS)

clean : 
	rm -f *.o lex.hs parser.hs hw1 *~ *.result *.hi

solver: lexer parser compile

test: all
	./run_tests.sh

