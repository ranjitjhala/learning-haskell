# Graduate Programming Languages - Wes Weimer - Homework 1 Makefile
# 
# When you're feeling lucky, try "make test" 

.PHONY : all test

all : clean imp 

lexer: lex.x
	alex lex.x

parser: lex.hs parser.y
	happy -a parser.y

IMP_SRCS = \
	hw1.hs \
	lex.hs \
	parser.hs

compile: $(IMP_SRCS)
	ghc -o imp $(IMP_SRCS)

clean : 
	rm -f *.o lex.hs parser.hs hw1 *~ *.result *.hi

imp: lexer parser compile

test: all
	./run_tests.sh

