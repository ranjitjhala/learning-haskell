         Graduate Programming Languages Homework #3 (Code Portion)

-1. This README is just like the README for Homework #1, so you don't need
to read it again if you remember how that worked. 

0. There is a written component to the homework as well. Don't forget!

1. This is version _1_ of the Homework #3 code pack. Before submitting your
work or reporting a bug check to make sure that a more recent version has
not been released. 

2. This assignment will require you to write in OCaml, a popular and
efficient ML variant. ML variants are favored by PL researchers because
they are particularly good for writing programs that manipulate other
programs. 

Manual:         http://caml.inria.fr/pub/docs/manual-ocaml/index.html
Tutorials:      http://caml.inria.fr/pub/docs/manual-ocaml/manual003.html
                http://www.ocaml-tutorial.org/
Download It:    http://caml.inria.fr/download.en.html

In this assignment we'll be implementing a satisfiability modulo theories
(SMT) solver based on the DPLL(T) algorithm. I have done all of the
"undergrad" work (e.g., the lexer, the parser). You need only flesh out the
theorem prover. 

3. Manifest: 

README.txt              this file
Makefile                "make all", "make clean" and "make test"
main.ml                 The driver for our solver
arith.ml                Simple bounded arithmetic solver
cnf.ml                  Conjunctive normal form
dpll.ml                 DPLL(T) solver for CNF SAT formulae
exp.ml                  Mixed boolean and arithmetic expressions
lexer.mll               A "lex" file for our concrete syntax
parser.mly              A "yacc" file for our concrete syntax

4. Get ocaml up and running on your system. Make sure that "make all"
works. Part of comparing and evaluating languages involves being able to 
run and try out new languages and run-time systems. 

5. Run "make all" to build the skeletal SMT solver. 

6. Run the resulting solver executable and type in "p -> (q <-> p) ."
as input. You should see something like this: 

$ ./solver
p -> (q <-> p) .


Satisfiable!

p = true
q = true


The harness accepts a predicate involving propositional and arithmetic
components (terminated with a "." or EOF) and then determines if it is
satisfiable or not. If satisfiable, a model is produced. Our concrete math
and logic syntax is more or less what you would expect. It also supports
()'s and comments.  Examples: 

  p => q .

  ((x * x) = 25) .

  p && (!p || q) .

Note that until you finish this assignment, queries involving arithmetic
may not return the correct answer.

Note that the satisfying assignment printed may not include unconstrained
variables. For example, in "p => q", once we decide that "q = true", the
value of "p" does not matter (check both cases in your mind to verify).
Thus you may not see an explicit listing for "p" in the printed model.

5. Inspect the source files and get a feeling for how we are encoding
expressions. The translation is quite direct. 

6. Inspect main.ml to see the skeletal solver. You must add code to 
the Main.solver method that correctly integrates the simple bounded
arithmetic theory (arith.ml) with the simple DPLL SAT solver (dpll.ml) so
that we can handle queries that mix propositional and arithmetical
components. Look for the (* FIXME! *) line and change it. 

You should not need to change any other source files (or, indeed, any other
methods in main.ml). 

7. Keep at it until you pass all of the tests in "make test". 

8. Write some tests of your own. Put your best test case in the file
"test-mst3k.input" (where mst3k is your UVA ID). 

9. Rename "main.ml" to "test-mst3k.input" and submit them as per the
directions in the Homework. 
