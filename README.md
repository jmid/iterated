This directory contains the prototype implementation of the static
analysis described in the paper

    Iterated Process Analysis over Lattice-Valued Regular Expressions
    Jan Midtgaard, Flemming Nielson, and Hanne Riis Nielson   
    PPDP 2016

The prototype supports a slightly larger language than described in
the paper. In particular, the supported Boolean expressions are:

    b ::= tt | ff | neg b | a0 = a1 | a0 < a1 | a0 <= a1

The supported statements of the core language is available in ast.ml
as the type 'stmt'.

A slightly extended statement language is available as 'exstmt' along
with a desugarer ('desugar_exstmt' and 'desugar_exblock') for
translating the extended language back to the core language.


To run examples:
----------------

It is easiest to try the examples through [the web-client](https://jmid.github.io/iterated/).
To do so, simply open [the link](https://jmid.github.io/iterated/) in a browser that supports JavaScript.

The opened window now summarizes the supported syntax and furthermore
contains two input fields for process code.

If you choose the 'simple' option in the drop-down menu, the program

    spawn proc1() { ch?x }
    spawn proc2() { ch!42 }

should appear with one process in each input field.

If you now press the 'Analyze'-bottom you should see output of this form:

    Channel name mapping:
    ----------------------
    ch -> 0
    
    
    Iteration 0
    ------------
                                  ([], ε, Top*)
    1:0? x;                              ([x -> [-oo;+oo]], ?([0;0], [-oo;+oo]), Top*)
    2:skip;                              ([x -> [-oo;+oo]], ?([0;0], [-oo;+oo]), Top*)
    
    
    Collective prefix': (ε + ?([0;0], [-oo;+oo]))
    
    
                                  ([], ε, (ε + ?([0;0], [-oo;+oo])))
    3:0! 42;                              ([], !([0;0], [42;42]), ε)
    4:skip;                              ([], !([0;0], [42;42]), ε) 
    
    Collective prefix': (ε + !([0;0], [42;42]))
    
    Iteration 1
    ------------
                                  ([], ε, (ε + !([0;0], [42;42])))
    1:0? x;                              ([x -> [42;42]], ?([0;0], [42;42]), ε)
    2:skip;                              ([x -> [42;42]], ?([0;0], [42;42]), ε) 
    
    Collective prefix': (ε + ?([0;0], [42;42]))
    
    
                                  ([], ε, (ε + ?([0;0], [42;42])))
    3:0! 42;                              ([], !([0;0], [42;42]), ε)
    4:skip;                              ([], !([0;0], [42;42]), ε) 
    
    Collective prefix': (ε + !([0;0], [42;42]))
    
    Reached fixed point, bailing early


The 'Channel name mapping' says that the channel name 'ch' has been
numbered 0. The statement ch?x is therefore printed as 0?x under this
renaming. The output now lists the result of two iterations, each
analysing each process once.

Each single process analysis result is printed as a decorated program
with a precondition above and postcondition at the right of each
statement. Recall that we automatically insert a dummy skip-statement
at the end of each process. Each precondition and postcondition is
shaped as a triple consisting of an abstract store, a history and a
future. A line such as

    ([x -> [-oo;+oo]], ?([0;0], [-oo;+oo]), Top*)

should therefore be understood as

- a store where the variable 'x' can take any value (between negative
  infinity and positive infinity in the interval lattice),

- a history consisting of just a single 'read action' of any value
  (also in the above interval) from a channel in the interval [0;0],
  i.e., channel 'ch' under the above channel numbering,

- a future which represents the worst case assumption (any communication)

In addition, we compute and print the collective communication prefix
of the process. For example, the first such

    Collective prefix': (ε + ?([0;0], [-oo;+oo]))

represents a prefix-closure of the above read.


The examples from the paper are available from the dropdown menu as
'simple', 'deadlock', and 'non-termination'.



To run examples from the command line:
--------------------------------------

In a terminal, build the extended top-level as described below and start it:

    $ ./ledit ./main.top
            OCaml version 4.02.3
    
    found .ocamlinit
    # 


Now try analyzing a simple program consisting of two processes: one
process reads a value from channel 'ch' and assigns the obtained
value to the variable 'x', and another process writes the value '42'
to the channel 'ch':

    # Main.eval_twoproc_pp ([Ast.ExChread ("ch","x")], [Ast.ExChwrite ("ch", Ast.Num 42)]) 2;;

    [ ...output as above... ]


At the end of the file 'ast.ml' we include a number of additional examples.


To run the QuickCheck tests:
----------------------------

To re-run the QuickCheck tests in the terminal first build the quickcheck tests as described below and then simply execute:

    $ ./redomcheck.byte 
    check 1209 properties...
    testing property 'parity.d increasing in argument 1'...
      [✔] passed 1000 tests (0 preconditions failed)
    testing property 'parity.d invariant in argument 1'...
      [✔] passed 1000 tests (0 preconditions failed)
    testing property 'parity.d increasing in argument 2'...
      [✔] passed 1000 tests (0 preconditions failed)

    [...many lines cut...]

      [✔] passed 1000 tests (0 preconditions failed)
    testing property 'Prodlattice(Storelattice(interval),interval,interval).widening invariant in argument 2'...
      [✔] passed 1000 tests (0 preconditions failed)
    tests run in 4480.10s
    [✔] Success! (passed 1209 tests)

Requirements:
-------------

To build the prototype and try the examples you need OCaml version
4.02.3 (or so) and the menhir parser generator, and to build and
QuickCheck the abstract domains you need the library 'qcheck' v.0.3
both of which are available through the package manager OPAM.

To build the web-client you need the js_of_ocaml compiler version 2.7
or so which is also available through OPAM. The web interface further
requires [CodeMirror](http://codemirror.net) (copy included).

The QuickCheck code builds on an extended version of 'LCheck' which is
included with the prototype's source code. LCheck is a module for
randomized, property-based testing (QuickChecking) of lattices and
lattice operations.  It is furthermore described in the paper

    QuickChecking Static Analysis Properties
    Jan Midtgaard and Anders Moeller, ICST'15
    http://janmidtgaard.dk/papers/Midtgaard-Moeller:ICST15.pdf  

The module is also separately available [here](https://github.com/jmid/lcheck).


Build instructions:
-------------------

Provided the above requirements are met, building the top-level should
be as simple as

    $ make top

To build the web-client

    $ make js

To build the quickcheck test

    $ make domcheck
