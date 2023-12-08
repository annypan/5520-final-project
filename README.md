# Javascript Static Type Checker

Project members: Chenxi Leng (lengch), Anni Pan (annipan).

## Module organization

Our project consists of two main parts: the JS flow parser and the typechecker. The parser consumes type-annotated JS program and produces an AST, which is then fed into the typechecker to check for type errors.

**Syntax.hs**: Defines the AST syntax that the JS programs are expected to translate into.

**Parser.hs**: Taken from the lecture on parser; parsing with applicative functors.

**FlowParser.hs**: Translates a type-annotated JS program into an AST with defined syntax.

**State.hs**: Taken from the lecture on states; a generic state transformer.

**TypeChecker.hs**: Contains all the logic for typechecking an AST.

**PrettyPrinter.hs**: Describes how to pretty print different components of the AST.

**ASTExamples.hs**: Hardcodes some ASTs that are compared against the AST generated from the parser to check its correctness.

**Spec.hs**: Unit tests and property-based QC tests for the parser and the typechecker.

**Lib.hs**: Contains a prompt loop that keeps prompting the user for the name of the file to typecheck, and returns the user the check results.

## Building, running, and testing

This project compiles with `stack build`. 

You can run the main executable with `stack run`. To typecheck a JS file, simply put it into the `js/` directory, and enter its name  (including the `.js` suffix) in the prompt.
You can run the tests with `stack test`. 

Finally, you can start a REPL with `stack ghci`.
