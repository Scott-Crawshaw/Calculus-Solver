# Calculus-Solver
## Completed
We have created appropriate data structures and have developed a parser that can parse even complex expressions into said data structures. The parser can work with binary operations such as +, -, *, ^, and /, as well as unary operations such as sin, cos, ln, -, and + (+ and - as prefixes).  
  
We have also created functions that can perform the steps needed to solve a given expression using a series of hard coded laws. Currently, we can successfully rewrite an expression given a law and make the necessary substitutions to apply the law.  

## Need to Complete
Currently, there is a method in Calculate.hs named compatible that will check if two substitutions made are compatible with each other before making said substitutions.  However, this method is still in progress and as of now, only returns true.  Right now, when testing our program on d/dx x^2, the program makes some matches it should not and applies the derivative of self law one too many times.

## Instructions
To test our parser, simply run 'stack install' and then run the generated executable. You will be prompted to enter an expression, and will recieve a printed version of the expression parsed into data types.  
  
To test our calculus solving functions found in Calculate.hs, simply run 'stack ghci src/Calculate.hs'. To test our derive function, which will apply laws to an expression, run 'derive laws testExp', where testExp is a hardcoded test expression.

## Authors
Scott Crawshaw & Jessica Cheng 2020  
Assistance from Sebastiaan Joosten & Le Chang  
Referenced Thinking Functionally with Haskell by Richard Bird

