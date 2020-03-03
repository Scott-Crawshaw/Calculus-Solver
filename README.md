# Calculus-Solver
## Completed
We have created appropriate data structures and have developed a parser that can parse even complex expressions into said data structures. The parser can work with binary operations such as +, -, *, ^, and /, as well as unary operations such as sin, cos, ln, -, and + (+ and - as prefixes).  
  
We have also created functions that can perform the steps needed to solve a given expression using a set of hard coded laws. The function derive takes an expression and a set of laws and produces a derivation in human readable format. To write expressions in human readable format, we created our own show instances for many of our datatypes.

## Need to Complete
In terms of whats left of the main portion of the assignment, we need to decide where exactly the parentheses should be in the show instances of our datatypes. While their current placement is mathematically fine, it can be overwhelming to the reader.  
  
We also need to create our executable function in Main.hs. This function should prompt the user to input an expression, and it should output the derivation. While we have written a function that should do just that, it is currently commented out due to compile time errors.  
  
We also need to finish the "Special Feature" portion of the assignment. For special features, we will incorperate the following:  
- We would like our derivation to not stop when the calculus is complete, but to instead perform the final arithmatic and algebraic steps required to fully complete the derivation. We will hopefully use a package that can perform this arithmatic and algebra for us. It appears the "hint" package can perform the arithmatic.

- We would also like our derive function to pull its laws from a file containing the laws in human readable format. This would allow the user to make their own laws, broadening the scope of the project.

## Instructions
To test our parser, first run 'stack ghci src/Parse.hs' and then call 'parseInputExpr "your expression here"' with your desired expression. The result will be wrapped in either a Correct or an Error constructor.  
  
To test our calculus solving functions found in Calculate.hs, first run 'stack ghci src/Calculate.hs' and then call 'derive laws testExp'. This will test our derive function on the expression 'd/dx x/2'. If you would like to change the testExp, feel free to edit it in Calculate.hs.

## Authors
Scott Crawshaw & Jessica Cheng 2020  
Assistance from Sebastiaan Joosten & Le Chang  
Referenced Thinking Functionally with Haskell by Richard Bird

