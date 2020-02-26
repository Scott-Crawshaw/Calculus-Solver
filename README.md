# Calculus-Solver
## Completed
We have created appropriate data structures and have developed a parser that can parse even complex expressions into said data structures. The parser can work with binary operations such as +, -, *, ^, and /, as well as unary operations such as sin, cos, ln, -, and + (+ and - as prefixes).  

We have also begun creating functions that can perform the steps needed to solve a given expression using a series of hard coded laws. Currently, we can successfully rewrite an expression given a law.

## Need to Complete
Unfortunately, there are some bugs in our calculus functions. For example, the function calculate never terminates. We need to repair this function. After that, we must implement configurable laws, deal with laws that involve constants, and implement an intelligent selection of laws (rather than randomly selecting any law that will work). We should have this done by Thursday evening.

## Instructions
To test our parser, simply run 'stack install' and then run the generated executable. You will be prompted to enter an expression, and will recieve a printed version of the expression parsed into data types.  

To test our various calculus solving functions found in Calculate.hs, simply run 'stack ghci src/Calculate.hs'. In this file is the sum rule equation (testEqn) and a basic expression that the sum rule can be applied to (testExp). To test our rewrite function, which rewrites an expression given a law equation, run 'rewrites testEqn testExp'.

## Authors
Scott Crawshaw & Jessica Cheng 2020  
Assistance from Sebastiaan Joosten & Le Chang  
Referenced Thinking Functionally with Haskell by Richard Bird

