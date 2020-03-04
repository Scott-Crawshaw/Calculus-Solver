# Calculus-Solver
## Completed
We have created appropriate data structures and have developed a parser that can parse even complex expressions into said data structures. The parser can work with binary operations such as +, -, *, ^, and /, as well as unary operations such as sin, cos, ln, -, and + (+ and - as prefixes).  
  
We have also created functions that can perform the steps needed to solve a given expression using a set of hard coded laws. The function derive takes an expression and a set of laws and produces a derivation in human readable format. To write expressions in human readable format, we created our own show instances for many of our datatypes. Our only issue is that these show instances don't always put parentheses in the correct locations.

Finally, we created a command line interface that ties all the pieces together. When prompted, the user inputs an expression, recieves a calculation in human-readable format, and can continue to input more expressions should they desire.

## Need to Complete
We also need to finish the "Special Feature" portion of the assignment. While we have allowed for laws to be read from a law file, we still need to implement arithmatic and alegebra. We would like our derivation to not stop when the calculus is complete, but to instead perform the final arithmetic and algebraic steps required to fully complete the derivation. 

## Instructions
To use our command line interface, simply run 'stack install', run the mentioned executable, and then input an expression when prompted. Instead of writing d/dx, write deriv x. If you would like to make adjustments to the laws, do so in LawList.txt.

## Authors
Scott Crawshaw & Jessica Cheng 2020  
Assistance from Sebastiaan Joosten & Le Chang  
Referenced Thinking Functionally with Haskell by Richard Bird

