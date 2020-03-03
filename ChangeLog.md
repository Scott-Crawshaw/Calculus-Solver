# Changelog for Calculus-Solver

## 0.1.0.0
- Created basic data structures to hold expressions, laws, equations, and calculations
- Created prinatable example usages of said data structures.

## 0.2.0.0
- Re-wrote data structures in term tree format.
- Developed a parser that can parse even complex expressions into our data stuctures.
- Added a small program to Main.hs that allows one to test our parser with any given input.

## 0.3.0.0
- Made new files to organize code into components
- Made progress on deriving an expression given a set of laws
- Have a working function for rewriting an expression given a law equation
- Do not have the ability to run a full derivation due to non-termination bug in our calculate function.

## 0.4.0.0
- Program can now complete the full calculus derivation of a given expression.
- Added custom show instances for human readable printing of derivation.
- Need to create executable that ties parser and deriver together.
- Need to allow for final arithmatic and algebraic steps.
- Need to allow for laws to be drawn from input file.

## 0.5.0.0
- 'stack install' will now create an executable that, when run, allows the user to enter an expression, recieve a human-readable calculation, and enter a new expression should they desire.
- The laws are now drawn from a LawList.txt, a file that contains all of the laws in human readable format.
- Need to implement final arithmatic and algebraic steps.
- Need to fix placement of parentheses in printed expression.