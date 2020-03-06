# Calculus-Solver
## Main Project

### Project Overview 
The goal of this project was to build a calculus solver that would compute the derivates of calculus expressions.
Our project will take a calculus expression from the user, show the intermediate steps of the derivative along with the laws used, and then output the final derivative of the original expression.

### Data Types
To accurately represent the order of operations with an expression, we built our expression data type using a term tree. This term tree can take binary operations such as addition, multiplication, etc., unary operations such as sin, cos, etc., variables, and constants. For all of our datatypes, we wrote custom show instances to display our data in human readable format. 

### Parsing
We developed two parsers, one for laws and one for expressions. Both parsers take a string as an input and either generate the desired object or print a detailed error message. The expression parser is used to process the initial input via the command line interface, while the law parser is used to parse laws from a plain text file.

### Generating the Derivation
We created a function that, given an expression and a set of laws, will generate the appropriate derivation, including all intermediary steps. This function is called via the command line interface to derive a provided expression.

### Handling Constants
In order to handle constants correctly in our derivations, we have a law called Derivative of Constant where we use the 'z' character to indicate the position of the constant in the law.  In our match function in Calculate.hs, we pattern match for the 'z' and a constant, and if this pattern is found, we allow the match function to return this match.  This ensures that the z will only be replaced by a constant when we do substitutions.  

### Testing
We created comprehensive test cases that ensure that our program functions as expected. This ensures that changes made to one part of the project do not impact other parts unexpectedly.

## Special Features
### Reading Laws from Text File
Our program reads in laws from the file LawList.txt. For ease of use, these laws are written in human readable format and are later parsed into the appropriate data types. Our pre-made set of laws is ordered in an ideal way to provide the shortest possible derivations.

### Performing Arithmetic
To generate a more simplified derivation, we wove basic arithmetic into our step generation. During each intermediary step, basic arithmetic is performed to simplify the output.

### Performing Algebra
To generate a more simplified derivation, we wove very basic algebra into our laws.  When the program tries to apply laws, we have specific laws that allow it to rewrite fractions as exponents and combine exponents of like terms as well. 

### Derivative of Fixed Variable
In our program, we treat any letter in the equation that we are not taking the derivative with respect to as a constant.  In order to do this, we added a law called Derivative of Fixed Variable with the character 'q' that acts as a placeholder for the fixed variable.  In order to treat this letter as a constant, we pattern match for the 'q' and a variable in our match function.  
Example: deriv x 3*x*y = 3*y

## Instructions
More to come...

## Authors
Scott Crawshaw & Jessica Cheng 2020  
Assistance from Sebastiaan Joosten & Le Chang  
Referenced Thinking Functionally with Haskell by Richard Bird

