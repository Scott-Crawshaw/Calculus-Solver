# Calculus-Solver
## Main Project
### Data Types
To accurately represent the order of operations with an expression, we built our expression data type using a term tree. This term tree can take binary operations such as addition, multiplication, etc., unary operations such as sin, cos, etc., variables, and constants. For all of our datatypes, we wrote custom show instances to display our data in human readable format. 

### Parsing
We developed two parsers, one for laws and one for expressions. Both parsers take a string as an input and either generate the desired object or print a detailed error message. The expression parser is used to process the initial input via the command line interface, while the law parser is used to parse laws from a plain text file.

### Generating the Derivation
We created a function that, given an expression and a set of laws, will generate the appropriate derivation, including all intermediary steps. This function is called via the command line interface to derive a provided expression.

### Testing
We created comprhensive test cases that ensure that our program functions as expected. This ensures that changes made to one part of the project do not impact other parts unexpectedly.

## Special Features
### Reading Laws from Text File
Our program reads in laws from the file LawList.txt. For ease of use, these laws are written in human readable format and are later parsed into the appropriate data types. Our pre-made set of laws is ordered in an ideal way to provide the shortest possible derivations.

### Performing Arithmetic
To generate a more simplified derivation, we wove basic arithmetic into our step generation. During each intermediary step, basic arithmetic is performed to simplify the output.

## Instructions

## Authors
Scott Crawshaw & Jessica Cheng 2020  
Assistance from Sebastiaan Joosten & Le Chang  
Referenced Thinking Functionally with Haskell by Richard Bird

