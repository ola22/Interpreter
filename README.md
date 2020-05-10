# Interpreter
Functional language (Olol) interpreter.


## Build:


## Running interpreter:
Main interpreters file (Main.hs) is in directory interpreter. Interpreter can be run in two ways:
* from file: ./Main <file_name>
* from console: ./Main (to get output you need to send EOF (eg. crl+d))


## Usage exaples
In directories good/bad you can find positive/negative usage examples of code written in Olol. You can run them for example with ./Main ../good/<file_name>. The output returned by given program is in its comments, next to each expression.


## Language features:
Functionality table with all accomplished language funcionalities is located in doc directory.
Here are its features:
* type inference using H-M algorithm W (with polymorphism) - static typing
* basic arithmetical, comparison and logical operations
* polymorphic ifs, functions, lambdas
* multivariable lambdas and functions
* polymorphic lists (including nested lists and lists of functions)
* builtin lists operations (tail, head, isEmpty) and also concat, empty (eg. tail l)
* sugar syntax for writing lists: eg. [1, 2, 3]
* partial application, closure, higher order functions
* error handling: including parse errors, typecheck errors and runtime errors (errors includes filename and error line)


Moreover Olol has its small standard library implemented in file 'lib.olol' with some more list functions (len, get, drop, take) and flip

