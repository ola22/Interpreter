# Interpreter
Functional language (Olol) interpreter.



## Build:
Project is built with cabal. I've tested it on students and my computer.
To build the project you just need to type 'make' in Aleksandra_Falkiewicz directory.
Cabal will make dist-newstyle directory, where are all executable files.


## Running interpreter:
Main interpreters file (Main.hs) is in directory interpreter. Interpreter is run using
cabal new-exec command. You can run it from Aleksandra_Falkiewicz directory.
It can be run in two ways:
* from file: cabal new-exec interpreter <input_file_name>
* from console: cabal new-exec interpreter (EOF signal needs to be sent (eg. crl+d))



## Usage exaples
In directories good/bad you can find positive/negative usage examples of code written in Olol. You can run them for example with cabal new-exec interpreter ../good/<input_file_name>. The output returned by given program is in its comments, next to each expression.



## Language features:
Functionality table with all accomplished language funcionalities is located in doc directory.
Non-obvious in Olol is function application, beacuse it uses ':' char (eg. f : 1 : 2) 
Here are its features:
* type inference using H-M algorithm W (with polymorphism) - static typing
* static identifier binding
* basic arithmetical, comparison and logical operations
* polymorphic ifs, functions
* multivariable lambdas and functions
* polymorphic lists (including nested lists and lists of functions)
* builtin lists operations (tail, head, isEmpty) and also concat, empty (eg. tail l)
* syntax sugar for writing lists: eg. [1, 2, 3]
* partial application, closure, higher order functions
* error handling: including parse errors, typecheck errors and runtime errors (errors includes filename and error line)

Moreover Olol has its small standard library implemented in file 'lib.olol' with some more list functions (len, get, drop, take) and flip

