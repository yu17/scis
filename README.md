# EECS345_interpreter

An Interpreter for a C-like language implemented in Scheme.

All features, including the optional features, are implemented.

## Usage:

The main program file is `scis.scm`. It's a scheme flavor implementation. Requires `mit-scheme` to be installed.<br/>

Save the program to `<filename>`.<br/>
Enter the mit-scheme interactive environment. (Run mit-scheme)<br/>
Execute the following command:<br/>

(load "scis.scm")<br/>
(interpret `<filename>`)<br/>

Or, open a terminal and navigate to the folder where the program is in, and execute:<br>

mit-scheme --load scis.scm --eval "(interpret `<filename>`)"

#### Features:<br/>
Variables types: integer, boolean, string<br/>
Case-sensitive variable names (boolean literals are case-insensitive)<br/>
Variables use-before-declaration check<br/>
Variables use-before-initialization check<br/>
Variables type check<br/>
Variables integer-to-boolean auto-casting<br/>
Arithmetic operators: +, -, \*, /, %<br/>
Logic operators: ==, !=, <, >, <=, >=<br/>
Boolean operators: &&, ||, !<br/>
Boolean operation short-circuit evaluation<br/>
Assignment operation =<br/>
Inline assignment (nested assignment and assignment within expression)<br/>

## About State Functions and Value Functions:

In order to allow assignments within evaluations, all the value functions return both a value and a state. Therefore, almost all the functions return states, but only the value functions would return a value as well, just to be clear.

## Function Naming Conventions:

Functions are classfied by the prefixes in their names.

#### SL\_\*
Acronym for StateList. State operation functions.<br/>
The states are stored in the format: ((variable\_name\_list) (variable\_value\_list))

#### eval\_\*
Acronym for evaluation. Value functions. Usually takes an expression and a state and returns the evaluation result value and the state after evaluation (with possible assignments applied).

#### terp\_\*
Acronym for Interpretation. State functions. Usually takes a few arguments and a state and returns the result state.

#### return\_\*
The functions that handle or present the return value.

## Todos:

Fix integer-to-boolean casting logic.<br/>
Edge case tests.<br/>
