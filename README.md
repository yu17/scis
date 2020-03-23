# EECS345_interpreter

An Interpreter for a C-like language implemented in Scheme.

All features, including the optional features, are implemented.

## Usage:

The main interpreter program file is `scis.scm`. It's a scheme flavor implementation. Requires `mit-scheme` to be installed.<br/>
The provided parser `simpleParser.scm` has been modified so as to be run under `mit-scheme`.<br/>

To run `scis.scm`:

Save the testing C program to `<filename>`.<br/>
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
Instructions: var, if, while, return, begin, break, continue, try, catch, finally, throw

#### Implementation:<br/>
The result of the parser is first passed to `intpn_stmt_auto`, which chooses the corresponding interpretation functions for each statement. As for the expression, we have `intpn_expr_auto` which returns the result state after evaluating the expression, and `eval_expr_auto` which returns the result value after evaluating it. When processing the expressions, we don't distinguish whether the function returns a boolean or a integer. For `if` or `while` statements, they call `bool_expr_auto` which automatically cast integer to boolean so as to ensure that the return value is a boolean.

## Function Naming Conventions:

Functions are classfied by the prefixes in their names.

#### L\_\*
Acronym for stateLayer. State layer operation functions.<br/>
Each layer of the state are stored in the format: ((variable\_name\_list) (variable\_value\_list))

#### SL\_\*
Acronym for StateList. State list operation function.<br/>
The layers of the states are stacked in the format: (layer1 layer2 layer3 ...)

#### eval\_expr\_\*
Acronym for EVALuation EXPRession. Value functions. Takes a state and returns the result of the corresponding calculation based on the state.

#### bool\_expr\_\*
Acronym for BOOLean EXPRession. Boolean function. This is a dummy function that basically calls the `eval\_expr\_\*` functions and convert the result to a boolean.

#### intrp\_expr\_\*
Acronym for INTeRPretation EXPRession. State functions. Takes a state and returns the result state of the corresponding calculation. State changes in these functions only occurs when there are assignment expression.

#### intrp\_*
Acronym for INTeRPretation. State function. Handles the statements. Takes the arguments of a statement and returns the result state.

#### invalid\_*
Dummy functions that are used to initialize break, continue, and throw continuations. These functions would throw an error since the only case when they are called is where these statements shouldn't exist.

## Todos:

Edge case tests.<br/>
