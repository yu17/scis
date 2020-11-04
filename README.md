# EECS345_interpreter

A Simple Interpreter for a C-like language implemented in Scheme.

All features, including the optional features, are implemented.

## Usage:

The main interpreter program file is `scis.scm`. It's a scheme flavor implementation. Requires `mit-scheme` to be installed.<br/>
The provided parser `functionParser.scm` has been modified so as to be run under `mit-scheme`.<br/>

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
Instructions: var, if, while, return, begin, break, continue, try, catch, finally, throw, function.

#### Implementation:<br/>
First of all, the implementation of the state machine has been changed to hold two stacks, one for the variables and the other for the functions. The two stacks have their own operating functions, and the two stacks are wrapped together as an "Environment". The environment is controlled through the wrapper functions. (See below for the detail about the data structure.)

The concept of "Environment" here is slightly different from what was defined in the assignment prompt. Here, we use "Environment simply to refer to the combination of the variable stack and the function stack. And we use a special technique to create proper execution environment for the functions.

Observe that the execution environment of a function is the code block in which it is defined. In other words, the correct stack layer on which the function should be executed is the layer on which we find its definition. Therefore, all we need to do to create the correct environment is to keep popping the stack until we see the function definition on the top.

However, notice that when parsing the argument list, we need to first add a new layer, on which the function would be executed, and then parse the argument list and store them on the new layer. While at the same time, the layers between the new layer and the layer on which the function is defined need to be popped out. As a result, we need a function that first captures the top layer, passes it to the other function that pops the layers, and then pushes the top layer back.

We implemented four functions, EL_dump, EL_dumper, EL_restore, EL_restorer, to do exactly what was described above. Such implementation saves the memory by not copying the content of the stack.

Also, the c-return continuation we creates accept both a state and a value so that both the state and the value after execution the function could be returned. However, the value function and the state function for interpreting the function ares still separated, and the c-return continuations in them are different.

## Function Naming Conventions:

Functions are classfied by the prefixes in their names.

#### S\_\*
Acronym for Statelayer. State layer operation functions.<br/>
Each layer of the states are stored in the format: ((variable\_name\_list) (variable\_value\_list))

#### SL\_\*
Acronym for StateList. State list operation function.<br/>
The layers of the states are stacked in the format: (layer1 layer2 layer3 ...)

#### F\_\*
Acronym for Functionlayer. Function layer operation functions.<br/>
Each layer of the functions are stored in the format: (function1 function2 function3)

#### FL\_\*
Acronym for FunctionList. State list operation function.<br/>
The layers of the functions are stacked in the format: (layer1 layer2 layer3 ...)

#### EL\_\*
Acronym for EnvironmentList. Environment list operation function.<br/>
The environment wraps the StateLists and the FunctionLists in the format: (StateList FunctionList)

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
