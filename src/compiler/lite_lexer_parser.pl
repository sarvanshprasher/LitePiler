:-table exp/2,verticalExp/2.

% Rule for the main function of language.
program -->structure,[.].

% Rule for structure inside the program
structure -->[enter],declaration,operation,[exit].

% Rule for declarations inside the structure
declaration -->[const],word,[=],number,[;],declaration.
declaration -->varType,word,[;],declaration.
declaration --> varType,word,[=],word,[;],declaration.
declaration--> [const],word,[=],number.
declaration--> varType,word.
declaration --> varType,word,[=],word.

% Rule for variable types in language.
varType --> [int].
varType --> [bool].
varType --> [string].

% Rule for assigning values to variable.
assignValue --> word, [=] ,exp, [;].
assignValue -->  word, [is], boolExp, [;].
assignValue -->  word, [=], ternary, [;].

% Rule for the operations done in between structure.
operation --> declaration,operation.
operation --> assignValue, operation.
operation --> routine, operation.
operation --> print, operation.
operation--> structure,[;],operation.
operation --> declaration.
operation --> assignValue.
operation --> routine.
operation --> print.

% Rule for the routines done in between operations.
routine --> word,[:=],exp,[;],routine.
routine --> structure,[;],routine.
routine --> word,[:=],exp.
routine --> [if], condition, [then], operation, [else], operation, [endif].
routine -->[while],condition,[do],operation,[endwhile]|structure.
routine --> [when], condition, [repeat], operation, [endrepeat].
routine --> [when], word, [in], [range],["("],number,number,[")"],
    [repeat],operation,[endrepeat].

% Rule for evaluating ternary expressions.
ternary --> ["("],boolExp,[")"],["?"],generalValue,[:],generalValue.

% Rule for conditions in routines.
condition --> boolExp, [and], boolExp.
condition --> boolExp, [or], boolExp.
condition --> [~], boolExp.
condition --> [not], boolExp.
condition --> boolExp.

% Rule for determining boolean expression.
boolExp --> [true].
boolExp --> [false].
boolExp --> [not], boolExp.
boolExp --> exp,[=],exp.
boolExp --> exp, [:=:], exp.
boolExp --> exp, [~=], exp.
boolExp --> exp, [<],[=], exp.
boolExp --> exp, [>],[=], exp.
boolExp --> exp, [<], exp.
boolExp --> exp, [>], exp.
boolExp --> exp, [:=:], boolExp.
boolExp --> exp, [~=], boolExp.

% Rule for evaluating the horizontal expression(includes addition & difference).
exp --> exp,horizontal,verticalExp | verticalExp.
horizontal --> [+].
horizontal --> [-].

% Rule for evaluating the vertical expression(includes multiplication & division).
verticalExp --> verticalExp,vertical,paranthesis | paranthesis.
vertical --> [*].
vertical --> [/].

% Rule for evaluating the expression inside paranthesis.
paranthesis --> ["("] , exp , [")"].
paranthesis --> generalValue.

% Rule for evaluating the expression inside paranthesis.
generalValue --> word|number.

% Rule for including word & numbers.
word --> [X],{atom(X)}.
number --> [X],{number(X)}.

% Rule for printing values.
print --> [display],exp,[;].
