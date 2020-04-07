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
