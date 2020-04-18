% @author Sarvansh Prasher
% @version 1.0

% @edited by Rohit Kumar Singh
% @version 2.0

% @edited by Surya Chatterjee
% @version 3.0

% @edited by Abhishek
% @version 4.0

% Name of Language : LitePiler

% This file contains the lexer and parser part of programming language.
% Role of lexer is to segregate the code in form of tokens
% which will help parser to understand the code better.

% Role of lexer is to find out if there is any unknown symbol which is being
% used or to detect typos if there are any.

% References for Lexer : Own interpreted programming language with prolog. Part 1 - Lexer
% (https://steemit.com/science/@whd/own-interpreted-programming-language-with-prolog-part-1-lexer)

%-----------------------------%%%%%%%%%%%%%%%%%%%-------------------------------

lexer(Tokens) -->
    white_space,
    (( ";",  !, { Token = ; };
        "!",  !, { Token = ! };
        "enter",  !, { Token = enter };
        "exit",  !, { Token = exit };
        "when",  !, { Token = when };
        "in",  !, { Token = in };
        "range",  !, { Token = range };
        "repeat",  !, { Token = repeat };
        "endrepeat",  !, { Token = endrepeat };
        "if",  !, { Token = if };
        "then",  !, { Token = then };
        "else",  !, { Token = else };
        "endif",  !, { Token = endif };
        "while",  !, { Token = while };
        "do",  !, { Token = do };
        "endwhile",  !, { Token = endwhile };
        "true",  !, { Token = true };
        "false",  !, { Token = false };
        "and",  !, { Token = and };
        "or",  !, { Token = or };
        "not",  !, { Token = not };
        "~",  !, { Token = ~ };
        "int",  !, { Token = var };
        "bool",  !, { Token = bool };
        "String",  !, { Token = String};
        ">",  !, { Token = > };
        "<",  !, { Token = < };
        "<=", !, { Token = <= };
        ">=", !, { Token = >= };
        "~=", !, { Token = ~= };
        "+",  !, { Token = +  };
        "-",  !, { Token = -  };
        "*",  !, { Token = *  };
        "/", !, { Token = /  };
        "=",  !, { Token = =  };
        ":=:",  !, { Token = :=:  };
        "is",  !, { Token = is  };
        ":", !, {Token = :};
        "?", !, {Token = ?};
        ".", !, {Token = .};
        "length", !, {Token = length};
        "join", !, {Token = join};
        "display", !, {Token = display};
        digit(D),  !, number(D, N), { Token = N };
        lowletter(L), !, identifier(L, Id),{  Token = Id};
        upletter(L), !, identifier(L, Id), { Token = Id };
        [Un], { Token = tkUnknown, throw((unrecognized_token, Un)) }),!,
        { Tokens = [Token | TokList] },lexer(TokList);
    	  [],{ Tokens = [] }).

white_space --> [Char], { code_type(Char,space) }, !, white_space.
white_space --> [].

digit(D) -->[D], { code_type(D, digit) }.
digits([D|T]) --> digit(D),!,digits(T).
digits([]) -->[].

number(D, N) -->digits(Ds),      { number_chars(N, [D|Ds]) }.

upletter(L) -->[L], { code_type(L, upper) }.

lowletter(L) -->[L], { code_type(L, lower) }.

alphanum([A|T]) --> [A], { code_type(A, csym) }, !, alphanum(T).

alphanum([]) --> [].

identifier(L, Id) --> alphanum(As),{ atom_codes(Id, [L|As]) }.


%-----------------------------%%%%%%%%%%%%%%%%%%%-------------------------------

:-table exp/2,verticalExp/2.

% Rule for the main function of language.
program(t_program(Structure)) -->structure(Structure).

% Rule for structure inside the program
structure(t_structure(Declaration,Operation)) -->[enter],declaration(Declaration),
    										operation(Operation),[exit].

% Rule for declarations inside the structure
declaration(t_declaration(GeneralValue,Expression)) -->[const],generalValue(GeneralValue),
    										[=],exp(Expression),[;],declaration.
declaration(t_declaration(VarType,GeneralValue)) --> varType(VarType),
    				generalValue(GeneralValue),[;],declaration.
declaration(t_declaration()) --> [].

% Rule for variable types in language.
varType(t_vartype(int)) --> [int].
varType(t_vartype(bool)) --> [bool].
varType(t_vartype(string)) --> [string].

% Rule for assigning values to variable.
assignValue(t_assign_expression(GeneralValue,Expression)) --> generalValue(GeneralValue),
    													[=] ,exp(Expression), [;].
assignValue(t_assign_wordlength(GeneralValue,WordLength)) --> generalValue(GeneralValue),
                                                                 [=] ,wordLength(WordLength), [;].
assignValue(t_assign_wordconcat(GeneralValue,WordConcat)) --> generalValue(GeneralValue)
                                                                 , [=] ,wordConcat(WordConcat), [;].
assignValue(t_assign_boolexp(GeneralValue,BoolExpression)) --> generalValue(GeneralValue),
                                                                 [is], boolExp(BoolExpression), [;].
assignValue(t_assign_ternary(GeneralValue,TernaryExpression))  --> generalValue(GeneralValue),
                                                                  [=], ternary(TernaryExpression), [;].
% Rule for the operations done in between structure.

operation(t_operation(Declaration,Operation)) --> declaration(Declaration),operation(Operation).

operation(t_operation(AssignValue,Operation)) --> assignValue(AssignValue), operation(Operation).

operation(t_operation(Routine,Operation)) --> routine(Routine), operation(Operation).

operation(t_operation(Print,Operation)) --> print(Print), operation(Operation).

operation(t_operation(Comment,Operation))  --> comment(Comment),operation(Operation).

operation(t_operation(Structure,Operation))--> structure(Structure),[;],operation(Operation).

operation(t_operation(Declaration)) --> declaration(Declaration).

operation(t_operation(AssignValue)) --> assignValue(AssignValue).

operation(t_operation(Routine)) --> routine(Routine).

operation(t_operation(Print)) --> print(Print).

operation(t_operation(Comment)) --> comment(Comment).


% Rule for the routines done in between operations.

routine(t_routine_structure(Structure)) --> structure(Structure),[;].

routine(t_if_routine(Condition,TrueOperation,FalseOperation)) --> [if], condition(Condition), [then],

                                          operation(TrueOperation), [else], operation(FalseOperation), [endif].

routine(t_while_routine(Condition,Operation)) -->[while],condition(Condition),[do],operation(Operation),[endwhile].

routine(t_for_routine(Condition,Operation)) --> [when], condition(Condition), [repeat], operation(Operation), [endrepeat].

routine(t_for_range_routine(GeneralValue,FromNumber,ToNumber,Operation)) --> [when], generalValue(GeneralValue), [in], [range],["("],number(FromNumber),number(ToNumber),[")"],

    [repeat],operation(Operation),[endrepeat].

% Rule for evaluating ternary expressions.
ternary --> ["("],boolExp,[")"],[?],generalValue,[:],generalValue.

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
boolExp --> exp, [:=:], boolExp.
boolExp --> exp, [~=], boolExp.
boolExp --> exp, [<], exp.
boolExp --> exp, [>], exp.
boolExp --> exp, [<],[=], exp.
boolExp --> exp, [>],[=], exp.

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

% Rule for negative numbers.
negativeNumber --> [-],number.

% Rule for including word & numbers.
word --> [X],{atom(X)}.
number --> [X],{number(X)}.

% Rule for printing values.
print --> [display],exp,[;].

% Rule for comments inside block.
comment --> [!] , statement, [!].

% Rules for statements inside comment.
statement --> word,statement.
statement --> number,statement.
statement --> word.
statement --> number.

% Rule for finding length of string
wordLength --> word,[.],[length].

% Rule for string concatenation operation
wordConcat --> word,[.],[join],[.],word.
