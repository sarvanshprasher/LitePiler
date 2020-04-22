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

%% Sample parser run query :
%% trace, (L = [enter,string,sar,;,const, x, ;,int, y, ;, int, z,;,x,=,1,;
%% ,!,t,s,roh,!,z,=,sar,.,length,;,if, x, :=:, y, +, 2, then, z , = , 5, ;,else, z, =, 3, ;,endif,
%% while, not, x, :=:, z, do, z, =, z, +, 2,;, endwhile,when,i,in,range,"(",1,4,")",repeat,x,=,2,;
%% ,endrepeat,display,x,;,exit],program(P, L, [])).

:- style_check(-singleton).

litepiler(FileName) :- open(FileName, read, InStream),
              tokenCodes(InStream, TokenCodes),
               phrase(lexer(Tokens), TokenCodes),
              parse(ParseTree, Tokens, []),
              close(InStream),
              split_string(FileName, ".", "", L),
              L = [H|_T],
              atom_concat(H, ".lpy", X),
              open(X, write, OutStream),
              write(OutStream, ParseTree),
              write(OutStream, '.'),
              close(OutStream),
              eval_parse(ParseTree, EnvOut).


%-----------------------------%%%%%%%%%%%%%%%%%%%-------------------------------

tokenCodes(InStream, []) :- at_end_of_stream(InStream), !.
tokenCodes(InStream, [TokenCode|RemTokens]) :- get_code(InStream, TokenCode), tokenCodes(InStream, RemTokens).

%-----------------------------%%%%%%%%%%%%%%%%%%%-------------------------------

% Lexer for conversion

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
        "String",  !, { Token = string};
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
     	"input", !, {Token = input};
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
% Parser for language

:- use_rendering(svgtree).

:- table exp/3,verticalExp/3.

parse(Program) --> program(Program).

% Rule for the main function of language.
program(t_program(Structure)) --> structure(Structure).

% Rule for structure inside the program
structure(t_structure(Declaration,Operation)) -->[enter],declaration(Declaration),
    										operation(Operation),[exit].

% Rule for variable types in language.
varType(t_vartype(int)) --> [int].
varType(t_vartype(bool)) --> [bool].
varType(t_vartype(string)) --> [string].

% Rule for declarations inside the structure
declaration(t_declaration(VarType,GeneralValue)) --> varType(VarType),
   				word(GeneralValue),[;].
declaration(t_declaration(VarType,GeneralValue,Declaration)) --> varType(VarType),
  				word(GeneralValue),[;],declaration(Declaration).

% Rule for assigning values to variable.
assignValue(t_assign(GeneralValue,Expression)) --> word(GeneralValue),
    													[=] ,exp(Expression), [;].
assignValue(t_assign(GeneralValue,BoolExpression)) --> word(GeneralValue),
                                                                 [is], boolExp(BoolExpression), [;].
assignValue(t_assign_wordlength(GeneralValue,WordLength)) --> word(GeneralValue),
                                                                 [=] ,wordLength(WordLength), [;].
assignValue(t_assign_wordconcat(GeneralValue,WordConcat)) --> word(GeneralValue)
                                                                 , [=] ,wordConcat(WordConcat), [;].
assignValue(t_assign(GeneralValue,TernaryExpression))  --> word(GeneralValue),
                                                                  [=], ternary(TernaryExpression), [;].

% Rule for reading the input from system.
readValue(t_read_input(Identifier)) --> [input], word(Identifier), [;].

% Rule for the operations done in between structure.
operation(t_operation(AssignValue,Operation)) --> assignValue(AssignValue), operation(Operation).
operation(t_operation(Routine,Operation)) --> routine(Routine), operation(Operation).
operation(t_operation(Print,Operation)) --> print(Print), operation(Operation).
operation(t_operation(Comment,Operation))  --> comment(Comment),operation(Operation).
operation(t_operation(ReadValue, Operation)) --> readValue(ReadValue), operation(Operation).
operation(t_operation(AssignValue)) --> assignValue(AssignValue).
operation(t_operation(Routine)) --> routine(Routine).
operation(t_operation(Print)) --> print(Print).
operation(t_operation(Comment)) --> comment(Comment).
operation(t_operation(ReadValue)) --> readValue(ReadValue).

% Rule for the routines done in between operations.
routine(t_if_routine(Condition,TrueOperation,FalseOperation)) --> [if], condition(Condition), [then],
                                          operation(TrueOperation), [else], operation(FalseOperation), [endif].
routine(t_while_routine(Condition,Operation)) -->[while],condition(Condition),[do],operation(Operation),[endwhile].
routine(t_for_routine(Condition,Operation)) --> [when], condition(Condition), [repeat], operation(Operation), [endrepeat].
routine(t_for_range_routine(GeneralValue,FromNumber,ToNumber,Operation)) --> [when], word(GeneralValue), [in], [range],["("],number(FromNumber),number(ToNumber),[")"],
    [repeat],operation(Operation),[endrepeat].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO : See if this fits anywhere else then routine.

routine(t_inc_operator(Identifier)) --> word(Identifier),[+],[+],[;].
routine(t_dec_operator(Identifier)) --> word(Identifier),[-],[-],[;].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Rule for evaluating ternary expressions.
ternary(t_ternary(BoolExp,GeneralValue1,GeneralValue2)) --> ["("],boolExp(BoolExp),[")"],[?],exp(GeneralValue1),[:],exp(GeneralValue2).

% Rule for conditions in routines.
condition(t_and_condition(BoolExp1,BoolExp2)) --> boolExp(BoolExp1), [and], boolExp(BoolExp2).
condition(t_or_condition(BoolExp1,BoolExp2)) --> boolExp(BoolExp1), [or], boolExp(BoolExp2).
condition(t_not_condition(BoolExp1)) --> [not], boolExp(BoolExp1).
condition(t_condition(BoolExp)) --> boolExp(BoolExp).

% Rule for determining boolean expression.
boolExp(t_bool_exp(true)) --> [true].
boolExp(t_bool_exp(false)) --> [false].
boolExp(t_equal_expression(Expression1,Expression2)) --> exp(Expression1),[:=:],exp(Expression2).
boolExp(t_not_equal_expression(Expression1,Expression2)) --> exp(Expression1), [~=], exp(Expression2).
boolExp(t_bool_equal_expression(Expression,BoolExpression)) --> exp(Expression), [:=:], boolExp(BoolExpression).
boolExp(t_bool_not_equal_expression(Expression,BoolExpression)) --> exp(Expression), [~=], boolExp(BoolExpression).
boolExp(t_less_than_expression(Expression1,Expression2)) --> exp(Expression1), [<], exp(Expression2).
boolExp(t_greater_than_expression(Expression1,Expression2)) --> exp(Expression1), [>], exp(Expression2).
boolExp(t_less_than_equal_expression(Expression1,Expression2)) --> exp(Expression1), [<],[=], exp(Expression2).
boolExp(t_greater_than_equal_expression(Expression1,Expression2)) -->  exp(Expression1), [>],[=], exp(Expression2).

% Rule for evaluating the horizontal expression(includes addition & difference).
exp(t_add_horizontal_expression(Expression,VerticalExpression)) --> verticalExp(VerticalExpression),[+],exp(Expression).
exp(t_sub_horizontal_expression(Expression,VerticalExpression)) --> verticalExp(VerticalExpression),[-],exp(Expression).
exp(t_expr(Expression)) --> verticalExp(Expression).

% Rule for evaluating the vertical expression(includes multiplication & division).

verticalExp(t_div_vertical_expression(Number, Expression)) --> negativeNumber(Number),
    													[/], verticalExp(Expression).
verticalExp(t_div_vertical_expression(Number, Expression)) --> number(Number),
    													[/], verticalExp(Expression).
verticalExp(t_div_vertical_expression(Identifier, Expression)) --> word(Identifier),
    													[/], verticalExp(Expression).
verticalExp(t_mul_vertical_expression(Number, Expression)) --> negativeNumber(Number),
    													[*], verticalExp(Expression).
verticalExp(t_mul_vertical_expression(Number, Expression)) --> number(Number),
    													[*], verticalExp(Expression).
verticalExp(t_mul_vertical_expression(Identifier, Expression)) --> word(Identifier),
    													[*], verticalExp(Expression).
verticalExp(t_id(Identifier)) --> word(Identifier).
verticalExp(t_id(Number)) --> number(Number).
verticalExp(t_id(NegNumber)) --> negativeNumber(NegNumber).

% Rule for negative numbers,negative numbers,words.
negativeNumber(t_negative_number(Number)) --> [-],number(Number).
word(t_word(Word)) --> [Word],{atom(Word)}.
number(t_number(Number)) --> [Number],{number(Number)}.

% Rule for comments inside block.
comment(t_comment(Statement)) --> [!] , statement(Statement), [!].

% Rules for statements inside comment.
statement(t_statement(Word,Statement)) --> word(Word),statement(Statement).
statement(t_statement(Number,Statement)) --> number(Number),statement(Statement).
statement(t_statement(Word)) --> word(Word).
statement(t_statement(Number)) --> number(Number).

% Rule for finding length of string
wordLength(t_wordlength(Word)) --> word(Word),[.],[length].

% Rule for string concatenation operation
wordConcat(t_word_concat(Word,Word)) --> word(Word),[.],[join],[.],word(Word).

% Rule for printing values.
print(t_print_expression(Expression)) --> [display],exp(Expression),[;].

print(t_print(Words)) --> [display], [@],statement(Words),[@],[;],!.
