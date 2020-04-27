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
    (   ( ";",  !, { Token = ; };
        "@",  !, { Token = @ };
        "!",  !, { Token = ! };
        "enter",  !, { Token = enter };
        "exit",  !, { Token = exit };
        "when",  !, { Token = when };
        "range",  !, { Token = range };
        "between",  !, { Token = between };
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
        "display", !, {Token = display};
        "input", !, {Token = input};
        "int",  !, { Token = int };
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
assignValue(t_assign(GeneralValue,TernaryExpression)) --> word(GeneralValue),
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
%routine(t_for_routine(Declaration,Condition,Expression,Operation)) --> [when],[(],declaration(Declaration), condition(Condition),exp(Expression)
%                                                       ,[)], [repeat], operation(Operation), [endrepeat].
routine(t_for_range_routine(GeneralValue,FromNumber,ToNumber,Operation)) --> [when], word(GeneralValue), [between], [range],
    ["("],number(FromNumber),number(ToNumber),[")"],
    [repeat],operation(Operation),[endrepeat].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO : See if this fits anywhere else then routine.

routine(t_inc_operator(Identifier)) --> word(Identifier),[+],[+],[;].
routine(t_dec_operator(Identifier)) --> word(Identifier),[-],[-],[;].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Rule for evaluating ternary expressions.
ternary(t_ternary(BoolExp,GeneralValue1,GeneralValue2)) --> ['('],condition(BoolExp),[')'],[?],number(GeneralValue1),[:],number(GeneralValue2).

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

exp(t_add_horizontal_expression(Expression, VerticalExpression)) --> verticalExp(Expression), [+], exp(VerticalExpression).
exp(t_sub_horizontal_expression(Expression, VerticalExpression)) --> verticalExp(Expression), [-], exp(VerticalExpression).
exp(t_expr(Expression)) --> verticalExp(Expression).

verticalExp(t_div_vertical_expression(Number, Expression)) --> negativeNumber(Number),[/], verticalExp(Expression).
verticalExp(t_div_vertical_expression(Number, Expression)) --> number(Number),[/], verticalExp(Expression).
verticalExp(t_div_vertical_expression(Identifier, Expression)) --> word(Identifier),[/], verticalExp(Expression).
verticalExp(t_mul_vertical_expression(Number, Expression)) --> negativeNumber(Number),[*], verticalExp(Expression).
verticalExp(t_mul_vertical_expression(Number, Expression)) --> number(Number),[*], verticalExp(Expression).
verticalExp(t_mul_vertical_expression(Identifier, Expression)) --> word(Identifier), [*], verticalExp(Expression).
verticalExp(t_id(Number)) --> number(Number).
verticalExp(t_id(NegNumber)) --> negativeNumber(NegNumber).
verticalExp(t_id(Identifier)) --> word(Identifier).


%Rule for Identifier
word(t_word(Word)) --> [Word],{atom(Word)}.

%Rules for numbers
negativeNumber(t_negative_number(NumberNode)) --> [-],number(NumberNode).
number(t_number(NumberNode)) --> [NumberNode],{number(NumberNode)}.

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

%-----------------------------%%%%%%%%%%%%%%%%%%%-------------------------------

%-----------------------------%%%%%%%%%%%%%%%%%%%-------------------------------

% Interpreter for language

eval_parse(Program, EnvOut) :- eval_program(Program,[],EnvOut).

% "lookup" predicate looks up the value of identifier in environment.
lookup(_,[],0).
lookup(Key, [(Key,Val)|_],Val).
lookup(Key, [_|Tail],Val) :- lookup(Key,Tail,Val).

% lookup(Key, [], _Val) :- write(Key), write("doesn't exist"),
% writeln("Error : This variable has not been declared"),fail.

% "update" predicate updates the value of required identifier in environment.
update(Key,Value,[],[(Key,Value)]).
update(Key,Value,[(Key,_)|Tail],[(Key,Value)|Tail]).
update(Key,Value,[Head|Tail],[Head|Tail1]) :- update(Key,Value,Tail,Tail1).

% 'eval_program' evaluates the program block.

eval_program(t_program(Structure),EnvIn, EnvOut) :- eval_structure(Structure,EnvIn, EnvOut).

% 'eval_structure' evaluates the structure block.

eval_structure(t_structure(Declaration,Operation),EnvIn,EnvOut) :- eval_declaration(Declaration,EnvIn,EnvIn1)
    ,eval_operation(Operation,EnvIn1,EnvOut).

% 'eval_data_type' evaluates the datatype.

eval_var_type(t_vartype(string),EnvOut,EnvIn,EnvIn):- EnvOut=string,!.
eval_var_type(t_vartype(bool),EnvOut,EnvIn,EnvIn):- EnvOut=bool,!.
eval_var_type(t_vartype(int),EnvOut,EnvIn,EnvIn):- EnvOut=int,!.

% 'eval_declaration' evaluates the declaration block.

eval_declaration(t_declaration(VarType,Identifier),EnvIn,EnvOut) :-eval_var_type(VarType,_,EnvIn,EnvIn),
    eval_word(Identifier,_,EnvIn,EnvIn,Iden),update(Iden,0, EnvIn, EnvOut).

eval_declaration(t_declaration(VarType,Identifier,Declaration),EnvIn,EnvOut) :-eval_var_type(VarType,_,EnvIn,EnvIn),
    eval_word(Identifier,_,EnvIn,EnvIn,Iden),update(Iden,0, EnvIn, EnvIn1),
eval_declaration(Declaration,EnvIn1,EnvOut).


% 'eval_routine' evaluates the declaration block.

eval_assign(t_assign(Identifier,Expression),EnvIn,EnvOut) :- eval_word(Identifier,_,EnvIn,EnvIn,Ident),
    eval_expr(Expression,Val,EnvIn,EnvIn),
    update(Ident,Val,EnvIn,EnvOut),!.

% eval_assign(t_assign(Identifier,Expression),EnvIn,EnvOut) :- eval_word(Identifier,_,EnvIn,EnvIn,Ident),
%     eval_bool(Expression,Val,EnvIn,EnvIn),
%   update(Ident,Val,EnvIn,EnvOut),!.

 eval_assign(t_assign_wordlength(Identifier,Word),EnvIn,EnvOut) :- eval_word_length(Word,Val,EnvIn,EnvIn),
    update(Identifier,Val,EnvIn,EnvOut).

 eval_assign(t_assign(Identifier,TernaryExpression),EnvIn,EnvOut) :- eval_word(Identifier,_,EnvIn1,EnvIn,Ident),
     eval_ternary(TernaryExpression,EnvIn1,Val),
     update(Ident,Val,EnvIn,EnvOut),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO : eval assign for wordconcat,ternary.

eval_ternary(t_ternary(Boolean,_,FalseRoutine),EnvIn,EnvOut):-eval_condition(Boolean,Val,EnvIn,EnvIn),
                                                              Val = false,!,
                                                              eval_number(FalseRoutine,EnvOut,EnvIn,EnvIn).

eval_ternary(t_ternary(Boolean,TrueRoutine,_),EnvIn,EnvOut):- eval_condition(Boolean,Val,EnvIn,EnvIn),
                                                              Val = true,
                                                              eval_number(TrueRoutine,EnvOut,EnvIn,EnvIn).

eval_read(t_read_input(Identifier), EnvIn, EnvOut):- read(Id), eval_word(Identifier,_,EnvIn,EnvIn1,Ident),
                                        update(Ident, Id, EnvIn1, EnvOut).

% 'eval_operation' evaluates the operation block.

eval_operation(t_operation(AssignValue,Operation), EnvIn, EnvOut) :- eval_assign(AssignValue, EnvIn, EnvIn1),
                                                                     eval_operation(Operation,EnvIn1,EnvOut),!.

eval_operation(t_operation(Routine,Operation), EnvIn, EnvOut) :- eval_routine(Routine, EnvIn, EnvIn1),
                                                                  eval_operation(Operation,EnvIn1,EnvOut),!.

eval_operation(t_operation(Print,Operation), EnvIn, EnvOut) :- eval_print(Print, EnvIn, EnvIn1),
                                                                eval_operation(Operation,EnvIn1,EnvOut),!.

eval_operation(t_operation(ReadValue,Operation), EnvIn, EnvOut) :- eval_read(ReadValue, EnvIn, EnvIn1),
                                                                eval_operation(Operation,EnvIn1,EnvOut),!.

eval_operation(t_operation(AssignValue), EnvIn, EnvOut) :- eval_assign(AssignValue, EnvIn, EnvOut),!.

eval_operation(t_operation(Routine), EnvIn, EnvOut) :- eval_routine(Routine, EnvIn, EnvOut),!.

eval_operation(t_operation(Print), EnvIn, EnvOut) :- eval_print(Print, EnvIn, EnvOut),!.

eval_operation(t_operation(ReadValue), EnvIn, EnvOut) :- eval_read(ReadValue, EnvIn, EnvOut),!.

% 'eval_routine' evaluates the operation block.

eval_routine(t_if_routine(Boolean,_,FalseRoutine),EnvIn,EnvOut):-eval_condition(Boolean,Val,EnvIn,EnvIn),
                                                              Val = false,!,
                                                              eval_operation(FalseRoutine,EnvIn,EnvOut).

eval_routine(t_if_routine(Boolean,TrueRoutine,_),EnvIn,EnvOut):- eval_condition(Boolean,Val,EnvIn,EnvIn),
                                                              Val = true,
                                                              eval_operation(TrueRoutine,EnvIn,EnvOut).

eval_routine(t_while_routine(Boolean,_),EnvIn,EnvOut):- eval_condition(Boolean,Val,EnvIn,EnvIn), Val = false,!,EnvOut = EnvIn.

eval_routine(t_while_routine(Boolean,Routine),EnvIn,EnvOut) :- eval_condition(Boolean,Val,EnvIn,EnvIn),Val = true ,
                                                              eval_operation(Routine,EnvIn,EnvIn1),
                                                              eval_routine(t_while_routine(Boolean,Routine),EnvIn1,EnvOut).

eval_routine(t_inc_operator(Identifier),EnvIn,EnvOut) :- eval_word(Identifier,Val,EnvIn,EnvIn,Ident), Val1 is Val + 1,
    update(Ident,Val1,EnvIn,EnvOut).

eval_routine(t_dec_operator(Identifier),EnvIn,EnvOut) :- eval_word(Identifier,Val,EnvIn,EnvIn,Ident), Val1 is Val - 1,
    update(Ident,Val1,EnvIn,EnvOut).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO : eval routine for loop(traditional and range)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 'eval_condition' evaluates the condition block.

and(false,_,false).
and(_,false,false).
and(true,true,true).

or(true,_,true).
or(_,true,true).
or(false,false,false).

not(true,false).
not(false,true).

eval_condition(t_and_condition(BoolExp1,BoolExp2),EnvOut,EnvIn,EnvIn):- eval_bool(BoolExp1,Val1,EnvIn,EnvIn),
                                                                        eval_bool(BoolExp2,Val2,EnvIn,EnvIn),
                                                                        and(Val1,Val2,EnvOut),!.

eval_condition(t_or_condition(BoolExp1,BoolExp2),EnvOut,EnvIn,EnvIn):-eval_bool(BoolExp1,Val1,EnvIn,EnvIn),
    eval_bool(BoolExp2,Val2,EnvIn,EnvIn),
                                                                     or(Val1,Val2,EnvOut),!.

eval_condition(t_not_condition(BoolExp),EnvOut,EnvIn,EnvIn):-eval_bool(BoolExp,BoolOutput,EnvIn,EnvIn),
                                         not(BoolOutput,EnvOut),!.

eval_condition(t_condition(BoolExp),EnvOut,EnvIn,EnvIn):-eval_bool(BoolExp,EnvOut,EnvIn,EnvIn),!.


% 'eval_bool' evaluates the bool block.

eval_bool(t_bool_exp(false),false,EnvIn,EnvIn).
eval_bool(t_bool_exp(true),true,EnvIn,EnvIn).

eval_bool(t_equal_expression(Expr1,Expr2),EnvOut,EnvIn,EnvIn):- eval_expr(Expr1,Val1,EnvIn,EnvIn),
                                                          eval_expr(Expr2,Val2,EnvIn,EnvIn),
                                                          Val1 =:= Val2, EnvOut = true; EnvOut = false,!.

eval_bool(t_not_equal_expression(Expr1,Expr2),EnvOut,EnvIn,EnvIn):- eval_expr(Expr1,Val1,EnvIn,EnvIn),
                                                          eval_expr(Expr2,Val2,EnvIn,EnvIn),
                                                          Val1 =\= Val2, EnvOut = true; EnvOut = false,!.

eval_bool(t_bool_equal_expression(Expr1,Expr2),EnvOut,EnvIn,EnvIn):- eval_expr(Expr1,Val1,EnvIn,EnvIn),
                                                         eval_bool(Expr2,Val2,EnvIn,EnvIn),
                                                         Val1 =:= Val2, EnvOut = true; EnvOut = false,!.

eval_bool(t_bool_not_equal_expression(Expr1,Expr2),EnvOut,EnvIn,EnvIn):- eval_expr(Expr1,Val1,EnvIn,EnvIn),
                                                         eval_bool(Expr2,Val2,EnvIn,EnvIn),
                                                         Val1 =\= Val2, EnvOut = true; EnvOut = false,!.

eval_bool(t_less_than_expression(Expr1,Expr2),EnvOut,EnvIn,EnvIn):- eval_expr(Expr1,Val1,EnvIn,EnvIn),
                                                          eval_expr(Expr2,Val2,EnvIn,EnvIn),
                                                          Val1 < Val2, EnvOut = true; EnvOut = false,!.

eval_bool(t_greater_than_expression(Expr1,Expr2),EnvOut,EnvIn,EnvIn):- eval_expr(Expr1,Val1,EnvIn,EnvIn),
                                                          eval_expr(Expr2,Val2,EnvIn,EnvIn),
                                                          Val1 > Val2, EnvOut = true; EnvOut = false,!.

eval_bool(t_less_than_equal_expression(Expr1,Expr2),EnvOut,EnvIn,EnvIn):- eval_expr(Expr1,Val1,EnvIn,EnvIn),
                                                          eval_expr(Expr2,Val2,EnvIn,EnvIn),
                                                          Val1 =< Val2, EnvOut = true; EnvOut = false,!.

eval_bool(t_greater_than_equal_expression(Expr1,Expr2),EnvOut,EnvIn,EnvIn):- eval_expr(Expr1,Val1,EnvIn,EnvIn),
                                                          eval_expr(Expr2,Val2,EnvIn,EnvIn),
                                                          Val1 >= Val2, EnvOut = true; EnvOut = false,!.


% 'eval_expr' evaluates the expression block.


eval_expr(t_add_horizontal_expression(TermNode,ExpressionNode),EnvOut,EnvIn,EnvIn):- eval_vertical_expr(TermNode,Output1,EnvIn,EnvIn),
                                                                    eval_expr(ExpressionNode,Output2,EnvIn,EnvIn),
                                                                    EnvOut is Output1 + Output2,!.

eval_expr(t_sub_horizontal_expression(Expr1,Expr2),EnvOut,EnvIn,EnvIn) :- eval_vertical_expr(Expr1,Val1,EnvIn,EnvIn),
       eval_expr(Expr2,Val2,EnvIn,EnvIn),
                                                                        EnvOut is Val1-Val2,!.

eval_expr(t_expr(Expr1),EnvOut,EnvIn,EnvIn) :- eval_vertical_expr(Expr1,EnvOut,EnvIn,EnvIn).


eval_vertical_expr(t_mul_vertical_expression(Expr1,Expr2),EnvOut,EnvIn,EnvIn) :-
       eval_word(Expr1,Val1,EnvIn,EnvIn,_),eval_vertical_expr(Expr2,Val2,EnvIn,EnvIn),
    EnvOut is Val1*Val2,!.

eval_vertical_expr(t_mul_vertical_expression(Expr1,Expr2),EnvOut,EnvIn,EnvIn) :-
       eval_neg_number(Expr1,Val1,EnvIn,EnvIn),eval_vertical_expr(Expr2,Val2,EnvIn,EnvIn),
    EnvOut is Val1*Val2,!.

eval_vertical_expr(t_mul_vertical_expression(Expr1,Expr2),EnvOut,EnvIn,EnvIn) :-
       eval_number(Expr1,Val1,EnvIn,EnvIn),eval_vertical_expr(Expr2,Val2,EnvIn,EnvIn),
    EnvOut is Val1*Val2,!.

eval_vertical_expr(t_div_vertical_expression(Expr1,Expr2),EnvOut,EnvIn,EnvIn) :-
       eval_word(Expr1,Val1,EnvIn,EnvIn,_),eval_vertical_expr(Expr2,Val2,EnvIn,EnvIn),
    EnvOut is Val1/Val2,!.

eval_vertical_expr(t_div_vertical_expression(Expr1,Expr2),EnvOut,EnvIn,EnvIn) :-
       eval_neg_number(Expr1,Val1,EnvIn,EnvIn),eval_vertical_expr(Expr2,Val2,EnvIn,EnvIn),
    EnvOut is Val1/Val2,!.

eval_vertical_expr(t_div_vertical_expression(Expr1,Expr2),EnvOut,EnvIn,EnvIn) :-
       eval_number(Expr1,Val1,EnvIn,EnvIn),eval_vertical_expr(Expr2,Val2,EnvIn,EnvIn),
    EnvOut is Val1/Val2,!.

eval_vertical_expr(t_id(Identifier),EnvOut,EnvIn,EnvIn):-eval_word(Identifier,EnvOut,EnvIn,EnvIn,_).

eval_vertical_expr(t_id(Number),EnvOut,EnvIn,EnvIn):-eval_number(Number,EnvOut,EnvIn,EnvIn),!.

eval_vertical_expr(t_id(NegativeNumber),EnvOut,EnvIn,EnvIn):-eval_neg_number(NegativeNumber,EnvOut,EnvIn,EnvIn),!.

eval_neg_number(t_negative_number(Number),EnvOut,EnvIn,EnvIn):-eval_number(Number,EnvOut1,EnvIn,EnvIn),EnvOut is 0-EnvOut1,!.

eval_number(t_number(Number),EnvOut,EnvIn,EnvIn):-EnvOut is Number,!.

eval_word(t_word(Identifier),Output,EnvIn,EnvIn,Ident):-lookup(Identifier, EnvIn, Output),!,Ident = Identifier.

% 'eval_print' for printing expressions.

eval_print(t_print_expression(Expr),EnvIn,EnvOut) :-  eval_expr(Expr,Val,EnvIn,EnvOut),write(Val).

eval_print(t_print(Word), EnvIn, EnvIn) :- write(Word), write(" "),!.

eval_word_length(t_wordlength(Word),EnvOut,EnvIn,EnvIn) :- eval_word(Word,EnvOut,EnvIn,EnvIn,_), atom_length(Word,EnvOut).

eval_word_concat(t_word_concat(Word1,Word2),EnvIn,EnvOut) :- eval_word(Word1,EnvOut,EnvIn,EnvIn,_) ,
    eval_word(Word2,EnvOut,EnvIn,EnvIn,_),atom_concat(Word1,Word2,EnvOut).
