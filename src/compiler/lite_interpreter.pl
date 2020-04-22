
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


% 'eval_assign' evaluates the assignment block.

eval_assign(t_assign(Identifier,Expression),EnvIn,EnvOut) :- eval_word(Identifier,_,EnvIn,EnvIn,Ident),
    																eval_expr(Expression,Val,EnvIn,EnvIn),
    																update(Ident,Val,EnvIn,EnvOut),!.

eval_assign(t_assign(Identifier,Expression),EnvIn,EnvOut) :- eval_word(Identifier,_,EnvIn,EnvIn,Ident),
    																eval_bool(Expression,Val,EnvIn,EnvIn),
    																update(Ident,Val,EnvIn,EnvOut),!.

 eval_assign(t_assign_wordlength(Identifier,Word),EnvIn,EnvOut) :- eval_word_length(Word,Val,EnvIn,EnvIn),
    																update(Identifier,Val,EnvIn,EnvOut).

eval_assign(t_assign(Identifier,TernaryExpression),EnvIn,EnvOut) :- eval_word(Identifier,_,EnvIn,EnvIn,Ident),
    																eval_ternary(TernaryExpression,Val,EnvIn),
    																update(Ident,Val,EnvIn,EnvOut),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO : eval assign for wordconcat,ternary.


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

eval_routine(t_inc_operator(Identifier),EnvIn,EnvOut) :- eval_expr(Identifier,Val,EnvIn,EnvIn), Val1 is Val + 1,
    update(Identifier,Val1,EnvIn,EnvOut).

eval_routine(t_dec_operator(Identifier),EnvIn,EnvOut) :- eval_expr(Identifier,Val,EnvIn,EnvIn), Val1 is Val - 1,
    update(Identifier,Val1,EnvIn,EnvOut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO : eval routine for loop(traditional and range)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 'eval_ternary' for evaluating ternary block.

eval_ternary(t_ternary(Boolean,_,FalseRoutine),EnvIn,EnvOut):-eval_bool(Boolean,Val,EnvIn,EnvIn),
                                                              Val = false,
                                                              eval_expr(FalseRoutine,EnvOut,EnvIn,EnvIn).

eval_ternary(t_ternary(Boolean,TrueRoutine,_),EnvIn,EnvOut):- eval_condition(Boolean,Val,EnvIn,EnvIn),
                                                              Val = true,
                                                              eval_expr(TrueRoutine,EnvOut,EnvIn,EnvIn).

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

eval_condition(t_not_condition(BoolExp),EnvOut,EnvIn,EnvIn):-
    eval_bool(BoolExp,BoolOutput,EnvIn,EnvIn),
                                         not(BoolOutput,EnvOut),!.

eval_condition(t_condition(BoolExp),EnvOut,EnvIn,EnvIn):-
    eval_bool(BoolExp,EnvOut,EnvIn,EnvIn),!.


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
