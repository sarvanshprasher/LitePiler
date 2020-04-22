
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
