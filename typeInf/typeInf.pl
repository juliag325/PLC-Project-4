:- dynamic 
    gvar/2.
:- dynamic 
    localVar/2.
/* match functions by unifying with arguments 
    and infering the result
*/
/*global var*/
typeExp(X, T) :-
    \+ var(X),
    gvar(X, T),
    not(is_list(T)).

/*local var*/
typeExp(X, T) :-
    \+ var(X),
    localVar(X, T).

/*float type*/
typeExp(X, float) :-
    \+ var(X),
    float(X).

/*int type*/
typeExp(X, int) :-
    \+ var(X),
    integer(X).

/*bool type*/
typeExp(X, bool) :-
    \+ var(X),
    typeBoolExp(X).

typeExp(Fct, T):-
    \+ var(Fct), /* make sure Fct is not a variable */ 
    \+ atom(Fct), /* or an atom */
    functor(Fct, Fname, _Nargs), /* ensure we have a functor */
    !, /* if we make it here we do not try anything else */
    Fct =.. [Fname|Args], /* get list of arguments */
    append(Args, [T], FType), /* make it loook like a function signature */
    functionType(Fname, TArgs), /* get type of arguments from definition */
    typeExpList(FType, TArgs). /* recurisvely match types */

/* propagate types */
typeExp(T, T).

/* list version to allow function mathine */
typeExpList([], []).
typeExpList([Hin|Tin], [Hout|Tout]):-
    typeExp(Hin, Hout), /* type infer the head */
    typeExpList(Tin, Tout). /* recurse */


hasComparison(int).
hasComparison(float).
hasComparison(string).

hasBoolean(bool).

hasInt(int).

hasAdd(float).
hasAdd(int).


/* predicate to infer types for boolean expressions */
typeBoolExp(true).
typeBoolExp(false). 
typeBoolExp(X):-
    localVar(X, bool).
typeBoolExp(X):-
    gvar(X, bool).
typeBoolExp( X >= Y) :-
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).
typeBoolExp(X < Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).
typeBoolExp(X > Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).
typeBoolExp( X =< Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).
typeBoolExp( X == Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).
typeBoolExp( not(X) ) :- 
    typeExp(X, T),
    hasBoolean(T).
typeBoolExp( X \== Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).
typeBoolExp( X ; Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasBoolean(T).
typeBoolExp( X , Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasBoolean(T).

/* TODO: add statements types and their type checking */
/* global variable definition
    Example:
        gvLet(v, T, int) ~ let v = 3;
 */
typeStatement(X, T) :-
    typeExp(X, T).

typeStatement(gvLet(Name, T, Code), unit):- 
    atom(Name), /* make sure we have a bound name */
    typeExp(Code, T), /* infer the type of Code and ensure it is T */
    bType(T), /* make sure we have an infered type */
    asserta(gvar(Name, T)). /* add definition to database */
/*typeStatement(gfLet(hi, [a,b,c], [gvLet(mult, T, (2+5)), exp(2+4)]), T1).*/
typeStatement(gfLet(Name, Args, Code), T):- /*match code to args*/
    atom(Name),
    is_list(Code),
    is_list(Args),
    typeCodeFunc(Code, Args,[],ReturnT, T0),
    append(T0,[ReturnT],T),
    asserta(gvar(Name, T)). 
/*typeStatement(if((3 < 10),[gvLet(mult, T, iplus(X,Y))],[gvLet(mult, T, iplus(X,Y))]), T1).*/
typeStatement(if(Cond, TrueB, FalseB), T) :-
    typeBoolExp(Cond),
    typeCode(TrueB, T),
    typeCode(FalseB, T).
/*typeStatement(for(i, 5+6, 7-9, [gvLet(mult, T, iplus(X,Y))]), T1).*/
typeStatement(for(Name, CodeS, CodeE, Code), T):- 
    atom(Name), 
    typeExp(CodeS, T1),
    typeExp(CodeE, T1), 
    hasInt(T1),
    asserta(localVar(Name,T1)),
    is_list(Code),
    typeCode(Code, T),
    deleteLocalVars().
/*typeStatement(exp(2+4),T).*/
typeStatement(exp(Code), T):-
    typeExp(Code, T),
    bType(T).
/*typeStatement(letin(a, T1, 2+5, [exp(a+6)]),T).*/
/*typeCode([gvLet(mult, T2, 2+7),exp(mult + 9) ,gfLet(hi, [a,b], [for(i, 2, 5, [letin(c, T3, 2+5, [exp(a+c)])])]), exp(9 < 3), hi(2,mult), letin(a, T1, 2+5, [exp(a+6)])], T).*/
typeStatement(letin(Name, T1, CodeE, Code), T):- /*function and var LOCAL*/
    atom(Name), 
    typeExp(CodeE, T1),
    bType(T1),
    asserta(localVar(Name,T1)),
    is_list(Code),
    typeCode(Code, T),
    deleteLocalVars(). /*store in local*/

/* Code is simply a list of statements. The type is 
    the type of the last statement 
*/
/*typeCode([gfLet(hi, [a,b,c], [gvLet(mult, T, (2+5)), exp(2+4)]), exp(9 < 3), gfLet(hi, [a,b], [gvLet(mult, T, (2+5)), exp(5 < 9)])], T1).*/
/*typeCode([gfLet(hi, [a,b], [for(i, 2, 5, [exp(a+b)])]), exp(9 < 3), hi(2.2,5.6)], T1).*/
/*typeCode([gfLet(hi, [a,b], [for(i, 2, 5, [exp(a+b)])]), exp(9 < 3), hi(2.2,5.6), letin(a, T1, 2+5, [exp(a+6)])], T).*/
/*typeCode([gvLet(mult, T2, 2+7),exp(mult + 9) ,gfLet(hi, [a,b], [for(i, 2, 5, [letin(c, T3, 2+5, [exp(a+b)])])]), exp(9 < 3), hi(2,mult), letin(a, T1, hi(9,mult), [exp(a+6)])], T).*/
%Code Block
typeCode([S], T):- typeStatement(S, T).
typeCode([S, S2|Code], T):-
    typeStatement(S,_T),
    typeCode([S2|Code], T).

/* top level function */
infer(Code, T) :-
    is_list(Code), /* make sure Code is a list */
    deleteGVars(), /* delete all global definitions */
    typeCode(Code, T).

/* Basic types
    TODO: add more types if needed
 */
bType(int).
bType(float).
bType(string).
bType(bool).
bType(unit). /* unit type for things that are not expressions */
/*  functions type.
    The type is a list, the last element is the return type
    E.g. add: int->int->int is represented as [int, int, int]
    and can be called as add(1,2)->3
 */
bType([H]):- bType(H).
bType([H|T]):- bType(H), bType(T).

/*
    TODO: as you encounter global variable definitions
    or global functions add their definitions to 
    the database using:
        asserta( gvar(Name, Type) )
    To check the types as you encounter them in the code
    use:
        gvar(Name, Type) with the Name bound to the name.
    Type will be bound to the global type
    Examples:
        g

    Call the predicate deleveGVars() to delete all global 
    variables. Best wy to do this is in your top predicate
*/

deleteGVars():-retractall(gvar(_,_)), asserta(gvar(_X,_Y):-false()).

deleteLocalVars():-retractall(localVar(_,_)), asserta(localVar(_X,_Y):-false()).

/*  builtin functions
    Each definition specifies the name and the 
    type as a function type

    TODO: add more functions
*/

fType(+, [T, T, T]) :- 
    hasAdd(int) ;
    hasAdd(float).
fType(-, [T, T, T]) :- 
    hasAdd(int);
    hasAdd(float).
fType(*, [T, T, T]) :- 
    hasAdd(int);
    hasAdd(float).
fType(/, [T, T, T]) :- 
    hasAdd(int);
    hasAdd(float).
fType(fToInt, [float,int]).
fType(iToFloat, [int,float]).
fType(print, [_X, unit]). /* simple print */

/* Find function signature
   A function is either buld in using fType or
   added as a user definition with gvar(fct, List)
*/

% Check the user defined functions first
functionType(Name, Args):-
    gvar(Name, Args),
    is_list(Args). % make sure we have a function not a simple variable

% Check first built in functions
functionType(Name, Args) :-
    fType(Name, Args), !. % make deterministic

% This gets wiped out but we have it here to make the linter happy
gvar(_, _) :- false().

localVar(_, _) :- false().

typeCodeFunc([], _Args,Tc,_ReturnT, T):- T = Tc.
typeCodeFunc([S], Args,Tc,ReturnT, T):- 
    typeStatementFunc(S, Args,Tc,ReturnT, T).
typeCodeFunc([S, S2|Code], Args,Tc,ReturnT, T):-
    typeStatementFunc(S, Args,Tc,ReturnT, T1),
    typeCodeFunc([S2|Code], Args,T1,ReturnT, [T1|T]).

typeStatementFunc(X,Args,Tc,ReturnT, T) :-
    typeExpFunc(X,Args,Tc,ReturnT, T).
/*typeStatement(gfLet(hi, [2.2,4.5,c], [if(c < 2, [exp(2.2+4.5)],[])]), T1).*/
typeStatementFunc(if(Cond, TrueB, FalseB),Args,Tc,ReturnT, T) :-
    typeBoolExpFunc(Cond, Args,Tc,_RetT, T0),
    typeCodeFunc(TrueB, Args,T0,ReturnT, T1),
    typeCodeFunc(FalseB, Args,T1,ReturnT, T).
/*typeStatement(for(i, 5+6, 7-9, [gvLet(mult, T, iplus(X,Y))]), T1).*/
typeStatementFunc(for(Name, CodeS, CodeE, Code),Args,Tc,ReturnT, T):- 
    atom(Name), 
    (member(Name, Args) ->
    append(Tc, [int], T2);
    T2 = Tc),
    typeExpFunc(CodeS,Args,T2,ReturnT1, T1),
    typeExpFunc(CodeE,Args,T1,ReturnT1, T0),
    asserta(localVar(Name,int)),
    is_list(Code),
    typeCodeFunc(Code, Args,T0,ReturnT, T),
    deleteLocalVars().
/*typeStatement(gfLet(hi, [2.2,4.5,c], [exp(2.2+4.5)]), T1).*/
typeStatementFunc(exp(Code),Args,Tc,ReturnT, T):-
    typeExpFunc(Code,Args,Tc,ReturnT, T).
/*typeStatement(letin(a, T1, 2+5, [exp(a+6)]),T).*/
typeStatementFunc(letin(Name,T1, CodeE, Code),Args,Tc,ReturnT, T):- /*function and var LOCAL*/
    atom(Name), 
    typeExpFunc(CodeE,Args,Tc,T1, T0),
    bType(T1),
    asserta(localVar(Name,T1)),
    is_list(Code),
    typeCodeFunc(Code,Args,T0,ReturnT, T),
    deleteLocalVars(). /*store in local*/


/*global var*/
typeExp(X,_Args,Tc,ReturnT, T) :-
    gvar(X, ReturnT),
    not(is_list(ReturnT)),
    T=Tc.

/*local var*/
typeExp(X,_Args,Tc,ReturnT, T) :-
    localVar(X, ReturnT),
    T=Tc.
/*float type*/
typeExpFunc(X,_Args,Tc,float, T) :-
    float(X),
    T = Tc.

/*int type*/
typeExpFunc(X,_Args,Tc,int, T) :-
    integer(X),
    T = Tc.

/*bool type*/
typeExpFunc(X,Args,Tc,bool, T) :-
    typeBoolExpFunc(X, Args, Tc,_ReturnT, T).

typeExpFunc(Fct,ArgsIn,Tc,ReturnT, T):-
    \+ var(Fct), /* make sure Fct is not a variable */ 
    \+ atom(Fct), /* or an atom */
    functor(Fct, Fname, _Nargs), /* ensure we have a functor */
    !, /* if we make it here we do not try anything else */
    Fct =.. [Fname|Args], /* get list of arguments */
    append(Args, [ReturnT], FType), /* make it loook like a function signature */
    functionType(Fname, TArgs), /* get type of arguments from definition */
    typeExpListFunc(FType,ArgsIn,Tc, ReturnT,T,TArgs). /* recurisvely match types */

/* propagate types */
typeExpFunc(T,_Args,_Tc,_ReturnT, T).

/* list version to allow function mathine */
typeExpListFunc([],_Args,Tc,_ReturnT, T, []):- T = Tc.
typeExpListFunc([_Hin],Args,Tc,ReturnT,T, [Hout]):-
    %typeExp(Hin, Hout),
    ReturnT = Hout,
    typeExpListFunc([],Args, Tc, ReturnT, T, []). /* recurse */
typeExpListFunc([Hin|Tin],Args,Tc,ReturnT,T, [Hout|Tout]):-
    %typeExp(Hin, Hout),
    (member(Hin, Args) ->
    append(Tc, [Hout], T0);
    T0 = Tc),
    %delete(Args, Hin, Args2),
    typeExpListFunc(Tin,Args, T0, ReturnT1, T, Tout),
    ReturnT = ReturnT1. /* recurse */


typeBoolExpFunc(true, _Args, Tc,bool, T):- T = Tc.
typeBoolExpFunc(false, _Args, Tc,bool, T):- T = Tc. 
typeBoolExpFunc( X >= Y, Args, Tc,bool, T) :-
    (member(X, Args) ->
    append(Tc, [int], T0);
    T0 = Tc),
    (member(Y, Args) ->
    append(T0, [int], T);
    T = T0).
typeBoolExpFunc(X < Y, Args, Tc,bool, T) :- 
    (member(X, Args) ->
    append(Tc, [int], T0);
    T0 = Tc),
    (member(Y, Args) ->
    append(T0, [int], T);
    T = T0).
typeBoolExpFunc(X > Y, Args, Tc,bool, T) :- 
    (member(X, Args) ->
    append(Tc, [int], T0);
    T0 = Tc),
    (member(Y, Args) ->
    append(T0, [int], T);
    T = T0).
typeBoolExpFunc( X =< Y, Args, Tc,bool, T) :- 
     (member(X, Args) ->
    append(Tc, [int], T0);
    T0 = Tc),
    (member(Y, Args) ->
    append(T0, [int], T);
    T = T0).
typeBoolExpFunc( X == Y, Args, Tc,bool, T) :- 
     (member(X, Args) ->
     append(Tc, [int], T0);
    T0 = Tc),
    (member(Y, Args) ->
    append(T0, [int], T);
    T = T0).
typeBoolExpFunc( not(X), Args, Tc,bool, T ) :- 
     (member(X, Args) ->
    append(Tc, [bool], T);
    T = Tc).
typeBoolExpFunc( X \== Y, Args, Tc,bool, T) :- 
     (member(X, Args) ->
    append(Tc, [int], T0);
    T0 = Tc),
    (member(Y, Args) ->
    append(T0, [int], T);
    T = T0).
typeBoolExpFunc( X ; Y, Args, Tc,bool, T) :- 
    (member(X, Args) ->
    append(Tc, [bool], T0);
    T0 = Tc),
    (member(Y, Args) ->
    append(T0, [bool], T);
    T = T0).
typeBoolExpFunc( X , Y, Args, Tc,bool, T) :- 
    (member(X, Args) ->
    append(Tc, [bool], T0);
    T0 = Tc),
    (member(Y, Args) ->
    append(T0, [bool], T);
    T = T0).