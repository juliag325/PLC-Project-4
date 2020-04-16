:- dynamic 
    gvar/2.
/* match functions by unifying with arguments 
    and infering the result
*/
/*float type*/
typeExp(X, float) :-
    float(X).

/*int type*/
typeExp(X, int) :-
    integer(X).

/*bool type*/
typeExp(X, bool) :-
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
typeBoolExp( X , Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasBoolean(T).
typeBoolExp( X ; Y) :- 
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
    typeCode(Code, T),
    bType(T),
    append(TArgs, [T], RArgs),
    asserta(gvar(Name, RArgs)). 
/*typeStatement(if((3 < 10),[gvLet(mult, T, iplus(X,Y))],[gvLet(mult, T, iplus(X,Y))]), T1).*/
typeStatement(if(Cond, TrueB, FalseB), T) :-
    typeBoolExp(Cond),
    typeCode(TrueB, T),
    typeCode(FalseB, T).
/*typeStatement(for(i, 5+6, 7-9, [gvLet(mult, T, iplus(X,Y))]), T1).*/
typeStatement(for(Name, CodeS, CodeE, Code), T):- /*local var?*/
    atom(Name), 
    typeExp(CodeS, T1),
    typeExp(CodeE, T1), 
    hasInt(T1),
    is_list(Code),
    typeCode(Code, T).
/*typeStatement(exp(2+4),T).*/
typeStatement(exp(Code), T):-
    typeExp(Code, T),
    bType(T).
typeStatement(letin(Name, T, CodeE, Code), unit):- /*function and var LOCAL*/
    atom(Name), 
    typeExp(Code, T),
    bType(T). /*store in local*/



/* Code is simply a list of statements. The type is 
    the type of the last statement 
*/
/*typeCode([gfLet(hi, [a,b,c], [gvLet(mult, T, (2+5)), exp(2+4)]), exp(9 < 3), gfLet(hi, [a,b], [gvLet(mult, T, (2+5)), exp(5 < 9)])], T1).*/
typeCode([S], T):-typeStatement(S, T).
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

deleteGVars():-retractall(gvar), asserta(gvar(_X,_Y):-false()).

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