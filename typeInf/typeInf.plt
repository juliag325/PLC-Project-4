:- begin_tests(typeInf).
:- include(typeInf). 

/* Note: when writing tests keep in mind that 
    the use of of global variable and function definitions
    define facts for gvar() predicate. Either test
    directy infer() predicate or call
    delegeGVars() predicate to clean up gvar().
*/

% tests for typeExp
test(typeExp_add) :-
    typeExp(+(int,int), T),
    assertion(T == int).

% Should fail
test(typeExp_add_F, [fail]) :- 
    typeExp(+(int, int), float).

test(typeExp_add_F2, [fail]) :-
    typeExp(+(float, float), int).

test(typeExp_add_F, [fail]) :- 
    typeExp(+(int, float), int).

test(typeExp_sub) :- 
    typeExp(-(int, int), T),
    assertion(T == int).

% Should fail
test(typeExp_sub_F, [fail]) :- 
    typeExp(-(int, int), float).

test(typeExp_sub_F2, [fail]) :-
    typeExp(-(float, float), int).


test(typeExp_sub_F3, [fail]) :- 
    typeExp(-(int, float), int).

test(typeExp_mult) :-
    typeExp(*(int, int), T),
    assertion(T == int). 
  
% Should fail
test(typeExp_mult_F, [fail]) :- 
    typeExp(*(int, int), float).

test(typeExp_mult_F2, [fail]) :-
    typeExp(*(float, float), int).

test(typeExp_mult_F3, [fail]) :- 
    typeExp(*(int, float), int).

test(typeExp_div) :-
    typeExp(/(int,int), T), 
    assertion(T == int).

% Should fail
test(typeExp_div_F, [fail]) :- 
    typeExp(/(int, int), float).

test(typeExp_div_F2, [fail]) :-
    typeExp(/(float, float), int).

test(typeExp_div_F3, [fail]) :- 
    typeExp(+(int, float), int).

test(typeExp_fToInt) :-
    typeExp(fToInt(float), T),
    assertion(T == int).

test(typeExp_fToInt, [nondet]) :-
    typeExp(fToInt(3.4), T),
    assertion(T == int).

test(typeExp_fToInt, [fail]) :-
    typeExp(fToInt(float), float).

test(typeExp_itToFloat) :- 
    typeExp(iToFloat(int), T), 
    assertion(T == float).

test(typeExp_itToFloat, [fail]) :- 
    typeExp(iToFloat(int), int).

test(typeExp_typeInt, [nondet]) :- 
    typeExp(3, T), 
    assertion(T == int).

test(typeExp_typeInt_F, [fail]) :- 
    typeExp(3, float).

test(typeExp_typeFloat, [nondet]) :- 
    typeExp(3.2, T),
    assertion(T == float).

test(typeExp_typeFloat, [fail]) :- 
    typeExp(3.2, int).

test(typeBoolExp) :-
    true.

test(typeBoolExp, [fail]) :- 
    false.

test(typeExp_bool_1, [nondet, true]) :- 
    typeBoolExp(2 < 3),
    typeExp(2, T),
    typeExp(3, T),
    assertion(T == int).

test(typeExp_bool_1, [nondet, true]) :- 
    typeBoolExp(-2 < 3),
    typeExp(-2, T),
    typeExp(3, T),
    assertion(T == int).


test(typeExp_bool_2, [nondet, true]) :-
    typeBoolExp(3 > 2),
     typeExp(3, T),
    typeExp(2, T),
    assertion(T == int).

test(typeExp_bool_2, [nondet, true]) :-
    typeBoolExp(-3 > -4),
     typeExp(-3, T),
    typeExp(-4, T),
    assertion(T == int).

test(typeExp_bool_2, [nondet, true]) :-
    typeBoolExp(-3.2 < 4.5), 
    typeExp(-3.2, T),
    typeExp(4.5, T),
    assertion(T == float).

test(typeExp_bool_3, [nondet, true]) :-
    typeBoolExp(3 == 3),
    typeExp(3, T),
    assertion(T == int).

test(typeBoolExp_bool_4, [nondet, true]) :- 
    typeBoolExp(3 =< 3),
    typeExp(3, T),
    assertion(T == int).

test(typeBoolExp_bool_4, [nondet, true]) :- 
    typeBoolExp(5.6 =< 5.7),
    typeExp(5.6, T),
    typeExp(5.7, T),
    assertion(T == float).

test(typeBoolExp_bool_5, [nondet, true]) :- 
    typeBoolExp(3 =< 5),
    typeExp(3, T),
    typeExp(5, T),
    assertion(T == int).
    
test(typeBoolExp_bool_div, [nondet, true]) :- 
    typeBoolExp(4 \== 2).

% this test should fail
test(typeExp_iplus_F, [fail]) :-
    typeExp(iplus(int, int), float).

/*test(typeExp_iplus_T, [true(T == int)]) :-
    typeExp(iplus(int, int), T).*/

% type statement 
test(typeStatement, [nondet]) :- 
    typeExp(3, T), 
    assertion(T == int).

test(hasComparison) :- 
    hasComparison(int).

test(hasComparison) :- 
    hasComparison(float).

test(hasComparison) :- 
    hasComparison(string).

test(hasComparison, [fail]) :-
    hasComparison(unit).

test(hasBoolean) :- 
    hasBoolean(bool).

test(hasBoolean, [fail]) :- 
    hasBoolean(float).

test(hasBoolean, [fail]) :- 
    hasBoolean(int).

test(hasBoolean, [fail]) :- 
    hasBoolean(unit).

test(hasInt) :- 
    hasInt(int).

test(hasInt, [fail]) :- 
    hasInt(float).

test(hasInt, [fail]) :- 
    hasInt(unit).

test(hasInt, [fail]) :- 
    hasInt(bool).

test(hasAdd) :- 
    hasAdd(int).

test(hasAdd) :- 
    hasAdd(float).

test(hasAdd, [fail]) :- 
    hasAdd(bool).

test(typeExpList) :-
    typeExpList([], []).

test(bType) :- 
    bType(int). 


% NOTE: use nondet as option to test if the test is nondeterministic

% test for statement with state cleaning
test(typeStatement_gvar, [nondet, true(T == float)]) :- % should succeed with T=int
    deleteGVars(), /* clean up variables */
    typeStatement(gvLet(v, T, 1.2+3.4), unit),
    gvar(v, float). % make sure the global variable is defined

% test for code 
test(typeCode_single, [nondet]) :-
    typeCode([gvLet(v, T, 2+3)], unit),
    assertion(T==int),
    gvar(v,int).

test(typeCode_mult, [nondet]) :-
    typeCode([gfLet(hi, [a,b], [for(i, 2, 5, [exp(a+b)])]), exp(9 < 3), hi(2.2,5.6)], T),
    gvar(hi, [float,float,float]),
    \+ localVar(i, int),
    assertion(T==float).

test(typeCode_codeBlock, [nondet]) :-
    typeCode([gfLet(hi, [a,b], [letin(c, T1, a+b, [exp(1+c)])]), for(i, hi(3,4), hi(9,10), [exp(i * hi(i,9))])], T),
    assertion(T1 == int),
    \+ localVar(c, int),
    \+ localVar(i, int),
    gvar(hi, [int, int, int]),
    assertion(T == int).

test(typeCode_notList, [fail]) :-
    typeCode([if(4 < 7, [letin(b, _T1, 8-9,[for(a,b*b, 2*b, [exp(a+b)])])], exp(4+4))], _T).

test(typeCode_empty, [fail]) :-
    typeCode([],_T).

% same test as above but with infer 
test(infer_gvar, [nondet]) :-
    infer([gvLet(v, T1, 2+3), exp(v+v)], T),
    assertion(T1==int),
    assertion(T == int),
    gvar(v,int).

% either float or int
test(infer_gfunc, [nondet]) :-
    infer([gfLet(hi, [a,b], [a+b]), hi(9,0)], T),
    assertion(T==int),
    gvar(hi,[int,int,int]).

test(infer_gfunc_bool, [nondet]) :-
    infer([gfLet(hi, [a,b], [a >= b]), hi(2,3)], T),
    assertion(T==bool),
    gvar(hi,[int,int,bool]).

test(infer_gvlet_exp_gflet_func_letin, [nondet]) :-
    infer([gvLet(mult, T2, 2+7),exp(mult + 9) ,gfLet(hi, [a,b], [for(i, 2, 5, [letin(c, T3, 2+5, [exp(a+b)])])]), exp(9 < 3), hi(2,mult), letin(a, T1, hi(9,mult), [exp(a+6)])], T),
    assertion(T2==int),
    gvar(mult, int),
    assertion(T3==int),
    \+ localVar(i, int),
    \+ localVar(a, int),
    gvar(hi, [int,int,int]),
    assertion(T1==int),
    assertion(T==int).

test(infer_gflet_float, [nondet]) :-
    infer([gfLet(hi, [a,b], [for(i, 2, 5, [exp(a+b)])]), exp(9 < 3), hi(2.2,5.6)], T),
    gvar(hi, [float,float,float]),
    \+ localVar(i, int),
    assertion(T==float).

test(infer_letin_if, [nondet]) :-
    infer([letin(a, T1, 2 < 3, [if(a, [exp(2+9)], [letin(b, T2, 2-9, [exp(b*7)])])])],T),
    assertion(T1 == bool),
    \+ localVar(a, bool),
    \+ localVar(b, int),
    assertion(T2 == int),
    assertion(T == int).

test(infer_multGvLet, [nondet]) :-
    infer([gvLet(b, T1, true),gvLet(y, T2, 5+6), gvLet(x, T3, 7+y), gvLet(z, T4, 6.9+0.9), for(i, y, x, [exp(x+y), letin(f, T5, 7.8/5.6, [exp(f*z), exp(f < 7.8), exp(z =< f)])])],T),
    assertion(T1 == bool),
    gvar(b, bool),
    assertion(T2 == int),
    gvar(y, int),
    assertion(T3 == int),
    gvar(x, int),
    \+ localVar(i, int),
    \+ localVar(f, float),
    assertion(T4 == float),
    gvar(z, float),
    assertion(T5 == float),
    assertion(T == bool).

test(infer_if_for_print, [nondet]) :-
    infer([if(5 \== 7, [print('hi')] , [for(i, 5, 10, [print('bye')])])],T),
    \+ localVar(i, int),
    assertion(T == unit).

test(infer_functionWithinFunction, [nondet]) :-
    infer([gfLet(hi, [a,b], [for(i, 2, 5, [letin(c, T1, 2+5, [exp(a<b)])])]), gfLet(bye, [x,y], [if(x ; y, [hi(3,4)], [hi(7,8)])]), bye(false, true)],T),
    assertion(T1 == int),
    \+ localVar(c, int),
    gvar(hi, [int, int, bool]),
    gvar(bye, [bool, bool, bool]),
    assertion(T == bool).

test(infer_for_function, [nondet]) :-
    infer([gfLet(hi, [a,b], [letin(c, T1, a+b, [exp(1+c)])]), for(i, hi(3,4), hi(9,10), [exp(i * hi(i,9))])], T),
    assertion(T1 == int),
    \+ localVar(c, int),
    \+ localVar(i, int),
    gvar(hi, [int, int, int]),
    assertion(T == int).

test(infer_globalVarPassIntoFunction, [nondet]) :-
    infer([gvLet(b, T1, true), gvLet(a, T2, 5 =< 9), gfLet(hi, [a,b], [letin(c, T3, (b ; a), [exp(not(c))])]), hi(a, b)],T),
    assertion(T1 == bool),
    gvar(b, bool),
    \+ localVar(c, bool),
    assertion(T2 == bool),
    gvar(a, bool),
    assertion(T3 == bool),
    gvar(hi, [bool, bool, bool]),
    assertion(T == bool).

test(infer_codeBlocks, [nondet]) :-
    infer([gvLet(b, T1, 6*9), gvLet(a, T2, b+7), gfLet(also, [a,b], [if(a < b, [exp(4 + 6)], [exp(4 - 6)])]), for(i, also(a,4), also(b,10), [exp(also(a,b) * also(i,9)), exp(also(a,b) =< also(i,9))])], T),
    assertion(T1 == int),
    gvar(b, int),
    assertion(T2 == int),
    \+ localVar(i, int),
    gvar(a, int),
    gvar(also, [int, int, int]),
    assertion(T == bool).

test(infer_localVars, [nondet]) :-
    infer([for(i, 5*7, 6-5, [exp(i * i), letin(a, T1, i, [if(i < 7, [a+i], [a-i])])])], T),
    assertion(T1 == int),
    \+ localVar(i, int),
    \+ localVar(a, int), 
    assertion(T == int).

test(infer_multgvar, [nondet]) :-
    infer([gvLet(mult, T1, 2.5+7.8), gvLet(cost, T2, 76.7+7.0), if(mult \== cost, [mult*cost], [mult*mult]) ], T),
    assertion(T1 == float),
    gvar(mult, float),
    assertion(T2 == float),
    gvar(cost, float),
    assertion(T == float).

test(infer_functionsAsInput, [nondet]) :-
    infer([gfLet(test, [a,b], [a >= b]), exp(test(2,3) ; test(9,8))], T),
    gvar(test, [int, int, bool]),
    assertion(T == bool).

test(infer_functionsAsVarIntoFunctions, [nondet]) :-
    infer([gfLet(hi, [a,b], [for(i, 2, 5, [exp(a+b)])]), gfLet(test, [a,b], [a >= b]), test(hi(2,3), hi(9,8))], T),
    gvar(hi, [int, int, int]),
    gvar(test, [int, int, bool]),
    assertion(T == bool).

test(infer_recursiveFuncCall, [nondet]) :-
    infer([gfLet(hi, [a,b], [for(i, 2, 5, [exp(a+b)])]), gfLet(test, [a,b], [a * b]), test(test(hi(hi(3,4),hi(6,7)), hi(hi(7,9),hi(8,9))), test(hi(hi(3,4),hi(6,7)), hi(hi(7,9),hi(8,9))))], T),
    gvar(hi, [int, int, int]),
    gvar(test, [int, int, int]),
    assertion(T == int).

test(infer_recursiveLetin, [nondet]) :-
    infer([letin(a, T1, 6.5+9.6, [letin(b, T2, a > 8.6,[if(b,[exp(a*a)], [exp(a+a)])])])], T),
    assertion(T1 == float),
    assertion(T2 == bool),
    \+ localVar(a, float),
    \+ localVar(b, bool), 
    assertion(T == float).

test(infer_setFunctionAsAnotherFunction, [nondet]) :-
    infer([gfLet(hi, [a,b], [for(i, 2, 5, [exp(a < b)])]), gfLet(test, [a,b], [hi(a,b)])], T),
    gvar(hi, [int, int, bool]),
    \+ localVar(i, int),
    gvar(test, [int, int, bool]),
    assertion(T = [int,int, bool]).

test(infer_letin_for, [nondet]) :-
    infer([letin(b, T1, 8-9,[for(a,b*b, 2*b, [exp(a+b)])])], T),
    assertion(T1 == int),
    \+ localVar(a, int),
    \+ localVar(b, int),
    assertion(T == int).

test(infer_if_letin_for, [nondet]) :-
    infer([if(4 < 7, [letin(b, T1, 8-9,[for(a,b*b, 2*b, [exp(a+b)])])], [exp(4+4)])], T),
    assertion(T1 == int),
    \+ localVar(a, int),
    \+ localVar(b, int),
    assertion(T == int).

test(infer_wrongInputsToFunction, [fail]) :-
    infer([gfLet(test, [a,b], [a >= b]), exp(test(true,false) ; test(9,8))], _T).

test(infer_mixedInputs, [fail]) :-
    infer([if(4 < 7.5, [letin(b, _T1, 8-9,[for(a,b*b, 2*b, [exp(a+b)])])], [exp(4+4)])], _T).


test(infer_notList, [fail]) :-
    infer([if(4 < 7, [letin(b, _T1, 8-9,[for(a,b*b, 2*b, [exp(a+b)])])], exp(4+4))], _T).

test(infer_notBool, [fail]) :-
    infer([if(4, [letin(b, _T1, 8-9,[for(a,b*b, 2*b, [exp(a+b)])])], exp(4+4))], _T).

test(infer_localVarOutofScope, [fail]) :-
    infer([letin(a, _T1, 6.5+9.6, [letin(b, _T2, a > 8.6,[if(b,[exp(a*a)], [exp(a+a)])])]), exp(a+b)], _T).

% test custom function with mocked definition
test(mockedFct, [nondet]) :-
    deleteGVars(), % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
    typeExp(my_fct(X), T), % infer type of expression using or function
    assertion(X==int), assertion(T==float). % make sure the types infered are correct

:-end_tests(typeInf).
