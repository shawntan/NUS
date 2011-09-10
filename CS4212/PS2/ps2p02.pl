
/*
High-level language: arithmetic expressions (no booleans)
Low-level language: three adress code with no jumps
For each language, we have the following elements:
  * operators that allow convenient syntax
  * syntax checker that validates strings of the language
  * operational semantics given in the form of an interpreter or evaluator

Finally, we provide a compiler from the high-level language to
the low level one.
*/

/*
 *  Arithmetic expressions with the binary operators:
 *   + - * / mod and(logical conjunction) or(logical disjunction)
 *   /\(bitwise conjunction) \/(bitwise disjunction) xor(bitwise exclusive or)
 *   >>(shift left) <<(shift right)
 *   and unary operators: +, -, \(bitwise inversion)
 *
 */

% Operator declarations
:- op(800,yfx,and).  % Equivalent to && in C
:- op(810,yfx,or).   % Equivalent to || in C

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- op(700,fx, @).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Syntax checker
isExpr(X) :-  % binary operators
        X =.. [F,A,B],
        member(F,[ +, -, *, /, mod, and, or, /\, \/, <<, >> , xor ]),!,
        isExpr(A), isExpr(B).
isExpr(X) :- % unary operators
        X =.. [F,A],
        member(F,[+,-,\, @]), !,
        isExpr(A).
isExpr(X) :- % identifiers : must not begin with "v_" -- reserved for internally generated ids
        integer(X),! ; atom(X), \+ atom_prefix(X,v_).

% Test syntax checker
/*:- Expr = ( 3+ -2 * 4 mod (x -3/y) << (100 >> 2) * 
            (- 2 * +3 + (-2 - 1)*(10+2/(1 xor 2))) /\ 
            (-1 <<20 \/ 1 and 1 or 1) ),
   writeln('====================================='),
   write('Testing syntax checker for expression: '), writeln(Expr),
   isExpr(Expr), writeln('Passed.').*/

% Operational semantics: evaluator for arithmetic expressions
% Based on an environment that defines values for the variables
% appearing in the expression

evalExpr(E,E,_) :- integer(E),!.
evalExpr(E,Val,Env) :- atom(E),!,get_assoc(E,Env,Val).
evalExpr(X+Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx+Vy.
evalExpr(X-Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx-Vy.
evalExpr(X*Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx*Vy.
evalExpr(X/Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx//Vy.
evalExpr(X mod Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx mod Vy.
evalExpr(X /\ Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx /\ Vy.
evalExpr(X \/ Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx \/ Vy.
evalExpr(X and Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is abs(sign(Vx)) /\ abs(sign(Vy)).
evalExpr(X or Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is abs(sign(Vx)) \/ abs(sign(Vy)).
evalExpr(X xor Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx xor Vy.
evalExpr(X << Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx << Vy.
evalExpr(X >> Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env), Val is Vx >> Vy.
evalExpr(@ X,Val,Env) :- !,evalExpr(X,Vx,Env), Val is Vx, writeln(Vx).
evalExpr(+ X,Val,Env) :- !,evalExpr(X,Vx,Env), Val is Vx .
evalExpr(- X,Val,Env) :- !,evalExpr(X,Vx,Env), Val is - Vx .
evalExpr(\ X,Val,Env) :- !,evalExpr(X,Vx,Env), Val is \ Vx .

/*% Test evaluator
:- Expr = x + y,
   isExpr(Expr),
   writeln('====================================='),
   write('Testing evaluation for expression: '), writeln(Expr),
   writeln('With initial values x=10 and y=20'),
   empty_assoc(Empty), put_assoc(x,Empty,10,Ex), put_assoc(y,Ex,20,Exy),
   evalExpr(Expr,Value,Exy), write('Value returned: '), writeln(Value).

:- Expr = (y << x) * (y -x) / - 2, isExpr(Expr),
   writeln('====================================='),
   write('Testing evaluation for expression: '), writeln(Expr),
   writeln('With initial values x=10 and y=20'),
   empty_assoc(Empty), put_assoc(x,Empty,10,Ex), put_assoc(y,Ex,20,Exy),
   evalExpr(Expr,Value,Exy), write('Value returned: '), writeln(Value).

:- Expr = ( 3+ -2 * 4 mod (x -3/y) << (100 >> 2) * (-2 * +3 + (-2 - 1)*(10+2/(1 xor 2))) /\
          (-1 <<20 \/ 1 and 1 or 1) ),
   isExpr(Expr),
   writeln('====================================='),
   write('Testing evaluation for expression: '), writeln(Expr),
   writeln('With initial values x=10 and y=20'),
   empty_assoc(Empty), put_assoc(x,Empty,10,Ex), put_assoc(y,Ex,20,Exy),
   evalExpr(Expr,Value,Exy), write('Value returned: '), writeln(Value).*/

/*
 *  Three-address intermediate code, currently without jumps.
 *  Each instruction has the format X = Y op Z, where X is a variable,
 *  Y and Z are either variables or integers, and "op" is a binary operator
 *  that is legal in the arithmetic expressions defined above.
 *  We also allow the unary operator "\" that performs bitwise negation.
 *  A three-address program is a sequence of instructions, separated by ';'
 */

% Unary semicolon serves as statement terminator
:- op(1099,yf,;).

% Syntax checker for three address code
isTai(X = Y) :-  % instruction validator, Tai = three address instruction
        atom(X),
        ( integer(Y) ; atom(Y) ;
          Y =.. [F,A,B],
          (   atom(A),! ; integer(A)   ),
          (   atom(B),! ; integer(B)   ),
          member(F,[+,-,*,/,mod,/\,\/,xor,<<,>>]) ),!.
isTai(X= \ Y) :- atom(X), (atom(Y) ; integer(Y)),!.
% A sequence of instructions separated by ';' is a valid three adress program
isTac(X) :- isTai(X),!.
isTac(X;) :- isTac(X),!. % single instruction may be terminated by semicolon
isTac((X;Y)) :- isTac(X), isTac(Y).

% Test TAC syntax checker
/*:- Code = (
         x = y + 10 ;
         y = y + 1 ;
         y = y * x ;
         z = x << y;
         y = \ x ;
         ),
   writeln('====================================='),
   writeln('Syntax checking TAC code:'),
   writeln(Code),
   isTac(Code),
   writeln('Passed.').*/

/*
 * Operational semantics for TAC in the form of an interpreter
 * Each execution changes the environment, so the predicate
 * will have an input environment and an output one.
 */
execTai(Env0,(X=Y),Env1) :- % execution of one instruction
        evalExpr(Y,Val,Env0),   % add a variable to the env if not already there
        put_assoc(X,Env0,Val,Env1),!.
% execution of sequences of instructions
execTac(Env0,X,Env1) :- execTai(Env0,X,Env1),!.
execTac(Env0,X;,Env1) :- execTai(Env0,X,Env1),!.
execTac(Env0,(X;Y),Env) :- execTac(Env0,X,Env1), execTac(Env1,Y,Env).

% Test execTac

/*:- Code = (
     y = 5 ;
     x = y + 10 ;
         y = y + 1 ;
         y = y * x ;
         y = y >> 10 ;
         z = x << y;
         y = \ x ;
         ), isTac(Code),
         writeln('====================================='),
         write('Testing execution of TAC:'), writeln(Code),
         empty_assoc(Empty), execTac(Empty,Code,OutEnv),
         write('Execution completed successfully with '),
         write('x = '), get_assoc(x,OutEnv,X),write(X),
         write(' y = '), get_assoc(y,OutEnv,Y),write(Y),
         write(' z = '), get_assoc(z,OutEnv,Z),writeln(Z).

:- Code = (
     y = 7 ;
     x = y \/ 8 ;
         y = y + 1 ;
         y = y + x ;
         y = y >> 1 ;
         z = x << y;
         x = x xor y;
         ), isTac(Code),
         writeln('====================================='),
         write('Testing execution of TAC:'), writeln(Code),
         empty_assoc(Empty), execTac(Empty,Code,OutEnv),
         write('Execution completed successfully with '),
         write('x = '), get_assoc(x,OutEnv,X),write(X),
         write(' y = '), get_assoc(y,OutEnv,Y),write(Y),
         write(' z = '), get_assoc(z,OutEnv,Z),writeln(Z).*/

/********************************************
 * Compiler from expression language to TAC
 ********************************************/
:- dynamic auxvar/1.

% generate new variable names
newvar(X) :-
        retract(auxvar(Y))
        ->  Y1 is Y+1, assert(auxvar(Y1)), atomic_concat('v_',Y1,X)
        ;   assert(auxvar(0)), X = 'v_0' .

% utility predicate to reset the new variable counter
% not used in compiler, useful for debugging
resetnewvar :-
        (   retractall(auxvar(_)) -> true ; true ),
        assert(auxvar(0)).

compile(E,(X=E),X) :- integer(E),!,newvar(X).
compile(E,(X=E),X) :- atom(E),!,newvar(X).
compile(X+Y,Code,R) :- !,
        compile(X,Cx,Rx), compile(Y,Cy,Ry),
        newvar(R), Code = ( Cx ; Cy ; R = Rx + Ry ).
compile(X-Y,Code,R) :- !,
        compile(X,Cx,Rx), compile(Y,Cy,Ry),
        newvar(R), Code = ( Cx ; Cy ; R = Rx - Ry ).
compile(X*Y,Code,R) :- !,
        compile(X,Cx,Rx), compile(Y,Cy,Ry),
        newvar(R), Code = ( Cx ; Cy ; R = Rx * Ry ).
compile(X/Y,Code,R) :- !,
        compile(X,Cx,Rx), compile(Y,Cy,Ry),
        newvar(R), Code = ( Cx ; Cy ; R = Rx / Ry ).
compile(X mod Y,Code,R) :- !,
        compile(X,Cx,Rx), compile(Y,Cy,Ry),
        newvar(R), Code = ( Cx ; Cy ; R = Rx mod Ry ).
compile(X /\ Y,Code,R) :- !,
        compile(X,Cx,Rx), compile(Y,Cy,Ry),
        newvar(R), Code = ( Cx ; Cy ; R = Rx /\ Ry ).
compile(X \/ Y,Code,R) :- !,
        compile(X,Cx,Rx), compile(Y,Cy,Ry),
        newvar(R), Code = ( Cx ; Cy ; R = Rx \/ Ry ).
compile(X xor Y,Code,R) :- !,
        compile(X,Cx,Rx), compile(Y,Cy,Ry),
        newvar(R), Code = ( Cx ; Cy ; R = Rx xor Ry ).
compile(X << Y,Code,R) :- !,
        compile(X,Cx,Rx), compile(Y,Cy,Ry),
        newvar(R), Code = ( Cx ; Cy ; R = Rx << Ry ).
compile(X >> Y,Code,R) :- !,
        compile(X,Cx,Rx), compile(Y,Cy,Ry),
        newvar(R), Code = ( Cx ; Cy ; R = Rx >> Ry ).
compile(+ X,Code,R) :- !,compile(0+X,Code,R).
compile(- X,Code,R) :- !,compile(0-X,Code,R).
compile(\ X,Code,R) :- !,compile((-1) xor X,Code,R).

% pretty printing of compiled code
alignLabel(X,Y) :- % helper predicate
        atomic_concat(X,'          ',Z),
        atom_chars(Z,L), append(L1,_,L), length(L1,10),
        atomic_list_concat(L1,Y).
% print compiled code in a more human-readable form
writeTac((X;Y)) :- !, writeTac(X), writeTac(Y).
writeTac(X) :- write('            '),writeln(X).

:-resetnewvar.

% Test the compiler: result of interpreting HL code must
% be the same as result of interpreting TAC produced by compilation
/*:- Expr = (((1+2)*(20-10)/3+(1<<5)>>3 xor (- 1) )*(10+20/2)+x*y),
    isExpr(Expr),
	writeln('====================================='),
        write('Testing the compilation of expression:'), writeln(Expr),
        writeln('with initial values x = 10 and y = 20'),
        empty_assoc(EmptyEnv), % the start environment is empty
        execTac(EmptyEnv,(x=10;y=20),Env),
        evalExpr(Expr,ResultEval,Env), % interpret HL code
        compile(Expr,Code,ResultVar), isTac(Code),
        writeln('Compiled code:'),
        writeTac(Code),
        execTac(Env,Code,ResultEnv), % interpret compiled TAC code
        get_assoc(ResultVar,ResultEnv,ResultCode),
        write('Value after evaluation:'), % print results for comparison
        writeln(ResultEval),
        write('Value after execution of compiled code:'),
        writeln(ResultCode).*/


