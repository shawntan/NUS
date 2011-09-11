/*
	Benjamin Tan Wei Hao
	U077129N
	
	My additions are surrounded by '%%%%%%%%%%'.
*/

% Operator declarations
:- op(800,yfx,and).
:- op(810,yfx,or).

% Syntax checker
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isExpr(X ? Y) :- isExpr(X), isExpr(Y). 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isExpr(X) :-
        X =.. [F,A,B],
        % list augmented with new operators
        member(F,[ +, -, *, /, mod, and, or, /\, \/, <<, >> , xor,
                  < , > , =<, >= , == , \= ]),!,
        isExpr(A), isExpr(B).
isExpr(X) :-
        X =.. [F,A],
        member(F,[+,-,\]), !,
        isExpr(A).
isExpr(X) :-
        integer(X),! ; atom(X), \+ atom_prefix(X,v_).

% Test syntax checker
/*:- Expr = ( 3+ -2 * 4 mod (x -3/y) << (100 >> 2*(2<3)) * (-2 * +3 + (-2 - (1==1))*(10+2/(1 xor 2))) /\
          (-1 <<20 \/ 1 and 1 or ((x>y)?x:y)) ),
   writeln('===================================='),
   write('Testing syntax checker for expression: '), writeln(Expr),
   isExpr(Expr), writeln('Passed.').*/

/*
 * Operational semantics: evaluator for arith expr with bool
 */
% old rules from 01.pl
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
evalExpr(+ X,Val,Env) :- !,evalExpr(X,Vx,Env), Val is Vx .
evalExpr(- X,Val,Env) :- !,evalExpr(X,Vx,Env), Val is - Vx .
evalExpr(\ X,Val,Env) :- !,evalExpr(X,Vx,Env), Val is \ Vx .
% new rules added for booleans
evalExpr(X < Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),
        (   Vx < Vy -> Val = 1 ; Val = 0 ).
evalExpr(X > Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),
        (   Vx > Vy -> Val = 1 ; Val = 0 ).
evalExpr(X =< Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),
        (   Vx =< Vy -> Val = 1 ; Val = 0 ).
evalExpr(X >= Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),
        (   Vx >= Vy -> Val = 1 ; Val = 0 ).
evalExpr(X == Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),
        (   Vx = Vy -> Val = 1 ; Val = 0 ).
evalExpr(X \= Y,Val,Env) :- !,evalExpr(X,Vx,Env), evalExpr(Y,Vy,Env),
        (   Vx \= Vy -> Val = 1 ; Val = 0 ).
evalExpr(\+ X,Val,Env) :- !,evalExpr(X,Vx,Env), Vx \= 0 -> Val is 0 ; Val is 1 .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ? Operator
evalExpr(X ? Y, Val, Env) :- !,
        evalExpr(X,Vx,Env), (Vx \= 0 -> evalExpr(Y,Val,Env) ; Val is 0).


% Test evaluator
:- Expr = (x < y) ? y,
   writeln('================================='),
   write('Testing evaluation for expression: '), writeln(Expr),
   writeln('With initial values x=10 and y=20'),
   empty_assoc(Empty), put_assoc(x,Empty,10,Ex), put_assoc(y,Ex,20,Exy),
   evalExpr(Expr,Value,Exy), write('Value returned: '), writeln(Value).

:- Expr = (x > y) ? y,
   writeln('================================='),
   write('Testing evaluation for expression: '), writeln(Expr),
   writeln('With initial values x=10 and y=20'),
   empty_assoc(Empty), put_assoc(x,Empty,10,Ex), put_assoc(y,Ex,20,Exy),
   evalExpr(Expr,Value,Exy), write('Value returned: '), writeln(Value).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
 *  Three-address intermediate code with jumps.
 *  Each instruction may be labelled:
 *     label :: instruction ;
 *
 *  Added non-conditional jumps, of the form:
 *     goto Expr
 *  where Expr should evaluate to a label+offset
 *
 *  Added conditional jumps, of the form:
 *     if Cond goto Expr
 */

:- op(1099,yf,;).
:- op(950,fx,goto).
:- op(950,xfx,goto).
:- op(960,fx,if).
:- op(970,xfy,::). % We allow multiple labels on a instruction
:- op(969,xf,::).

/****************************************
  Syntax checker for three address code
*****************************************/

% isTaiNL : Is Three-address-instruction with No Label
%           checks if this is a valid opcode, after label has been stripped
%           Valid opcodes:
%              nop : no operation
%              X = Expr  : assignment (or 'load' and 'store' in machine code speak)
%                  operators in Expr must be all binary, except negation
%              goto X : unconditional jump
%                   X is either a label, or a label +/- displacement
%                                             the displacement can be either const or var
%              if cond goto X : conditional jump
%                   cond must be simple boolean expr with second operand = 0
isTaiNL(nop) :- !.  /* No operation */
isTaiNL(X = Y) :-  % is argument a three-address-instruction with no label?
        atom(X),
        (   atom(Y) ; integer(Y) ;
           (   Y =.. [F,A,B],
               (   atom(A),! ; integer(A)   ),
               (   atom(B),! ; integer(B)   ),
               member(F,[+,-,*,/,mod,/\,\/,xor,<<,>>]) ) ),!.
isTaiNL(X= \ Y) :- atom(X), (atom(Y) ; integer(Y)),!.
isTaiNL(goto X) :-
        ( atom(X) ; integer(X); ( X=..[F,A,B], (F = + ; F = -), atom(A), (atom(B);integer(B)) ) ),!.
isTaiNL(if E goto Y) :-
        E =.. [F,X,0], member(F,[==,\=,<,>,=<,>=]),
        (atom(X);integer(X)),
        (  atom(Y);integer(Y);
          ( Y=..[F1,A,B], (F1 = + ; F1 = -), atom(A), (atom(B);integer(B)) ) ),!.

% isTai : is Three address instruction (instruction may be labelled)
%         single label with no instruction is also legal
isTai((L::I)) :- !,atom(L), ( I =.. [(::)|_] -> isTai(I) ; isTaiNL(I)).
isTai((L::)) :- !,atom(L).
isTai(I) :- isTaiNL(I).

% isTac : is Three address code?
%         Three address code is semicolon separated sequence of TAIs
isTac(X) :- isTai(X),!.
isTac(X;) :- isTai(X),!.
isTac((X;Y)) :- isTac(X), isTac(Y).

% writeTac : Pretty-printing of TAC
alignLabel(X,Y) :- % helper predicate, appends spaces to make all labels of same length
        atomic_concat(X,'          ',Z),
        atom_chars(Z,L), append(L1,_,L), length(L1,10),
        atomic_list_concat(L1,Y).

writeTac((X;Y)) :- !, writeTac(X), writeTac(Y).
writeTac((X::Y)) :- !, alignLabel(X,X1),
                    (   Y =.. [(::)|_]
                     -> write(X1), writeln((::)), writeTac(Y)
                     ;  writeln((X1::Y)) ).
writeTac((X::)) :- !, alignLabel(X,X1), write(X1), writeln((::)).
writeTac(X) :- write('            '),writeln(X).

% Test for TAC syntax checker
/*:- Code = (
           x = 3 ;
           y = 4 ;
  label ::       ;
           z = 0 ;
  again::
  again1:: z = z + x ;
           y = y - 1 ;
           if y > 0 goto again
          ),
        writeln('================================'),
        writeln('Testing syntax checking for TAC:'), writeTac(Code),
        isTac(Code), writeln('Passed.').*/

/*
 * To define the operational semantics, we need a translation step
 * where labels are converted into numbers. We shall call this
 * new code "object code"
 * An object code program is encoded as an association list
 * where each instruction is indexed by a number which plays
 * the role of the instruction's address, similar to CPU
 * The addresses of labels are computed, and then labelled are
 * replaced in the code by their addresses.
 */

% Three-address-instruction with no label -> Object instruction
% -- collects target labels in an association list
taiNLtoObj(nop,P,P,nop) :- !.
taiNLtoObj(X=Y,P,P,X=Y) :- !.
taiNLtoObj(Iin,P,[X:Y|P],Iout) :-
        (   Iin = (goto E), Iout = (goto D)
        ;   Iin = (if Expr goto E), Iout = (if Expr goto D) ),
        (   E =.. [F,X,Z], D =.. [F,Y,Z]
        ;   atom(E), E = X, D = Y ) ,!.
taiNLtoObj(_I,_Pin,_Pout,_T).
% Three-address-instruction (possibly labelled) -> Object instruction
% -- collects address labels in Lin/Lout
% -- collects target labels in Pin/Pout
taiToObj(L::I,IP,Lin,Lout,Pin,Pout,T) :- !,
        get_assoc(L,Lin,_)
        ->  writeln('Duplicate labels'), abort
        ;   put_assoc(L,Lin,IP,Laux),
            (   I =.. [(::)|_]
             -> taiToObj(I,IP,Laux,Lout,Pin,Pout,T)
             ;  Lout=Laux, taiNLtoObj(I,Pin,Pout,T) ).
taiToObj((L::),IP,Lin,Lout,P,P,none) :-
        get_assoc(L,Lin,_)
        -> writeln('Duplicate labels'), abort
        ;  put_assoc(L,Lin,IP,Lout).
taiToObj(I,_,L,L,Pin,Pout,T) :- taiNLtoObj(I,Pin,Pout,T).
% helper predicate that translates instructions one by one,
% and collects target and address labels in two association lists
firstpass((C1;C2),IPin,IPout,Lin,Lout,Pin,Pout,Tin,Tout) :- !,
        firstpass(C1,IPin,IPaux,Lin,Laux,Pin,Paux,Tin,Taux),
        firstpass(C2,IPaux,IPout,Laux,Lout,Paux,Pout,Taux,Tout).
firstpass((I;),IPin,IPout,Lin,Lout,Pin,Pout,Tin,Tout) :- !,
        taiToObj(I,IPin,Lin,Lout,Pin,Pout,T),
        put_assoc(IPin,Tin,T,Tout),
        IPout is IPin + 1.
firstpass(I,IPin,IPout,Lin,Lout,Pin,Pout,Tin,Tout) :-
        taiToObj(I,IPin,Lin,Lout,Pin,Pout,T),
        ( T = none
          -> IPout = IPin, Tout = Tin
          ; put_assoc(IPin,Tin,T,Tout), IPout is IPin + 1 ).
% helper predicate that cross-references target and address labels
% collected by helper1
secondpass([],_).
secondpass([Lbl:P|T],L) :-
        get_assoc(Lbl,L,P) -> secondpass(T,L)
        ; write('Undefined label:'), writeln(Lbl), abort.

tacToObj(SourceCode,ObjectCode) :- % main translation predicate
        empty_assoc(Empty),
        firstpass(SourceCode,0,_,Empty,Labels,[],Placeholders,Empty,ObjectCode),
        secondpass(Placeholders,Labels).

% Pretty printer for object code
alignIP(X,Y) :- % helper
        atomic_concat('   ',X,Z),
        atom_chars(Z,L), append(_,L1,L), length(L1,3),!,
        atomic_list_concat(L1,Y).
writeObjectcode(Obj,IP) :- max_assoc(Obj,K,_), IP > K, !.
writeObjectcode(Obj,IP) :-
        get_assoc(IP,Obj,I), alignIP(IP,X),write(X), write(' :: '), writeln(I),
        IPnext is IP + 1, writeObjectcode(Obj,IPnext).

% Test translation to object code
/*:- Code = (
             x = 3 ;
             y = 4 ;
label     ::       ;
             z = 0 ;
again1    ::
again     :: z = z + x ;
             y = y - 1 ;
             if y > 0 goto again ;
             if x > x goto again1 ;
             if x < x goto label
          ),
          writeln('======================================'),
          writeln('Testing translation TAC -> object code,'),
          writeln('Original code:'),
          writeTac(Code),
          tacToObj(Code,Obj),
          writeln('Translated object code:'),
          writeObjectcode(Obj,0).*/

% Operational semantics of object code
% given as an interpreter -- replaces execTac of 01.pl
%  IP = instruction pointer, similar to CPU
%  execution of code changes one env into another
execTai(E,nop,E) :- !. % No Op has no effect, used just for padding
execTai(Env0,(X=Y),Env1) :- % execution of one instruction -- same as 01.pl
        evalExpr(Y,Val,Env0),
        put_assoc(X,Env0,Val,Env1).
execObj(Env,IP,Code,Env) :- % stop execution when reached non-existent address
        max_assoc(Code,K,_), IP > K, !.
execObj(EnvIn,IP,Code,EnvOut) :-
        get_assoc(IP,Code,Instr),
        (   Instr = (goto L), !, evalExpr(L,IPnext,EnvIn), EnvAux = EnvIn
        ;   Instr = (if E goto L), evalExpr(E,Val,EnvIn), EnvAux = EnvIn,
                  ( Val = 1
                    -> evalExpr(L,IPnext,EnvIn)
                    ;  IPnext is IP + 1 )
        ;   execTai(EnvIn,Instr,EnvAux), IPnext is IP+1 ),
        execObj(EnvAux,IPnext,Code,EnvOut).

execTAC(EnvIn,Code,EnvOut) :- tacToObj(Code,Obj), execObj(EnvIn,0,Obj,EnvOut).

/*% Test of object code interpreter
:- Code = (
            x = 3 ;
            y = 4 ;
dummylbl::        ;
            z = 0 ;
again    :: z = z + x ;
            y = y - 1 ;
            if y > 0 goto again ;
            if y > 0 goto dummylbl
          ),
          isTac(Code),
		  writeln('==========================='),
          writeln('Interpreting TAC:'),
          writeTac(Code),
          empty_assoc(Empty),
          tacToObj(Code,Obj),
          writeln('Translated object code:'),
          writeObjectcode(Obj,0),
          execObj(Empty,0,Obj,EnvOut),
          write('Values produced: x='),
          get_assoc(x,EnvOut,X), write(X),
          write(' y='), get_assoc(y,EnvOut,Y), write(Y),
          write(' z='), get_assoc(z,EnvOut,Z), writeln(Z).


:- Code = (
start::     x = 3 ;
            y = 4 ;
            z = 0 ;
again::     z = z + x ;
            y = y - 1 ;
            l = 3 ;
            if y > 0 goto start+l
          ),
          isTac(Code),
		  writeln('=========================='),
          writeln('Interpreting TAC:'),
          writeTac(Code),
          empty_assoc(Empty),
          tacToObj(Code,Obj),
          writeln('Translated object code:'),
          writeObjectcode(Obj,0),
          execObj(Empty,0,Obj,EnvOut),
          write('Values produced: x='),
          get_assoc(x,EnvOut,X), write(X),
          write(' y='), get_assoc(y,EnvOut,Y), write(Y),
          write(' z='), get_assoc(z,EnvOut,Z), writeln(Z).*/

/********************************************
 * Compiler from expression language to TAC
 ********************************************/

:- dynamic auxvar/1.

% generate new variable names
newvar(X) :-
        retract(auxvar(Y))
        ->  Y1 is Y+1, assert(auxvar(Y1)), atomic_concat('v_',Y1,X)
        ;   assert(auxvar(0)), X = 'v_0' .

resetnewvar :-
        (   retractall(auxvar(_)) -> true ; true ),
        assert(auxvar(0)).

list_prefix(_,0,[]):-!.
list_prefix([H|T],N,[H|L]) :- N1 is N-1, list_prefix(T,N1,L).

% generate new label names
newlabel(X) :-
        retract(auxlbl(Y))
        ->  Y1 is Y+1, assert(auxlbl(Y1)), atomic_concat('l_',Y1,X)
        ;   assert(auxlbl(0)), X = 'l_0' .

resetnewlabel :-
        (   retractall(auxlbl(_)) -> true ; true ),
        assert(auxlbl(0)).

% compile(Expr,TAC,Result)
%         takes in an expression 'Expr',
%         returns the 'TAC' that computes the expression
%         and the 'Result' register that contains the computed value
compile(E,(X=E),X) :- (integer(E);atom(E)),!,newvar(X).
compile(E,Code,R) :-
        E =.. [Op,X,Y], member(Op,[+,-,*,/,mod,/\,\/,xor,<<,>>]), !,
        compile(X,Cx,Rx), compile(Y,Cy,Ry),
        newvar(R), CE =.. [Op,Rx,Ry],
        Code = ( Cx ; Cy ; R = CE ) .
compile(+ X,Code,R) :- !,compile(0+X,Code,R).
compile(- X,Code,R) :- !,compile(0-X,Code,R).
compile(\ X,Code,R) :- !,compile((-1) xor X,Code,R).
compile(Cond, Code, R) :-
        Cond =.. [Op,X,Y], member(Op,[<,>,=<,>=,==,\=]), !,
        newvar(R), newlabel(Skip), newlabel(Lout),
        compile(X-Y,C1,Q), Cond1 =.. [Op,Q,0],
        C2 = ( if Cond1 goto Skip ; R = 0 ; goto Lout ; Skip::R = 1 ; Lout ::  ),
        Code = (C1 ; C2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compile((X ? Y), Code, R) :- !,
        newvar(R), newlabel(Skip), newlabel(Lout),
        compile(X,Cx,Qx),
        compile(Y,Cy,Qy),
        Code = (   Cx                   ;
                   if Qx \= 0 goto Skip ;
                   R = 0                ;
                   goto Lout            ;
            Skip ::                     ;
                   Cy                   ;
                   R = Qy               ;
            Lout ::                      ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*% Test compiler
%
:- resetnewvar, resetnewlabel.

:- Expression = (2<3),
   writeln('=================================='),
   write('Testing compilation of expression:'), writeln(Expression),
   compile(Expression,Code,Res),
   writeln('Compiled code:'),
   writeTac(Code),
   % tacToObj(Code,Obj),
   % writeln('Resulting object code:'),
   % writeObjectcode(Obj,0),
   write('Evaluation of expression:'),
   empty_assoc(Empty), evalExpr(Expression,Val,Empty),
   writeln(Val),
   execTAC(Empty,Code,Results),
   get_assoc(Res,Results,ObjVal),
   write('Execution of object code '),
   write(Res),write(' = '),writeln(ObjVal).


:- resetnewvar, resetnewlabel.

:- Expression = ((1+(2<3) < x + y * z)+10),
   writeln('=================================='),
   write('Testing compilation of expression:'), writeln(Expression),
   isExpr(Expression),
   compile(Expression,Code,Res),
   writeln('Compiled code:'),
   writeTac(Code),
   % tacToObj(Code,Obj),
   % writeln('Resulting object code:'),
   % writeObjectcode(Obj,0),
   write('Evaluation of expression:'),
   list_to_assoc([x-5,y-6,z-7],Start),
   evalExpr(Expression,Val,Start),
   writeln(Val),
   execTAC(Start,Code,Results),
   get_assoc(Res,Results,ObjVal),
   write('Execution of object code '),
   write(Res),write(' = '),writeln(ObjVal).

:- resetnewvar, resetnewlabel.
*/

/*:- Expression = ((2<((3<x) ? y))*(10+x-z/y)),
   writeln('=================================='),
   write('Testing compilation of expression:'), writeln(Expression),
   isExpr(Expression),
   compile(Expression,Code,Res),
   writeln('Compiled code:'),
   writeTac(Code),
   % tacToObj(Code,Obj),
   % writeln('Resulting object code:'),
   % writeObjectcode(Obj,0),
   write('Evaluation of expression:'),
   list_to_assoc([x-5,y-6,z-7],Start),
   evalExpr(Expression,Val,Start),
   writeln(Val),
   execTAC(Start,Code,Results),
   get_assoc(Res,Results,ObjVal),
   write('Execution of object code '),
   write(Res),write(' = '),writeln(ObjVal).*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- Expression = ((2<((3<x) ? z))*(10+x-z/y)),
   writeln('=================================='),
   write('Testing compilation of expression:'), writeln(Expression),
   isExpr(Expression),
   compile(Expression,Code,Res),
   writeln('Compiled code:'),
   writeTac(Code),
   % tacToObj(Code,Obj),
   % writeln('Resulting object code:'),
   % writeObjectcode(Obj,0),
   write('Evaluation of expression:'),
   list_to_assoc([x-5,y-6,z-7],Start),
   evalExpr(Expression,Val,Start),
   writeln(Val),
   execTAC(Start,Code,Results),
   get_assoc(Res,Results,ObjVal),
   write('Execution of object code '),
   write(Res),write(' = '),writeln(ObjVal).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





