{\rtf1\ansi\ansicpg1254\cocoartf2821
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\paperw11900\paperh16840\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 % Atama i\uc0\u351 lemi\
assign(Var, Expr, EnvIn, [Var-Val | EnvIn]) :-\
    eval(Expr, EnvIn, Val).\
\
% Aritmetik i\uc0\u351 lemler\
eval(+(A,B), Env, R) :- eval(A, Env, R1), eval(B, Env, R2), R is R1 + R2.\
eval(-(A,B), Env, R) :- eval(A, Env, R1), eval(B, Env, R2), R is R1 - R2.\
eval(*(A,B), Env, R) :- eval(A, Env, R1), eval(B, Env, R2), R is R1 * R2.\
eval(/(A,B), Env, R) :-\
    eval(A, Env, R1),\
    eval(B, Env, R2),\
    ( R2 =:= 0 -> write('Error: Division by zero'), nl, fail ; R is R1 / R2 ).\
eval(mod(A,B), Env, R) :- eval(A, Env, R1), eval(B, Env, R2), R is R1 mod R2.\
eval(**(A,B), Env, R) :- eval(A, Env, R1), eval(B, Env, R2), R is R1 ** R2.\
\
% De\uc0\u287 i\u351 ken ve say\u305 \
eval(Var, Env, Val) :- atom(Var), member(Var-Val, Env).\
eval(Val, _, Val) :- number(Val).\
\
% S\'fcrekli \'e7al\uc0\u305 \u351 an yorumlay\u305 c\u305  d\'f6ng\'fcs\'fc\
repl(EnvIn) :-\
    write('>>> '), read(Input),\
    ( Input == exit ->\
        write('Goodbye!'), nl\
    ; (\
        ( Input = assign(Var, Expr, _, _) ->\
            assign(Var, Expr, EnvIn, EnvOut),\
            write('Assigned: '), write(Var), write(' = '), member(Var-Val, EnvOut), write(Val), nl,\
            repl(EnvOut)\
        ; eval(Input, EnvIn, Result) ->\
            write('Result: '), write(Result), nl,\
            repl(EnvIn)\
        ; write('Invalid input.'), nl,\
          repl(EnvIn)\
        )\
      )\
    ).\
\
% Ba\uc0\u351 lang\u305 \'e7\
main :- \
    write('Welcome to Prolog Calculator! Type `exit.` to quit.'), nl,\
    repl([]).\
\
:- initialization(main).\
}