% Atama işlemi
assign(Var, Expr, EnvIn, [Var-Val | EnvIn]) :-
    eval(Expr, EnvIn, Val).

% Aritmetik işlemler
eval(+(A,B), Env, R) :- eval(A, Env, R1), eval(B, Env, R2), R is R1 + R2.
eval(-(A,B), Env, R) :- eval(A, Env, R1), eval(B, Env, R2), R is R1 - R2.
eval(*(A,B), Env, R) :- eval(A, Env, R1), eval(B, Env, R2), R is R1 * R2.
eval(/(A,B), Env, R) :-
    eval(A, Env, R1),
    eval(B, Env, R2),
    ( R2 =:= 0 -> write('Error: Division by zero'), nl, fail ; R is R1 / R2 ).
eval(mod(A,B), Env, R) :- eval(A, Env, R1), eval(B, Env, R2), R is R1 mod R2.
eval(**(A,B), Env, R) :- eval(A, Env, R1), eval(B, Env, R2), R is R1 ** R2.

% Değişken ve sayı
eval(Var, Env, Val) :- atom(Var), member(Var-Val, Env).
eval(Val, _, Val) :- number(Val).

% Sürekli çalışan yorumlayıcı döngüsü
repl(EnvIn) :-
    write('>>> '), read(Input),
    ( Input == exit ->
        write('Goodbye!'), nl
    ; (
        ( Input = assign(Var, Expr, _, _) ->
            assign(Var, Expr, EnvIn, EnvOut),
            write('Assigned: '), write(Var), write(' = '), member(Var-Val, EnvOut), write(Val), nl,
            repl(EnvOut)
        ; eval(Input, EnvIn, Result) ->
            write('Result: '), write(Result), nl,
            repl(EnvIn)
        ; write('Invalid input.'), nl,
          repl(EnvIn)
        )
      )
    ).

% Başlangıç
main :-
    write('Welcome to Prolog Calculator! Type `exit.` to quit.'), nl,
    repl([]).

:- initialization(main).
