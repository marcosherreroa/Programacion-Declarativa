% mi forma . Problema, instancia variables de loa argumentos. No
% se si debe. En principio vale, ya que está pensado para no ser
% reversible
%
term_atomic_list([],[]).
term_atomic_list([T|Ts], [T|Zs]) :- atomic(T),
                                term_atomic_list(Ts,Zs).

term_atomic_list([T|Ts], Zs) :- var(T),
                                term_atomic_list(Ts,Zs).

term_atomic_list([T|Ts], Zs) :- nonvar(T),
                               T =.. [_|Args],
                               append(Args,Ts,Ts1),
                               term_atomic_list(Ts1,Zs).

term_atomic(T,Zs) :- term_atomic_list([T],Zs).

%mi forma. Arregle el problema que tenia usando functor
%
subterm_list(T,[U|_]) :- ground(U),
                         T = U.

subterm_list(T,[U|Us]) :- atomic(U),
                          subterm_list(T,Us).

subterm_list(T,[U|Us]) :- var(U),
                          subterm_list(T,Us).

subterm_list(T,[U|Us]) :- nonvar(U),
                          functor(U,_,N),
                          N > 0,
                          U =.. [_|Args],
                          append(Args,Us,Us1),
                          subterm_list(T,Us1).

subterm(T,U) :- subterm_list(T,[U]).

%forma de susana con listas. Mirar a partir de aqui
%
subterm2_list(T, [U|_]) :- subterm2(T,U).
subterm2_list(T, [_|Us]) :- subterm2_list(T,Us).

subterm2(T,T).
subterm2(T,U) :- U =.. [_|Args],
                 subterm2_list(T, Args).










