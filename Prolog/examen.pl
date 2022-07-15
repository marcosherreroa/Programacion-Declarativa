%Ejercicio 5 2016

%a)

nveces(_, [], 0) :- !.
nveces(X, [X|Ys], N) :- !, nveces(X,Ys,N1), N is N1+1.
nveces(X,[_|Ys], N) :-  nveces(X,Ys,N).

vecesaux([],_,[]) :- !.
vecesaux([Y|Ys], Xs, [N|Ns]) :- nveces(Y,Xs,N), vecesaux(Ys,Xs,Ns).

veces(Xs,Ns) :- vecesaux(Xs,Xs,Ns).

%b) Forma1
ejvar(X,X) :- var(X), !.
ejvar(T,X) :- not(atomic(T)),arg(_,T,S), ejvar(S,X).

nvars1(T,N) :- setof(Y, ejvar(T,Y), L), length(L,N).

%c) Forma2
membnoinst(X,[Y|_]):- X == Y, !.
membnoinst(X,[_|Ys]):- membnoinst(X,Ys).

vars([],Vs,Vs) :- !.
vars([X|Ts],Vs,Ws) :- atomic(X),!,vars(Ts,Vs,Ws).
vars([X|Ts],Vs,Ws) :- var(X),membnoinst(X,Vs),!,vars(Ts,Vs,Ws).
vars([X|Ts],Vs,Ws) :- var(X),!,vars(Ts,[X|Vs],Ws).
vars([T|Ts],Vs,Ws) :- T=..[_|L],append(L,Ts,Ts1),vars(Ts1,Vs,Ws).

nvars2(T,N) :- vars([T],[],Vs), length(Vs,N).

%Ejercicio 6 2020
%
completo(void,0) :- !.
completo(arbol(_,I,D),N) :- completo(I,N1), completo(D,N1), N is N1+1.

completo(A) :- completo(A,_).



