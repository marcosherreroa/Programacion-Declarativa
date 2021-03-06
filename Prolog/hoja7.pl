%1
sumIntersec([],_,0):- !.
sumIntersec(_,[],0):- !.
sumIntersec([X|Xs],[Y|Ys], Z) :- X == Y -> sumIntersec(Xs,Ys,Z1), Z is X+Z1;
                                 X < Y ->  sumIntersec(Xs,[Y|Ys],Z);
                                 sumIntersec([X|Xs],Ys,Z).

%2
%a)
nomiembro(_,[]):- !.
nomiembro(X,[Y|Ys]) :- X \== Y, nomiembro(X,Ys).

%b)Dudoso
hazConjunto([],[]).
hazConjunto([X|Xs], [X|Ys]) :- nomiembro(X,Xs), !, hazConjunto(Xs,Ys).
hazConjunto([_|Xs], Ys) :- hazConjunto(Xs,Ys).

%c)
union_(Xs,[],Xs):- !.
union_(Xs,[Y|Ys], [Y|Zs]) :- nomiembro(Y,Xs),!, union_(Xs,Ys,Zs).
union_(Xs,[_|Ys], Zs) :- union_(Xs,Ys,Zs).

%Muy interesante: hacerse el arbol de
%union_(Xs,[],Xs) :- !.
%union_ (Xs, [Y|Ys], [Y|Zs]) :- nomiembro(Y,Zs),!,union(Xs,Ys,Zs).
%union_(Xs,[_|Ys], ZS) :- union(Xs,Ys,Zs).
%para ver que esta mal

%d)
interseccion(_,[],[]):- !.
interseccion(Xs,[Y|Ys],Zs) :- nomiembro(Y,Xs), !, interseccion(Xs,Ys,Zs).
interseccion(Xs,[Y|Ys],[Y|Zs]) :- interseccion(Xs,Ys,Zs).

%3)
%
inorden(void,[]):- !.
inorden(arbol(X,I,D), In) :- inorden(I,InI),
                             inorden(D,InD),
                             append(InI,[X], A),
                             append(A,InD, In).

insertarEl(X, void, arbol(X, void, void)) :- !.
insertarEl(X, arbol(Y,I,D), arbol(Y,I1,D)) :- X =< Y,!,insertarEl(X,I,I1).
insertarEl(X, arbol(Y,I,D), arbol(Y,I,D1)) :- insertarEl(X,D,D1).

insertarLista([],A,A):- !.
insertarLista([X|Xs], A, B) :- insertarEl(X,A,A1), insertarLista(Xs,A1,B).

treeSort(Xs, L) :- insertarLista(Xs,void,A), inorden(A,L).

%4)
%

pertenece(X, arbol(X,_,_)):- !.
pertenece(X, arbol(Y,I,_)) :- X < Y, !, pertenece(X,I).
pertenece(X, arbol(_,_,D)) :- pertenece(X,D).

contenido(void,_):- !.
contenido(arbol(X,I,D), A):- pertenece(X,A), contenido(I,A), contenido(D,A).

/*
Sin usar pertenece:

contenido(void,_):- !.
contenido(arbol(X,I1,D1),arbol(X,I2,D2) :- !,
                                          contenido(I1,I2),
                                          contenido(D1,D2).

contenido(arbol(X1,I1,D1),arbol(X2,I2,D2)) :- X1 @< X2, !,
                                          contenido(arbol(X1,I1,void),I2),
                                          contenido(D2,arbol(X2,I2,D2)).

contenido(arbol(X1,I1,D1),arbol(X2,I2,D2)):-contenido(arbol(X1,void,D1),D2),
                                           contenido(I1,arbol(X2,I2,D2)).





*/
% a)
monomio(K,_) :- number(K), !.
monomio(Y,X) :- var(Y),!, Y == X.
monomio(K*Y,X) :- !, number(K), var(Y), Y==X.
monomio(Y^N,X) :- !,number(N), var(Y), Y == X.
monomio(K*Y^N, X) :- !, number(K), integer(N), N >= 0, var(Y), Y == X.

%b)
polinomio(M,X) :- monomio(M,X), !.
polinomio(M+P,X) :- !, monomio(M,X),polinomio(P,X).
polinomio(M-P,X) :- monomio(M,X),polinomio(P,X).

%7)No se si estara bien. no era especifico el enunciado
%
appendList(_,[],[]):- !.
appendList(X, [Xs|Xss], [[X|Xs]|Zss]) :- appendList(X,Xss,Zss).

sublistasPrincipio([],[[]]).
sublistasPrincipio([X|Xs], [[]|Zss]) :- sublistasPrincipio(Xs,Zss1),
                                   appendList(X,Zss1,Zss).

sublistas([],[[]]).
sublistas([X|Xs], Zss) :- sublistasPrincipio(Xs, Zss1),
                          appendList(X,Zss1,Zss2),
                          sublistas(Xs,Zss3),
                          append(Zss3,Zss2, Zss).

%Mejor usar setof
%sublista(Xs,Ys) :- prefijo(Xs,Ys)
%sublista(Xs,[_|Ys] :- sublista(Xs,Ys








