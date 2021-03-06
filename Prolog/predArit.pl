fact(0,1).
fact(X,Z) :- X > 0, X1 is X-1, fact(X1,Z1), Z is X*Z1.

mas1([],[]).
mas1([X|Xs],[X1|Zs]) :- X1 is X+1, mas1(Xs,Zs).

sumaListas([],[],[]).
sumaListas([X|Xs],[Y|Ys],[Z|Zs]) :- Z is X+Y, sumaListas(Xs,Ys,Zs).

max(X,Y,X) :- X >= Y.
max(X,Y,Y) :- X < Y.

maxList([X],X).
maxList([X|Xs],Z) :- Xs \= [], maxList(Xs, Z1), max(Z1,X,Z).

maxArb(void,0).
maxArb(arbol(X,I,D), M) :- maxArb(I,MI), maxArb(D,MD),maxList([MI,X,MD],M).



