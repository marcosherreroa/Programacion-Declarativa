suma(A,B,C) :- number(A), number(B), C is A+B.
suma(A,B,C) :- number(A), number(C), B is C-A.
suma(A,B,C) :- number(B), number(C), A is C-B.

sumanum([],0).
sumanum([X|Xs],Z) :- number(X), !, sumanum(Xs,Z1), Z is X+Z1.
sumanum([_|Xs],Z) :- sumanum(Xs,Z).

listapos([],[]).
listapos([X|Xs],[X|Zs]) :- X > 0, listapos(Xs,Zs).
listapos([X|Xs],Zs) :- X =< 0, listapos(Xs,Zs).

particion(_,[],[],[]).
particion(X,[Y|Xs], [Y|Men], May) :- Y =< X, particion(X,Xs, Men,May).
particion(X,[Y|Xs], Men, [Y|May]) :- Y > X, particion(X,Xs, Men, May).

quicksort([],[]).
quicksort([X|Xs], Zs) :- particion(X,Xs,Men,May),
                     quicksort(Men,MenOrd),
                     quicksort(May,MayOrd),
                     append(MenOrd,[X|MayOrd], Zs).







