
sumaN(0,0).
sumaN(N,S) :- N > 0, N1 is N-1, sumaN(N1,S1), S is S1+N.

qsort([],[]).
qsort([X|Xs],S) :- menores(X,Xs,L1),
                   mayores(X,Xs,L2),
                   qsort(L1,S1),
                   qsort(L2,S2),
                   append(S1,[X|S2],S).

menores(_,[],[]).
menores(X,[Y|Ys],[Y|Zs]) :- Y < X, menores(X,Ys,Zs).
menores(X,[Y|Ys],Zs) :- Y > X, menores(X,Ys,Zs).

mayores(_,[],[]).
mayores(X,[Y|Ys],[Y|Zs]) :- Y > X, mayores(X,Ys,Zs).
mayores(X,[Y|Ys],Zs) :- Y < X, mayores(X,Ys,Zs).

