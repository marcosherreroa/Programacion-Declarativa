ordenar(Xs,Ys) :- append(L1, [X,Y|L2], Xs), X>Y,!,
                  append(L1,[Y,X|L2],Xs1), ordenar(Xs1,Ys).
ordenar(Xs,Xs) :- ordenada(Xs).
