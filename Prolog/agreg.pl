
evLista(_,[]) :- !.
evLista(P,[X|Xs]) :- call(P,X), evLista(P,Xs).
