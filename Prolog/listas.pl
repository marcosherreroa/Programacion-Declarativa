list_([]).
list_([_|Xs]) :- list_(Xs).

member_(X,[X|_]).
member_(X,[_|Xs]) :- member_(X,Xs).

prefijo([],_).
prefijo([X|Xs],[X|Ys]) :- prefijo(Xs,Ys).

sufijo([],_).
sufijo([X|Xs],[X|Ys]) :- Xs == Ys.
sufijo(Xs,[_|Ys]) :- sufijo(Xs,Ys).

sublista([],_).
sublista([X|Xs],[X|Ys]) :- prefijo(Xs,Ys).
sublista(Xs,[_|Ys]) :- sublista(Xs,Ys).

subsecuencia([],_).
subsecuencia([X|Xs],[X|Ys]) :- subsecuencia(Xs,Ys).
subsecuencia(Xs,[_|Ys]) :- subsecuencia(Xs,Ys).

concatenar([],Xs,Xs).
concatenar([X|Xs],Ys,[X|Zs]) :- concatenar(Xs,Ys,Zs).

invertir1([],[]).
invertir1([X|Xs],Zs) :- invertir1(Xs,Ys), concatenar(Ys,[X],Zs).

invertirAux([],Ys,Ys).
invertirAux([X|Xs],Ys,Zs) :- invertirAux(Xs,[X|Ys],Zs).
invertir2(Xs,Zs) :- invertirAux(Xs,[],Zs).

eliminar_uno([],_,[]).
eliminar_uno([Elem|Xs],Elem,Xs).
eliminar_uno([Y|Xs],Elem,[Y|Zs]) :- Y \= Elem, eliminar_uno(Xs,Elem,Zs).

eliminar_todos([],_,[]).
eliminar_todos([Elem|Xs],Elem,Zs) :- eliminar_todos(Xs,Elem,Zs).
eliminar_todos([Y|Xs],Elem,[Y|Zs]) :- Y \= Elem, eliminar_todos(Xs,Elem,Zs).

