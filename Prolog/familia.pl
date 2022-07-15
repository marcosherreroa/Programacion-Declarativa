hombre(javier).
hombre(pedro).
hombre(alfonso).
hombre(juan).
hombre(jorge).

mujer(marta).
mujer(maria).
mujer(carmen).
mujer(teresa).
mujer(alicia).

progenitor(pedro,juan).
progenitor(pedro,marta).
progenitor(pedro,alfonso).
progenitor(carmen,juan).
progenitor(carmen,alfonso).
progenitor(teresa,alicia).
progenitor(jorge,alicia).
progenitor(javier,pedro).
progenitor(javier,teresa).
progenitor(maria,pedro).
progenitor(maria,teresa).

padre(X,Y) :- progenitor(X,Y), hombre(X).
madre(X,Y) :- progenitor(X,Y), mujer(X).
hijo(X,Y) :-  progenitor(Y,X), hombre(X).
abuelo(X,Y) :- padre(X,Z), progenitor(Z,Y).
hermano(X,Y) :-  progenitor(Z,X), progenitor(Z,Y), X \= Y . % dudoso el \= ...
tio(X,Y) :- hermano(X,Z),progenitor(Z,Y).
descendiente(X,Y) :- progenitor(Y,X).
descendiente(X,Y) :- progenitor(Y,Z), descendiente(X,Z).
