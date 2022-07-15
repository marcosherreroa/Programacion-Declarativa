estudiante(pepe).
estudiante(rufo).
casado(pepe).
estudiante_soltero(X) :- not(casado(X)), estudiante(X).

max(X,Y,Z) :- Z = X, X >= Y; Z = Y, X < Y.

