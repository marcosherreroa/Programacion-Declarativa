arbol_binario(void).
arbol_binario(arbol(_,I,D)) :- arbol_binario(I), arbol_binario(D).

member_arbol(X,arbol(X,_,_)).
member_arbol(X,arbol(_,I,_)) :- member_arbol(X,I).
member_arbol(X,arbol(_,_,D)) :- member_arbol(X,D).

preorden(void,[]).
preorden(arbol(X,I,D),Pre) :- preorden(I,PreI),
                              preorden(D,PreD),
                              append([X|PreI],PreD, Pre).

inorden(void,[]).
inorden(arbol(X,I,D),In) :- inorden(I,InI),
                            inorden(D,InD),
                            append(InI,[X],Z1),
                            append(Z1,InD,In).

posorden(void,[]).
posorden(arbol(X,I,D),Pos) :- posorden(I,PosI),
                              posorden(D,PosD),
                              append(PosI,PosD,Z1),
                              append(Z1,[X],Pos).

nat(c).
nat(s(X)) :- nat(X).

suma(X,c,X) :- nat(X).
suma(X,s(Y),s(Z)) :- suma(X,Y,Z).

numnodos(void,c).
numnodos(arbol(_,I,D),s(Z)) :- numnodos(I,Z1), numnodos(D,Z2), suma(Z1,Z2,Z).














