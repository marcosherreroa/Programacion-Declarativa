%1)
%
%
elimina1([],X,[]).
elimina1([X|R],Y,NR) :- Y == X, elimina1(R,Y, NR).
elimina1([X|R],Y,[X|NR]) :- Y \== X, elimina1(R,Y,NR).

elimina2([],X,[]).
elimina2([X|R],Y,NR) :- Y = X, elimina2(R,Y, NR).
elimina2([X|R],Y,[X|NR]) :- Y \= X, elimina2(R,Y,NR).

elimina3([],X,[]).
elimina3([X|R],X,NR) :- elimina3(R,X,NR).
elimina3([X|R],Y,[X|NR]) :- Y \== X, elimina3(R,Y,NR).

% Para la primera consulta, eliminai([a,b,a,c],a,L), las tres funciones
% obtienen el mismo resultado:
%  L = [b,c]
% y no m�s respuestas. Es decir, se obtiene la lista resultante de
% eliminar todas las apariciones de a en la lista dada. Esto es porque
% las tres "comparaciones" utilizadas (=, == y ajuste de patrones) dan
% los mismos resultados cuando los elementos implicados son constantes
% (es equivalente que dos constantes sean sintacticamente id�nticos, a
% que unifiquen y a que ajusten )
%
% En cambio, la consulta eliminai([a,b,a,c],X,L) da respuestas
% diferentes para cada funci�n utilizada
%  * Con elimina1([a,b,a,c], X, L) se obtiene como �nica respuesta
%    L =[a,b,a,c]. No se instancia la variable X porque solo se compara
%    la igualdad sint�ctica (==) con elementos de la lista. Como no hay
%    nada sint�cticamente igual a X en la lista dada, no se elimina
%    ningun elemento
%
%  * Con elimina2([a,b,a,c],X,L) se obtiene como �nica respuesta:
%    X = a, L = [b,c]
%    En este caso, al usarse el operador de unificaci�n (=), s� que se
%    instancia la variable X a uno de los elementos de la lista, el
%    elemento a, y se obtiene como lista final la resultante de eliminar
%    a de la lista inicial. La raz�n por la que X no se instancia a
%    otros elementos de la lista es el uso de "Y \= X" en la �ltima
%    cl�usula. La consulta "X \= el" donde el es un elemento de la
%    lista siempre va a fallar porque la unificaci�n X = el siempre es
%    posible. Por tanto, la �ltima cl�usula nunca va a generar �xitos y,
%    dada cualquier consulta elimina2([e1,...,en], X, L) el programa
%    solo podr� progresar por la segunda cl�usula, con lo que
%    instanciar� la variable X al primer elemento de la lista
%
%  * Con elimina3([a,b,a,c],X,L) se obtienen tantas respuestas como
%    elementos tiene la lista dada. En cada una de ellas, se instancia
%    la variable X con un elemento de la lista dada y se eliminan todas
%    las apariciones de dicho elemento. En este caso, como en la �ltima
%    cl�usula se compara con "Y\==X" ( que siempre va a ser cierto,
%    porque no hay ning�n X en la lista) no hay nada que impida que X se
%    vaya instanciando a distintos elementos de la lista al pedir m�s
%    soluciones

%a)
sumatree(void,0).
sumatree(arbol(X,I,D),S) :- integer(X), sumatree(I,SI),
                            sumatree(D,SD), S is X+SI+SD.
%b)
maximo(void,0).
maximo(arbol(X,I,D), M) :- maximo(I,MI), maximo(D,MD),
                           M1 is max(X,MI), M is max(M1,MD).


%3

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

%4)
%a)
hanoi(0,_,_,_,[]) :- !.
hanoi(N,A,B,C,M) :- N1 is N-1,
                    hanoi(N1,A,C,B, M1),
                    append(M1,[A,C], M2),
                    hanoi(N1,B,A,C, M3),
                    append(M2,M3, M).

%b)
:- op(300, yfx, -->).
:- op(400, yfx, yy).

hanoi2(0,_,_,_, ' ').
hanoi2(1,A,_,C, A-->C) :- !.
hanoi2(N,A,B,C,M) :- N1 is N-1,
                    hanoi2(N1,A,C,B, M1),
                    hanoi2(N1,B,A,C, M3),
                    M = M1 yy A --> C yy M3.















