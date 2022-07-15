%1a)

nat(c).
nat(s(X)) :- nat(X).

%1b)
%Importante: corregir todo bien. Hay que comprbar siempre que es natural
%suma
sum(X,c,X) :- nat(X).
sum(X,Y,Z) :- Y = s(Y1), sum(X,Y1,Z1), Z = s(Z1).

%producto
prod(X,c,c) :- nat(X).
prod(X,Y,Z) :- Y = s(Y1), prod(X,Y1,Z1), sum(X,Z1,Z).

%potencia IMportante: X no puede ser 0 !!
pot(s(X),c,s(c)) :- nat(X).
pot(s(X),s(N1),Y) :- pot(X,N1,Y1), prod(X,Y1,Y).

%factorial
fact(c,s(c)).
fact(s(X1),Y) :- fact(X1,Y1), prod(s(X1),Y1,Y).

%fibonacci
fib(c,s(c)).
fib(s(c),s(c)).
fib(s(s(N1)),Y) :- fib(s(N1),Y1), fib(N1,Y2), sum(Y1,Y2,Y).

%1c)

%suma
sumf(X,c,X).
sumf(X,Y,Z) :- X1 = s(X), Y = s(Y1), sumf(X1,Y1,Z).

%producto
prodf(X,Y,Z) :- prodf(X,Y,c,Z).

prodf(X,c,A,A):- nat(X).
prodf(X,s(Y),A,Z) :- sumf(A,X,A1), prodf(X,Y,A1,Z).

%potencia
potf(X,N,Y) :- potfAux(X,N,s(c),Y).

potfAux(_,c,A,A).
potfAux(X,N,A,Y) :- N = s(N1), prodf(A,X,A1), potfAux(X,N1,A1,Y).

%factorial
factf(X,Y) :- factfAux(X,s(c),Y).

factfAux(c,A,A).
factfAux(s(X1),A,Y) :- prodf(A,s(X1),A1), factfAux(X1,A1,Y).

%fibonacci
fibf(N,Y) :- fibfAux(N,s(c),s(c),Y).

fibfAux(c,_,B,B).
fibfAux(s(c),A,_,A).
fibfAux(s(N),A,B,Y) :-  sum(A,B,A1) , fibfAux(N,A1,A,Y).

%3)

polfib(N,X,PF) :- polfibAux(N,X,_,_,PF).

polfibAux(0,_,1,0,1).
polfibAux(N,X,A,B,PF) :- N > 0 , N1 is N-1,
                     polfibAux(N1,X,A1,B1,PF1),
                     A is A1+B1, B is A1, PF is A*X^N + PF1.


%Pensar si habrá forma con recursión final


%4)

p(a).
p(f(f(X))) :- p(X).
q(a,a).
q(f(X), f(f(f(f(Y))))) :- q(X,Y).
r(a,f(a)).
r(f(X),Z) :- r(X,Y), q(Y,Z).

