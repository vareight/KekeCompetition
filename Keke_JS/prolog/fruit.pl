% load lists module                          
:- use_module(library(lists)).              

% fruit/1                                    
fruit(apple). fruit(pear). fruit(banana).    

% fruits_in/2                                
fruits_in(Xs, X) :- member(X, Xs), fruit(X).

subset2([], []).
subset2([E|Tail], [E|NTail]):-
  subset2(Tail, NTail).
subset2([_|Tail], NTail):-
  subset2(Tail, NTail).

memberchk(X,[X|_]) :- !.
memberchk(X,[_|T]):- memberchk(X,T).
subtract2([], _, []).
subtract2([Head|Tail], L2, L3) :-
                memberchk(Head, L2),
                !,
                subtract2(Tail, L2, L3).
subtract2([Head|Tail1], L2, [Head|Tail3]) :-
                subtract2(Tail1, L2, Tail3).