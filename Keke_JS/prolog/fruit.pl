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