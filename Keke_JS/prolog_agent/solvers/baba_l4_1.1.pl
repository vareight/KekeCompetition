%    First levels of Baba is You Thesis Competition 

% ******* LOW LEVEL PLANNER ********
% stupid but useful

% In this version, we try to make a planner so that it is a mere executor 
% (tells us exactly where to go: right, left, ...)
% we introduce rules (win condition)
% we push objects and need a distinction between word and phys objects
% isYou changed to rule(Obj, is, you)

% *****************************************
% *** FACTS *******************************
% *****************************************

% *** PHYSICAL OBJECTS ***
phys(baba).
phys(flag).
phys(grass).
phys(love).
pyhs(rock).
phys(wall).
phys(bone).
phys(goop).
phys(lava).
phys(keke).

% *** WORDS OBJECTS ***
word(babaWord).
word(flagWord).
word(grassWord).
word(loveWord).
word(rockWord).
word(wallWord).
word(boneWord).
word(goopWord).
word(lavaWord).
word(kekeWord).

word(is).
word(you).
word(win).
word(push).
word(stop).
word(kill).
word(sink).
word(hot).
word(melt).

% *** LINKS ***
link(baba, babaWord).
link(flag, flagWord).
link(love, loveWord).
link(grass, grassWord).
link(rock, rockWord).
link(wall, wallWord).
link(bone, boneWord).
link(goop, goopWord).
link(lava, lavaWord).
link(keke, kekeWord).

% *** UTILS ***
succ(0,1).
succ(1,2).
succ(2,3).
succ(3,4).
succ(4,5).
succ(5,6).
succ(6,7).
succ(7,8).
succ(8,9).

num(1).
num(2).
num(3).
num(4).
num(5).
num(6).
num(7).
num(8).

step(1).
step(2).
step(3).
step(4).
step(5).
%step(6).

:- discontiguous can/3.
:- discontiguous adds/4.
:- discontiguous deletes/3.
:- discontiguous solve_l4/4.

% obstacle(wall).
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% GENERAL %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RIGHT
listR(Result,Result,Last,Y,0) :- !.
listR(Acc,Result,Last,Y,N) :- 
    N1 is N-1,
    succ(Last,X1),
    listR([empty(Last,Y)|Acc], Result, X1, Y,N1).

% LEFT
listL(Result,Result,Last,Y,0) :- !.
listL(Acc,Result,Last,Y,N) :- 
    N1 is N-1,
    succ(X1,Last),
    listL([empty(Last,Y)|Acc], Result, X1, Y,N1).

% UP
listU(Result,Result,X,Last,0) :- !.
listU(Acc,Result,X,Last,N) :- 
    N1 is N-1,
    succ(Last,Y1),
    listU([empty(X,Last)|Acc], Result, X, Y1,N1).

% DOWN
listD(Result,Result,X,Last,0) :- !.
listD(Acc,Result,X,Last,N) :- 
    N1 is N-1,
    succ(Y1,Last),
    listD([empty(X,Last)|Acc], Result, X, Y1,N1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% RIGHT %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MOVE
can(moveRight(YouPhys, X, Y, N), Result, baba):-
    num(X),
    X1 is X+1,
    num(Y),
    step(N),
    listR([rule(YouPhys,you), pos(YouPhys, X, Y)],Result,X1,Y,N).
    %write(Result),nl,nl.

adds(moveRight(YouPhys, X, Y, N), [pos(YouPhys, Xf, Y), empty(X,Y)], _, baba):-
    num(X),
    num(Y),
    step(N),
    Xf is X+N.

deletes(moveRight(YouPhys, X, Y, N), [pos(YouPhys, X, Y), empty(Xf,Y)], baba):-
    num(X),
    num(Y),
    step(N),
    Xf is X+N.

% PUSH 1
can(push1MoveRight(YouPhys, X, Y, Word, N), Result, baba):-
    num(X),
    num(Y),
    X1 is X+1,
    X2 is X1+1,
    step(N),
    listR([rule(YouPhys,you), pos(YouPhys, X, Y), pos(Word,X1,Y), word(Word)], Result, X2, Y, N).

adds(push1MoveRight(YouPhys, X, Y, Word, N), [pos(Word,Xf1,Y), pos(YouPhys, Xf, Y), empty(X,Y), empty(X1,Y)], _, baba):-
    num(X),
    num(Y),
    X1 is X+1,
    step(N),
    Xf1 is X1+N,
    Xf is X+N.

deletes(push1MoveRight(YouPhys, X, Y, Word, N), [pos(YouPhys, X, Y), pos(Word,X1,Y), empty(Xf1,Y), empty(Xf,Y)], baba):-
    num(X),
    num(Y),
    X1 is X+1,
    step(N),
    Xf1 is X1+N,
    Xf is X+N.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEFT %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MOVE
can(moveLeft(YouPhys, X, Y, N), Result, baba):-
    num(X),
    X1 is X-1,
    num(Y),
    step(N),
    listL([rule(YouPhys,you), pos(YouPhys, X, Y)],Result,X1,Y,N).
    %write(Result),nl,nl.

adds(moveLeft(YouPhys, X, Y, N), [pos(YouPhys, Xf, Y), empty(X,Y)], _, baba):-
    num(X),
    num(Y),
    step(N),
    Xf is X-N.

deletes(moveLeft(YouPhys, X, Y, N), [pos(YouPhys, X, Y), empty(Xf,Y)], baba):-
    num(X),
    num(Y),
    step(N),
    Xf is X-N.

% PUSH 1
can(push1MoveLeft(YouPhys, X, Y, Word, N), Result, baba):-
    num(X),
    num(Y),
    X1 is X-1,
    X2 is X1-1,
    step(N),
    listL([rule(YouPhys,you), pos(YouPhys, X, Y), pos(Word,X1,Y), word(Word)], Result, X2, Y, N).

adds(push1MoveLeft(YouPhys, X, Y, Word, N), [pos(Word,Xf1,Y), pos(YouPhys, Xf, Y), empty(X,Y), empty(X1,Y)], _, baba):-
    num(X),
    num(Y),
    X1 is X-1,
    step(N),
    Xf1 is X1-N,
    Xf is X-N.

deletes(push1MoveLeft(YouPhys, X, Y, Word, N), [pos(YouPhys, X, Y), pos(Word,X1,Y), empty(Xf1,Y), empty(Xf,Y)], baba):-
    num(X),
    num(Y),
    X1 is X-1,
    step(N),
    Xf1 is X1-N,
    Xf is X-N.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% UP %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MOVE
can(moveUp(YouPhys, X, Y, N), Result, baba):-
    num(X),
    num(Y),
    Y1 is Y+1,
    step(N),
    listU([rule(YouPhys,you), pos(YouPhys, X, Y)],Result,X,Y1,N).
    %write(Result),nl,nl.

adds(moveUp(YouPhys, X, Y, N), [pos(YouPhys, X, Yf), empty(X,Y)], _, baba):-
    num(X),
    num(Y),
    step(N),
    Yf is Y+N.

deletes(moveUp(YouPhys, X, Y, N), [pos(YouPhys, X, Y), empty(X,Yf)], baba):-
    num(X),
    num(Y),
    step(N),
    Yf is Y+N.

% PUSH 1
can(push1MoveUp(YouPhys, X, Y, Word, N), Result, baba):-
    num(X),
    num(Y),
    Y1 is Y+1,
    Y2 is Y1+1,
    step(N),
    listU([rule(YouPhys,you), pos(YouPhys, X, Y), pos(Word,X,Y1), word(Word)], Result, X, Y2, N).

adds(push1MoveUp(YouPhys, X, Y, Word, N), [pos(Word,X,Yf1), pos(YouPhys, X, Yf), empty(X,Y), empty(X,Y1)], _, baba):-
    num(X),
    num(Y),
    Y1 is Y+1,
    step(N),
    Yf1 is Y1+N,
    Yf is Y+N.

deletes(push1MoveUp(YouPhys, X, Y, Word, N), [pos(YouPhys, X, Y), pos(Word,X,Y1), empty(X,Yf1), empty(X,Yf)], baba):-
    num(X),
    num(Y),
    Y1 is Y+1,
    step(N),
    Yf1 is Y1+N,
    Yf is Y+N.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% DOWN %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MOVE
can(moveDown(YouPhys, X, Y, N), Result, baba):-
    num(X),
    num(Y),
    Y1 is Y-1,
    step(N),
    listD([rule(YouPhys,you), pos(YouPhys, X, Y)],Result,X,Y1,N).
    %write(Result),nl,nl.

adds(moveDown(YouPhys, X, Y, N), [pos(YouPhys, X, Yf), empty(X,Y)], _, baba):-
    num(X),
    num(Y),
    num(N),
    Yf is Y-N.

deletes(moveDown(YouPhys, X, Y, N), [pos(YouPhys, X, Y), empty(X,Yf)], baba):-
    num(X),
    num(Y),
    step(N),
    Yf is Y-N.

% PUSH 1
can(push1MoveDown(YouPhys, X, Y, Word, N), Result, baba):-
    num(X),
    num(Y),
    Y1 is Y-1,
    Y2 is Y1-1,
    step(N),
    listD([rule(YouPhys,you), pos(YouPhys, X, Y), pos(Word,X,Y1), word(Word)], Result, X, Y2, N).

adds(push1MoveDown(YouPhys, X, Y, Word, N), [pos(Word,X,Yf1), pos(YouPhys, X, Yf), empty(X,Y), empty(X,Y1)], _, baba):-
    num(X),
    num(Y),
    Y1 is Y-1,
    step(N),
    Yf1 is Y1-N,
    Yf is Y-N.

deletes(push1MoveDown(YouPhys, X, Y, Word, N), [pos(YouPhys, X, Y), pos(Word,X,Y1), empty(X,Yf1), empty(X,Yf)], baba):-
    num(X),
    num(Y),
    Y1 is Y-1,
    step(N),
    Yf1 is Y1-N,
    Yf is Y-N.

% PUSH 2
can(push2MoveDown(YouPhys, X, Y, Word1, Word2, N), Result, baba):-
    num(X),
    num(Y),
    Y1 is Y-1,
    Y2 is Y1-1,
    Y3 is Y2-1,
    step(N),
    listD([rule(YouPhys,you), pos(YouPhys, X, Y), pos(Word1,X,Y1), word(Word1), pos(Word2,X,Y2), word(Word2)], Result, X, Y3, N).

adds(push2MoveDown(YouPhys, X, Y, Word1, Word2, N), [pos(Word1,X,Yf2), pos(Word1,X,Yf1), pos(YouPhys, X, Yf), empty(X,Y), empty(X,Y1), empty(X,Y2)], _, baba):-
    num(X),
    num(Y),
    Y1 is Y-1,
    Y2 is Y1-1,
    step(N),
    Yf2 is Y2-N,
    Yf1 is Y1-N,
    Yf is Y-N.

deletes(push2MoveDown(YouPhys, X, Y, Word1, Word2, N), [pos(YouPhys, X, Y), pos(Word1,X,Y1), pos(Word2,X,Y2), empty(X,Yf2), empty(X,Yf1), empty(X,Yf)], baba):-
    num(X),
    num(Y),
    Y1 is Y-1,
    Y2 is Y1-1,
    step(N),
    Yf2 is Y2-N,
    Yf1 is Y1-N,
    Yf is Y-N.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% SOLVE LEVEL %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%*
can(solveLevel(YouPhys, WinPhys, X, Y), [rule(YouPhys,you), pos(YouPhys, X, Y),
            rule(WinPhys,win), pos(WinPhys, X, Y)], baba).

adds(solveLevel(YouPhys, WinPhys, X, Y), [won(YouPhys, WinPhys)], _, baba).

deletes(solveLevel(YouPhys, WinPhys, X, Y), [], baba).


%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% RULES %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%
% HORIZONTAL RULES
can(createRule(Phys1, Word2), [phys(Phys1), link(Phys1, Word1), pos(Word1, X1, Y), pos(is, Xis, Y), pos(Word2, X2, Y), word(Word1), word(Word2)], baba):-
    succ(X1,Xis),
    succ(Xis,X2).

% VERTICAL RULES
can(createRule(Phys1, Word2), [phys(Phys1), link(Phys1, Word1), pos(Word1, X, Y1), pos(is, X, Yis), pos(Word2, X, Y2), word(Word1), word(Word2)], baba):-
    succ(Y2,Yis),
    succ(Yis,Y1).

% HORIZONTAL RULES WITH OBJECTS
can(createRule(Phys1, Phys2), [phys(Phys1), link(Phys1, Word1), pos(Word1, X1, Y), pos(is, Xis, Y), phys(Phys2), link(Phys2, Word2),  pos(Word2, X2, Y),  word(Word1), word(Word2)], baba):-
    succ(X1,Xis),
    succ(Xis,X2).

% VERTICAL RULES  WITH OBJECTS
can(createRule(Phys1, Phys2), [phys(Phys1), link(Phys1, Word1), pos(Word1, X, Y1), pos(is, X, Yis), phys(Phys2), link(Phys2, Word2),  pos(Word2, X, Y2),  word(Word1), word(Word2)], baba):-
    succ(Y2,Yis),
    succ(Yis,Y1).

adds(createRule(Word1, Word2), [rule(Word1, Word2), searchMoveOk()], _, baba).

deletes(createRule(Word1, Word2), [], baba).

% we need a specific delete in case of PHYS-IS-PHYS rule
deletes(createRule(Phys, Phys), [mutable(Phys)], baba).

%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% TRANSFORM %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%
can(transform(Phys1, Phys2,X,Y),[rule(Phys1,Phys2), phys(Phys1), phys(Phys2), pos(Phys1,X,Y), mutable(Phys1)], baba).

adds(transform(Phys1, Phys2,X,Y),[pos(Phys2,X,Y), transform(Phys1,Phys2,X,Y)], _, baba).

deletes(transform(Phys1, Phys2,X,Y),[pos(Phys1,X,Y)], baba).

%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% DESTROY %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%

%%% OBJECTS %%%
% HORIZONTAL RULES - DESTROY PUSHING DOWN
can(destroyRule(Phys1, Phys2), [phys(Phys1), link(Phys1, Word1), phys(Phys2), link(Phys2, Word2), 
                        rule(Phys1, Phys2), pos(Word1, X1, Y1), pos(is, Xis, Y), pos(Word2, X2, Y),
                        pos(Baba,Xb,Yb), rule(Baba,you)], baba):-
    succ(Y1,Y), num(Yb), Yb >= Y,
    succ(X1,Xis),
    succ(Xis,X2).
% HORIZONTAL RULES - DESTROY PUSHING UP
can(destroyRule(Phys1, Phys2), [phys(Phys1), link(Phys1, Word1), phys(Phys2), link(Phys2, Word2), 
                        rule(Phys1, Phys2), pos(Word1, X1, Y1), pos(is, Xis, Y), pos(Word2, X2, Y),
                        pos(Baba,Xb,Yb), rule(Baba,you)], baba):-
    succ(Y,Y1), num(Yb), Yb =< Y,
    succ(X1,Xis),
    succ(Xis,X2).

% VERTICAL RULES - DESTROY PUSHING RIGHT
can(destroyRule(Phys1, Phys2), [phys(Phys1), link(Phys1, Word1), phys(Phys2), link(Phys2, Word2), 
                        rule(Phys1,Phys2), pos(Word1, X1, Y1), pos(is, X, Yis), pos(Word2, X, Y2),
                        pos(Baba,Xb,Yb), rule(Baba,you)], baba):-
    succ(X,X1), num(Xb), Xb =< X,
    succ(Y2,Yis),
    succ(Yis,Y1).
% VERTICAL RULES - DESTROY PUSHING LEFT
can(destroyRule(Phys1, Phys2), [phys(Phys1), link(Phys1, Word1), phys(Phys2), link(Phys2, Word2), 
                        rule(Phys1,Phys2), pos(Word1, X1, Y1), pos(is, X, Yis), pos(Word2, X, Y2),
                        pos(Baba,Xb,Yb), rule(Baba,you)], baba):-
    succ(X1,X), num(Xb), Xb >= X,
    succ(Y2,Yis),
    succ(Yis,Y1).


%%% WORDS %%%
% HORIZONTAL RULES - DESTROY PUSHING DOWN
can(destroyRule(Phys1, Word2), [phys(Phys1), link(Phys1, Word1), word(Word2),
                        rule(Phys1, Word2), pos(Word1, X1, Y1), pos(is, Xis, Y), pos(Word2, X2, Y),
                        pos(Baba,Xb,Yb), rule(Baba,you)], baba):-
    succ(Y1,Y), num(Yb), Yb >= Y,
    succ(X1,Xis),
    succ(Xis,X2).
% HORIZONTAL RULES - DESTROY PUSHING UP
can(destroyRule(Phys1, Word2), [phys(Phys1), link(Phys1, Word1), word(Word2), 
                        rule(Phys1, Word2), pos(Word1, X1, Y1), pos(is, Xis, Y), pos(Word2, X2, Y),
                        pos(Baba,Xb,Yb), rule(Baba,you)], baba):-
    succ(Y,Y1), num(Yb), Yb =< Y,
    succ(X1,Xis),
    succ(Xis,X2).

% VERTICAL RULES - DESTROY PUSHING RIGHT
can(destroyRule(Phys1, Word2), [phys(Phys1), link(Phys1, Word1), word(Word2), 
                        rule(Phys1,Word2), pos(Word1, X1, Y1), pos(is, X, Yis), pos(Word2, X, Y2),
                        pos(Baba,Xb,Yb), rule(Baba,you)], baba):-
    succ(X,X1), num(Xb), Xb =< X,
    succ(Y2,Yis),
    succ(Yis,Y1).
% VERTICAL RULES - DESTROY PUSHING LEFT
can(destroyRule(Phys1, Word2), [phys(Phys1), link(Phys1, Word1), word(Word2), 
                        rule(Phys1,Word2), pos(Word1, X1, Y1), pos(is, X, Yis), pos(Word2, X, Y2),
                        pos(Baba,Xb,Yb), rule(Baba,you)], baba):-
    succ(X1,X), num(Xb), Xb >= X,
    succ(Y2,Yis),
    succ(Yis,Y1).


adds(destroyRule(Word1, Word2), [destroyRule(Word1, Word2)], _, baba):-
    Word1\=Word2.
adds(destroyRule(Word1, Word2), [destroyRule(Word1, Word2), mutable(Word1)], _, baba):-
    Word1=Word2.

deletes(destroyRule(Word1, Word2), [rule(Word1,Word2)], baba).

%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% FREE %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%
can(free(Phys, Obstacle, You),[rule(You,you), phys(Phys), phys(Obstacle)], baba). % l'ostacolo c'è ma è stato rimosso (distuggendo la regola)

adds(free(Phys, Obstacle, You),[free(Phys, Obstacle, You)|EmptyPos], _, baba):-
    prioritary(Obstacle),
    findall(empty(X,Y), pos(Obstacle,X,Y), EmptyPos).

deletes(free(Phys, Obstacle, You),[], baba).

%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% UNBOND %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%
% we have only 2 cases, hot and melt
can(unbond(Phys1, Rule, Phys2),[rule(Phys1,Rule), word(Rule), phys(Phys1), phys(Phys2)], baba).

adds(unbond(Phys1, Rule, Phys2),[unbond(Phys1, Rule, Phys2)], _, baba).
deletes(unbond(Phys1, Rule, Phys2),[], baba).

% ***************
% *** TESTS *****
% ***************

test(P) :-
    solve(lvl_01, Plan, pos(baba,2,5),[]),
    solve(lvl_02, P, pos(baba,3,5), Plan).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% STATE %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
previous_actions([], NewState,NewState).
previous_actions([H|Actions],State,NewState):-
    adds(H, AddConditions, _, Domain),
    deletes(H, DelPreConditions, Domain),

    update_state(AddConditions, State, AddedState),
    subtract(AddedState, DelPreConditions, CurrentState),

    previous_actions(Actions, CurrentState, NewState).

update_state([], CurrentState, CurrentState).
update_state([H|AddConditions], State, CurrentState):-
    update_state(AddConditions, [H|State], CurrentState).

save_prio_pos([],Prio).
save_prio_pos([H|T],Prio):- 
    H = pos(Prio,X,Y),
    assert(pos(Prio,X,Y)),
    %write(H),nl,nl,
    save_prio_pos(T,Prio).
save_prio_pos([H|T],Prio):- 
    H \= pos(Prio,X,Y),
    save_prio_pos(T,Prio).

prioritize(free(_,Phys,_), Phys):-
    assert(prioritary(Prio)).
prioritize(_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% SOLVER %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic prioritary/1.
:- dynamic pos/3.

solve_l4(Level,P,Goal,Actions) :-
    
    consult('levels/levels_l4.pl'),
    level(Level,State),
    prioritize(Goal, Prio),

    flatten(Actions,FlatActions),
    previous_actions(FlatActions,State,NewState),

    %retractall(prioritary(_)),

    %assert(prioritary(Prio)),
    save_prio_pos(NewState, Prio),

    plan(NewState,
            [Goal], baba,
            %[pos(baba,6,4)], baba,
            P).
