%    First levels of Baba is You Thesis Competition 

% ******* INERMEDIUM LEVEL PLANNER ********
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
succ(1,2).
succ(2,3).
succ(3,4).
succ(4,5).
succ(5,6).
succ(6,7).
succ(7,8).

num(1).
num(2).
num(3).
num(4).
num(5).
num(6).
num(7).
num(8).

:- discontiguous can/3.
:- discontiguous adds/4.
:- discontiguous deletes/3.


% obstacle(wall).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% GENERAL %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
path_to_obj(0,Xf,Yf, Xf,Yf, Path, Path).

path_to_obj(N,Xi,Yi, Xf,Yf, Acc, Path):-
    Xi = Xf,
    get_closer(N,Xi,Yi, Xf,Yf, Acc, Path).

path_to_obj(N,Xi,Yi, Xf,Yf, Acc, Path):-
    Yi = Yf,
    get_closer(N,Xi,Yi, Xf,Yf, Acc, Path).

path_to_obj(N,Xi,Yi, Xf,Yf, Acc, Path):-
    Xi \= Xf,
    Yi \= Yf,
    get_closer(N,Xi,Yi, Xf,Yf, Acc, Path).

% CLOSER X - RIGHT
get_closer(N,Xi,Yi, Xf,Yf, Acc, Path):-
    Xi < Xf,
    X is Xi+1,
    N1 is N-1,
    path_to_obj(N1,X,Yi,Xf,Yf,[empty(X,Yi)|Acc], Path).

% CLOSER X - LEFT
get_closer(N,Xi,Yi, Xf,Yf, Acc, Path):-
    Xi > Xf,
    X is Xi-1,
    N1 is N-1,
    path_to_obj(N1,X,Yi,Xf,Yf,[empty(X,Yi)|Acc], Path).

% CLOSER Y - UP
get_closer(N,Xi,Yi, Xf,Yf, Acc, Path):-
    Yi < Yf,
    Y is Yi+1,
    N1 is N-1,
    path_to_obj(N1,Xi,Y,Xf,Yf,[empty(Xi,Y)|Acc], Path).
    
% CLOSER Y - DOWN
get_closer(N,Xi,Yi, Xf,Yf, Acc, Path):-
    Yi > Yf,
    Y is Yi-1,
    N1 is N-1,
    path_to_obj(N1,Xi,Y,Xf,Yf,[empty(Xi,Y)|Acc], Path).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% MOVE &%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%can(move(Obj, X, Y, Xf, Yf), [pos(Obj, X, Y), empty(Xf,Yf), searchMoveOk(), prioritary(Obj)], baba).

%adds(move(Obj, X, Y, Xf, Yf), [pos(Obj, Xf, Yf), empty(X,Y)], _, baba).

%deletes(move(Obj, X, Y, Xf, Yf), [pos(Obj, X, Y), empty(Xf,Yf), searchMoveOk()], baba).

% pos - any obj movement
can(pos(Obj, Xf, Yf), [pos(Obj, X, Y), empty(Xf,Yf), searchMoveOk(), prioritary(Obj)], baba).

adds(pos(Obj, Xf, Yf), [pos(Obj, Xf, Yf)], _, baba).

deletes(pos(Obj, Xf, Yf), [empty(Xf,Yf), searchMoveOk()], baba).

% pos - any obj movement - REPLACE JUST FOR THE LAST LEVEL
can(pos(Obj, Xf, Yf), [pos(Obj, X, Y), pos(Word,Xf,Yf), searchMoveOk(), prioritary(Obj)], baba).

adds(pos(Obj, Xf, Yf), [pos(Obj, Xf, Yf)], _, baba).

deletes(pos(Obj, Xf, Yf), [empty(Xf,Yf), searchMoveOk()], baba).

% new pos
%can(pos(Obj, Xf, Yf), Path, baba):-
%    Xf is 7, Yf is 4, X is 2, Y is 4,
%    num(Xf),num(Yf),num(X),num(Y),
%    Nx is abs(Xf-X),
%    Ny is abs(Yf-Y),
%    N is Nx+Ny,
%    path_to_obj(N, X,Y, Xf,Yf, [pos(Obj, X, Y), searchMoveOk(), prioritary(Obj)], Path),
%    write(Path),nl,nl.

%adds(pos(Obj, Xf, Yf), [pos(Obj, Xf, Yf)], _, baba).

%deletes(pos(Obj, Xf, Yf), [empty(Xf,Yf), searchMoveOk()], baba).

% pos - PUSH 
%can(pos(Obj, Xf, Yf), [pos(Obj, Xf, Y), empty(Xf,Yf), searchMoveOk(), prioritary(Obj)], baba).
%can(pos(Obj, Xf, Yf), [pos(Obj, X, Yf), empty(Xf,Yf), searchMoveOk(), prioritary(Obj)], baba).

%adds(pos(Obj, Xf, Yf), [pos(Obj, Xf, Yf), push(Obj)], _, baba).

%deletes(pos(Obj, Xf, Yf), [empty(Xf,Yf), searchMoveOk()], baba).
% ******************************
% ********** RULES *************
% ******************************

%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% CREATE %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%
% HORIZONTAL RULES
can(rule(Phys1, Word2), [phys(Phys1), link(Phys1, Word1), pos(Word1, X1, Y), pos(is, Xis, Y), pos(Word2, X2, Y), word(Word1), word(Word2)], baba):-
    succ(X1,Xis),
    succ(Xis,X2).

% VERTICAL RULES
can(rule(Phys1, Word2), [phys(Phys1), link(Phys1, Word1), pos(Word1, X, Y1), pos(is, X, Yis), pos(Word2, X, Y2), word(Word1), word(Word2)], baba):-
    succ(Y2,Yis),
    succ(Yis,Y1).

% HORIZONTAL RULES WITH OBJECTS
can(rule(Phys1, Phys2), [phys(Phys1), link(Phys1, Word1), pos(Word1, X1, Y), pos(is, Xis, Y), phys(Phys2), link(Phys2, Word2),  pos(Word2, X2, Y),  word(Word1), word(Word2)], baba):-
    succ(X1,Xis),
    succ(Xis,X2).

% VERTICAL RULES  WITH OBJECTS
can(rule(Phys1, Phys2), [phys(Phys1), link(Phys1, Word1), pos(Word1, X, Y1), pos(is, X, Yis), phys(Phys2), link(Phys2, Word2),  pos(Word2, X, Y2),  word(Word1), word(Word2)], baba):-
    succ(Y2,Yis),
    succ(Yis,Y1).

adds(rule(Word1, Word2), [rule(Word1, Word2), searchMoveOk()], _, baba).

deletes(rule(Word1, Word2), [], baba).


%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% DESTROY %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%

%%% OBJECTS %%%
% HORIZONTAL RULES - DESTROY PUSHING DOWN
can(destroyRule(Phys1, Phys2), [phys(Phys1), link(Phys1, Word1), phys(Phys2), link(Phys2, Word2), 
                        rule(Phys1, Phys2), pos(Word1, X1, Y1), pos(is, Xis, Y), pos(Word2, X2, Y),
                        pos(Baba,Xb,Yb), rule(Baba,you)], baba):-
    succ(Y1,Y), num(Yb), Yb > Y,
    succ(X1,Xis),
    succ(Xis,X2).
% HORIZONTAL RULES - DESTROY PUSHING UP
can(destroyRule(Phys1, Phys2), [phys(Phys1), link(Phys1, Word1), phys(Phys2), link(Phys2, Word2), 
                        rule(Phys1, Phys2), pos(Word1, X1, Y1), pos(is, Xis, Y), pos(Word2, X2, Y),
                        pos(Baba,Xb,Yb), rule(Baba,you)], baba):-
    succ(Y,Y1), num(Yb), Yb < Y,
    succ(X1,Xis),
    succ(Xis,X2).

% VERTICAL RULES - DESTROY PUSHING RIGHT
can(destroyRule(Phys1, Phys2), [phys(Phys1), link(Phys1, Word1), phys(Phys2), link(Phys2, Word2), 
                        rule(Phys1,Phys2), pos(Word1, X1, Y1), pos(is, X, Yis), pos(Word2, X, Y2),
                        pos(Baba,Xb,Yb), rule(Baba,you)], baba):-
    succ(X,X1), num(Xb), Xb < X,
    succ(Y2,Yis),
    succ(Yis,Y1).
% VERTICAL RULES - DESTROY PUSHING LEFT
can(destroyRule(Phys1, Phys2), [phys(Phys1), link(Phys1, Word1), phys(Phys2), link(Phys2, Word2), 
                        rule(Phys1,Phys2), pos(Word1, X1, Y1), pos(is, X, Yis), pos(Word2, X, Y2),
                        pos(Baba,Xb,Yb), rule(Baba,you)], baba):-
    succ(X1,X), num(Xb), Xb > X,
    succ(Y2,Yis),
    succ(Yis,Y1).


%%% WORDS %%%
% HORIZONTAL RULES - DESTROY PUSHING DOWN
can(destroyRule(Phys1, Word2), [phys(Phys1), link(Phys1, Word1), word(Word2),
                        rule(Phys1, Word2), pos(Word1, X1, Y1), pos(is, Xis, Y), pos(Word2, X2, Y),
                        pos(Baba,Xb,Yb), rule(Baba,you)], baba):-
    succ(Y1,Y), num(Yb), Yb > Y,
    succ(X1,Xis),
    succ(Xis,X2).
% HORIZONTAL RULES - DESTROY PUSHING UP
can(destroyRule(Phys1, Word2), [phys(Phys1), link(Phys1, Word1), word(Word2), 
                        rule(Phys1, Word2), pos(Word1, X1, Y1), pos(is, Xis, Y), pos(Word2, X2, Y),
                        pos(Baba,Xb,Yb), rule(Baba,you)], baba):-
    succ(Y,Y1), num(Yb), Yb < Y,
    succ(X1,Xis),
    succ(Xis,X2).

% VERTICAL RULES - DESTROY PUSHING RIGHT
can(destroyRule(Phys1, Word2), [phys(Phys1), link(Phys1, Word1), word(Word2), 
                        rule(Phys1,Word2), pos(Word1, X1, Y1), pos(is, X, Yis), pos(Word2, X, Y2),
                        pos(Baba,Xb,Yb), rule(Baba,you)], baba):-
    succ(X,X1), num(Xb), Xb < X,
    succ(Y2,Yis),
    succ(Yis,Y1).
% VERTICAL RULES - DESTROY PUSHING LEFT
can(destroyRule(Phys1, Word2), [phys(Phys1), link(Phys1, Word1), word(Word2), 
                        rule(Phys1,Word2), pos(Word1, X1, Y1), pos(is, X, Yis), pos(Word2, X, Y2),
                        pos(Baba,Xb,Yb), rule(Baba,you)], baba):-
    succ(X1,X), num(Xb), Xb > X,
    succ(Y2,Yis),
    succ(Yis,Y1).


adds(destroyRule(Word1, Word2), [destroyRule(Word1, Word2), searchMoveOk()], _, baba):-
    Word1\=Word2.
adds(destroyRule(Word1, Word2), [destroyRule(Word1, Word2), searchMoveOk(), mutable(Word1)], _, baba):-
    Word1=Word2.

deletes(destroyRule(Word1, Word2), [rule(Word1,Word2)], baba).

%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% FREE %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%
can(free(Phys, Obstacle, You),[rule(You,you), prioritary(Obstacle), phys(Phys), phys(Obstacle)], baba). % l'ostacolo c'è ma è stato rimosso (distuggendo la regola)

adds(free(Phys, Obstacle, You),[free(Phys, Obstacle, You)|EmptyPos], _, baba):-
    prioritary(Obstacle),
    findall(empty(X,Y), pos(Obstacle,X,Y), EmptyPos).

deletes(free(Phys, Obstacle, You),[], baba).

%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% TRANSFORM %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%
can(transform(Phys1, Phys2,X,Y),[rule(Phys1,Phys2), phys(Phys1), phys(Phys2), pos(Phys1,X,Y), mutable(Phys1)], baba).

adds(transform(Phys1, Phys2,X,Y),[pos(Phys2,X,Y), transform(Phys1,Phys2)], _, baba).

deletes(transform(Phys1, Phys2,X,Y),[pos(Phys1,X,Y)], baba).

%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% UNBOND %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%
% we have only 2 cases, hot and melt
can(unbond(Phys1, Rule, Phys2),[rule(Phys1,Rule), word(Rule), phys(Phys1), phys(Phys2)], baba).

adds(unbond(Phys1, Rule, Phys2),[unbond(Phys1, Rule, Phys2)], _, baba).
deletes(unbond(Phys1, Rule, Phys2),[], baba).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% SOLVE LEVEL %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Non mi serve controllare qua che la regola sia presente, è il pianificatore di alto livello che ha già provveduto a farmi fare le mosse per raggiungere le condizioni sufficienti
can(won(YouPhys, WinPhys), [phys(YouPhys), phys(WinPhys), pos(YouPhys, X, Y), pos(WinPhys,X,Y)], baba).

adds(won(YouPhys, WinPhys), [won(YouPhys, WinPhys)], _, baba).

deletes(won(YouPhys, WinPhys), [], baba).


% ******************
% *** PRIORITY *****
% ******************
prioritize(won(Phys,_),Phys).
prioritize(rule(Phys,_),Word):-
    link(Phys,Word).
prioritize(destroyRule(Phys,_),Word):-
    link(Phys,Word).
prioritize(transform(Phys,_),Phys).
prioritize(push(Phys),Phys).
prioritize(free(_,Phys,_), Phys):-
    assert(prioritary(Prio)).
prioritize(pos(Phys,_,_),Phys).
prioritize(_,_).



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

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% SOLVER %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic prioritary/1.
:- dynamic pos/3.

solve_l2(Level,P,Goal,Actions) :-
    
    consult('levels/levels_l2.pl'),

    prioritize(Goal, Prio),
    level(Level,State,Prio),
    %write(Prio),nl,nl,

    flatten(Actions,FlatActions),
    previous_actions(FlatActions,State,NewState),

    %retractall(prioritary(_)),
    %retractall(pos(_,_,_)),

    %assert(prioritary(Prio)),
    save_prio_pos(NewState, Prio),

    % we must have a X-is-you rule
    %member(rule(You,you), NewState),
    %member(pos(You,X,Y), NewState),
    %write(pos(You,X,Y)),nl,nl,

    plan(NewState,
            [Goal], baba,
            %[pos(baba,6,4)], baba,
            P).