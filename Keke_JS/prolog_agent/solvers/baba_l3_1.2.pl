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
%succ(8,9).

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% MOVE &%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BABA 4 the win
can(pos(Baba, Xf, Yf, X, Y), [pos(Baba, X, Y), empty(Xf,Yf), rule(Baba,you), prioritary(Baba), target(Xf), target(Yf)], baba):-
adds(pos(Baba, Xf, Yf, X, Y), [pos(Baba, Xf, Yf), empty(X,Y)], _, baba).
deletes(pos(Baba, Xf, Yf, X, Y), [pos(Baba,X,Y), empty(Xf,Yf)], baba).

% pos Baba to Pivot obj
% UP
can(pos(Baba, Xf, Yf, X, Y), [pos(Baba, X, Y), empty(Xf,Yf), rule(Baba,you), pos(Pivot, Xf, Yp), prioritary(Pivot)], baba):-
    num(Yp), Yf is Yp+1.
adds(pos(Baba, Xf, Yf, X, Y), [pos(Baba, Xf, Yf), empty(X,Y)], _, baba).
deletes(pos(Baba, Xf, Yf, X, Y), [pos(Baba,X,Y), empty(Xf,Yf)], baba).

% DOWN
can(pos(Baba, Xf, Yf, X, Y), [pos(Baba, X, Y), empty(Xf,Yf), rule(Baba,you), pos(Pivot, Xf, Yp), prioritary(Pivot)], baba):-
    num(Yp), Yf is Yp-1.
adds(pos(Baba, Xf, Yf, X, Y), [pos(Baba, Xf, Yf), empty(X,Y)], _, baba).
deletes(pos(Baba, Xf, Yf, X, Y), [pos(Baba,X,Y), empty(Xf,Yf)], baba).

% RIGHT
can(pos(Baba, Xf, Yf, X, Y), [pos(Baba, X, Y), empty(Xf,Yf), rule(Baba,you), pos(Pivot, Xp, Yf), prioritary(Pivot)], baba):-
    num(Xp), Xf is Xp+1.
adds(pos(Baba, Xf, Yf, X, Y), [pos(Baba, Xf, Yf), empty(X,Y)], _, baba).
deletes(pos(Baba, Xf, Yf, X, Y), [pos(Baba,X,Y), empty(Xf,Yf)], baba).

% LEFT
can(pos(Baba, Xf, Yf, X, Y), [pos(Baba, X, Y), empty(Xf,Yf), rule(Baba,you), pos(Pivot, Xp, Yf), prioritary(Pivot)], baba):-
    num(Xp), Xf is Xp-1.
adds(pos(Baba, Xf, Yf, X, Y), [pos(Baba, Xf, Yf), empty(X,Y)], _, baba).
deletes(pos(Baba, Xf, Yf, X, Y), [pos(Baba,X,Y), empty(Xf,Yf)], baba).

% pos Pivot moving
% PUSH RIGHT
can(pos(Pivot, Xfin, Yfin, Xpivot, Yfin), [pos(Baba, Xbaba, Yfin), empty(Xfin,Yfin), rule(Baba,you), pos(Pivot, Xpivot, Yfin), prioritary(Pivot), target(Xfin)], baba):-
    num(Xfin),
    num(Xpivot),
    num(Xbaba),
    Xpivot is Xbaba+1, Xfin > Xpivot.
adds(pos(Pivot, Xfin, Yfin, Xpivot, Yfin), [pos(Pivot, Xfin, Yfin), empty(Xpivot, Yfin)], _, baba).
deletes(pos(Pivot, Xfin, Yfin, Xpivot, Yfin), [pos(Pivot,Xpivot,Yfin), empty(Xfin,Yfin)], baba).

% PUSH LEFT
can(pos(Pivot, Xfin, Yfin, Xpivot, Yfin), [pos(Baba, Xbaba, Yfin), empty(Xfin,Yfin), rule(Baba,you), pos(Pivot, Xpivot, Yfin), prioritary(Pivot), target(Xfin)], baba):-
    num(Xfin),
    num(Xpivot),
    num(Xbaba),
    Xpivot is Xbaba-1, Xfin < Xpivot.
adds(pos(Pivot, Xfin, Yfin, Xpivot, Yfin), [pos(Pivot, Xfin, Yfin), empty(Xpivot, Yfin)], _, baba).
deletes(pos(Pivot, Xfin, Yfin, Xpivot, Yfin), [pos(Pivot,Xpivot,Yfin), empty(Xfin,Yfin)], baba).

% PUSH UP
can(pos(Pivot, Xfin, Yfin, Xfin, Ypivot), [pos(Baba, Xfin, Ybaba), empty(Xfin,Yfin), rule(Baba,you), pos(Pivot, Xfin, Ypivot), prioritary(Pivot), target(Yfin)], baba):-
    num(Yfin),
    num(Ypivot),
    num(Ybaba),
    Ypivot is Ybaba+1, Yfin > Ypivot.
adds(pos(Pivot, Xfin, Yfin, Xfin, Ypivot), [pos(Pivot, Xfin, Yfin), empty(Xfin,Ypivot)], _, baba).
deletes(pos(Pivot, Xfin, Yfin, Xfin, Ypivot), [pos(Pivot,Xfin,Ypivot), empty(Xfin,Yfin)], baba).

% PUSH DOWN
can(pos(Pivot, Xfin, Yfin, Xfin, Ypivot), [pos(Baba, Xfin, Ybaba), empty(Xfin,Yfin), rule(Baba,you), pos(Pivot, Xfin, Ypivot), prioritary(Pivot), target(Yfin)], baba):-
    num(Yfin),
    num(Ypivot),
    num(Ybaba),
    Ypivot is Ybaba-1, Yfin < Ypivot.
adds(pos(Pivot, Xfin, Yfin, Xfin, Ypivot), [pos(Pivot, Xfin, Yfin), empty(Xfin,Ypivot)], _, baba).
deletes(pos(Pivot, Xfin, Yfin, Xfin, Ypivot), [pos(Pivot,Xfin,Ypivot), empty(Xfin,Yfin)], baba).

% pos Pivot moving - REPLACE FOR LEVEL 14
% PUSH RIGHT
can(pos(Pivot, Xfin, Yfin, Xpivot, Yfin), [pos(Baba, Xbaba, Yfin), pos(Word,Xfin,Yfin), empty(Xf1,Yfin), rule(Baba,you), pos(Pivot, Xpivot, Yfin), prioritary(Pivot), target(Xfin)], baba):-
    num(Xfin),
    num(Xpivot),
    num(Xbaba),
    Xf1 is Xfin+1,
    Xpivot is Xbaba+1, Xfin > Xpivot.
adds(pos(Pivot, Xfin, Yfin, Xpivot, Yfin), [pos(Pivot, Xfin, Yfin), empty(Xpivot, Yfin)], _, baba).
deletes(pos(Pivot, Xfin, Yfin, Xpivot, Yfin), [pos(Pivot,Xpivot,Yfin), empty(Xfin,Yfin)], baba).

% PUSH LEFT
can(pos(Pivot, Xfin, Yfin, Xpivot, Yfin), [pos(Baba, Xbaba, Yfin), pos(Word,Xfin,Yfin), empty(Xf1,Yfin), rule(Baba,you), pos(Pivot, Xpivot, Yfin), prioritary(Pivot), target(Xfin)], baba):-
    num(Xfin),
    num(Xpivot),
    num(Xbaba),
    Xf1 is Xfin-1,
    Xpivot is Xbaba-1, Xfin < Xpivot.
adds(pos(Pivot, Xfin, Yfin, Xpivot, Yfin), [pos(Pivot, Xfin, Yfin), empty(Xpivot, Yfin)], _, baba).
deletes(pos(Pivot, Xfin, Yfin, Xpivot, Yfin), [pos(Pivot,Xpivot,Yfin), empty(Xfin,Yfin)], baba).

% PUSH UP
can(pos(Pivot, Xfin, Yfin, Xfin, Ypivot), [pos(Baba, Xfin, Ybaba), pos(Word,Xfin,Yfin), empty(Xfin,Yf1), rule(Baba,you), pos(Pivot, Xfin, Ypivot), prioritary(Pivot), target(Yfin)], baba):-
    num(Yfin),
    num(Ypivot),
    num(Ybaba),
    Yf1 is Yfin+1,
    Ypivot is Ybaba+1, Yfin > Ypivot.
adds(pos(Pivot, Xfin, Yfin, Xfin, Ypivot), [pos(Pivot, Xfin, Yfin), empty(Xfin,Ypivot)], _, baba).
deletes(pos(Pivot, Xfin, Yfin, Xfin, Ypivot), [pos(Pivot,Xfin,Ypivot), empty(Xfin,Yfin)], baba).

% PUSH DOWN
can(pos(Pivot, Xfin, Yfin, Xfin, Ypivot), [pos(Baba, Xfin, Ybaba), pos(Word,Xfin,Yfin), empty(Xfin,Yf1), rule(Baba,you), pos(Pivot, Xfin, Ypivot), prioritary(Pivot), target(Yfin)], baba):-
    num(Yfin),
    num(Ypivot),
    num(Ybaba),
    Yf1 is Yfin-1,
    Ypivot is Ybaba-1, Yfin < Ypivot.
adds(pos(Pivot, Xfin, Yfin, Xfin, Ypivot), [pos(Pivot, Xfin, Yfin), empty(Xfin,Ypivot)], _, baba).
deletes(pos(Pivot, Xfin, Yfin, Xfin, Ypivot), [pos(Pivot,Xfin,Ypivot), empty(Xfin,Yfin)], baba).

% *********************
% *** RULES ***********
% *********************


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% SOLVE LEVEL %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Non mi serve controllare qua che la regola sia presente, è il pianificatore di alto livello che ha già provveduto a farmi fare le mosse per raggiungere le condizioni sufficienti
can(won(YouPhys, WinPhys), [phys(YouPhys), phys(WinPhys), pos(YouPhys, X, Y), pos(WinPhys,X,Y)], baba).

adds(won(YouPhys, WinPhys), [won(YouPhys, WinPhys)], _, baba).

deletes(won(YouPhys, WinPhys), [], baba).


% *********************
% *** RULES ***********
% *********************
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
    %write(Obstacle),nl,nl,
    %write(EmptyPos),nl,nl.

deletes(free(Phys, Obstacle, You),[], baba).

%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% UNBOND %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%
% we have only 2 cases, hot and melt
can(unbond(Phys1, Rule, Phys2),[rule(Phys1,Rule), word(Rule), phys(Phys1), phys(Phys2)], baba).

adds(unbond(Phys1, Rule, Phys2),[unbond(Phys1, Rule, Phys2)], _, baba).
deletes(unbond(Phys1, Rule, Phys2),[], baba).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% STATE %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
previous_actions([], NewState,NewState).
previous_actions([H|Actions],State,NewState):-
    adds(H, AddConditions, _, Domain),
    deletes(H, DelPreConditions, Domain),
    %write(H),nl,nl,
    %write(AddConditions),nl,nl,

    update_state(AddConditions, State, AddedState),
    subtract(AddedState, DelPreConditions, CurrentState),

    previous_actions(Actions, CurrentState, NewState).

update_state([], CurrentState, CurrentState).
update_state([H|AddConditions], State, CurrentState):-
    update_state(AddConditions, [H|State], CurrentState).

%prioritize(rule(Phys,_),Word):-
%   link(Phys,Word).
prioritize(pos(Obj,X,Y),Obj, X, Y).
prioritize(free(_,Phys,_), Phys, X,Y):-
    assert(prioritary(Phys)).
prioritize(won(Phys,_),Phys,X,Y).
prioritize(_,_,_,_).

refactor([pos(Obj,X,Y,_,_)|T], Acc, NewPlan):-
    refactor(T, [pos(Obj,X,Y)|Acc], NewPlan).
refactor([], Plan, NewPlan):-
    reverse(Plan,NewPlan).
refactor([H|T],Acc,NewPlan):-
    refactor(T, [H|Acc], NewPlan).


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

solve_l3(Level,P,Goal,PrevActions, Plan) :-
    
    consult('levels/levels_l3.pl'),

    prioritize(Goal, Prio, X, Y),
    level(Level,State,Prio,X,Y),
    %write(Prio),nl,nl,

    flatten(PrevActions,FlatActions),
    previous_actions(FlatActions,State,NewState),
    %write(NewState),nl,nl,

    %retractall(prioritary(_)),
    %retractall(pos(_,_,_)),

    %assert(prioritary(Prio)),
    save_prio_pos(NewState, Prio),

    plan(NewState,
            [Goal], baba,
            %[pos(baba,6,4)], baba,
            Plan),

    flatten(Plan, FlatPlan),
    refactor(FlatPlan,[],P).



    