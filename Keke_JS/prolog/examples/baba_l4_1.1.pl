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

setLevel(Level,State,State).

% *** PHYSICAL OBJECTS ***
phys(baba).
phys(flag).
phys(grass).
phys(love).
pyhs(rock).

% *** WORDS OBJECTS ***
word(babaWord).
word(flagWord).
word(grassWord).
word(loveWord).
word(rockWord).
word(is).
word(you).
word(win).

% *** LINKS ***
link(baba, babaWord).
link(flag, flagWord).
link(love, loveWord).
link(grass, grassWord).
link(rock, rockWord).

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
    listR([link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y)],Result,X1,Y,N).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEFT %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MOVE
can(moveLeft(YouPhys, X, Y, N), Result, baba):-
    num(X),
    X1 is X-1,
    num(Y),
    step(N),
    listL([link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y)],Result,X1,Y,N).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% UP %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MOVE
can(moveUp(YouPhys, X, Y, N), Result, baba):-
    num(X),
    num(Y),
    Y1 is Y+1,
    step(N),
    listU([link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y)],Result,X,Y1,N).
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
    listU([link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y), pos(Word,X,Y1), word(Word)], Result, X, Y2, N).

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
%%%%%%%%%% UP %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MOVE
can(moveDown(YouPhys, X, Y, N), Result, baba):-
    num(X),
    num(Y),
    Y1 is Y-1,
    step(N),
    listD([link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y)],Result,X,Y1,N).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% SOLVE LEVEL %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% *****************************************
% *** solveLevelR(You, Pos) *****************
% *****************************************
can(solveLevelMoveRight(YouPhys, WinPhys, Xwin, Ywin), [link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y),
            rule(WinPhys,win), pos(WinPhys, Xwin, Ywin)], baba):-
    Y=Ywin,
    succ(X,Xwin).

adds(solveLevelMoveRight(YouPhys, WinPhys, Xwin, Ywin), [won(YouPhys, WinPhys)], _, baba).

deletes(solveLevelMoveRight(YouPhys, WinPhys, Xwin, Ywin), [], baba).

% *****************************************
% *** solveLevelL(You, Pos) *****************
% *****************************************
can(solveLevelMoveLeft(YouPhys, WinPhys, Xwin, Ywin), [link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y),
            rule(WinPhys,win), pos(WinPhys, Xwin, Ywin)], baba):-
    Y=Ywin,
    succ(Xwin,X).

adds(solveLevelMoveLeft(YouPhys, WinPhys, Xwin, Ywin), [won(YouPhys, WinPhys)], _, baba).

deletes(solveLevelMoveLeft(YouPhys, WinPhys, Xwin, Ywin), [], baba).

% *****************************************
% *** solveLevelD(You, Pos) *****************
% *****************************************
can(solveLevelMoveDown(YouPhys, WinPhys, Xwin, Ywin), [link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y),
            rule(WinPhys,win), pos(WinPhys, Xwin, Ywin)], baba):-
    X=Xwin,
    succ(Ywin,Y).

adds(solveLevelMoveDown(YouPhys, WinPhys, Xwin, Ywin), [won(YouPhys, WinPhys)], _, baba).

deletes(solveLevelMoveDown(YouPhys, WinPhys, Xwin, Ywin), [], baba).

% *****************************************
% *** solveLevelU(You, Pos) *****************
% *****************************************
can(solveLevelMoveUp(YouPhys, WinPhys, Xwin, Ywin), [link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y),
            rule(WinPhys,win), pos(WinPhys, Xwin, Ywin)], baba):-
    X=Xwin,
    succ(Y,Ywin).

adds(solveLevelMoveUp(YouPhys, WinPhys, Xwin, Ywin), [won(YouPhys, WinPhys)], _, baba).

deletes(solveLevelMoveUp(YouPhys, WinPhys, Xwin, Ywin), [], baba).

% *********************
% *** RULES ***********
% *********************
% HORIZONTAL RULES
can(createRule(Phys1, Word2), [phys(Phys1), link(Phys1, Word1), pos(Word1, X1, Y), pos(is, Xis, Y), pos(Word2, X2, Y), word(Word1), word(Word2)], baba):-
    succ(X1,Xis),
    succ(Xis,X2).

% VERTICAL RULES
can(createRule(Phys1, Word2), [phys(Phys1), link(Phys1, Word1), pos(Word1, X, Y1), pos(is, X, Yis), pos(Word2, X, Y2), word(Word1), word(Word2)], baba):-
    succ(Y1,Yis),
    succ(Yis,Y2).

% HORIZONTAL RULES WITH OBJECTS
can(createRule(Phys1, Phys2), [phys(Phys1), link(Phys1, Word1), pos(Word1, X1, Y), pos(is, Xis, Y), phys(Phys2), link(Phys2, Word2),  pos(Word2, X2, Y),  word(Word1), word(Word2)], baba):-
    succ(X1,Xis),
    succ(Xis,X2).

% VERTICAL RULES  WITH OBJECTS
can(createRule(Phys1, Phys2), [phys(Phys1), link(Phys1, Word1), pos(Word1, X, Y1), pos(is, X, Yis), phys(Phys2), link(Phys2, Word2),  pos(Word2, X, Y2),  word(Word1), word(Word2)], baba):-
    succ(Y1,Yis),
    succ(Yis,Y2).

adds(createRule(Word1, Word2), [rule(Word1, Word2), searchMoveOk()], _, baba).

deletes(createRule(Word1, Word2), [], baba).

% *********************
% *** TRANSFORM *******
% *********************
can(transform(Phys1, Phys2),[rule(Phys1,Phys2), phys(Phys1), phys(Phys2), link(Phys1,Word1), mutable(Phys1)], baba).

adds(transform(Phys1, Phys2),[phys(Phys2)], _, baba):-
	word(Phys1),
    word(Phys2),
    phys(Phys1).

deletes(transform(Phys1, Phys2),[phys(Phys1)], baba):-
	word(Phys1),
    word(Phys2),
    phys(Phys1).

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

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 01 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
level(lvl_01,State):-
    setLevel(lvl_01, 
        [
        word(flagWord), word(is), word(win), word(babaWord), word(you), link(baba,babaWord), link(flag,flagWord), phys(flag), phys(baba), rule(babaWord,you), rule(flag,win),

        pos(babaWord,1,8),  pos(is,2,8),    pos(you,3,8),   empty(4,8), empty(5,8), pos(flagWord,6,8),  pos(is,7,8),    pos(win,8,8),
        empty(1,7),         empty(2,7),     empty(3,7),     empty(4,7), empty(5,7), empty(6,7),         empty(7,7),     empty(8,7),
        empty(1,6),         empty(2,6),     empty(3,6),     empty(4,6), empty(5,6), empty(6,6),         empty(7,6),     empty(8,6),  
        empty(1,5),         empty(2,5),     empty(3,5),     empty(4,5), empty(5,5), empty(6,5),         empty(7,5),     empty(8,5),
        empty(1,4),         pos(baba,2,4),  empty(3,4),     empty(4,4), empty(5,4), empty(6,4),         pos(flag,7,4),  empty(8,4),  
        empty(1,3),         empty(2,3),     empty(3,3),     empty(4,3), empty(5,3), empty(6,3),         empty(7,3),     empty(8,3),
        empty(1,2),         empty(2,2),     empty(3,2),     empty(4,2), empty(5,2), empty(6,2),         empty(7,2),     empty(8,2),
        empty(1,1),         empty(2,1),     empty(3,1),     empty(4,1), empty(5,1), empty(6,1),         empty(7,1),     empty(8,1)
        ], State).

solve_l4(lvl_01,P,Goal,Actions) :-
    
    level(lvl_01,State),

    flatten(Actions,FlatActions),
    previous_actions(FlatActions,State,NewState),

    plan(NewState,
            [Goal], baba,
            %[pos(baba,6,4)], baba,
            P).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 02 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
level(lvl_02,State):-
    setLevel(lvl_02, 
        [word(flagWord), word(is), word(win), word(babaWord), word(you), link(baba,babaWord), link(flag,flagWord), phys(flag), phys(baba), rule(babaWord,you),
    
        pos(babaWord,1,8),  pos(is,2,8),    pos(you,3,8),   empty(4,8), empty(5,8), empty(6,8),         pos(is,7,8),    pos(win,8,8),
        empty(1,7),         empty(2,7),     empty(3,7),     empty(4,7), empty(5,7), empty(6,7),         empty(7,7),     empty(8,7),
        empty(1,6),         empty(2,6),     empty(3,6),     empty(4,6), empty(5,6), empty(6,6),         empty(7,6),     empty(8,6),  
        empty(1,5),         empty(2,5),     empty(3,5),     empty(4,5), empty(5,5), pos(flagWord,6,5),  empty(7,5),     empty(8,5),
        empty(1,4),         pos(baba,2,4),  empty(3,4),     empty(4,4), empty(5,4), empty(6,4),         pos(flag,7,4),  empty(8,4),  
        empty(1,3),         empty(2,3),     empty(3,3),     empty(4,3), empty(5,3), empty(6,3),         empty(7,3),     empty(8,3),
        empty(1,2),         empty(2,2),     empty(3,2),     empty(4,2), empty(5,2), empty(6,2),         empty(7,2),     empty(8,2),
        empty(1,1),         empty(2,1),     empty(3,1),     empty(4,1), empty(5,1), empty(6,1),         empty(7,1),     empty(8,1)
        ], State).

solve_l4(lvl_02,P,Goal,Actions) :-
    level(lvl_02,State),

    flatten(Actions,FlatActions),
    previous_actions(FlatActions,State,NewState),
    
    plan(NewState,
            [Goal], baba,
            P).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 03 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
level(lvl_03,State):-
    setLevel(lvl_03, 
        [word(is), word(win), word(babaWord), word(you), link(baba,babaWord), phys(baba), rule(babaWord,you),
    
        empty(1,8),         empty(2,8),     empty(3,8),     empty(4,8),         empty(5,8),     empty(6,8),         empty(7,8),     empty(8,8),
        empty(1,7),         empty(2,7),     empty(3,7),     pos(babaWord,4,7),  pos(is,5,7),    pos(you,6,7),       empty(7,7),     empty(8,7),
        empty(1,6),         empty(2,6),     empty(3,6),     empty(4,6),         empty(5,6),     empty(6,6),         empty(7,6),     empty(8,6),  
        empty(1,5),         pos(is,2,5),    empty(3,5),     empty(4,5),         empty(5,5),     empty(6,5),         empty(7,5),     empty(8,5),
        empty(1,4),         pos(win,2,4),   empty(3,4),     empty(4,4),         pos(baba,5,4),  empty(6,4),         empty(7,4),     empty(8,4),  
        empty(1,3),         empty(2,3),     empty(3,3),     empty(4,3),         empty(5,3),     empty(6,3),         empty(7,3),     empty(8,3),
        empty(1,2),         empty(2,2),     empty(3,2),     empty(4,2),         empty(5,2),     empty(6,2),         empty(7,2),     empty(8,2),
        empty(1,1),         empty(2,1),     empty(3,1),     empty(4,1),         empty(5,1),     empty(6,1),         empty(7,1),     empty(8,1)
        ], State).

solve_l4(lvl_03,P,Goal,Actions) :-
    level(lvl_03,State),

    flatten(Actions,FlatActions),
    previous_actions(FlatActions,State,NewState),
    
    plan(NewState,
            [Goal], baba,
            P).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 04 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
level(lvl_04,State):-
    setLevel(lvl_04, 
        [
        word(flagWord), word(babaWord), word(grassWord), word(loveWord), word(is), word(win), word(you),
        link(baba,babaWord), link(flag,flagWord), link(love, loveWord), link(grass, grassWord),
        phys(flag), phys(baba), phys(love), phys(grass),
        rule(flagWord,you), rule(baba,win), rule(loveWord, you), rule(grass,win),

        pos(babaWord,1,8),  pos(is,2,8),    pos(win,3,8),   empty(4,8), empty(5,8), pos(flagWord,6,8),  pos(is,7,8),    pos(you,8,8),
        empty(1,7),         empty(2,7),     empty(3,7),     empty(4,7), empty(5,7), empty(6,7),         empty(7,7),     empty(8,7),
        empty(1,6),         pos(baba,2,6),  empty(3,6),     empty(4,6), empty(5,6), empty(6,6),         pos(flag,7,6),  empty(8,6),  
        empty(1,5),         empty(2,5),     empty(3,5),     empty(4,5), empty(5,5), empty(6,5),         empty(7,5),     empty(8,5),
        empty(1,4),         empty(2,4),     empty(3,4),     empty(4,4), empty(5,4), empty(6,4),         empty(7,4),     empty(8,4),  
        empty(1,3),         pos(grass,2,3), empty(3,3),     empty(4,3), empty(5,3), empty(6,3),         pos(love,7,3),  empty(8,3),
        empty(1,2),         empty(2,2),     empty(3,2),     empty(4,2), empty(5,2), empty(6,2),         empty(7,2),     empty(8,2),
        pos(grassWord,1,1), pos(is,2,1),    pos(win,3,1),   empty(4,1), empty(5,1), pos(loveWord,6,1),  pos(is,7,1),    pos(you,8,1)
        ], State).

solve_l4(lvl_04,P,Goal,Actions) :-
    
    level(lvl_04,State),

    flatten(Actions,FlatActions),
    previous_actions(FlatActions,State,NewState),

    plan(NewState,
            [Goal], baba,
            %[pos(baba,6,4)], baba,
            P).



%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 05 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
level(lvl_05,State):-
    setLevel(lvl_05, 
        [
        word(flagWord), word(is), word(win), word(babaWord), word(you), word(rockWord), link(baba,babaWord), link(flag,flagWord), link(rock,rockWord),
        phys(rock), phys(baba), phys(flag), rule(babaWord,you), rule(flag,win), 

        pos(babaWord,1,8),  pos(is,2,8),    pos(you,3,8),   empty(4,8),         empty(5,8), pos(flagWord,6,8),  pos(is,7,8),    pos(win,8,8),
        empty(1,7),         empty(2,7),     empty(3,7),     empty(4,7),         empty(5,7), empty(6,7),         empty(7,7),     empty(8,7),
        empty(1,6),         empty(2,6),     empty(3,6),     empty(4,6),         empty(5,6), empty(6,6),         empty(7,6),     empty(8,6),  
        empty(1,5),         pos(baba,2,5),  empty(3,5),     empty(4,5),         empty(5,5), empty(6,5),         pos(rock,7,5),  empty(8,5),
        empty(1,4),         empty(2,4),     empty(3,4),     empty(4,4),         empty(5,4), empty(6,4),         empty(7,4),     empty(8,4),  
        empty(1,3),         empty(2,3),     empty(3,3),     pos(rockWord,4,3),  empty(5,3), empty(6,3),         empty(7,3),     empty(8,3),
        empty(1,2),         empty(2,2),     empty(3,2),     empty(4,2),         empty(5,2), empty(6,2),         empty(7,2),     empty(8,2),
        empty(1,1),         empty(2,1),     empty(3,1),     empty(4,1),         empty(5,1), empty(6,1),         pos(is,7,1),    pos(flagWord,8,1)
        ], State).

solve_l4(lvl_05,P,Goal,Actions) :-
    
    level(lvl_05,State),

    flatten(Actions,FlatActions),
    previous_actions(FlatActions,State,NewState),

    plan(NewState,
            [Goal], baba,
            %[pos(baba,6,4)], baba,
            P).