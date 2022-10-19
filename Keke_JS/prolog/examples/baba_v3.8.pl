%    First levels of Baba is You Thesis Competition 

% ******* LOW LEVEL PLANNER ********
% stupid but useful
% PUSH BETTER FUNCTIONING - AND ALL DIRECTIONS

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

% *** WORDS OBJECTS ***
word(babaWord).
word(flagWord).
word(is).
word(you).
word(win).

% *** LINKS ***
link(baba, babaWord).
link(flag, flagWord).

% *** UTILS ***
succ(1,2).
succ(2,3).
succ(3,4).
succ(4,5).
succ(5,6).
succ(6,7).
succ(7,8).
succ(8,9).


% GAME STATE
state(Level,State) :-     
    levelState(Level,[], State,0),
    nb_setval(state, State).

levelState(lvl_01,State,State,1) :- !.
levelState(lvl_01,Empty,State,0) :-
    levelState(lvl_01,
        [word(flagWord), word(is), word(win), word(babaWord), word(you), link(baba,babaWord), link(flag,flagWord), phys(flag), phys(baba), rule(babaWord,you),
        pos(babaWord,1,8),  pos(is,2,8),    pos(you,3,8),   empty(4,8), empty(5,8), pos(flagWord,6,8),  pos(is,7,8),    pos(win,8,8),
        empty(1,7),         empty(2,7),     empty(3,7),     empty(4,7), empty(5,7), empty(6,7),         empty(7,7),     empty(8,7),
        empty(1,6),         empty(2,6),     empty(3,6),     empty(4,6), empty(5,6), empty(6,6),         empty(7,6),     empty(8,6),  
        empty(1,5),         empty(2,5),     empty(3,5),     empty(4,5), empty(5,5), empty(6,5),         empty(7,5),     empty(8,5),
        empty(1,4),         pos(baba,2,4),  empty(3,4),     empty(4,4), empty(5,4), empty(6,4),         pos(flag,7,4),  empty(8,4),  
        empty(1,3),         empty(2,3),     empty(3,3),     empty(4,3), empty(5,3), empty(6,3),         empty(7,3),     empty(8,3),
        empty(1,2),         empty(2,2),     empty(3,2),     empty(4,2), empty(5,2), empty(6,2),         empty(7,2),     empty(8,2),
        empty(1,1),         empty(2,1),     empty(3,1),     empty(4,1), empty(5,1), empty(6,1),         empty(7,1),     empty(8,1)
        |Empty], State,1).

levelState(2,State,State,1) :- !.
levelState(2,Empty,State,0) :-
    levelState(2,
        [word(flagWord), word(is), word(win), word(babaWord), word(you), link(baba,babaWord), link(flag,flagWord), phys(flag), phys(baba), rule(babaWord,you),
        pos(babaWord,1,8),  pos(is,2,8),    pos(you,3,8),   empty(4,8), empty(5,8), empty(6,8),         pos(is,7,8),    pos(win,8,8),
        empty(1,7),         empty(2,7),     empty(3,7),     empty(4,7), empty(5,7), empty(6,7),         empty(7,7),     empty(8,7),
        empty(1,6),         empty(2,6),     empty(3,6),     empty(4,6), empty(5,6), empty(6,6),         empty(7,6),     empty(8,6),  
        empty(1,5),         empty(2,5),     empty(3,5),     empty(4,5), empty(5,5), pos(flagWord,6,5),  empty(7,5),     empty(8,5),
        empty(1,4),         pos(baba,2,4),  pos(flagWord,3,4),     empty(4,4), empty(5,4), empty(6,4),         pos(flagWord,7,4),  empty(8,4),  
        empty(1,3),         empty(2,3),     empty(3,3),     empty(4,3), empty(5,3), empty(6,3),         empty(7,3),     empty(8,3),
        empty(1,2),         empty(2,2),     empty(3,2),     empty(4,2), empty(5,2), empty(6,2),         empty(7,2),     empty(8,2),
        empty(1,1),         empty(2,1),     empty(3,1),     empty(4,1), empty(5,1), empty(6,1),         empty(7,1),     empty(8,1)
        |Empty], State,1).

% obstacle(wall).

% newMove
reachableR(State, X, Y, PossibleX) :-
    findall(X1, ( member(empty(X1,Y), State), X1 > X ), PossibleX).

preconditions(State,Result,Xf) :-
    member(link(YouPhys, YouWord),State),
    member(rule(YouWord,you),State),
    member(pos(YouPhys, X,Y),State),
    reachableR(State,X,Y,PossibleX),
    min_list(PossibleX, Xf),
    findList(X,Xf,Y,[],Result).

findList(Xi,Xf,Y,Result,Result) :- 
    X is Xi+1,
    X = Xf,
    !.
findList(Xi,Xf,Y,Acc,Result) :- 
    X is Xi+1,
    X < Xf,
    findList(X,Xf,Y,[pos(Word,X,Y), word(Word)|Acc], Result).

% adds
addList(State, [], AddList, AddList) :- !.
addList(State, [pos(Word,X,Y),word(Word)|T],Acc,AddList) :-
    member(word(Word), State),
    member(pos(Word,X,Y), State),
    X1 is X+1,
    addList(State,T,[pos(Word,X1,Y)|Acc], AddList).

% deletes
delList(State, [], DelList, DelList) :- !.
delList(State, [pos(Word,X,Y),word(Word)|T],Acc,DelList) :-
    member(word(Word), State),
    member(pos(Word,X,Y), State),
    delList(State,T,[pos(Word,X,Y)|Acc], DelList).

updateState(State,[]).
updateState(State,[H|T]) :-
    b_setval(state,[H|State]),
    updateState([H|State], T).

% deletes the element in the List only once
subtract_once(Remainder, [], Remainder).
subtract_once(List, [Current|Delete], X) :-
    select(Current, List, Rest),
    subtract_once(Rest, Delete, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% ACTONS %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% moveRight()
can(moveRight(YouPhys, X, Y, Xf), [link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X,Y), empty(Xf,Y)|Result], baba):-
    %succ(X,X1),
    level(Level),
    nb_getval(state,State),
    preconditions(State,Result,Xf).

% forse bisogna mettere Xf qua, così nelle add lo lego a findList (e gli aggiungo 1 per avere le posizioni successive aggiornate con gli oggetti spostati)
adds(moveRight(YouPhys, X, Y, Xf), [empty(X,Y),pos(YouPhys,X1,Y)|AddList], _, baba):-
    succ(X,X1),
    nb_getval(state, State),
    preconditions(State,Result,Xf),
    addList(State, Result, [empty(X,Y),pos(YouPhys,X1,Y)], AddList),
    updateState(State, AddList),

    %deletes from internal state - need this to avoid double terms during deletes
    nb_getval(state, FinalState),
    delList(State, Result, [empty(Xf,Y),pos(YouPhys,X,Y)], DelList),
    subtract_once(FinalState, DelList, NewState),
    nb_setval(state,NewState),
    nb_setval(deleted,DelList).

deletes(moveRight(YouPhys, X, Y, Xf), [empty(Xf,Y),pos(YouPhys,X,Y)|DelList], baba):-
    nb_getval(deleted,DelList),
    nb_getval(state, State),
    sort(State,OrderedState),
    %write(OrderedState),nl,nl,
    nb_setval(state,OrderedState).


% *****************************************
% *** createRule(Word1, Word2) ************
% *****************************************
can(createRule(Word1, Word2), [pos(Word1, X1, Y1), pos(is, Xis, Yis), pos(Word2, X2, Y2)], baba):-
    succ(X1,Xis),
    succ(Xis,X2),
    Y2 = Yis,
    Yis = Y1;
    succ(Y2,Yis),
    succ(Yis,Y1),
    X2 = Xis,
    Xis = X1.

adds(createRule(Word1, Word2), [rule(Word1, Word2)], _, baba):-
    word(Word1),
    word(Word2).

deletes(createRule(Word1, Word2), [], baba):-
    word(Word1),
    word(Word2).

% *****************************************
% *** solveLevelR(You, Pos) *****************
% *****************************************
can(solveLevelMoveRight(YouPhys, WinPhys, Xwin, Ywin), [link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y),
            link(WinPhys, WinWord), rule(WinWord,win), pos(WinPhys, Xwin, Ywin)], baba):-
    Y=Ywin,
    succ(X,Xwin).

adds(solveLevelMoveRight(YouPhys, WinPhys, Xwin, Ywin), [won(YouPhys, WinPhys)], _, baba).

deletes(solveLevelMoveRight(YouPhys, WinPhys, Xwin, Ywin), [], baba).

% *****************************************
% *** solveLevelL(You, Pos) *****************
% *****************************************
can(solveLevelMoveLeft(YouPhys, WinPhys, Xwin, Ywin), [link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y),
            link(WinPhys, WinWord), rule(WinWord,win), pos(WinPhys, Xwin, Ywin)], baba):-
    Y=Ywin,
    succ(Xwin,X).

adds(solveLevelMoveLeft(YouPhys, WinPhys, Xwin, Ywin), [won(YouPhys, WinPhys)], _, baba).

deletes(solveLevelMoveLeft(YouPhys, WinPhys, Xwin, Ywin), [], baba).

% *****************************************
% *** solveLevelD(You, Pos) *****************
% *****************************************
can(solveLevelMoveDown(YouPhys, WinPhys, Xwin, Ywin), [link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y),
            link(WinPhys, WinWord), rule(WinWord,win), pos(WinPhys, Xwin, Ywin)], baba):-
    X=Xwin,
    succ(Ywin,Y).

adds(solveLevelMoveDown(YouPhys, WinPhys, Xwin, Ywin), [won(YouPhys, WinPhys)], _, baba).

deletes(solveLevelMoveDown(YouPhys, WinPhys, Xwin, Ywin), [], baba).

% *****************************************
% *** solveLevelU(You, Pos) *****************
% *****************************************
can(solveLevelMoveUp(YouPhys, WinPhys, Xwin, Ywin), [link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y),
            link(WinPhys, WinWord), rule(WinWord,win), pos(WinPhys, Xwin, Ywin)], baba):-
    X=Xwin,
    succ(Y,Ywin).

adds(solveLevelMoveUp(YouPhys, WinPhys, Xwin, Ywin), [won(YouPhys, WinPhys)], _, baba).

deletes(solveLevelMoveUp(YouPhys, WinPhys, Xwin, Ywin), [], baba).

% ***************
% *** TESTS *****
% ***************

test(P,Goal) :-
    plan([%word(flagWord), word(is), word(win), word(babaWord), word(you), link(baba,babaWord), link(flag,flagWord), phys(flag), phys(baba),
    
        pos(babaWord,1,8),  pos(is,2,8),    pos(you,3,8),   empty(4,8),     empty(5,8),     pos(flagWord,6,8),  pos(is,7,8),    pos(win,8,8),
        empty(1,7),         empty(2,7),     empty(3,7),     empty(4,7),     empty(5,7),     empty(6,7),         empty(7,7),     empty(8,7),
        empty(1,6),         empty(2,6),     empty(3,6),     empty(4,6),     empty(5,6),     empty(6,6),         empty(7,6),     empty(8,6),  
        empty(1,5),         obstacle(2,5),  obstacle(3,5),  obstacle(4,5),  obstacle(5,5),  empty(6,5),         empty(7,5),     empty(8,5),
        obstacle(1,4),      pos(baba,2,4),  empty(3,4),     empty(4,4),     empty(5,4),     empty(6,4),         pos(flag,7,4),  empty(8,4),  
        empty(1,3),         obstacle(2,3),  obstacle(3,3),  obstacle(4,3),  obstacle(5,3),  empty(6,3),         empty(7,3),     empty(8,3),
        empty(1,2),         empty(2,2),     empty(3,2),     empty(4,2),     empty(5,2),     empty(6,2),         empty(7,2),     empty(8,2),
        empty(1,1),         empty(2,1),     empty(3,1),     empty(4,1),     empty(5,1),     empty(6,1),         empty(7,1),     empty(8,1)
        ],
            [Goal], baba,
            %[pos(baba,6,4)], baba,
            P).

solve(lvl_01,P,Goal) :-
    assert(level(lvl_01)),
    state(lvl_01,State),
    plan(State,
            [Goal], baba,
            %[won(baba,flag)], baba,
            %[pos(baba,3,4)], baba,
            P).

lvl(2,P) :-
    assert(level(2)),
    state(2,State),
    plan(State,
            %[rule(babaWord,you)], baba, % aborted after 1 hour
            [pos(baba,6,4)], baba,
            P).

                

