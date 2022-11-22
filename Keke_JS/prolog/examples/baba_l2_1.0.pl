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
succ(1,2).
succ(2,3).
succ(3,4).
succ(4,5).
succ(5,6).
succ(6,7).
succ(7,8).
%succ(8,9).

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
%can(move(Obj, X, Y, Xf, Yf), [pos(Obj, X, Y), empty(Xf,Yf), searchMoveOk(), prioritary(Obj)], baba).

%adds(move(Obj, X, Y, Xf, Yf), [pos(Obj, Xf, Yf), empty(X,Y)], _, baba).

%deletes(move(Obj, X, Y, Xf, Yf), [pos(Obj, X, Y), empty(Xf,Yf), searchMoveOk()], baba).

% pos
can(pos(Obj, Xf, Yf), [pos(Obj, X, Y), empty(Xf,Yf), searchMoveOk(), prioritary(Obj)], baba).

adds(pos(Obj, Xf, Yf), [pos(Obj, Xf, Yf)], _, baba).

deletes(pos(Obj, Xf, Yf), [empty(Xf,Yf), searchMoveOk()], baba).

% *********************
% *** RULES ***********
% *********************
% HORIZONTAL RULES
can(rule(Phys1, Word2), [phys(Phys1), link(Phys1, Word1), pos(Word1, X1, Y), pos(is, Xis, Y), pos(Word2, X2, Y), word(Word1), word(Word2)], baba):-
    succ(X1,Xis),
    succ(Xis,X2).

% VERTICAL RULES
can(rule(Phys1, Word2), [phys(Phys1), link(Phys1, Word1), pos(Word1, X, Y1), pos(is, X, Yis), pos(Word2, X, Y2), word(Word1), word(Word2)], baba):-
    succ(Y1,Yis),
    succ(Yis,Y2).

% HORIZONTAL RULES WITH OBJECTS
can(rule(Phys1, Phys2), [phys(Phys1), link(Phys1, Word1), pos(Word1, X1, Y), pos(is, Xis, Y), phys(Phys2), link(Phys2, Word2),  pos(Word2, X2, Y),  word(Word1), word(Word2)], baba):-
    succ(X1,Xis),
    succ(Xis,X2).

% VERTICAL RULES  WITH OBJECTS
can(rule(Phys1, Phys2), [phys(Phys1), link(Phys1, Word1), pos(Word1, X, Y1), pos(is, X, Yis), phys(Phys2), link(Phys2, Word2),  pos(Word2, X, Y2),  word(Word1), word(Word2)], baba):-
    succ(Y1,Yis),
    succ(Yis,Y2).

adds(rule(Word1, Word2), [rule(Word1, Word2), searchMoveOk()], _, baba).

deletes(rule(Word1, Word2), [], baba).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% SOLVE LEVEL %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
can(won(YouPhys, WinPhys), [phys(YouPhys), phys(WinPhys)], baba).

adds(won(YouPhys, WinPhys), [won(YouPhys, WinPhys)], _, baba).

deletes(won(YouPhys, WinPhys), [], baba).


% ***************
% *** TESTS *****
% ***************

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 01 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
solve_l2(lvl_01,P,Goal) :-
    plan([word(flagWord), word(is), word(win), word(babaWord), word(you), link(baba,babaWord), link(flag,flagWord), phys(flag), phys(baba), rule(babaWord,you), rule(flag,win),
    
        pos(babaWord,1,8),  pos(is,2,8),    pos(you,3,8),   empty(4,8), empty(5,8), pos(flagWord,6,8),  pos(is,7,8),    pos(win,8,8),
        empty(1,7),         empty(2,7),     empty(3,7),     empty(4,7), empty(5,7), empty(6,7),         empty(7,7),     empty(8,7),
        empty(1,6),         empty(2,6),     empty(3,6),     empty(4,6), empty(5,6), empty(6,6),         empty(7,6),     empty(8,6),  
        empty(1,5),         empty(2,5),     empty(3,5),     empty(4,5), empty(5,5), empty(6,5),         empty(7,5),     empty(8,5),
        empty(1,4),         pos(baba,2,4),  empty(3,4),     empty(4,4), empty(5,4), empty(6,4),         pos(flag,7,4),  empty(8,4),  
        empty(1,3),         empty(2,3),     empty(3,3),     empty(4,3), empty(5,3), empty(6,3),         empty(7,3),     empty(8,3),
        empty(1,2),         empty(2,2),     empty(3,2),     empty(4,2), empty(5,2), empty(6,2),         empty(7,2),     empty(8,2),
        empty(1,1),         empty(2,1),     empty(3,1),     empty(4,1), empty(5,1), empty(6,1),         empty(7,1),     empty(8,1)
        ],
            [Goal], baba,
            %[pos(baba,6,4)], baba,
            P).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 02 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
solve_l2(lvl_02,P,Goal) :-
    plan([word(flagWord), word(is), word(win), word(babaWord), word(you), link(baba,babaWord), link(flag,flagWord), phys(flag), phys(baba), prioritary(flagWord), searchMoveOk(),
    
    pos(babaWord,1,8),  pos(is,2,8),    pos(you,3,8),   empty(4,8), empty(5,8), empty(6,8),         pos(is,7,8),    pos(win,8,8),
    empty(1,7),         empty(2,7),     empty(3,7),     empty(4,7), empty(5,7), empty(6,7),         empty(7,7),     empty(8,7),
    empty(1,6),         empty(2,6),     empty(3,6),     empty(4,6), empty(5,6), empty(6,6),         empty(7,6),     empty(8,6),  
    empty(1,5),         empty(2,5),     empty(3,5),     empty(4,5), empty(5,5), pos(flagWord,6,5),  empty(7,5),     empty(8,5),
    empty(1,4),         pos(baba,2,4),  empty(3,4),     empty(4,4), empty(5,4), empty(6,4),         pos(flag,7,4),  empty(8,4),  
    empty(1,3),         empty(2,3),     empty(3,3),     empty(4,3), empty(5,3), empty(6,3),         empty(7,3),     empty(8,3),
    empty(1,2),         empty(2,2),     empty(3,2),     empty(4,2), empty(5,2), empty(6,2),         empty(7,2),     empty(8,2),
    empty(1,1),         empty(2,1),     empty(3,1),     empty(4,1), empty(5,1), empty(6,1),         empty(7,1),     empty(8,1)
        ],
            %[won(baba,flag)], baba, % aborted after 1 hour
            [Goal], baba,
            P).

                
%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 03 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 04 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
solve_l2(lvl_04,P,Goal) :-
    plan(
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
        ]
        ,
            %[won(baba,flag)], baba, % aborted after 1 hour
            [Goal], baba,
            P).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 05 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
solve_l2(lvl_05,P,Goal) :-
    plan(
        [
            word(flagWord), word(is), word(win), word(babaWord), word(you), word(rockWord), link(baba,babaWord), link(flag,flagWord), link(rock,rockWord),
            phys(rock), phys(baba), rule(babaWord,you), prioritary(rockWord), phys(flag), rule(babaWord,you), rule(flag,win), searchMoveOk(),
    
            pos(babaWord,1,8),  pos(is,2,8),    pos(you,3,8),   empty(4,8),         empty(5,8), pos(flagWord,6,8),  pos(is,7,8),    pos(win,8,8),
            empty(1,7),         empty(2,7),     empty(3,7),     empty(4,7),         empty(5,7), empty(6,7),         empty(7,7),     empty(8,7),
            empty(1,6),         empty(2,6),     empty(3,6),     empty(4,6),         empty(5,6), empty(6,6),         empty(7,6),     empty(8,6),  
            empty(1,5),         pos(baba,2,5),  empty(3,5),     empty(4,5),         empty(5,5), empty(6,5),         pos(rock,7,5),  empty(8,5),
            empty(1,4),         empty(2,4),     empty(3,4),     empty(4,4),         empty(5,4), empty(6,4),         empty(7,4),     empty(8,4),  
            empty(1,3),         empty(2,3),     empty(3,3),     pos(rockWord,4,3),  empty(5,3), empty(6,3),         empty(7,3),     empty(8,3),
            empty(1,2),         empty(2,2),     empty(3,2),     empty(4,2),         empty(5,2), empty(6,2),         empty(7,2),     empty(8,2),
            empty(1,1),         empty(2,1),     empty(3,1),     empty(4,1),         empty(5,1), empty(6,1),         pos(is,7,1),    pos(flagWord,8,1)
            ]
        ,
            %[won(baba,flag)], baba, % aborted after 1 hour
            [Goal], baba,
            P).


