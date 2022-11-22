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


% obstacle(wall).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% GENERAL %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% MOVE &%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LEFT
can(approachLeft(YouPhys, X, Y, Obj, Xo, Yo), [link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y), pos(Obj,Xo,Yo), empty(Xf,Yo)], baba):-
    succ(Xf,Xo).

adds(approachLeft(YouPhys, X, Y, Obj, Xo, Yo), [pos(YouPhys, Xf, Yo), empty(X,Y)], _, baba):-
    succ(Xf,Xo).

deletes(approachLeft(YouPhys, X, Y, Obj, Xo, Yo), [pos(YouPhys, X, Y), empty(Xf,Yo)], baba):-  % Yi e Yf sono le Y di inizio e fine della posizione di arrivo Xf
    succ(Xf,Xo).

% DOWN
can(approachDown(YouPhys, X, Y, Obj, Xo, Yo), [link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y), pos(Obj,Xo,Yo), empty(Xo,Yf)], baba):-
    succ(Yf,Yo).

adds(approachDown(YouPhys, X, Y, Obj, Xo, Yo), [pos(YouPhys, Xo, Yf), empty(X,Y)], _, baba):-
    succ(Yf,Yo).

deletes(approachDown(YouPhys, X, Y, Obj, Xo, Yo), [pos(YouPhys, X, Y), empty(Xo,Yf)], baba):-  % Yi e Yf sono le Y di inizio e fine della posizione di arrivo Xf
    succ(Yf,Yo).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% PUSH &%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
can(pushUp1(YouPhys, X, Y, Obj, X, Yo, Yf), [link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y), pos(Obj,X,Yo), empty(X,Yf1), empty(X,Yf), word(Obj)], baba):-
    succ(Yf1,Yf),
    succ(Y,Yo).

% devo aggiungere il caso in cui sposto in alto solo di una casella: Yf1 = Yo
adds(pushUp1(YouPhys, X, Y, Obj, X, Yo, Yf), [pos(YouPhys, X, Yf1), pos(Obj,X,Yf), empty(X,Y), empty(X,Yo)], _, baba):- 
    succ(Yf1,Yf),
    succ(Y,Yo).

deletes(pushUp1(YouPhys, X, Y, Obj, X, Yo, Yf), [pos(YouPhys, X, Y), pos(Obj,X,Yo), empty(X,Yf1), empty(X,Yf)], baba):-  % Yi e Yf sono le Y di inizio e fine della posizione di arrivo Xf
    succ(Yf1,Yf),
    succ(Y,Yo).

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


% ***************
% *** TESTS *****
% ***************


solve(lvl_01,P,Goal) :-
    plan([word(flagWord), word(is), word(win), word(babaWord), word(you), link(baba,babaWord), link(flag,flagWord), phys(flag), phys(baba), rule(babaWord,you), rule(flagWord,win),
    
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

solve(lvl_02,P,Goal) :-
    plan([word(flagWord), word(is), word(win), word(babaWord), word(you), link(baba,babaWord), link(flag,flagWord), phys(flag), phys(baba),
    
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

                

