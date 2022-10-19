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

prec(9,8).
prec(8,7).
prec(7,6).
prec(6,5).
prec(5,4).
prec(4,3).
prec(3,2).
prec(2,1).
prec(1,0).

% obstacle(wall).

% *****************************************
% *** moveDx(You, Pos) ********************
% *****************************************
can(moveRight(YouPhys, X1, Y), [link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y), empty(X1,Y)], baba):-
    succ(X,X1).

adds(moveRight(YouPhys, X1, Y), [pos(YouPhys, X1, Y), empty(X,Y)], _, baba):-
    succ(X,X1).

deletes(moveRight(YouPhys, X1, Y), [pos(YouPhys, X, Y), empty(X1,Y)], baba):-
    succ(X,X1).

% *****************************************
% *** moveLeft(You, Pos) ********************
% *****************************************
can(moveLeft(YouPhys, X1, Y), [link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y), empty(X1,Y)], baba):-
    succ(X1,X).

adds(moveLeft(YouPhys, X1, Y), [pos(YouPhys, X1, Y), empty(X,Y)], _, baba):-
    succ(X1,X).

deletes(moveLeft(YouPhys, X1, Y), [pos(YouPhys, X, Y), empty(X1,Y)], baba):-
    succ(X1,X).

% *****************************************
% *** moveUp(You, Pos) ********************
% *****************************************
can(moveUp(YouPhys, X, Y1), [link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y), empty(X,Y1)], baba):-
    succ(Y,Y1).

adds(moveUp(YouPhys, X, Y1), [pos(YouPhys, X, Y1), empty(X,Y)], _, baba):-
    succ(Y,Y1).

deletes(moveUp(YouPhys, X, Y1), [pos(YouPhys, X, Y), empty(X,Y1)], baba):-
    succ(Y,Y1).

% *****************************************
% *** moveDown(You, Pos) ********************
% *****************************************
can(moveDown(YouPhys, X, Y1), [link(YouPhys, YouWord), rule(YouWord,you), pos(YouPhys, X, Y), empty(X,Y1)], baba):-
    succ(Y1,Y).

adds(moveDown(YouPhys, X, Y1), [pos(YouPhys, X, Y1), empty(X,Y)], _, baba):-
    succ(Y1,Y).

deletes(moveDown(YouPhys, X, Y1), [pos(YouPhys, X, Y), empty(X,Y1)], baba):-
    succ(Y1,Y).

%available
can(available(X,Y), [empty(X,Y)], baba).

adds(available(X,Y), [reachableR(X,Y), reachableL(X,Y), reachableU(X,Y), reachableD(X,Y)], _, baba).

deletes(available(X,Y), [], baba).

% pushR
can(pushR(Word, X, Y), [pos(Word,X,Y), word(Word), reachableR(X1,Y)], baba):-
    succ(X,X1).

adds(pushR(Word, X, Y), [reachableR(X,Y), pos(Word,X1,Y)], _, baba):-
    succ(X,X1).

deletes(pushR(Word, X, Y), [pos(Word,X,Y),reachableR(X1,Y),empty(X1,Y)], baba):-
    succ(X,X1).

% pushL
can(pushL(Word, X, Y), [pos(Word,X,Y), word(Word), reachableL(X1,Y)], baba):-
    succ(X1,X).

adds(pushL(Word, X, Y), [reachableL(X,Y), pos(Word,X1,Y)], _, baba):-
    succ(X1,X).

deletes(pushL(Word, X, Y), [pos(Word,X,Y),reachableL(X1,Y),empty(X1,Y)], baba):-
    succ(X1,X).

% pushU
can(pushU(Word, X, Y), [pos(Word,X,Y), word(Word), reachableU(X,Y1)], baba):-
    succ(Y,Y1).

adds(pushU(Word, X, Y), [reachableU(X,Y), pos(Word,X,Y1)], _, baba):-
    succ(Y,Y1).

deletes(pushU(Word, X, Y), [pos(Word,X,Y),reachableU(X,Y1),empty(X,Y1)], baba):-
    succ(Y,Y1).

% pushD
can(pushD(Word, X, Y), [pos(Word,X,Y), word(Word), reachableD(X,Y1)], baba):-
    succ(Y1,Y).

adds(pushD(Word, X, Y), [reachableD(X,Y), pos(Word,X,Y1)], _, baba):-
    succ(Y1,Y).

deletes(pushD(Word, X, Y), [pos(Word,X,Y),reachableD(X,Y1),empty(X,Y1)], baba):-
    succ(Y1,Y).


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
    plan([word(flagWord), word(is), word(win), word(babaWord), word(you), link(baba,babaWord), link(flag,flagWord), phys(flag), phys(baba),
    
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

lvl_01(P, Goal) :-
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

lvl_02(P) :-
    plan([word(flagWord), word(is), word(win), word(babaWord), word(you), link(baba,babaWord), link(flag,flagWord), phys(flag), phys(baba),
    
        pos(babaWord,1,8),  pos(is,2,8),    pos(you,3,8),   empty(4,8), empty(5,8), empty(6,8),         pos(is,7,8),    pos(win,8,8),
        empty(1,7),         empty(2,7),     empty(3,7),     empty(4,7), empty(5,7), empty(6,7),         empty(7,7),     empty(8,7),
        empty(1,6),         empty(2,6),     empty(3,6),     empty(4,6), empty(5,6), empty(6,6),         empty(7,6),     empty(8,6),  
        empty(1,5),         empty(2,5),     empty(3,5),     empty(4,5), empty(5,5), pos(flagWord,6,5),  empty(7,5),     empty(8,5),
        empty(1,4),         pos(baba,2,4),  empty(3,4),     empty(4,4), empty(5,4), pos(baba,6,4),         pos(flag,7,4),  empty(8,4),  
        empty(1,3),         empty(2,3),     empty(3,3),     empty(4,3), empty(5,3), empty(6,3),         empty(7,3),     empty(8,3),
        empty(1,2),         empty(2,2),     empty(3,2),     empty(4,2), empty(5,2), empty(6,2),         empty(7,2),     empty(8,2),
        empty(1,1),         empty(2,1),     empty(3,1),     empty(4,1), empty(5,1), empty(6,1),         empty(7,1),     empty(8,1)
        ],
            %[won(baba,flag)], baba, % aborted after 1 hour
            [pos(flagWord,6,8)], baba,
            P).

                

