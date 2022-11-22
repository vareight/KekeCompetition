%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVELS FOR THE SOLVER AT L-02 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setLevel(Level,State,State).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 01 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
level(lvl_01,State, Prio):-
    setLevel(lvl_01, 
        [
        word(flagWord), word(is), word(win), word(babaWord), word(you),
        link(baba,babaWord), link(flag,flagWord),
        phys(flag), phys(baba),
        rule(baba,you), rule(flag,win),
        empty(7,4), searchMoveOk(), prioritary(Prio),
    
        pos(babaWord,1,8),  pos(is,2,8),    pos(you,3,8),   empty(4,8), empty(5,8), pos(flagWord,6,8),  pos(is,7,8),    pos(win,8,8),
        empty(1,7),         empty(2,7),     empty(3,7),     empty(4,7), empty(5,7), empty(6,7),         empty(7,7),     empty(8,7),
        empty(1,6),         empty(2,6),     empty(3,6),     empty(4,6), empty(5,6), empty(6,6),         empty(7,6),     empty(8,6),  
        empty(1,5),         empty(2,5),     empty(3,5),     empty(4,5), empty(5,5), empty(6,5),         empty(7,5),     empty(8,5),
        empty(1,4),         pos(baba,2,4),  empty(3,4),     empty(4,4), empty(5,4), empty(6,4),         pos(flag,7,4),  empty(8,4),  
        empty(1,3),         empty(2,3),     empty(3,3),     empty(4,3), empty(5,3), empty(6,3),         empty(7,3),     empty(8,3),
        empty(1,2),         empty(2,2),     empty(3,2),     empty(4,2), empty(5,2), empty(6,2),         empty(7,2),     empty(8,2),
        empty(1,1),         empty(2,1),     empty(3,1),     empty(4,1), empty(5,1), empty(6,1),         empty(7,1),     empty(8,1)
        ],
        State).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 02 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
level(lvl_02,State, Prio):-
    setLevel(lvl_02, 
        [
        word(flagWord), word(is), word(win), word(babaWord), word(you),
        link(baba,babaWord), link(flag,flagWord),
        phys(flag), phys(baba), 
        rule(baba,you), 
        prioritary(Prio), searchMoveOk(), empty(7,4),
        
        pos(babaWord,1,8),  pos(is,2,8),    pos(you,3,8),   empty(4,8), empty(5,8), empty(6,8),         pos(is,7,8),    pos(win,8,8),
        empty(1,7),         empty(2,7),     empty(3,7),     empty(4,7), empty(5,7), empty(6,7),         empty(7,7),     empty(8,7),
        empty(1,6),         empty(2,6),     empty(3,6),     empty(4,6), empty(5,6), empty(6,6),         empty(7,6),     empty(8,6),  
        empty(1,5),         empty(2,5),     empty(3,5),     empty(4,5), empty(5,5), pos(flagWord,6,5),  empty(7,5),     empty(8,5),
        empty(1,4),         pos(baba,2,4),  empty(3,4),     empty(4,4), empty(5,4), empty(6,4),         pos(flag,7,4),  empty(8,4),  
        empty(1,3),         empty(2,3),     empty(3,3),     empty(4,3), empty(5,3), empty(6,3),         empty(7,3),     empty(8,3),
        empty(1,2),         empty(2,2),     empty(3,2),     empty(4,2), empty(5,2), empty(6,2),         empty(7,2),     empty(8,2),
        empty(1,1),         empty(2,1),     empty(3,1),     empty(4,1), empty(5,1), empty(6,1),         empty(7,1),     empty(8,1)
        ],
             State).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 03 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
level(lvl_03,State, Prio):-
    setLevel(lvl_03, 
        [
        word(is), word(win), word(babaWord), word(you),
        link(baba,babaWord),
        phys(baba),
        rule(baba,you), searchMoveOk(), prioritary(Prio),
    
        empty(1,8),         empty(2,8),     empty(3,8),     empty(4,8),         empty(5,8),     empty(6,8),         empty(7,8),     empty(8,8),
        empty(1,7),         empty(2,7),     empty(3,7),     pos(babaWord,4,7),  pos(is,5,7),    pos(you,6,7),       empty(7,7),     empty(8,7),
        empty(1,6),         empty(2,6),     empty(3,6),     empty(4,6),         empty(5,6),     empty(6,6),         empty(7,6),     empty(8,6),  
        empty(1,5),         pos(is,2,5),    empty(3,5),     empty(4,5),         empty(5,5),     empty(6,5),         empty(7,5),     empty(8,5),
        empty(1,4),         pos(win,2,4),   empty(3,4),     empty(4,4),         pos(baba,5,4),  empty(6,4),         empty(7,4),     empty(8,4),  
        empty(1,3),         empty(2,3),     empty(3,3),     empty(4,3),         empty(5,3),     empty(6,3),         empty(7,3),     empty(8,3),
        empty(1,2),         empty(2,2),     empty(3,2),     empty(4,2),         empty(5,2),     empty(6,2),         empty(7,2),     empty(8,2),
        empty(1,1),         empty(2,1),     empty(3,1),     empty(4,1),         empty(5,1),     empty(6,1),         empty(7,1),     empty(8,1)
        ],
         State).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 04 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
level(lvl_04,State, Prio):-
    setLevel(lvl_04, 
        [
        word(flagWord), word(babaWord), word(grassWord), word(loveWord), word(is), word(win), word(you),
        link(baba,babaWord), link(flag,flagWord), link(love, loveWord), link(grass, grassWord),
        phys(flag), phys(baba), phys(love), phys(grass),
        rule(flag,you), rule(baba,win), rule(love, you), rule(grass,win), 
        searchMoveOk(), prioritary(Prio), empty(2,6), empty(2,3),

        pos(babaWord,1,8),  pos(is,2,8),    pos(win,3,8),   empty(4,8), empty(5,8), pos(flagWord,6,8),  pos(is,7,8),    pos(you,8,8),
        empty(1,7),         empty(2,7),     empty(3,7),     empty(4,7), empty(5,7), empty(6,7),         empty(7,7),     empty(8,7),
        empty(1,6),         pos(baba,2,6),  empty(3,6),     empty(4,6), empty(5,6), empty(6,6),         pos(flag,7,6),  empty(8,6),  
        empty(1,5),         empty(2,5),     empty(3,5),     empty(4,5), empty(5,5), empty(6,5),         empty(7,5),     empty(8,5),
        empty(1,4),         empty(2,4),     empty(3,4),     empty(4,4), empty(5,4), empty(6,4),         empty(7,4),     empty(8,4),  
        empty(1,3),         pos(grass,2,3), empty(3,3),     empty(4,3), empty(5,3), empty(6,3),         pos(love,7,3),  empty(8,3),
        empty(1,2),         empty(2,2),     empty(3,2),     empty(4,2), empty(5,2), empty(6,2),         empty(7,2),     empty(8,2),
        pos(grassWord,1,1), pos(is,2,1),    pos(win,3,1),   empty(4,1), empty(5,1), pos(loveWord,6,1),  pos(is,7,1),    pos(you,8,1)
        ], 
        State).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 05 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
level(lvl_05,State,Prio):-
    setLevel(lvl_05, 
        [
        word(flagWord), word(is), word(win), word(babaWord), word(you), word(rockWord),
        link(baba,babaWord), link(flag,flagWord), link(rock,rockWord),
        phys(rock), phys(baba), phys(flag),
        rule(baba,you), rule(flag,win),
        prioritary(Prio), searchMoveOk(), empty(7,5), mutable(rock),

        pos(babaWord,1,8),  pos(is,2,8),    pos(you,3,8),   empty(4,8),         empty(5,8), pos(flagWord,6,8),  pos(is,7,8),    pos(win,8,8),
        empty(1,7),         empty(2,7),     empty(3,7),     empty(4,7),         empty(5,7), empty(6,7),         empty(7,7),     empty(8,7),
        empty(1,6),         empty(2,6),     empty(3,6),     empty(4,6),         empty(5,6), empty(6,6),         empty(7,6),     empty(8,6),  
        empty(1,5),         pos(baba,2,5),  empty(3,5),     empty(4,5),         empty(5,5), empty(6,5),         pos(rock,7,5),  empty(8,5),
        empty(1,4),         empty(2,4),     empty(3,4),     empty(4,4),         empty(5,4), empty(6,4),         empty(7,4),     empty(8,4),  
        empty(1,3),         empty(2,3),     empty(3,3),     pos(rockWord,4,3),  empty(5,3), empty(6,3),         empty(7,3),     empty(8,3),
        empty(1,2),         empty(2,2),     empty(3,2),     empty(4,2),         empty(5,2), empty(6,2),         empty(7,2),     empty(8,2),
        empty(1,1),         empty(2,1),     empty(3,1),     empty(4,1),         empty(5,1), empty(6,1),         pos(is,7,1),    pos(flagWord,8,1)
        ], 
        State).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 06 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
level(lvl_06,State,Prio):-
    setLevel(lvl_06, 
        [
        word(flagWord), word(is), word(win), word(babaWord), word(you), word(rockWord),
        link(baba,babaWord), link(flag,flagWord), link(rock,rockWord),
        phys(rock), phys(baba), phys(flag),
        rule(baba,you), rule(flag,win), rule(rock,flag), rule(rock,rock),
        prioritary(Prio), searchMoveOk(), empty(7,5),

        pos(babaWord,1,8),  pos(is,2,8),        pos(you,3,8),   empty(4,8),         empty(5,8), pos(flagWord,6,8),  pos(is,7,8),    pos(win,8,8),
        empty(1,7),         empty(2,7),         empty(3,7),     empty(4,7),         empty(5,7), empty(6,7),         empty(7,7),     empty(8,7),
        empty(1,6),         empty(2,6),         empty(3,6),     empty(4,6),         empty(5,6), empty(6,6),         empty(7,6),     empty(8,6),  
        empty(1,5),         pos(baba,2,5),      empty(3,5),     empty(4,5),         empty(5,5), empty(6,5),         pos(rock,7,5),  empty(8,5),
        empty(1,4),         empty(2,4),         empty(3,4),     empty(4,4),         empty(5,4), empty(6,4),         empty(7,4),     empty(8,4),  
        empty(1,3),         pos(rockWord,2,3),  pos(is,3,3),    pos(rockWord,4,3),  empty(5,3), empty(6,3),         empty(7,3),     empty(8,3),
        empty(1,2),         empty(2,2),         empty(3,2),     empty(4,2),         empty(5,2), empty(6,2),         empty(7,2),     empty(8,2),
        empty(1,1),         empty(2,1),         empty(3,1),     empty(4,1),         empty(5,1), pos(rockWord,6,1),  pos(is,7,1),    pos(flagWord,8,1)
        ], 
        State).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 07 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
level(lvl_07,State,Prio):-
    setLevel(lvl_07, 
        [
        word(flagWord), word(is), word(win), word(babaWord), word(you), word(rockWord), word(push),
        link(baba,babaWord), link(flag,flagWord), link(rock,rockWord),
        phys(rock), phys(baba), phys(flag),
        rule(baba,you), rule(flag,win), rule(rock,push),
        prioritary(Prio), searchMoveOk(), empty(5,5),

        pos(babaWord,1,8),  pos(is,2,8),        pos(you,3,8),   empty(4,8),         empty(5,8),         pos(flagWord,6,8),  pos(is,7,8),    pos(win,8,8),
        empty(1,7),         empty(2,7),         empty(3,7),     empty(4,7),         empty(5,7),         empty(6,7),         empty(7,7),     empty(8,7),
        empty(1,6),         empty(2,6),         empty(3,6),     pos(rock,4,6),      pos(rock,5,6),      empty(6,6),         empty(7,6),     empty(8,6),  
        empty(1,5),         empty(2,5),         pos(rock,3,5),  empty(4,5),         pos(flag,5,5),      pos(rock,6,5),      empty(7,5),     empty(8,5),
        empty(1,4),         empty(2,4),         pos(rock,3,4),  empty(4,4),         empty(5,4),         pos(rock,6,4),      empty(7,4),     empty(8,4),  
        empty(1,3),         empty(2,3),         empty(3,3),     pos(rock,4,3),      pos(rock,5,3),      empty(6,3),         empty(7,3),     empty(8,3),
        empty(1,2),         pos(baba,2,2),      empty(3,2),     empty(4,2),         empty(5,2),         empty(6,2),         empty(7,2),     empty(8,2),
        empty(1,1),         empty(2,1),         empty(3,1),     empty(4,1),         empty(5,1),         pos(rockWord,6,1),  pos(is,7,1),    pos(flagWord,8,1)
        ], 
        State).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 08 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
level(lvl_08,State, Prio):-
    setLevel(lvl_08, 
        [
        word(flagWord), word(babaWord), word(kekeWord), word(is), word(win), word(you), word(move),
        link(baba,babaWord), link(flag,flagWord), link(keke, kekeWord),
        phys(flag), phys(baba), phys(keke),
        rule(flag,win), rule(baba,you), rule(keke, move),
        searchMoveOk(), prioritary(Prio), empty(5,4),

        pos(kekeWord, 1,8), empty(2,8),         empty(3,8),     empty(4,8),     empty(5,8),     empty(6,8),         empty(7,8),     empty(8,8),
        pos(is,1,7),        pos(keke,2,7),      empty(3,7),     empty(4,7),     empty(5,7),     pos(babaWord,6,7),  pos(is,7,7),    pos(you,8,7),
        pos(move,1,6),      empty(2,6),         empty(3,6),     empty(4,6),     empty(5,6),     empty(6,6),         empty(7,6),     empty(8,6),  
        empty(1,5),         empty(2,5),         empty(3,5),     empty(4,5),     empty(5,5),     empty(6,5),         empty(7,5),     empty(8,5),
        empty(1,4),         empty(2,4),         pos(baba,3,4),  empty(4,4),     pos(flag,5,4),  empty(6,4),         empty(7,4),     empty(8,4),  
        empty(1,3),         empty(2,3),         empty(3,3),     empty(4,3),     empty(5,3),     empty(6,3),         empty(7,3),     empty(8,3),
        empty(1,2),         empty(2,2),         empty(3,2),     empty(4,2),     empty(5,2),     empty(6,2),         empty(7,2),     empty(8,2),
        empty(1,1),         empty(2,1),         empty(3,1),     empty(4,1),     empty(5,1),     pos(flagWord,6,1),  pos(is,7,1),    pos(win,8,1)
        ], 
        State).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 09 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
level(lvl_09,State, Prio):-
    setLevel(lvl_09, 
        [
        word(flagWord), word(babaWord), word(wallWord), word(is), word(win), word(you), word(stop),
        link(baba,babaWord), link(flag,flagWord), link(wall, wallWord),
        phys(flag), phys(baba), phys(wall),
        rule(flag,win), rule(baba,you), rule(wall, stop),
        searchMoveOk(), prioritary(Prio), empty(5,8),

        empty(1,8),         empty(2,8),         empty(3,8),     empty(4,8),     pos(flag,5,8),  empty(6,8),         empty(7,8),     empty(8,8),
        empty(1,7),         empty(2,7),         empty(3,7),     empty(4,7),     empty(5,7),     empty(6,7),         empty(7,7),     empty(8,7),
        pos(wall,1,6),      pos(wall,2,6),      pos(wall,3,6),  pos(wall,4,6),  pos(wall,5,6),  pos(wall,6,6),      pos(wall,7,6),  pos(wall,8,6),  
        empty(1,5),         pos(wallWord,2,5),  empty(3,5),     empty(4,5),     empty(5,5),     empty(6,5),         empty(7,5),     empty(8,5),
        empty(1,4),         pos(is,2,4),        empty(3,4),     empty(4,4),     pos(baba,5,4),  empty(6,4),         empty(7,4),     empty(8,4),  
        empty(1,3),         pos(stop,2,3),      empty(3,3),     empty(4,3),     empty(5,3),     empty(6,3),         empty(7,3),     empty(8,3),
        empty(1,2),         empty(2,2),         empty(3,2),     empty(4,2),     empty(5,2),     empty(6,2),         empty(7,2),     empty(8,2),
        pos(babaWord,1,1),  pos(is,2,1),        pos(you,3,1),   empty(4,1),     empty(5,1),     pos(flagWord,6,1),  pos(is,7,1),    pos(win,8,1)
        ], 
        State).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 10 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
level(lvl_10,State, Prio):-
    setLevel(lvl_10, 
        [
        word(flagWord), word(babaWord), word(boneWord), word(is), word(win), word(you), word(kill),
        link(baba,babaWord), link(flag,flagWord), link(bone, boneWord),
        phys(flag), phys(baba), phys(bone),
        rule(flag,win), rule(baba,you), rule(bone, kill),
        searchMoveOk(), prioritary(Prio), empty(6,5),

        empty(1,8),         empty(2,8),         empty(3,8),     empty(4,8),     empty(5,8),     empty(6,8),         empty(7,8),     empty(8,8),
        pos(boneWord, 1,7), pos(is,2,7),        pos(kill,3,7),  empty(4,7),     empty(5,7),     empty(6,7),         empty(7,7),     empty(8,7),
        empty(1,6),         empty(2,6),         empty(3,6),     empty(4,6),     pos(bone,5,6),  pos(bone,6,6),      pos(bone,7,6),  empty(8,6),  
        empty(1,5),         pos(baba,2,5),      empty(3,5),     empty(4,5),     pos(bone,5,5),  pos(flag,6,5),      pos(bone,7,5),  empty(8,5),
        empty(1,4),         empty(2,4),         empty(3,4),     empty(4,4),     pos(bone,5,4),  pos(bone,6,4),      pos(bone,7,4),  empty(8,4),  
        empty(1,3),         empty(2,3),         empty(3,3),     empty(4,3),     empty(5,3),     empty(6,3),         empty(7,3),     empty(8,3),
        empty(1,2),         empty(2,2),         empty(3,2),     empty(4,2),     empty(5,2),     empty(6,2),         empty(7,2),     empty(8,2),
        pos(babaWord,1,1),  pos(is,2,1),        pos(you,3,1),   empty(4,1),     empty(5,1),     pos(flagWord,6,1),  pos(is,7,1),    pos(win,8,1)
        ], 
        State).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 11 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
level(lvl_11,State, Prio):-
    setLevel(lvl_11, 
        [
        word(flagWord), word(babaWord), word(goopWord), word(is), word(win), word(you), word(sink),
        link(baba,babaWord), link(flag,flagWord), link(goop, goopWord),
        phys(flag), phys(baba), phys(goop),
        rule(flag,win), rule(baba,you), rule(goop, sink),
        searchMoveOk(), prioritary(Prio), empty(7,7),

        pos(babaWord,1,8),  pos(is,2,8),        pos(you,3,8),   pos(goop,4,8),     pos(goop,5,8),  empty(6,8),         empty(7,8),     empty(8,8),
        pos(flagWord, 1,7), pos(is,2,7),        pos(win,3,7),   pos(goop,4,7),     pos(goop,5,7),  empty(6,7),         pos(flag,7,7),  empty(8,7),
        pos(goop,1,6),      pos(baba,2,6),      pos(goop,3,6),  pos(goop,4,6),     pos(goop,5,6),  empty(6,6),         empty(7,6),     empty(8,6),  
        pos(goop,1,5),      empty(2,5),         pos(goop,3,5),  pos(goop,4,5),     pos(goop,5,5),  pos(goop,6,5),      pos(goop,7,5),  pos(goop,8,5),
        pos(goop,1,4),      empty(2,4),         pos(goop,3,4),  pos(goop,4,4),     pos(goop,5,4),  pos(goop,6,4),      pos(goop,7,4),  pos(goop,8,4),  
        pos(goop,1,3),      empty(2,3),         pos(goop,3,3),  pos(goop,4,3),     empty(5,3),     empty(6,3),         empty(7,3),     empty(8,3),
        pos(goop,1,2),      empty(2,2),         empty(3,2),     empty(4,2),        empty(5,2),     pos(goopWord,6,2),  pos(is,7,2),    pos(sink,8,2),
        pos(goop,1,1),      pos(goop,2,1),      pos(goop,3,1),  pos(goop,4,1),     empty(5,1),     empty(6,1),         empty(7,1),     empty(8,1)
        ], 
        State).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 12 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 13 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
level(lvl_13,State, Prio):-
    setLevel(lvl_13, 
        [
        word(flagWord), word(babaWord), word(lavaWord), word(is), word(win), word(you), word(hot), word(melt),
        link(baba,babaWord), link(flag,flagWord), link(lava, lavaWord),
        phys(flag), phys(baba), phys(lava),
        rule(flag,win), rule(baba,you), rule(lava, hot), rule(baba,melt),
        searchMoveOk(), prioritary(Prio), empty(4,8),

        empty(1,8),         empty(2,8),         empty(3,8),     pos(flag,4,8),  empty(5,8),     empty(6,8),         empty(7,8),         empty(8,8),
        empty(1,7),         empty(2,7),         empty(3,7),     empty(4,7),     empty(5,7),     empty(6,7),         empty(7,7),         empty(8,7),
        pos(lava,1,6),      pos(lava,2,6),      pos(lava,3,6),  pos(lava,4,6),  pos(lava,5,6),  pos(lava,6,6),      pos(lava,7,6),      pos(lava,8,6),  
        pos(lavaWord,1,5),  empty(2,5),         empty(3,5),     empty(4,5),     empty(5,5),     empty(6,5),         pos(babaWord,7,5),  empty(8,5),
        pos(is,1,4),        empty(2,4),         empty(3,4),     empty(4,4),     pos(baba,5,4),  empty(6,4),         pos(is,7,4),        empty(8,4),  
        pos(hot,1,3),       empty(2,3),         empty(3,3),     empty(4,3),     empty(5,3),     empty(6,3),         pos(melt,7,3),      empty(8,3),
        empty(1,2),         empty(2,2),         empty(3,2),     empty(4,2),     empty(5,2),     empty(6,2),         empty(7,2),         empty(8,2),
        pos(babaWord,1,1),  pos(is,2,1),        pos(you,3,1),   empty(4,1),     empty(5,1),     pos(flagWord,6,1),  pos(is,7,1),        pos(win,8,1)
        ], 
        State).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 14 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
level(lvl_14,State, Prio):-
    setLevel(lvl_14, 
        [
        word(flagWord), word(babaWord), word(wallWord), word(kekeWord), word(is), word(win), word(you), word(stop),
        link(baba,babaWord), link(flag,flagWord), link(wall, wallWord), link(keke, kekeWord),
        phys(flag), phys(baba), phys(wall), phys(keke),
        rule(flag,win), rule(baba,you), rule(wall, stop),
        searchMoveOk(), prioritary(Prio), empty(6,7),

        empty(1,8),         empty(2,8),         empty(3,8),         empty(4,8),     empty(5,8),     empty(6,8),         empty(7,8),         empty(8,8),
        empty(1,7),         empty(2,7),         pos(keke,3,7),      empty(4,7),     empty(5,7),     pos(flag,6,7),      empty(7,7),         empty(8,7),
        pos(wall,1,6),      pos(wall,2,6),      pos(wall,3,6),      pos(wall,4,6),  pos(wall,5,6),  pos(wall,6,6),      pos(wall,7,6),      pos(wall,8,6),  
        empty(1,5),         empty(2,5),         empty(3,5),         empty(4,5),     empty(5,5),     empty(6,5),         empty(7,5),         pos(wallWord,8,5),
        empty(1,4),         empty(2,4),         pos(kekeWord,3,4),  empty(4,4),     empty(5,4),     pos(baba,6,4),      empty(7,4),         pos(is,8,4),  
        empty(1,3),         empty(2,3),         empty(3,3),         empty(4,3),     empty(5,3),     empty(6,3),         empty(7,3),         pos(stop,8,3),
        pos(babaWord,1,2),  pos(is,2,2),        pos(you,3,2),       empty(4,2),     empty(5,2),     empty(6,2),         empty(7,2),         empty(8,2),
        empty(1,1),         empty(2,1),         empty(3,1),         empty(4,1),     empty(5,1),     pos(flagWord,6,1),  pos(is,7,1),        pos(win,8,1)
        ], 
        State).