%    First level of Baba is You Thesis Competition

% the planner is a thinker, not an executor
% REFACTOR NAMES --- HERE THE STEPS ARE GOALS FOR THE LOW LEVEL PLANNER -- REFACTOR NAMES

% *****************************************
% *** FACTS *******************************
% *****************************************
% Note that these "word" predicates do not describe a changeable state but fixed facts that will not be changed during the planning 

% *** PHYSICAL OBJECTS ***
phys(baba).
phys(flag).
phys(love).
phys(grass).
phys(rock).
phys(wall).
phys(bone).
phys(goop).
phys(lava).
phys(keke).

% *** WORDS OBJECTS ***
word(baba).
word(flag).
word(love).
word(grass).
word(rock).
word(wall).
word(bone).
word(goop).
word(lava).
word(keke).

word(is).
word(you).
word(win).
word(push).
word(stop).
word(kill).
word(sink).
word(melt).
word(hot).

:- discontiguous can/3.
:- discontiguous adds/4.
:- discontiguous deletes/3.

% *** RULES *** --> non sono fatti
%isYou(baba).
%isYou(flag).
%isWin(baba).
%isWin(flag).

% obstacle(wall).


% *****************************************
% *** solved(You, WinCond) *****************
% *****************************************
can(won(You,Phys), [rule(You,you), rule(Phys,win), phys(You), phys(Phys), free(Phys)], baba).

adds(won(You,Phys), [won(You,Phys)], _, baba). % NB: posso mettere anche won() senza argomenti, ma nei livelli complicati mi dà tutte le possibilità. Meglio delegare questo all'esecutore

deletes(won(You,Phys), [], baba).


% **********************************************
% *** createRule(Word1, Connector, Word2) *****
% **********************************************
can(rule(Phys, Rule),[word(Phys), word(Rule), phys(Phys)], baba).

adds(rule(Phys, Rule),[rule(Phys,Rule)], _, baba):-
    phys(Phys),
	word(Phys),
    word(Rule).

deletes(rule(Phys, Rule),[], baba):-
    phys(Phys), 
	word(Phys),
    word(Rule).

deletes(rule(Phys, Rule),[mutable(Phys)], baba):-
    phys(Phys),
	Rule = Phys.

% **********************************************
% *** destroyRule(Word1, Connector, Word2) *****
% **********************************************
can(destroyRule(Phys, Rule),[rule(Phys,Rule), breakable(Phys,Rule), word(Phys), word(Rule), phys(Phys)], baba).

adds(destroyRule(Phys, Rule),[removed(Phys)], _, baba):- % Ostacolo rimosso
    phys(Phys),
	word(Phys),
    word(Rule).

adds(destroyRule(Phys, Rule),[mutable(Phys)], _, baba):-
    phys(Phys),
	Rule = Phys.

deletes(destroyRule(Phys, Rule),[rule(Phys,Rule)], baba):-
    phys(Phys),
	word(Phys),
    word(Rule).

% **********************************************
% ****** unbond(Word1, Connector, Word2) *******
% **********************************************
% we have only 2 cases, hot and melt
can(unbond(Phys1, Rule, Phys2),[rule(Phys1,Rule), removed(Phys2), word(Phys1), word(Phys2), phys(Phys1), phys(Phys2)], baba).

adds(unbond(Phys1, Rule, Phys2),[removed(Phys1)], _, baba). % Ostacolo rimosso

deletes(unbond(Phys1, Rule, Phys2),[], baba).

% **********************************************
% ******* sink(Word1, Connector, Word2) ********
% **********************************************
can(sink(Phys1, Phys2),[rule(Phys1,push), rule(Phys2,sink), word(Phys1), word(Phys2), phys(Phys1), phys(Phys2)], baba).

adds(sink(Phys1, Phys2),[removed(Phys2)], _, baba). % Ostacolo rimosso

deletes(sink(Phys1, Phys2),[], baba).


% *******************
% *** TRANSFORM *****
% *******************
can(transform(Phys1, Phys2),[rule(Phys1,Phys2), word(Phys1), word(Phys2), phys(Phys1), mutable(Phys1)], baba).

adds(transform(Phys1, Phys2),[phys(Phys2), free(Phys2)], _, baba):-
	word(Phys1),
    word(Phys2),
    phys(Phys1).

deletes(transform(Phys1, Phys2),[phys(Phys1),free(Phys1)], baba):-
	word(Phys1),
    word(Phys2),
    phys(Phys1).


% *******************
% ****** FREE *******
% *******************
% We need to "free" the object in order to reach it:
% we will find out how only in the next level-planner,
% so here we just state some conditions that will help clear the way: PUSH - DESTROY - CREATE
% Non ci interessa sapere che tipo di ostacoli sono, ci basta conoscere che sono degli ostacoli generici
% REMOVED (after DESTROY)
can(free(Phys, Obstacle, You),[obstacle(Obstacle), removed(Obstacle), phys(Phys), phys(Obstacle), denies(Obstacle,You)], baba). % l'ostacolo c'è ma è stato rimosso (distuggendo la regola)

adds(free(Phys, Obstacle, You),[free(Phys)], _, baba).

deletes(free(Phys, Obstacle, You),[], baba).

% FREE - PUSHING 
can(push(Obstacle),[obstacle(Obstacle), rule(Obstacle,push), phys(Obstacle)], baba). % ostacolo presente ma si può spostare con una push

adds(push(Obstacle),[removed(Obstacle)], _, baba).

deletes(push(Obstacle),[], baba).
% ***************
% *** LEVELS ****
% ***************

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 01 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
solve_l1(lvl_01,P) :-
	plan([phys(baba), phys(flag), word(is), word(baba), word(flag), word(win), word(you),
            rule(baba,you), rule(flag,win),
            free(flag), free(baba)],
            [won(baba,flag)], baba,
	        P).

% è uguale perchè diciamo solo di creare la regola, non come.
% Nel lvl_01 è già fatto mentre qua bisogna effettuare delle mosse, ma a livello logico bisogna solo creare la regola
%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 02 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
solve_l1(lvl_02,P) :-
    plan([phys(baba), phys(flag), word(is), word(baba), word(flag), word(win), word(you),
            rule(baba,you), 
            free(flag), free(baba)],
            [won(baba,flag)], baba,
            P).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 03 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
solve_l1(lvl_03,P) :-
    plan([phys(baba), word(is), word(baba), word(win), word(you),
            rule(baba,you),
            free(baba)],
            [won(baba,baba)], baba,
            P).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 04 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
solve_l1(lvl_04,P) :-
    plan([
        phys(baba), phys(flag), phys(love), phys(grass),
        word(is), word(baba), word(flag), word(love), word(grass), word(win), word(you),
        rule(baba,win), rule(grass,win), rule(flag,you), rule(love,you),
        free(flag), free(baba), free(love), free(grass)],
            [won(love,grass)], baba,
            P).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 05 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
solve_l1(lvl_05,P) :-
    plan([
        phys(baba), phys(rock), mutable(rock),
        word(is), word(baba), word(flag), word(rock), word(win), word(you),
        rule(baba,you), rule(flag,win),
        free(rock), free(baba)],
            [won(baba,flag)], baba,
            P).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 06 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
% ROCK IS ROCK -> mutable
solve_l1(lvl_06,P) :-
    plan([
        phys(baba), phys(rock),
        word(is), word(baba), word(flag), word(rock), word(win), word(you),
        rule(baba,you), rule(flag,win), rule(rock,rock), rule(rock,flag), breakable(rock,rock),
        free(baba), free(rock)],
            [won(baba,flag)], baba,
            P).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 07 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
% SURROUNDED WIN OBJ -> FREE
solve_l1(lvl_07,P) :-
    plan([
    phys(baba), phys(rock), phys(flag),
        word(is), word(baba), word(flag), word(rock), word(win), word(you), word(push),
        rule(baba,you), rule(flag,win), rule(rock,push),
        free(baba), obstacle(rock)],
            [won(baba,flag)], baba,
            P).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 08 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
% NON POSSIAMO SCRIVERE LA PRESENZA DI KEKE: LE REGOLE MOVE SONO UN FATTORE ESTERNO
solve_l1(lvl_08,P) :-
	plan([phys(baba), phys(flag), 
        word(is), word(baba), word(flag), word(win), word(you),
        rule(baba,you), rule(flag,win),
        free(flag), free(baba)],
        [won(baba,flag)], baba,
        P).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 09 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
% SURROUNDED WIN OBJ -> FREE
solve_l1(lvl_09,P) :-
    plan([
    phys(baba), phys(wall), phys(flag),
        word(is), word(baba), word(flag), word(wall), word(win), word(you), word(stop),
        rule(baba,you), rule(flag,win), rule(wall,stop), breakable(wall,stop),
        free(baba), obstacle(wall)],
            [won(baba,flag)], baba,
            P).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 10 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
% SURROUNDED WIN OBJ -> FREE
solve_l1(lvl_10,P) :-
    plan([
    phys(baba), phys(bone), phys(flag),
        word(is), word(baba), word(flag), word(bone), word(win), word(you), word(kill),
        rule(baba,you), rule(flag,win), rule(bone,kill), breakable(bone,kill),
        free(baba), obstacle(bone)],
            [won(baba,flag)], baba,
            P).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 11 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
% SURROUNDED WIN OBJ -> FREE
solve_l1(lvl_11,P) :-
    plan([
    phys(baba), phys(goop), phys(flag),
        word(is), word(baba), word(flag), word(goop), word(win), word(you), word(sink),
        rule(baba,you), rule(flag,win), rule(goop,sink), breakable(goop,sink),
        free(baba), obstacle(goop)],
            [won(baba,flag)], baba,
            P).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 12 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
% SURROUNDED WIN OBJ -> FREE
% Introduzione di breakable: altrimenti qua mi prova a rompere una regola
% che non è distruttibile (fatto). Devo aggiungere breakable a tutte le regole precedenti.
solve_l1(lvl_12,P) :-
    plan([
    phys(baba), phys(goop), phys(flag), phys(rock),
        word(is), word(baba), word(flag), word(goop), word(rock), word(win), word(you), word(sink), word(push),
        rule(baba,you), rule(flag,win), rule(goop,sink), rule(rock,push),
        free(baba), obstacle(goop)],
            [won(baba,flag)], baba,
            P).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 13 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
% SURROUNDED WIN OBJ -> FREE
% Introduzione di breakable: altrimenti qua mi prova a rompere una regola
% che non è distruttibile (fatto). Devo aggiungere breakable a tutte le regole precedenti.
% COPPIA DI REGOLE - HOT/MELT
solve_l1(lvl_13,P) :-
    plan([
    phys(baba), phys(lava), phys(flag),
        word(is), word(baba), word(flag), word(lava), word(hot), word(win), word(you), word(melt), word(push),
        rule(baba,you), rule(flag,win), rule(lava,hot), rule(baba,melt), breakable(baba,melt),
        free(baba), obstacle(lava)],
            [won(baba,flag)], baba,
            P).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 14 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
% SURROUNDED WIN OBJ -> FREE
% Introduzione di breakable: altrimenti qua mi prova a rompere una regola
% che non è distruttibile (fatto). Devo aggiungere breakable a tutte le regole precedenti.
% COPPIA DI REGOLE - HOT/MELT
solve_l1(lvl_14,P) :-
    plan([
    phys(baba), phys(wall), phys(flag), phys(keke),
        word(is), word(baba), word(flag), word(wall), word(stop), word(win), word(you), word(keke),
        rule(baba,you), rule(flag,win), rule(wall,stop), rule(baba,melt), breakable(baba,you),
        free(baba), obstacle(wall)],
            [won(keke,flag)], baba,
            P).