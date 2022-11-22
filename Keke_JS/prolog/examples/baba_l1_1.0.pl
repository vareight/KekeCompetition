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

% *** WORDS OBJECTS ***
word(baba).
word(flag).
word(is).
word(you).
word(win).
word(love).
word(grass).
word(rock).

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
can(won(You,Phys), [rule(You,you), rule(Phys,win), phys(You), phys(Phys)], baba).

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
can(destroyRule(Phys, Rule),[rule(Phys,Rule), word(Phys), word(Rule), phys(Phys)], baba).

adds(destroyRule(Phys, Rule),[], _, baba):-
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
% *** destroyRule(Word1, Connector, Word2) *****
% **********************************************
can(transform(Phys1, Phys2),[rule(Phys1,Phys2), word(Phys1), word(Phys2), phys(Phys1), mutable(Phys1)], baba).

adds(transform(Phys1, Phys2),[phys(Phys2)], _, baba):-
	word(Phys1),
    word(Phys2),
    phys(Phys1).

deletes(transform(Phys1, Phys2),[phys(Phys1)], baba):-
	word(Phys1),
    word(Phys2),
    phys(Phys1).


% ***************
% *** LEVELS ****
% ***************

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 01 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
solve_l1(lvl_01,P) :-
	plan([phys(baba), phys(flag), word(is), word(baba), word(flag), word(win), word(you),
            rule(baba,you), rule(flag,win)],
            [won(baba,flag)], baba,
	        P).

% è uguale perchè diciamo solo di creare la regola, non come.
% Nel lvl_01 è già fatto mentre qua bisogna effettuare delle mosse, ma a livello logico bisogna solo creare la regola
%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 02 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
solve_l1(lvl_02,P) :-
    plan([phys(baba), phys(flag), word(is), word(baba), word(flag), word(win), word(you),
            rule(baba,you)],
            [won(baba,flag)], baba,
            P).

lvl_03(P) :-
    plan([phys(baba), word(is), word(baba), word(win), word(you),
            rule(baba,you)],
            [won(baba,baba)], baba,
            P).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% LEVEL 04 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
solve_l1(lvl_04,P) :-
    plan([
        phys(baba), phys(flag), phys(love), phys(grass),
        word(is), word(baba), word(flag), word(love), word(grass), word(win), word(you),
        rule(baba,win), rule(grass,win), rule(flag,you), rule(love,you)],
            [won(love,grass)], baba,
            P).

solve_l1(lvl_05,P) :-
    plan([
        phys(baba), phys(rock), mutable(rock),
        word(is), word(baba), word(flag), word(rock), word(win), word(you),
        rule(baba,you), rule(flag,win)],
            [won(baba,flag)], baba,
            P).

% ROCK IS ROCK -> mutable
lvl_06(P) :-
    plan([
        phys(baba), phys(rock),
        word(is), word(baba), word(flag), word(rock), word(win), word(you),
        rule(baba,you), rule(flag,win), rule(rock,rock), rule(rock,flag)],
            [won(baba,flag)], baba,
            P).