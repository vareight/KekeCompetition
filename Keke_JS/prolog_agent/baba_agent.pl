:- dynamic prioritary/1.
:- dynamic pos/3.

:- retractall(prioritary(_)).
:- retractall(pos(_,_,_)).

solve_level(Level,SubGoals) :- 
    consult('solvers/baba_l1_1.1.pl'),
    %write('**************** L-01 *************'),nl,nl,
    solve_l1(Level, SubGoals),

    flatten(SubGoals, FlatGoals),

    %write('L01: '),write(FlatGoals),nl,nl,
    planner_l2(Level,FlatGoals, [], [], []).



planner_l2(Level,[Goal|Tail], PrevActionsL2, PrevActionsL3, PrevActionsL4):-
    consult('solvers/baba_l2_1.0.pl'),
    %write('**************** L-02 *************'),nl,nl,
    %write('GOAL: '),write(Goal),nl,nl,
    %write('PREV: '),write(PrevActionsL2),nl,nl,
    solve_l2(Level, SubGoals, Goal, PrevActionsL2),
    flatten(SubGoals, FlatSubGoals),
    %write('L02: '),write(FlatSubGoals),nl,nl,
    append(PrevActionsL2, FlatSubGoals, CurrentActionsL2),
    
    planner_l3(Level,FlatSubGoals,PrevActionsL3, PrevActionsL4, PostLevelActionsL3, PostLevelActionsL4),
    planner_l2(Level,Tail,CurrentActionsL2,PostLevelActionsL3, PostLevelActionsL4).
planner_l2(Level, [], PostLevelActionsL2, PostLevelActionsL3, PostLevelActionsL4).


planner_l3(Level,[Goal|Tail], PrevActionsL3, PrevActionsL4, PostLevelActionsL3, FinalActionsL4):-
    consult('solvers/baba_l3_1.2.pl'),
    %write('**************** L-03 *************'),nl,nl,
    %write('PREV: '),write(PrevActionsL3),nl,nl,
    %write('GOAL: '),write(Goal),nl,nl,
    solve_l3(Level, SubGoals, Goal, PrevActionsL3, FullActions),
    %write(Plan),nl,nl,
    flatten(SubGoals, FlatSubGoals),
    %write('L03: '),write(FullActions),nl,nl,
    flatten(FullActions, FlatFullActions),

    append(PrevActionsL3, FlatFullActions, CurrentActionsL3),
    %write('L03 current actions: '),write(FlatFullActions),nl,nl, % QUI SI VEDE L'ELENCO DELLE AZIONI

    planner_l4(Level,FlatSubGoals,PrevActionsL4, PostLevelActionsL4),
    planner_l3(Level,Tail, CurrentActionsL3, PostLevelActionsL4, PostLevelActionsL3, FinalActionsL4).
planner_l3(Level,[], PostLevelActionsL3, PostLevelActionsL4, PostLevelActionsL3, PostLevelActionsL4).


planner_l4(Level,[Goal|Tail], PrevActionsL4, PostLevelActionsL4):-
    consult('solvers/baba_l4_1.1.pl'),
    %write('**************** L-04 *************'),nl,nl,
    %write('PREV: '),write(PrevActionsL4),nl,nl,
    %write('GOAL: '),write(Goal),nl,nl,
    solve_l4(Level, SubGoals, Goal, PrevActionsL4),
    %write(Plan),nl,nl,
    flatten(SubGoals, FlatSubGoals),
    %write('L04: '),write(FlatSubGoals),nl,nl,
    append(PrevActionsL4, FlatSubGoals, CurrentActionsL4),
    %write(CurrentActions),nl,nl, % QUI SI VEDE L'ELENCO DELLE AZIONI
    planner_l4(Level,Tail, CurrentActionsL4, PostLevelActionsL4).
planner_l4(Level,[], PostLevelActionsL4, PostLevelActionsL4).