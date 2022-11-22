solve_level(Level,SubGoals) :- 
    consult('examples/baba_l1_1.0.pl'),
    solve_l1(Level, SubGoals),

    flatten(SubGoals, FlatGoals),

    %write('High: '),write(FlatGoals),nl,nl,
    intermediate_planner(Level,FlatGoals, []).



intermediate_planner(Level,[Goal|Tail], PrevActions):-
    consult('examples/baba_l2_1.0.pl'),
    %write('Medium: '),write(Goal),nl,nl,
    solve_l2(Level, SubGoals, Goal),
    flatten(SubGoals, FlatSubGoals),
    %write('Medium result: '),write(FlatSubGoals),nl,nl,
    
    low_planner(Level,FlatSubGoals,PrevActions, PostLevelActions),
    intermediate_planner(Level,Tail,PostLevelActions).
intermediate_planner(Level, [], PrevActions).



low_planner(Level,[Goal|Tail], PrevActions, PostLevelActions):-
    consult('examples/baba_l4_1.1.pl'),
    %write('Low: '),write(Goal),nl,nl,
    %write('Prev Actions: '),write(PrevActions),nl,nl,
    solve_l4(Level, Plan, Goal, PrevActions),
    %write(Plan),nl,nl,
    flatten(Plan, Actions),
    append(PrevActions, Actions, CurrentActions),
    %write(CurrentActions),nl,nl, % QUI SI VEDE L'ELENCO DELLE AZIONI
    low_planner(Level,Tail, CurrentActions, PostLevelActions).
low_planner(Level,[], PostLevelActions, PostLevelActions).