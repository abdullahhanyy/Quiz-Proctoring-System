assign_proctors(AllTAs, Quizzes, TeachingSchedule, ProctoringSchedule) :-
    free_schedule(AllTAs, TeachingSchedule, FreeSchedule),
    assign_quizzes(Quizzes, FreeSchedule, ProctoringSchedule).
	
free_schedule(_, [], []).

free_schedule(AllTAs, [day(DayName, DaySchedule)|D], [day(DayName, FreeSlots)|F]) :-
    freeslothelper(AllTAs, DayName, DaySchedule, FreeSlots),
    free_schedule(AllTAs, D, F).

freeslothelper(_, _, [], []).

freeslothelper(AllTAs, Day, [I|Slots], [H|T]) :-
    find_free_tas(AllTAs, Day, I, FreeTAs),
    permutation(FreeTAs, H),  
    freeslothelper(AllTAs, Day, Slots, T).

find_free_tas([], _, _, []).

find_free_tas([ta(Name, DayOff)|T1], Day, I, [Name|T2]) :-
    \+ member(Name, I),  
    Day \= DayOff,       
    find_free_tas(T1, Day, I, T2).

find_free_tas([ta(Name, DayOff)|T1], Day, I, FreeTAs) :-
    (member(Name, I) ; Day = DayOff),  
    find_free_tas(T1, Day, I, FreeTAs).

assign_quizzes([], _, []).

assign_quizzes([quiz(Course, Day, Slot, Count) | Quizzes], FreeSchedule, [H | T2]) :-
	member(day(DayName, DaySchedule), FreeSchedule),
	Day = DayName,
	slotchecker(Slot, DaySchedule, M1),
	permutation(M1,M2),
	countchecker(Count, M2, M3),
	H = proctors(quiz(Course, Day, Slot, Count), M3),
	assign_quizzes(Quizzes, FreeSchedule, T2).

slotchecker(1, [H | _], H).

slotchecker(Slot, [_ | T], M1) :-
	Slot > 1,
	Slot1 is Slot - 1,
	slotchecker(Slot1, T, M1).

countchecker(0, _, []).

countchecker(Count, [H | T], [H | T1]) :-
	Count > 0,
	Count1 is Count - 1,
	countchecker(Count1, T, T1).

assign_quiz(quiz(_, Day, Slot, Count),FreeSchedule,AssignedTAs):-
	member(day(DayName, DaySchedule), FreeSchedule),
	Day = DayName,
	slotchecker(Slot, DaySchedule, M1),
	permutation(M1,M2),
	countchecker(Count, M2, AssignedTAs).