:- consult('helpers.pl').

generate_start_times_helper(Start, End, Duration, StartTimes) :-
    add_minutes(Start, Duration, EndTime),
    EndTime =< End,
    StartTimes = [Start | RestStartTimes],
    add_minutes(Start, 5, NextStart), % can modify time added for more granularity
    generate_start_times_helper(NextStart, End, Duration, RestStartTimes),
    !. % List is fully generated, stop backtracking
generate_start_times_helper(_, _, _, []).

generate_start_times(activity(_, StartRange, EndRange, Duration, _, _, _), StartTimes) :-
    generate_start_times_helper(StartRange, EndRange, Duration, StartTimes).

assign_start_time(activity(Name, StartRange, EndRange, Duration, _, _, _), activity(Name, StartRange, EndRange, Duration, _, ActualStart, ActualEnd)) :-
    generate_start_times(activity(Name, StartRange, EndRange, Duration, _, _, _), StartTimes),
    member(ActualStart, StartTimes),
    add_minutes(ActualStart, Duration, ActualEnd).

schedule(Activities, ScheduledActivities) :-
    schedule_helper(Activities, [], ScheduledActivities).

schedule_helper([], ScheduledActivities, ScheduledActivities).
schedule_helper([Activity|Rest], PreviousActivities, ScheduledActivities) :-
    assign_start_time(Activity, ScheduledActivity),
    conflict_check(ScheduledActivity, PreviousActivities),
    append([ScheduledActivity], PreviousActivities, UpdatedPreviousActivities),
    schedule_helper(Rest, UpdatedPreviousActivities, ScheduledActivities).

conflict_check(_, []).
conflict_check(Activity, [OtherActivity|Rest]) :-
    \+ conflict_found(Activity, OtherActivity),
    conflict_check(Activity, Rest).

conflict_found(activity(_, _, _, _, _, Start1, End1), activity(_, _, _, _, _, Start2, End2)) :-
    Start1 < End2,
    End1 > Start2.