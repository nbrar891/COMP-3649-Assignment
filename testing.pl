generate_start_times_helper(Start, End, Duration, StartTimes) :-
    add_minutes(Start, Duration, EndTime),
    EndTime =< End,
    StartTimes = [Start | RestStartTimes],
    add_minutes(Start, 15, NextStart),
    generate_start_times_helper(NextStart, End, Duration, RestStartTimes).
generate_start_times_helper(_, _, _, []).

generate_start_times(activity(_, StartRange, EndRange, Duration, _, _, _), StartTimes) :-
    generate_start_times_helper(StartRange, EndRange, Duration, StartTimes).

add_minutes(MilitaryTime, MinutesToAdd, Result) :-
    Hours is div(MilitaryTime, 100),
    Minutes is mod(MilitaryTime, 100),
    TotalMinutes is Hours * 60 + Minutes + MinutesToAdd,
    ResultHours is div(TotalMinutes, 60),
    ResultMinutes is mod(TotalMinutes, 60),
    Result is ResultHours * 100 + ResultMinutes.

assign_start_time(activity(Name, StartRange, EndRange, Duration, Allowed, _, _), activity(Name, StartRange, EndRange, Duration, Allowed, ActualStart, ActualEnd)) :-
    generate_start_times(activity(Name, StartRange, EndRange, Duration, Allowed, _, _), StartTimes),
    member(ActualStart, StartTimes),
    add_minutes(ActualStart, Duration, ActualEnd).
