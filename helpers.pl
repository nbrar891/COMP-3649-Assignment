sort_activities_by_end_time(Activities, SortedActivities) :-
    predsort(sort_by_end_time, Activities, SortedActivities).

sort_by_end_time(Order, activity(_, _, _, _, _, ActualStart1, ActualEnd1), activity(_, _, _ ,_, _, ActualStart2, ActualEnd2)) :-
    compare(Order1, ActualEnd1, ActualEnd2),
    (   Order1 = (=)
    ->  compare(Order2, ActualStart1, ActualStart2),
        Order = Order2
    ;   Order = Order1
    ).

add_minutes(MilitaryTime, MinutesToAdd, Result) :-
    Hours is div(MilitaryTime, 100),
    Minutes is mod(MilitaryTime, 100),
    TotalMinutes is Hours * 60 + Minutes + MinutesToAdd,
    ResultHours is div(TotalMinutes, 60),
    ResultMinutes is mod(TotalMinutes, 60),
    Result is ResultHours * 100 + ResultMinutes.