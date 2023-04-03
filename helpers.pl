sort_activities_by_end_time(Activities, SortedActivities) :-
    predsort(sort_by_end_time, Activities, SortedActivities).

sort_by_end_time(Order, activity(_, _, _, _, _, ActualStart1, ActualEnd1), activity(_, _, _, _, _, ActualStart2, ActualEnd2)) :-
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

subtract_time_minutes(Time1, Time2, Minutes) :-
    Hour1 is Time1 // 100,       % Get the hour part of Time1
    Minute1 is Time1 mod 100,    % Get the minute part of Time1
    Hour2 is Time2 // 100,       % Get the hour part of Time2
    Minute2 is Time2 mod 100,    % Get the minute part of Time2
    Minutes is (Hour1 - Hour2) * 60 + Minute1 - Minute2.  % Calculate the difference in minutes

% This predicate is used to convert a 24-hour time to 12-hour time
convert_to_12_hour_format(Time24Hr, Time12Hr) :-
    Hour is integer(Time24Hr / 100),
    Minutes is Time24Hr mod 100,
    (Hour >= 12 -> Hour12 is Hour - 12, TimePeriod = 'pm' ; Hour12 = Hour, TimePeriod = 'am'),
    (Hour12 =:= 0 -> Hour12_12 is 12 ; Hour12_12 = Hour12),
    format(string(Time12Hr), '~|~`0t~d~2+:~|~`0t~d~2+~w', [Hour12_12, Minutes, TimePeriod]).

