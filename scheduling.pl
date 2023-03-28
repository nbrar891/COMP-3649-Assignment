:- consult('helpers.pl').

schedule([], _, []).

schedule([activity(Name, StartRange, EndRange, Dur, _, _, _)|Rest], EndTimes, [activity(Name, StartRange, EndRange, Dur, true, ActualStart, ActualEnd)|ScheduledRest]) :-
    % Check for conflicts with previously scheduled activities
    find_conflict(StartRange, EndTimes, ConflictStart),
    % Calculate a new start time if there is a conflict
    ( ConflictStart =< StartRange
    -> ActualStart = StartRange
    ; ActualStart = ConflictStart
    ),
    % Calculate the end time of the activity
    add_minutes(ActualStart, Dur, ActualEnd),
    % Add the activity to the list of scheduled activities and update the end times
    append(EndTimes, [ActualEnd], NewEndTimes),
    % Recursively schedule the remaining activities
    schedule(Rest, NewEndTimes, ScheduledRest).

% Find the earliest start time for an activity, given the start and end times of previously scheduled activities
find_conflict(StartRange, [], StartRange).

find_conflict(StartRange, [EndRange|Rest], ConflictStart) :-
    % Check for conflicts between the current activity and the previous activity
    EndRange > StartRange,
    % Calculate a new start time that does not overlap with the previous activity
    ConflictStart is max(StartRange, EndRange),
    % Recursively check for conflicts with the remaining activities
    find_conflict(ConflictStart, Rest, _).

find_conflict(_, [_|Rest], ConflictStart) :-
    % Skip over activities that end before the current activity starts
    find_conflict(0, Rest, ConflictStart).