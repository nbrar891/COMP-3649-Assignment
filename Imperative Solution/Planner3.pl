main(File) :-
    % Parse the file and get the list of activities
    parse_file(File, Activities),
    schedule(Activities, NewAct),
    sort_activities_by_end_time(NewAct, Sorted),
    print_activities(Sorted).


print_activities([]).

print_activities([activity(_, _, _, _, Allowed, ActualStart, ActualEnd)|Rest]) :-
    % Print the allowed, actualstart, and actualend fields
    format("Allowed: ~w, ActualStart: ~w, ActualEnd: ~w~n", [Allowed, ActualStart, ActualEnd]),
    print_activities(Rest).


parse_file(File, Activities) :-
    % Open the file in read mode
    open(File, read, Stream),
    % Read each line from the file and parse it into an activity
    parse_lines(Stream, Activities),
    % Close the file
    close(Stream).

parse_lines(Stream, []) :-
    % If we have reached the end of the file, stop parsing
    at_end_of_stream(Stream).

parse_lines(Stream, [Activity|Rest]) :-
    % Read the next line from the file
    read_line_to_codes(Stream, Line),
    % Convert the line from a list of ASCII codes to a string
    atom_codes(Atom, Line),
    % Split the string into a list of components using the "-" delimiter
    split_string(Atom, "-", "", [NameStr, StartStr, EndStr, DurStr|_]),
    % Convert the start and end times from strings to Prolog number
    atom_number(StartStr, StartRange),
    atom_number(EndStr, EndRange),
    % Extract the duration value from the duration string
    split_string(DurStr, " ", "", [DurValue,_]),
    atom_number(DurValue, DurNum),
    % Ignore the duration unit and create an activity term from the components
    Activity = activity(NameStr, StartRange, EndRange, DurNum, true, 0, 0),
    % Parse the remaining lines from the file
    parse_lines(Stream, Rest).

sort_activities_by_end_time(Activities, SortedActivities) :-
    predsort(sort_by_end_time, Activities, SortedActivities).

sort_by_end_time(Order, activity(_, _, _, _, _, ActualStart1, ActualEnd1), activity(_, _, _ ,_, _, ActualStart2, ActualEnd2)) :-
    compare(Order1, ActualEnd1, ActualEnd2),
    (   Order1 = (=)
    ->  compare(Order2, ActualStart1, ActualStart2),
        Order = Order2
    ;   Order = Order1
    ).


schedule([], []).

schedule([activity(Name, StartRange, EndRange, Dur, _, 0, 0)|Rest], [activity(Name, StartRange, EndRange, Dur, true, ActualStart, ActualEnd)|ScheduledRest]) :-
  find_earliest_start_time(StartRange, Rest, EarliestStart),
  ActualStart is max(StartRange, EarliestStart),
  % ActualEnd is ActualStart + Dur,
  add_minutes(ActualStart, Dur, ActualEnd),
  % Recursively schedule the remaining activities
  schedule(Rest, ScheduledRest).

% Find the earliest start time for an activity, given the start and end times of previously scheduled activities
find_earliest_start_time(EndRange, [], EndRange).

find_earliest_start_time(EndRange, [activity(_, StartRange, EndRange, Dur)|Rest], EarliestStart) :-
    StartRange - EndRange >= Dur, % Ensure there is enough time between activities
    find_earliest_start_time(EndRange, Rest, NextEarliestStart),
    EarliestStart is max(StartRange, NextEarliestStart).

find_earliest_start_time(EndRange, [_|Rest], EarliestStart) :-
    find_earliest_start_time(EndRange, Rest, EarliestStart).


add_minutes(MilitaryTime, MinutesToAdd, Result) :-
    Hours is div(MilitaryTime, 100),
    Minutes is mod(MilitaryTime, 100),
    TotalMinutes is Hours * 60 + Minutes + MinutesToAdd,
    ResultHours is div(TotalMinutes, 60),
    ResultMinutes is mod(TotalMinutes, 60),
    Result is ResultHours * 100 + ResultMinutes.
