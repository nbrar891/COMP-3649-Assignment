main(File) :-
    % Parse the file and get the list of activities
    parse_file(File, Activities),
    sort_activities_by_end_time(Activities, SortedActivities),
    % Print each activity
    print_activities(SortedActivities).

print_activities([]).
print_activities([activity(Name, StartRange, EndRange, Dur, Allowed, ActualStart, ActualEnd)|Rest]) :-
    % Print the activity name, start and end times, and duration
    format("Activity: ~w, StartRange: ~w, EndRange: ~w, Duration: ~w, Allowed: ~w, ActualStart: ~w, ActualEnd: ~w~n", [Name, StartRange, EndRange, Dur,Allowed, ActualStart, ActualEnd]),
    % Recursively print the remaining activities
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

sort_by_end_time(Order, activity(_, _, EndTime1, _, _, _, _), activity(_, _, EndTime2, _, _, _, _)) :-
    compare(Order, EndTime1, EndTime2).
