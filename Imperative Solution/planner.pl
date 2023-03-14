main(File) :-
    % Parse the file and get the list of activities
    parse_file(File, Activities),
    % Print each activity
    print_activities(Activities).

print_activities([]).
print_activities([activity(Name, Start, End, Dur, Allowed, ActualStart, ActualEnd)|Rest]) :-
    % Print the activity name, start and end times, and duration
    format("Activity: ~w, Start: ~w, End: ~w, Duration: ~w, Allowed: ~w, ActualStart: ~w, ActualEnd: ~w~n", [Name, Start, End, Dur,Allowed, ActualStart, ActualEnd]),
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
    atom_number(StartStr, StartTime),
    atom_number(EndStr, EndTime),
    % Extract the duration value from the duration string
    split_string(DurStr, " ", "", [DurValue,_]),
    atom_number(DurValue, DurNum),
    % Ignore the duration unit and create an activity term from the components
    Activity = activity(NameStr, StartTime, EndTime, DurNum, true, 0, 0),
    % Parse the remaining lines from the file
    parse_lines(Stream, Rest).

