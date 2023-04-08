:- consult('scheduling.pl').
:- consult('helpers.pl').

main(File) :-
(   catch(parse_file(File, Activities, 830, 2230), error(existence_error(source_sink, _), _), fail)
->  allowed_activities(Activities, AllowedActivities),
    schedule(AllowedActivities, NewAct),
    sort_activities_by_end_time(NewAct, Sorted),
    print_activities(Sorted)
;   write('Unable to open file.')
).

% Create a predicate that creates a list of only activities that are allowed
allowed_activities(Activities, AllowedActivities) :-
    allowed_activities(Activities, [], AllowedActivities).

allowed_activities([], Acc, Acc).
allowed_activities([Activity|Rest], Acc, AllowedActivities) :-
    Activity = activity(_, _, _, _, true, _, _),
    append(Acc, [Activity], NewAcc),
    allowed_activities(Rest, NewAcc, AllowedActivities).
allowed_activities([_|Rest], Acc, AllowedActivities) :-
    allowed_activities(Rest, Acc, AllowedActivities).



print_activities([]). 
print_activities([activity(Name, _, _, _, _, ActualStart, ActualEnd)|Rest]) :-
    write('Activity: '), write(Name), nl,
    write('Start Time: '), convert_to_12_hour_format(ActualStart, Start12Hr), write(Start12Hr), nl,
    write('End Time: '), convert_to_12_hour_format(ActualEnd, End12Hr), write(End12Hr), nl,
    nl,
    print_activities(Rest).

parse_file(File, Activities, StartLimit, EndLimit) :-
    % Open the file in read mode
    open(File, read, Stream),
    % Read each line from the file and parse it into an activity
    parse_lines(Stream, Activities, StartLimit, EndLimit),
    % Close the file
    close(Stream).

parse_lines(Stream, [], _, _) :-
    % If we have reached the end of the file, stop parsing
    at_end_of_stream(Stream).

parse_lines(Stream, [Activity|Rest], StartLimit, EndLimit) :-
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
    split_string(DurStr, " ", "", [DurValue, _]),
    atom_number(DurValue, DurNum),
    % Check for errors
    (   error_checking(DurNum, StartRange, EndRange, StartLimit, EndLimit)
    ->  % Create an activity term from the components
        Activity = activity(NameStr, StartRange, EndRange, DurNum, true, 0, 0),
        % Parse the remaining lines from the file
        parse_lines(Stream, Rest, StartLimit, EndLimit)
    ;   % Error occurred, make activity Allowed = false
        Activity = activity(NameStr, StartRange, EndRange, DurNum, false, 0, 0),
        parse_lines(Stream, Rest, StartLimit, EndLimit)
    ).


error_checking(DurNum, StartRange, EndRange, StartLimit, EndLimit) :-
    DurNum > 0,
    EndRange > StartRange,
    subtract_time_minutes(EndRange, StartRange, TimeRange),
    DurNum =< TimeRange,
    StartRange >= StartLimit,
    EndRange =< EndLimit.



