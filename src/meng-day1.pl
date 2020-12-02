readFile(Filename, Content) :-
    read_file_to_string(Filename, In, []),
    split_string(In,"\n", "\n", InStrings),
    maplist(number_string, Content, InStrings).

solveN(Filename, Sum, Answer) :-
    readFile(Filename, Input),
    maplist(flip_member(Input),Answer), % Answer is a subset of Input
    sum(Answer, #=, Sum).               % they sum to the goal

flip_member(X,Y) :- member(Y,X).        % needed for maplist




% cardinality 2:
% ?- solveN("input.txt", 2020, [X,Y]), Product is X*Y.
% X = 1743,
% Y = 277,
% Product = 482811 .

% cardinality 3:
% ?- solveN("input.txt", 2020, [X,Y,Z]), Product is X*Y*Z.
% X = 1067,
% Y = 691,
% Z = 262,
% Product = 193171814 .
