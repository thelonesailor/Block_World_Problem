:- set_prolog_flag(answer_write_options,[quoted(true),portray(true),spacing(next_argument)]).

%% append two lists
append([], L, L).
append([A|L1], L2, [A|L]) :- append(L1, L2, L).

in_list(A, [B|L]) :- A \= B, in_list(A, L).
in_list(A, [A|L]).

union([], L, L).
%% X is in L2
union([X|L1], L2, L) :- in_list(X, L2), union(L1, L2, L).
%% X is not in L2
union([X|L1], L2, [X|L]) :- \+ in_list(X, L2), union(L1, L2, L).


subtract([], Ls, []).
subtract([X|L1], L2, L) :- in_list(X, L2), subtract(L1, L2, L).
subtract([X|L1], L2, [X|L]) :- \+ in_list(X, L2), subtract(L1, L2, L).