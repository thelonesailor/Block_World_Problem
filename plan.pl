
%% take initial state and goal state
%% make stack with goal states
%% if top predicate true pop else replace by operator(and precondotions) to make it true
%% if top is an operator add to list of plans

%% predicates
%% ont(X).
%% on(X,Y).
%% CL(X).
%% HOLD(X).

%% operators
%% S(X,Y)
%% US(X,Y)
%% PU(X)
%% PD(X)

%% opn(operator,pre,del,add).

opn(s(X,Y),[cl(X),hold(X)],[cl(X),hold(X)],[on(X,Y),hold(-1)]).
opn(us(X,Y),[cl(X),on(X,Y),hold(-1)],[on(X,Y),hold(-1)],[cl(Y),hold(X)]).
opn(pu(X),[cl(X),ont(X),hold(-1)],[cl(X),hold(X)],[on(X,Y),hold(-1)]).
opn(pd(X,Y),[cl(X),hold(X)],[cl(X),hold(X)],[on(X,Y),hold(-1)]).

solve(State, Goal, Plan, Plan):-is_subset(Goal, State).

on(monkey,floor),on(box,floor),at(monkey,a),
at(box,b),at(bananas,c),status(bananas,hanging)
solve(State, Goal, Sofar, Plan):-
% get first operator
notmember(Op, Sofar),
% check for looping
is_subset(Precons, State),
% check preconditions hold
delete_list(Delete, State, Remainder), % delete old facts
append(Add, Remainder, NewState),
% add new facts
solve(NewState, Goal, [Op|Sofar], Plan). % recurse