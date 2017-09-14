:- use_module(library(lists)).
:- set_prolog_flag(answer_write_options,[quoted(true),portray(true),spacing(next_argument)]).


%% States are sets of predicates(don't want to repeat predicates)
%% Start, Goal, Plan
gsp(Start,Goal,Plan) :-
    list_to_set(Start,SS),
    list_to_set(Goal,GS),
    gsp(SS,GS,Plan,_).

gsp(S,G,[],S) :- holds(G,S).
%% Goal state should be a substate of S1
%% Cut takes care of multiple solution
gsp(S,[G|Gs],P,S1) :-
    holds([G],S),!,
    gsp(S,Gs,P,S1).


gsp(S,[G|Gs],Plan,S4):-
%% write(S),write(G),nl,
    %% Find operator O to get from start state S to satisfy sub goal G
    find(G,S,Op),
    %% Get Pre, Del, Add
    operator(Op,Pre,Del,Add),
    %% After satisfying Pre, state S1 comes
    gsp(S,Pre,P1,S1),

    apply(S1,Add,Del,S2),
    gsp(S2,Gs,P2,S3),

    gsp(S3,[G],P3,S4),
    append(P1,[Op|P2],P_),
    append(P_,P3,Plan).

%% find Y such that on(X,Y) is true
findon(X,[on(X,Y)|_],Y).
findon(X,[_|L],Y):-findon(X,L,Y).

%% find X such that hold(X) is true
findholding([hold(X)|_],X).
findholding([_|L],Y):-findholding(L,Y).

%% find Y such that on(Y,X) is true
findon2(X,[on(Y,X)|_],Y).
findon2(X,[_|L],Y):-findon2(X,L,Y).

%% find operator to satisfy each predicate
find(on(X,Y),_,stack(X,Y)).
find(ontable(X),_,put_down(X)).
find(clear(X),S,unstack(Y,X)):-findon2(X,S,Y).
find(hold(X),S,unstack(X,Y)):-findon(X,S,Y).
find(hold(X),_,pick_up(X)).
find(ae,S,put_down(Y)):-findholding(S,Y).


%% pre,del,add
operator(stack(X,Y),
    [clear(Y),hold(X)],
    [hold(X),clear(Y)],
    [on(X,Y),ae]).
operator(put_down(X),
    [hold(X)],
    [hold(X)],
    [ontable(X),ae]).
operator(pick_up(X),
    [ontable(X),clear(X),ae],
    [ontable(X),ae],
    [hold(X)]).
operator(unstack(X,Y),
    [on(X,Y),clear(X),ae],
    [on(X,Y),ae],
    [hold(X),clear(Y)]).

%% hold(arg1,arg2) :- Check if arg1 is a subset of arg2
holds([],_).
holds([Pre|Ps],S) :- select(Pre,S,S1), holds(Ps,S1).

%% Delets Del from S and adds Add to S.
apply(S,Add,Del,S1):-subtract(S,Del,S2), union(S2,Add,S1).

%% Generate predicates for input configuration
state_to_predicate([],[ae]).
state_to_predicate([H|A],List):-list_to_predicate(H,L),state_to_predicate(A,P),union(L,P,List).

%% Generate predicates for one 'tower'
list_to_predicate([],[]).
list_to_predicate([X],[ontable(X)|P]):-ltp([X],_,P).
list_to_predicate([X|L],[ontable(X)|P]):-ltp([X|L],_,P).
ltp([X],X,[clear(X)]).
ltp([X|L],X,[on(Last,X)|P]):-ltp(L,Last,P).


solve(Initial,Goal,Plan):-state_to_predicate(Initial,Initial_List),state_to_predicate(Goal,Goal_List),
						write(Initial_List),nl,write(Goal_List),nl,
						gsp(Initial_List,Goal_List,Plan),!.%% Cut to give first solution only

%%Queries
?- solve([[x,y],[z,w]],[[x,z],[w,y]],P),write(P),nl,nl.
?- solve([[x,y],[w,z]],[[z,x],[w,y]],P),write(P),nl,nl.
?- solve([[x,y],[z],[w]],[[z,x],[y,w]],P),write(P),nl,nl.
?- solve([[a],[c,e],[b,d]],[[e],[c,a],[b,d]],P),write(P),nl,nl.
%% Sussman anomaly
?- solve([[b],[a,c]],[[c,b,a]],P),write(P),nl,nl.
