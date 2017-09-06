:- use_module(library(lists)).
gsp(S,G,P) :-
    list_to_set(S,SS),
    list_to_set(G,GS),
    gsp(SS,GS,P,_).

gsp(S,G,[],S) :- holds(G,S).
gsp(S,[G|Gs],P,S1) :-
    holds([G],S), !,
    gsp(S,Gs,P,S1).


gsp(S,[G|Gs],P,S3) :-
    %% find(G,S,O),
    pop(O,Pre,D,A),
    member(G,A),
    gsp(S,Pre,P1,S1),
    apply(S1,A,D,S2),
    gsp(S2,Gs,P2,S3),
    append(P1,[O|P2],P).

find(on(X,Y),_,stack(X,Y)).
find(ontable(X),_,put_down(X)).
find(clear(X),S,unstack(Y,X)).
%% :-member(on(Y,X),S).
find(hold(X),S,unstack(X,Y)).
%% :-member(on(X,Y),S).
find(hold(X),S,pick_up(X)).
%% :-member(ontable(X),S).

%% pre,del,add
pop(stack(X,Y),
    [block(X),block(Y),hold(X),clear(Y)],
    [hold(X),clear(Y)],
    [on(X,Y),ae]).
pop(put_down(X),
    [block(X),hold(X)],
    [hold(X)],
    [ontable(X),ae]).
pop(pick_up(X),
    [block(X),ontable(X),clear(X),ae],
    [ontable(X),ae],
    [hold(X)]).
pop(unstack(X,Y),
    [block(X),block(Y),on(X,Y),clear(X),ae],
    [on(X,Y),ae],
    [hold(X),clear(Y)]).


holds([],_).
holds([Pre|Ps],S) :- select(Pre,S,S1), holds(Ps,S1).
apply(S,A,D,S1) :-
    subtract(S,D,S2), union(S2,A,S1).

list_to_pred([],[]).
state_to_pred([],[ae]).
state_to_pred([H|A],List):-list_to_pred(H,L),state_to_pred(A,P),union(L,P,List).

list_to_pred([X],[ontable(X)|P]):-ltp([X],_,P).
list_to_pred([X|L],[ontable(X)|P]):-ltp([X|L],_,P).

ltp([X],X,[clear(X)]).
ltp([X|L],X,[on(Last,X)|P]):-ltp(L,Last,P).




%% initial_st([ontable(b),on(c,a),ontable(a),clear(b),clear(c)]).
%% goal_st([clear(a),on(a,b),on(b,c),ontable(c)]).
