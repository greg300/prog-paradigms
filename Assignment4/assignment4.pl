/* YOUR CODE HERE (Prolem 1, delete the following line) */
range(S, E, M) :-
    M =< E, M >= S.  % Make sure M is less than or equal to E or greater than or equal to S.

?- range(1,2,2).
?- not(range(1,2,3)).

/* YOUR CODE HERE (Prolem 2, delete the following line) */
reverseL([], []).  % An empty list is reversed by default.
reverseL([X], [X]).  % A single-item list is reversed by default.
reverseL([H | T], RevX) :-  % General case for reversing a list with length > 1.
    reverseL(T, RevT),  % Reverse the tail.
    append(RevT, [H], RevX).  % Append the head to the reversed tail.

?- reverseL([],X).
?- reverseL([1,2,3],X).
?- reverseL([a,b,c],X).

/* YOUR CODE HERE (Prolem 3, delete the following line) */
memberL(X, []) :- false.  % X cannot be in the empty list.
memberL(X, [X | T]).  % X is the first element in the list.
memberL(X, [H | T]) :-   % For a nonempty list:
    memberL(X, T).  % Check whether X is in the sublist of the rest of the elements of L.


?- not(memberL(1, [])).
?- memberL(1,[1,2,3]).
?- not(memberL(4,[1,2,3])).
?- memberL(X, [1,2,3]).

/* YOUR CODE HERE (Prolem 4, delete the following line) */
zip([], [], []).
zip([XH | XT], [], []).
zip([], [YH | YT], []).
zip([XH | XT], [YH | YT], XYs) :-
    zip(XT, YT, XYsNew),
    append([XH-YH], XYsNew, XYs).

?- zip([1,2],[a,b],Z).
?- zip([a,b,c,d], [1,X,y], Z).
?- zip([a,b,c],[1,X,y,z], Z).
?- length(A,2), length(B,2), zip(A, B, [1-a, 2-b]).

/* YOUR CODE HERE (Prolem 5, delete the following line) */
insert(X, [], [X]).
insert(X, [YH | YT], [X | [YH | YT]]) :-
    X =< YH.
insert(X, [YH | YT], [YH | Ws]) :-
    X > YH,
    insert(X, YT, Ws).

?- insert(3, [2,4,5], L).
?- insert(3, [1,2,3], L).
?- not(insert(3, [1,2,4], [1,2,3])).
?- insert(3, L, [2,3,4,5]).
?- insert(9, L, [1,3,6,9]).
?- insert(3, L, [1,3,3,5]).

/* YOUR CODE HERE (Prolem 6, delete the following line) */
remove_duplicates(Xs, Ys) :-
    reverseL(Xs, RevXs),
    remove_duplicates_aux(RevXs, RevYs),
    reverseL(RevYs, Ys).

remove_duplicates_aux([], []).
remove_duplicates_aux([XH | XT], Ys) :-
    memberL(XH, XT),
    remove_duplicates_aux(XT, Ys).
remove_duplicates_aux([XH | XT], [XH | Ys]) :-
    remove_duplicates_aux(XT, Ys).

?- remove_duplicates([1,2,3,4,2,3],X).
?- remove_duplicates([1,4,5,4,2,7,5,1,3],X).
?- remove_duplicates([], X).

/* YOUR CODE HERE (Prolem 7, delete the following line) */
% Need to remove duplicates!
intersectionL(L1, L2, L3) :-
    intersectionL_aux(L1, L2, DupL3),
    remove_duplicates(DupL3, L3).

intersectionL_aux([], _, []).  % Base case.
intersectionL_aux([H1 | T1], L2, [H1 | Y]) :-  % H1 is in the intersection.
    memberL(H1, L2),
    intersectionL_aux(T1, L2, Y).
intersectionL_aux([H1 | T1], L2, Y) :-  % H1 is not in the intersection.
    intersectionL_aux(T1, L2, Y).

?- intersectionL([1,2,3,4],[1,3,5,6],[1,3]).
?- intersectionL([1,2,3,4],[1,3,5,6],X).
?- intersectionL([1,2,3],[4,3],[3]).

/* YOUR CODE HERE (Prolem 8, delete the following line) */
prefix(P,L) :- append(P,_,L).
suffix(S,L) :- append(_,S,L).

len([], 0).
len([H | T], N) :-
    len(T, M), N is M+1.

partition([], [], []).
partition([H], [H], []).
partition(L, P, S) :-
    append(P, S, L),
    len(L, N),
    DivN is div(N, 2),
    prefix(P, L),
    len(P, DivN),
    suffix(S, L),
    len(S, SLen),
    SLen is N - DivN.

?- partition([a],[a],[]).
?- partition([1,2,3],[1],[2,3]).
?- partition([a,b,c,d],X,Y).

/* YOUR CODE HERE (Prolem 9, delete the following line) */
merge([], Y, Y).
merge(X, [], X).
merge([XH | XT], [YH | YT], [XH | Z]) :-
    XH =< YH,
    merge(XT, [YH | YT], Z).
merge([XH | XT], [YH | YT], [YH | Z]) :-
    YH =< XH,
    merge([XH | XT], YT, Z).

?- merge([],[1],[1]).
?- merge([1],[],[1]).
?- merge([1,3,5],[2,4,6],X).

/* YOUR CODE HERE (Prolem 10, delete the following line) */
mergesort([], []).
mergesort([X], [X]).
mergesort(L, SL) :-
    partition(L, P, S),
    mergesort(P, SP),
    mergesort(S, SS),
    merge(SP, SS, SL).

?- mergesort([3,2,1],X).
?- mergesort([1,2,3],Y).
?- mergesort([],Z).
