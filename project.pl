block(bl1).
block(bl2).
block(bl3).
block(bl4).
block(bl5).
block(bl6).
block(bl7).
block(bl8).
block(bl9).
block(bl10).
block(bl11).
block(bl12).
block(bl13).
block(bl14).

time(0).
time(1).
time(2).

has(human, bl1, 0).
has(human, bl2, 0).
has(human, bl3,0).
has(human,bl4,0).
has(human,bl5,0).

has(robot,bl6,0).
has(robot,bl7,0).
has(robot,bl8,0).
has(robot,bl9,0).
has(robot,bl10,0).
has(robot,bl11,0).

color(bl1,red).
color(bl2,brown).
color(bl3,brown).
color(bl4,green).
color(bl5,blue).

color(bl6,red).
color(bl7,red).
color(bl8,blue).
color(bl9,blue).
color(bl10,green).
color(bl11,green).
color(bl12,red).
color(bl13,blue).
color(bl14,blue).

ontable(bl12,0).

ontable(bl13,0).
on(bl13,bl14,0).

%% Rule to put a block on top of another block
on(X, Y, T+1):-
	time(T),
	occ(put(X, Y), T).
on(X, Y, T+1):-
	time(T),
	on(X, Y, T),
	#count{Z:occ(put(Z, Y), T)}==0.
on(X, Y, T+1):-
	time(T),
	on(X, Y, T),
	not occ(putontable(Y), T).

%% Rule to put a block on top of table
ontable(X, T+1):-
	time(T),
	occ(putontable(X), T).
ontable(X, T+1):-
	time(T),
	ontable(X, T),
	#count{Z:occ(put(Z, X), T)}==0.

%% Action rules for putting the blocks
action(putontable(X)) :-
	block(X).
action(put(X,Y)) :-
	block(X),
	block(Y).

%% position check rules
occupied(X,T) :-
	time(T),
	block(X),
	block(Y),
	X!=Y,
	on(X, Y, T).

clear(X,T) :-
	time(T),
	block(X),
	not occupied(X, T).

above(X,Y,T) :-
	time(T),
	on(X,Y,T).
above(X,Y,T) :-
	time(T),
	on(Z,Y,T),
	above(X,Z,T).

%% Actions when to be executed by the Actor - Robot/Human
executable(putontable(X),T):-
	time(T),
	action(putontable(X)),
	clear(X,T).
executable(put(X,Y),T):-
	time(T),
	action(put(X,Y)),
	clear(X,T),
	clear(Y,T).

%% Action Generation Rules
1 {occ(A, T) : action(A)} 1 :-
	time(T), T < n.

:- occ(A, T), not executable(A, T).

%% Goal conditions
goal_cond(S, is, stack) :-
	block(S).
goal_cond(S, color, blue) :-
	block(S).
goal_cond(S, height, same) :-
	block(S).
goal_cond(S, type, another) :-
	block(S).

%% Goal Satisfaction
%% not_satisfied_goal(S, T) :-
%% 	block(S),
%% 	time(T),
%% 	goal_cond(X,Y,Z),
%% 	not satisfied(X,Y,Z,T).

%% satisfied_goal(S, T) :-
%% 	block(S),
%% 	time(T),
%% 	not not_satisfied_goal(S, T).

%% :- X = #count{S : satisfied_goal(S, n)}, X==0.

%% Condition to check satisfaction for different goal conditions
satisfied(S,is,stack,T) :-
	block(S),
	time(T),
	clear(S,T).
satisfied(S,color,blue,T) :-
	block(S),
	time(T),
	color(S,blue),
	clear(S,T),
	#count{U:above(U,S,T), not color(U,blue)}==0.
satisfied(S,height,same,T):-
	satisfied(S,is,stack,T),
	satisfied(U,is,stack,T),
	satisfied(U,color,blue,T),
	S != U,
	unchanged(U, T),
	clear(S, T),
	clear(U, T),
	same_height(S, U, T).
satisfied(S,type,another,T) :-
	block(U),
	unchanged(U,T),
	same_height(S,U,T).

%% Rules to decipher same height phrase for a given stack
same_height(S, U, T) :-
	block(S),
	block(U),
	S!=U,
	ontable(S, T),
	ontable(U, T).
same_height(S, U, T) :-
	block(S),
	block(U),
	S!=U,
	on(S1, S, T),
	on(U1, U, T),
	same_height(S1, U1, T).

%% Block U was not changed
unchanged(U,T) :-
	time(T),
	block(U),
	not changed(U,T).

%% Block U was changed
changed(U,T) :-
	time(T),
	T>0,
	block(U),
	above(V,U,0),
	not above(V,U,T).
changed(U,T) :-
	time(T),
	T > 0,
	block(U),
	ontable(U,0),
	not ontable(U,T).
changed(U,T) :-
	time(T),
	T > 0,
	block(U),
	not ontable(U,0),
	ontable(U,T).
changed(U,T) :-
	time(T),
	T > 0,
	block(U),
	not ontable(U,0),
	on(X,U,T).
