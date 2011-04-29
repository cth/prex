%% Working in non-prism Mar 3 22:35

noisy(In,Out) :-
	length(In,InL),
	noisy_max([InL,InL,InL],In,Out).

noisy_max([MaxMut,MaxDel,MaxIns],In,Out) :-
	retractall(noisy_store(_)),
	noisy_forward_store([[],[[MaxMut,MaxDel,MaxIns],[0,0,0]]]),
	noisy_rec(In,Out).

noisy_rec([],[]).

noisy_rec(In,Out) :-
	msw(noisy_action,Action),
	noisy_action(Action,In,Out).

noisy_action(mutate,[I|In],[O|Out]) :-
%	write(noisy_action(mutate,[I|In],[O|Out])),nl,
	noisy_get_store([As,[[MaxMut,MaxDel,MaxIns],[Mut,Del,Ins]]]),%
%	noisy_get_store([[As,[[MaxMut,MaxDel,MaxIns],[Mut,Del,Ins]]],_]),
	msw(mutate(I),O),
	((O == I) ->
		noisy_forward_store([[match|As],[[MaxMut,MaxDel,MaxIns],[Mut,Del,Ins]]])
		;
		Mut < MaxMut,
		NextMut is Mut + 1,
		noisy_forward_store([[mutate|As],[[MaxMut,MaxDel,MaxIns],[NextMut,Del,Ins]]])),
	noisy_rec(In,Out).
	
noisy_action(insert,In,[O|Out]) :-
%	write(noisy_action(insert,In,[O|Out])),nl,
	noisy_get_store([Actions,[[MaxMut,MaxDel,MaxIns],[Mut,Del,Ins]]]),
%	noisy_get_store([[Actions,[[MaxMut,MaxDel,MaxIns],[Mut,Del,Ins]]],_]),
	((Actions == [delete|_]) -> fail ; true),
	Ins < MaxIns,
	NextIns is Ins + 1,
	msw(insert,O),
	noisy_forward_store([[insert|Actions],[[MaxMut,MaxDel,MaxIns],[Mut,Del,NextIns]]]),
	noisy_rec(In,Out).

noisy_action(delete,[_|In],Out) :-
%	write(noisy_action(delete,[_|In],Out)),nl,
	noisy_get_store([Actions,[[MaxMut,MaxDel,MaxIns],[Mut,Del,Ins]]]),
	((Actions == [insert|_]) -> fail ; true),
	Del < MaxDel,
	NextDel is Del + 1,
	noisy_forward_store([[delete|Actions],[[MaxMut,MaxDel,MaxIns],[Mut,NextDel,Ins]]]),
	noisy_rec(In,Out).

noisy_forward_store(S) :- asserta(noisy_store(S)).
noisy_forward_store(S) :- retract(noisy_store(S)), fail.
noisy_get_store(S) :- noisy_store(S), !. 

%noisy_forward_store(S) :- write(assert_store(S)),nl, asserta(noisy_store(S)).
%noisy_forward_store(S) :- write(retract_store(S)),nl, retract(noisy_store(S))	, fail, !.
%noisy_get_store(S) :- write(get_store(S)), write( '--> '), (noisy_store(S) -> write(S),nl ; write(fail),nl), !.

%noisy_forward_store(N) :- (noisy_store(P) ; P=nil), asserta(noisy_store([N,P])).
%noisy_forward_store(N) :- retract(noisy_store([N,P])), asserta(noisy_store(P)), fail.
%noisy_get_store(S) :- noisy_store([S,_]), !.

t1 :-
	findall(X,noisy_max([2,2,2],[97,97,97,97],X),Xs),
	write(Xs), length(Xs,L), write(L).
	
t2 :-
	findall(X,noisy_max([0,0,1],[97],X),Xs),
	write(Xs).
	
t3 :-
	findall(X,noisy_max([0,1,0],[97],X),Xs),
	write(Xs).
	
pt1 :-
	viterbig(noisy_max([1,1,1],[97],G)),
	write(G).

% ==============  Some older revision 


:- [fakemsw].

t1 :- 
	findall(X,noisy_max([1,1,1],[97]))

noisy_max([MaxMut,MaxDel,MaxIns],In,Out) :-
	retract_all(noisy_store(_)),
	noisy_forward_store([[],[[MaxMut,MaxDel,MaxIns],[0,0,0]]]),
	noisy_rec(In,Out).
	
noisy_rec([],[]).

%noisy_rec(Input,Input) :-
%	noisy_get_store([_,[[_,_,MaxIns],[_,_,MaxIns]]]).

noisy_rec(In,Out) :-
%	noisy_get_store([ActionList,[MaxMut,MaxDel,MaxIns]-[Mut,Del,Ins]]),
%	Depth < MaxDepth,
%	NextDepth is Depth + 1,
	msw(noisy_action,Action),
%	noisy_forward_store([[Action|ActionList],MaxDepth-NextDepth]),
	noisy_action(Action,In,Out).

noisy_action(mutate,[I|In],[O|Out]) :-
	write(noisy_action(mutate,[I|In],[O|Out])),nl,
	noisy_get_store([As,[[MaxMut,MaxDel,MaxIns],[Mut,Del,Ins]]]),
	msw(mutate(I),O),
	((O == I) ->
		noisy_forward_store([[match|As],[[MaxMut,MaxDel,MaxIns],[Mut,Del,Ins]]])
		;
		Mut < MaxMut,
		NextMut is Mut + 1,
		noisy_forward_store([[mutate|As],[[MaxMut,MaxDel,MaxIns],[NextMut,Del,Ins]]])),
	noisy_rec(In,Out).
	
noisy_action(insert,In,[O|Out]) :-
	write(noisy_action(insert,In,[O|Out])),nl,
	noisy_get_store([Actions,[[MaxMut,MaxDel,MaxIns],[Mut,Del,Ins]]]),
	((Actions == [delete|_]) -> fail ; true),
	Ins < MaxIns,
	NextIns is Ins + 1,
	msw(insert,O),
	noisy_forward_store([[insert|Actions],[[MaxMut,MaxDel,MaxIns],[Mut,Del,NextIns]]]),
	noisy_rec(In,Out).

noisy_action(delete,[_|In],Out) :-
	write(noisy_action(delete,[_|In],Out)),nl,
	noisy_get_store([Actions,[[MaxMut,MaxDel,MaxIns],[Mut,Del,Ins]]]),
	((Actions == [insert|_]) -> fail ; true),
	Del < MaxDel,
	NextDel is Del + 1,
	noisy_forward_store([[delete|Actions],[[MaxMut,MaxDel,MaxIns],[Mut,NextDel,Ins]]]),
	noisy_rec(In,Out).

noisy_forward_store(S) :- (write(assert_store(S)),nl, asserta(noisy_store(S))) ; (write(retract_store(S)),nl, retract(noisy_store(S))).

noisy_get_store(S) :- write(get_store(S)), write( '--> '), (noisy_store(S) -> write(S),nl ; write(fail),nl,fail), !.