
:- ['prex.psm']. % Load prex as Prolog program

% Fake MSW to simulate PRISM
msw(A,B) :-
	values(A,C),
	member(B,C).


% Matching of a single character with single character regular expression
% OK
t1 :-
	RE = [grouped(1,[concat,97,[]])],
	pre_match1(RE,'a',Matches),
	write(Matches),nl.
	
% Matching of two characters with two character regular expression
t2 :-
	RE = [grouped(1, [concat,97,[concat,98,[]]])],
	pre_match1(RE,'ab',Matches),
	write(Matches),nl.



% Test that matching with the any character works as expected
t3 :-
	%re_compile('^..a*$',R), re_label(R,L).
	RE = [grouped(1,[concat,any(3),[]])],
	pre_match1(RE,'a',Matches),
	write(Matches),nl.	

% Multiple any characters
t4 :-
	%re_compile('^..a*$',R), re_label(R,L).
	RE = [grouped(1,[concat,any(3),[concat,any(5),[]]])],
	pre_match1(RE,'ab',Matches),
	write(Matches),nl.
	
	
% Multiple any characters
% should fail! 
t5_fail :-
	re_compile('^(..a*)$',R),
	re_label(R,RE),
	%RE = [ungrouped(1,[concat,any(3),[concat,any(5),[concat,[star(7),[concat,97,[]]],[]]]])],
	write(RE),nl,
	pre_match1(RE,'abba',M),
	write(M).
	
t5_success :-
	re_compile('^(...a*.)$',R),
	re_label(R,RE),
	%RE = [ungrouped(1,[concat,any(3),[concat,any(5),[concat,[star(7),[concat,97,[]]],[]]]])],
	write(RE),nl,
	pre_match1(RE,'abbaaaab',M),
	write(M).

% should fail
t6 :-
	re_compile('^.$',R), re_label(R,RE),
	pre_match(RE,'aa',M),
	write(M),nl.

t7 :-
	%re_compile('^a*b*$',R), re_label(R,RE),
	RE = [grouped(1,[concat,[star(3),[concat,97,[]]],[concat,[star(6),[concat,98,[]]],[]]])],
	pre_match1(RE,'aab',M),
	write(M),
	nl.
	
t8 :-
	re_compile('^a*b*a*$',R), re_label(R,RE),
	pre_match1(RE,'aabaaa',M),
	write(M),
	nl.