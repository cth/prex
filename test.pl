
:- ['prex.pl']. % Load prex as Prolog program

% Fake MSW to simulate PRISM
msw(A,B) :-
	values(A,C),
	member(B,C).
	
run_test(Name) :-
	write('running test '), 
	write(Name),
	write('\t'),
	call(Name),
	write('-\tok'),nl.
	

run_tests :-
	run_test(test1),!,
	run_test(test2),!,
	run_test(test3),!,
	run_test(test4),!,
	run_test(test5),!,
	run_test(test6),!,
	run_test(test7),!,
	run_test(test8),!,
	run_test(test9),!,
	run_test(test10),!,
	run_test(test11),!,
	run_test(test12),!,
	run_test(test13).
	
% Matching of a single character with single character regular expression
test1 :-
	re_compile('^a*$',RE),!,
	%write(RE),nl,
	re_label(RE,REL),!,
	%write(REL),nl,
	pre_match(REL,'a',[]).

% Same as 1.1, but with capture group	
test2 :-
	re_compile('^(a*)$',RE),!,
	%write(RE),nl,
	re_label(RE,REL),!,
	%write(REL),nl,
	pre_match(REL,'a',[a]).
	
% Matching of two characters with two character regular expression
test3 :-
	re_compile('^(a*)(b*)$',RE),!,
	%write(RE),nl,
	re_label(RE,REL),!,
	%write(REL),nl,
	pre_match(REL,'ab',[a,b]).

% Test that matching with the any character works as expected
test4 :-
	re_compile('^.$',RE),!,
	re_label(RE,REL),!,
	pre_match(REL,'b',[]).

% Capture groups and any characters
test5 :-
	re_compile('^..(..)..$',RE),
	%write(RE),nl,
	re_label(RE,REL),
	pre_match(REL,'ababab',[ab]).
	
% Combining capture groups, any characters and repetition.
test6 :-
	re_compile('^.(.*).(.)$',RE),
	%write(RE),nl,
	re_label(RE,REL),
	pre_match(REL,'ababab',[bab,b]).


% Nesting groups and capture groups
test7 :-
	re_compile('^a((bb)*)a$',RE),
	%write(RE),nl,
	re_label(RE,REL),
	pre_match(REL,'abbbba',[bbbb]).
	

% Ambiguous version of above
test8 :-
	re_compile('^.*((bb)*).*$',RE),
	%write(RE),nl,
	re_label(RE,REL),
	pre_match(REL,'abbbba',[bbbb]),
	pre_match(REL,'abbbba',[bb]),
	not(pre_match(REL,'abbbba',[b])).
	
% Playing with capture groups, e.g. match a repeat
test9 :-
	re_compile('^(.*)\\1$',RE),!,
	re_label(RE,REL),!,
	pre_match(REL,'bb',[b]).

% Two match groups 
test10 :-
	re_compile('^(.*)(.*)\\1\\2$',RE),!,
	re_label(RE,REL),!,
	pre_match(REL,'abab',[a,b]).
	
% Capturing sub expressions with backreferences
% This is sick :-)
test11 :-
	re_compile('^(.)(.)(\\1\\2\\2\\1)$',RE),!,
	re_label(RE,REL),!,
	pre_match(REL,'ababba',[a,b,abba]).
	
test11 :-
	re_compile('^(.)(.)(\\1\\2\\2\\1)$',RE),!,
	re_label(RE,REL),!,
	pre_match(REL,'ababba',[a,b,abba]).
	
% Testing the {m,n} operator
	
test12 :-
	re_compile('^(.{2,5})(.*)$',RE),!,
	re_label(RE,REL),!,
	findall(X,pre_match(REL,'bbaaa',X),MatchLists),
	subtract(MatchLists,[[bb,aaa],[bba,aa],[bbaa,a],[bbaaa,'']],[]).

%	pre_match(REL,'aaaaa',[aa,aaa]), 
%	pre_match(REL,'aaaaa',[aaa,aa]).
%	pre_match(REL,'aaaaa',[aaaa,a]).
%	pre_match(REL,'aaaaa',[aaaaa,[]]).


test13 :-
	re_compile('^(.*)(#{reverse(\\1)})$',RE),!,
	%write(RE),nl,
	re_label(RE,REL),!,
	%write(REL),nl,
	pre_match(REL,'abba',[ab,ba]).

test_call_goal :-
	atom_codes('rev(\\1)',Codes),
	call_goal(Goal,Codes,[]),
	write_canonical(Goal),nl.
	
test_callref :-
	atom_codes('#{rev(\\1,\\2,\\3)}',Codes),
	callref(Goal,Codes,[]),
	write(Goal).
	
	
		
	
	

