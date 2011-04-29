%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A probabilistic regular expression matcher in PRISM
% Author: Christian Theil Have
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Load compiler
:- [compiler].
:- prism(regex_model).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Before a probabilistic regular expressions can be used - it must be compiled.
% This is done by calling regex_compile(Regex) (def below)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 
% regex_compile(+Regex)
% Compiles a regex to an intermediary representation. 
% All inferences use last compiled regex
regex_compile(Regex) :-
	retractall(internal_compiled_regex(_,_,_)),
	re_compile(Regex,CompiledRegex),!,
	re_label(CompiledRegex,LabelledRegex,Variables),
	assert(internal_compiled_regex(Regex,LabelledRegex,Variables)),!,
	regex_init_variables,
	write('successfully compiled regular expression: '), write(Regex),nl.
	
regex_init_variables :-
	internal_compiled_regex(_,_,Vars),
	forall(member(Var,Vars),msw(Var,_)).
	
regex_init_noisy :-
	msw(noisy_action,_),
	active_alphabet(Alphabet),
	alphabet_codes(Alphabet,Codes),
	forall(member(Code,Codes),msw(mutate(Code),_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Probabilistic queries include learning (regex_learn), 
% probability calculation (regex_prob), viterbi (best) path calculation (regex_viterbi)
% and sampling (regex_sample). 
% These predicates are defined below.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
% regex_learn(+Strings,+Matches)
% (Supervised) parameter learning using both strings and matches 
% Strings is a list of strings to train on and Matches is a list of
% captures, i.e. what the regex should capture from the string
regex_learn(Strings,Matches) :-
	compiled_regex(_,CompiledRegex),
	maplist(S,M,G,goal_wrap_regex(CompiledRegex,S,M,G),Strings,Matches,Goals),
	write(Goals),nl,
	learn(Goals).

%%
% regex_learn(+Strings,+Matches)
% (Unsupervised) parameter learning from a lists of Strings
regex_learn(Strings) :-
	length(Strings,NumStrings),
	length(Matches,NumStrings),
	regex_learn(Strings,Matches).

% Probability calculation
regex_prob(String,Matches,P) :-
	compiled_regex(_,CompiledRegex),
	goal_wrap_regex(CompiledRegex,String,Matches,Goal),
	prob(Goal,P).

% Probability calculation
regex_prob(String,P) :-
	regex_prob(String,_,P).

% Viterbi path calculation
n_regex_match(N,String,Matches,P) :-
	compiled_regex(_,CompiledRegex),
	goal_wrap_regex(CompiledRegex,String,Matches,Goal),
	n_viterbig(N,Goal,P).

% Viterbi path calculation
regex_match(String,Matches,P) :-
	n_regex_match(1,String,Matches,P).

regex_match(String,Matches) :-
	n_regex_match(1,String,Matches,_).

regex_match(String) :-
	n_regex_match(1,String,_,_).
	
% Sampling
regex_sample(String,Matches) :-
	compiled_regex(_,CompiledRegex),
	pre_sample(CompiledRegex,String,Matches).

% Sampling
regex_sample(String) :-
	regex_sample(String,_).
	
% Displays information about the currently compiled regular expression
regex_show :-
	internal_compiled_regex(R,RC,RCL),
	write('The currently compiled regex is: '), write(R),nl,nl,
	write('It has been compiled to: '), nl,write(RC),nl,nl,
	write('It has the following random variables: '), nl,write(RCL),nl,nl,
	write('type show_sw to show the parameters of the those variables.'),nl,nl.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal utility predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
% compiled_regex(-Regex,-CompiledRegex)
% queries the currently active regular expression
compiled_regex(Regex,CompiledRegex) :-
	catch(internal_compiled_regex(Regex,CompiledRegex,_),_,(write('No regular expression has been compiled.'),nl,fail)).

% internal utility function
goal_wrap_regex(Regex,String,Matches,pre_match(Regex,String,Matches)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some test stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


	
	
