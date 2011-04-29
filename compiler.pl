% Defines how many times a * may repeat at most 
global_repeat_max(1000).

re_compile(RegexAtom,Regexp) :-
	atom(RegexAtom),
	atom_codes(RegexAtom,RegexCodes),
	re_compile(RegexCodes,Regexp).

re_compile(RegexpCodes,Regexp) :-
	% Does the regular start with start-of-line character '^' ?
	% Else add a little something match anything in the beginning of the string
	atom_codes('.*',MatchAnyCodes),
	RegexpCodes = [StartCode|RestCodes1],
	((StartCode = 94) -> Codes2 = RestCodes1 ;  append(MatchAnyCodes,RegexpCodes,Codes2)),
	% Similarly, does the regular end with end-of-line character '$' ?
	% Else add a little something match anything in the end of the string
	reverse(Codes2,RevCodes2),
	RevCodes2 = [EndCode|RevRestCodes2],
	((EndCode = 36) -> reverse(RevRestCodes2,FinalCodes) ; append(Codes2,MatchAnyCodes,FinalCodes)),
	match_groups(Regexp,FinalCodes,[]).

control_character(C) :-
	atom_codes('#()|?*+[]{}\\.$^',ControlCharacters),
	member(C,ControlCharacters).

non_control_character(C) :-
	not(control_character(C)).

match_groups([R]) --> match_group(R).
match_groups([R1|R2]) --> match_group(R1), match_groups(R2).

match_group(grouped(R)) --> lparen, regexp(R), rparen.
match_group(ungrouped(R)) --> regexp(R).

regexp(R) --> alternation(R).
regexp(R) --> repetition(R).
regexp(R) --> concatenation(R).
regexp(R) --> callref(R).

alternation([or,R1,R2]) -->
	alternation_primitive(R1),
	or,
	alternation(R2).
alternation([or,R1,R2]) --> alternation_primitive(R1), or, alternation_primitive(R2).

alternation_primitive(R) --> repetition(R).
alternation_primitive(R) --> concatenation(R).
alternation_primitive(R) --> callref(R).

repetition([star(0,Max), R]) --> { global_repeat_max(Max) }, repetition_primitive(R), star.
%repetition([concat, R, [star(0,n), R]]) --> repetition_primitive(R), plus.
repetition([concat, R, [star(0,Max), R]]) --> { global_repeat_max(Max) }, repetition_primitive(R), plus.
repetition([or,R,[]]) --> repetition_primitive(R), question_mark.

repetition([star(MinMatch,MaxMatch),R]) -->
	repetition_primitive(R),
	%{ write(R),nl },
	lcurly,
	integer_number(MinMatch),
	comma,
	integer_number(MaxMatch),
	rcurly.

repetition_primitive([concat, R, []]) --> symbol(R). % Note, single symbols are concatenated with empty list
repetition_primitive(R) --> bracket_expression(R).
repetition_primitive(R) --> lparen, concatenation(R), rparen.
repetition_primitive(R) --> lparen, alternation(R), rparen.
repetition_primitive(R) --> callref(R).
repetition_primitive(R) --> backref_primitive(R).

concatenation([concat,R1,R2]) --> concatenation_primitive(R1), concatenation(R2).
concatenation([concat,R,[]]) --> concatenation_primitive(R).

concatenation_primitive(R) --> backref_primitive(R).
concatenation_primitive(R) --> repetition(R).
concatenation_primitive(R) --> symbol(R).
concatenation_primitive(R) --> lparen, alternation(R), rparen.
concatenation_primitive(R) --> bracket_expression(R).

backref_primitive([backref,Id]) -->
	backslash,
	integer_number(Id).

% Callrefs
callref([function,Goal]) -->	
	hash,
	lcurly,
	call_goal(Goal),
	rcurly.

% A syntax for goals which can be call
call_goal(CallGoal) -->
	functor(FTORCodes),
	lparen,
	function_argument_list(ArgList),
	rparen,
	{ atom_codes(FTORAtom,FTORCodes), CallGoal =.. [ FTORAtom | ArgList ] }.

functor([C|FunctorRest]) -->
	character(lower_case_char,C),
	functor_characters(FunctorRest).

functor_characters([]) --> [].
functor_characters([C|FunctorRest]) -->
	alphanumeric_symbol(_,C),
	functor_characters(FunctorRest).

function_argument_list([])--> [].
function_argument_list([Id]) -->
	backref_primitive([backref,Id]).
function_argument_list([Id|IdRest]) -->
	backref_primitive([backref,Id]),
	comma,
	function_argument_list(IdRest).

% A ranges group is something like [Xa-zA-F].
bracket_expression(R) -->
	[91], % '['
	bracket_expression_elements(R),
	[93]. % ']'

bracket_expression_elements(R) -->
	bracket_expr_elem(R).

bracket_expression_elements([or,R1,R2]) -->
	bracket_expr_elem(R1),
	bracket_expression_elements(R2).

bracket_expr_elem(R) -->
	single_range(R).

bracket_expr_elem(R) -->
	alphanumeric_symbol(_,R).

% Base case for single range - really just one symbol
single_range(S) -->
	alphanumeric_symbol(T,S),
	[45], % Hyphen
	alphanumeric_symbol(T,S).

% Match a single range e.g. "a-z" : Note that single_range is called recursively,
% to build an "or" sequence.
single_range([or,S1,RangeRest]) -->
	alphanumeric_symbol(T,S1),
	[45], % Hyphen
	alphanumeric_symbol(T,S2),
	{
	 S1 < S2, % Make sure the range is valid
	 S1Next is S1 + 1,
	 single_range(RangeRest,[S1Next,45,S2],[]) % Call DCG predicate recursively
	}.

or --> [124]. % '|'
question_mark --> [63]. % '?'
star --> [42]. % '*'
plus --> [43]. % '+'
comma --> [44]. % ','
lparen --> [40]. % '('
rparen --> [41]. % ')'
backslash --> [92].
lcurly --> [123]. % '{'
rcurly --> [125]. % '}'
hash --> [35].

symbol(S) --> escaped_control_character(S).
symbol(S) --> simple_symbol(S).
symbol(S) --> any_symbol(S).

escaped_control_character(S) -->
	[92, S], % 92 is backslash
	{ control_character(S) }.

simple_symbol(S) --> [S], { non_control_character(S) }.

any_symbol(any) --> [46]. % Dot '.'

alphanumeric_symbol(Type,S) -->
	digit(Type,S).

alphanumeric_symbol(Type,S) -->
	character(Type,S).

digit(digit,S) -->
	[ S ],
	{ S >= 48, S =< 67 }.
	
integer_number(Number) -->
	integer_number_list(NumList),
	{ NumList \= [], number_chars(Number,NumList) }.

integer_number_list([Digit|DigitsRest]) -->
	digit(digit, DCode),
	{ atom_codes(Digit,[DCode]) },
	integer_number_list(DigitsRest).
integer_number_list([]) -->	[].	

% Lower case characters
character(lower_case_char,S) -->
	[ S ],
	{ S >= 97, S =< 122 }.

% Upper case characters
character(upper_case_char,S) -->
	[ S ],
	{ S >= 65, S =< 90 }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Numbering of regular expression constituents
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This numbering is used to name switches in the 
% probabilistic matching later on

re_label(RE,LabelledRE,Variables) :-
	re_label(1,_,1,_,RE,LabelledRE,Variables).
	
re_label(ElemId,ElemId,GroupId,GroupId,[],[],[]).

% Leaf symbol, e.g. integer symbol code:
re_label(ElemId,ElemId,GroupId,GroupId,Int,Int,[]) :- integer(Int).

re_label(ElemIdIn,ElemIdOut,GroupId,GroupId,any,any(ElemIdIn),[any(ElemIdIn)]) :- 
	ElemIdOut is ElemIdIn + 1.
	
re_label(ElemId,ElemId,GroupId,GroupId,[backref,Id], [backref,Id],[]).

re_label(ElemId,ElemId,GroupId,GroupId,[function,CallGoal], [function,CallGoal],[]).

re_label(ElemIdIn,ElemIdOut,GroupIdIn,GroupIdOut, [grouped(RE)|Rest], [grouped(GroupIdIn,RE1)|LabelledRest],Variables) :-
	GroupIdNext is GroupIdIn + 1,
	re_label(ElemIdIn,ElemIdOut1,GroupIdNext,GroupIdOut1,RE,RE1,Variables1),
	re_label(ElemIdOut1,ElemIdOut,GroupIdOut1,GroupIdOut,Rest,LabelledRest,VariablesRest),
	append(Variables1,VariablesRest,Variables).
	
re_label(ElemIdIn,ElemIdOut,GroupIdIn,GroupIdOut,[ungrouped(RE)|Rest], [ungrouped(RE1)|LabelledRest],Variables) :-
	re_label(ElemIdIn,ElemIdOut1,GroupIdIn,GroupIdOut1,RE,RE1,Variables1),
	re_label(ElemIdOut1,ElemIdOut,GroupIdOut1,GroupIdOut,Rest,LabelledRest,VariablesRest),
	append(Variables1,VariablesRest,Variables).

re_label(ElemIdIn,ElemIdOut,GroupIdIn,GroupIdOut, [star(Min,Max),RE], [star(ElemIdIn,constraint(0,Min,Max)),RE1],[star(ElemIdIn)|VariablesRest]) :-
	ElemIdNext is ElemIdIn + 1,
	re_label(ElemIdNext,ElemIdOut,GroupIdIn,GroupIdOut,RE,RE1,VariablesRest).

re_label(ElemIdIn,ElemIdOut,GroupIdIn,GroupIdOut,[concat,REA,REB], [concat,REA1,REB1],Variables) :-
	re_label(ElemIdIn,ElemId2,GroupIdIn,GroupIdInOut2,REA,REA1,VariablesA),
	re_label(ElemId2,ElemIdOut,GroupIdInOut2,GroupIdOut,REB,REB1,VariablesB),
	append(VariablesA,VariablesB,Variables).

re_label(ElemIdIn,ElemIdOut,GroupIdIn, GroupIdOut, [or, REA,REB], [or(ElemIdIn),REA1,REB1],[or(ElemIdIn)|Variables]) :-
	ElemIdNext is ElemIdIn + 1,
	re_label(ElemIdNext,ElemIdNext2,GroupIdIn,GroupIdInOut2,REA,REA1,VariablesA),
	re_label(ElemIdNext2,ElemIdOut,GroupIdInOut2,GroupIdOut,REB,REB1,VariablesB),
	append(VariablesA,VariablesB,Variables).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extract a lists of elements in the regular expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

