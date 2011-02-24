%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A probabilistic regular expression matcher in PRISM
%
% Author: Christian Theil Have
%
% Basic usage:
% re_compile(+RegexAtom,-Regex):
% Compiles an atom representing a regular expression to a
% prolog list representation.
%
% re_match(+Regex,+Atom,-Matches):
% Tries to match Atom with the regular expression Regex.
%
% The implementation supports basic regular expression
% operators such as ?, +, *, | and bracketed ranges and match groups
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Regular expression compilation
% A simple DCG for parsing regular expressions:
% A parameter is used to build the parsetree
% of the regular expression as an s-expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% TODO:
% - Making it work as probabilistic program.
% - Some range operators still missing,
% - More testing..

% IDEA:
% tabling may be used to for some call functions (perhaps a syntactic way of specifying this?)
% E.g. it would be a bad idea for the noisy function.

% We table re_compile/2 to avoid compiling the same regular expression
% twice
%:- table re_compile/2.

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

re_label(RE,LabelledRE) :-
	re_label(1,_,1,_,RE,LabelledRE).
	
re_label(ElemId,ElemId,GroupId,GroupId,[],[]).

% Leaf symbol, e.g. integer symbol code:
re_label(ElemId,ElemId,GroupId,GroupId,Int,Int) :- integer(Int).

re_label(ElemIdIn,ElemIdOut,GroupId,GroupId,any,any(ElemIdIn)) :- 
	ElemIdOut is ElemIdIn + 1.
	
re_label(ElemId,ElemId,GroupId,GroupId,[backref,Id], [backref,Id]).

re_label(ElemId,ElemId,GroupId,GroupId,[function,CallGoal], [function,CallGoal]).

re_label(ElemIdIn,ElemIdOut,GroupIdIn,GroupIdOut, [grouped(RE)|Rest], [grouped(GroupIdIn,RE1)|LabelledRest]) :-
	GroupIdNext is GroupIdIn + 1,
	re_label(ElemIdIn,ElemIdOut1,GroupIdNext,GroupIdOut1,RE,RE1),
	re_label(ElemIdOut1,ElemIdOut,GroupIdOut1,GroupIdOut,Rest,LabelledRest).
	
re_label(ElemIdIn,ElemIdOut,GroupIdIn,GroupIdOut,[ungrouped(RE)|Rest], [ungrouped(RE1)|LabelledRest]) :-
	re_label(ElemIdIn,ElemIdOut1,GroupIdIn,GroupIdOut1,RE,RE1),
	re_label(ElemIdOut1,ElemIdOut,GroupIdOut1,GroupIdOut,Rest,LabelledRest).

		
re_label(ElemIdIn,ElemIdOut,GroupIdIn,GroupIdOut, [star(Min,Max),RE], [star(ElemIdIn,constraint(0,Min,Max)),RE1]) :-
	ElemIdNext is ElemIdIn + 1,
	re_label(ElemIdNext,ElemIdOut,GroupIdIn,GroupIdOut,RE,RE1).

re_label(ElemIdIn,ElemIdOut,GroupIdIn,GroupIdOut,[concat,REA,REB], [concat,REA1,REB1]) :-
	re_label(ElemIdIn,ElemId2,GroupIdIn,GroupIdInOut2,REA,REA1),
	re_label(ElemId2,ElemIdOut,GroupIdInOut2,GroupIdOut,REB,REB1).

re_label(ElemIdIn,ElemIdOut,GroupIdIn, GroupIdOut, [or(REA,REB)], [or(ElemIdIn,REA1,REB1)]) :-
	ElemIdNext is ElemIdIn + 1,
	re_label(ElemIdNext,ElemIdNext2,GroupIdIn,GroupIdInOut2,REA,REA1),
	re_label(ElemIdNext2,ElemIdOut,GroupIdInOut2,GroupIdOut,REB,REB1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Construction of random varibles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The paramterized star(_) variable represents the choice
% between continued matching or stopping the match 
values(star(_Id), [continue, stop]).

% This variable represents choices between the left and 
% right side of a |
values(or(_Id), [left,right]).

values(any(_Id), AsciiCodes) :-
	ascii_codes(AsciiCodes), !.
		
ascii_codes(Codes) :-
%	ascii_codes_rec(1,255,Codes).
%	!,
	ascii_codes_rec(97,99,Codes).	% for easier debugging
	
% ascii_codes_rec(+Min,+Max,Codes) 
ascii_codes_rec(MinMax,MinMax,[MinMax]).
ascii_codes_rec(Min,Max,[Min|RestCodes]) :-
	NextCode is Min + 1, 
	ascii_codes_rec(NextCode,Max,RestCodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Regular expression matching
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% I should have a custom encoding of this to avoid 
% problems due to DCGs and PRISM

list_atom_codes([],[]).
list_atom_codes([CodeList|CodeListRest],[Atom|AtomsRest]) :-
	atom_codes(Atom,CodeList),
	list_atom_codes(CodeListRest,AtomsRest).

% re_match for input as atom
pre_match(Regex,String,MatchesAtoms) :-
	atom(String),
	atom_codes(String,StringCodes),
	pre_match_groups(Regex,[],ReverseMatches,StringCodes),
	reverse(ReverseMatches,Matches),
	list_atom_codes(Matches,MatchesAtoms).
	
% re_match for input as list of codes
pre_match(Regex,StringCodes,Matches) :-
	is_list(StringCodes), % verify that StringCodes is a list (e.g. not atom)
	reverse(ReverseMatches,Matches),
	pre_match_groups(Regex,[],ReverseMatches,StringCodes).

% pre_match_groups:
% Matches top-level expressed either encapsulated by a capture
% group or not
pre_match_groups([],MatchedGroups,MatchedGroups,[]).

pre_match_groups([grouped(_id,R)|Rest],MatchedGroupsIn,MatchedGroupsOut,String) :-
	append(StringMatchGroup,StringRest,String),
	pre_match_rec(R,MatchedGroupsIn,AccumulateMatches,StringMatchGroup),
	flatten(AccumulateMatches,MatchedGroup),
	pre_match_groups(Rest,[MatchedGroup|MatchedGroupsIn],MatchedGroupsOut,StringRest).

pre_match_groups([ungrouped(R)|Rest],MatchedGroupsIn,MatchedGroupsOut,String) :-
	append(StringFirst,StringRest,String),
	pre_match_rec(R,MatchedGroupsIn, _, StringFirst),
	pre_match_groups(Rest,MatchedGroupsIn,MatchedGroupsOut,StringRest).

%%% Regular expression matching recursive definition

% Match the empty string
pre_match_rec([],_,[],[]).

%% Choice operator:
pre_match_rec([or(Id), Left, Right],MatchedGroups,MatchAcc,String) :-
	msw(or(Id),LeftRightChoice),
	((LeftRightChoice==left) ->
		pre_match_rec(Left,MatchedGroups,MatchAcc,String)
		;
		pre_match_rec(Right,MatchedGroups,MatchAcc,String)).

% Concatenation:
pre_match_rec([concat(_),Left,[]],MatchedGroups,MatchAcc,String) :-
	pre_match_rec(Left,MatchedGroups,MatchAcc,String).

pre_match_rec([concat,Left,Right],MatchedGroups,MatchAcc,String) :-
	append(StringLeft,StringRight,String),
	pre_match_rec(Left,MatchedGroups,MatchAcc1,StringLeft),
	pre_match_rec(Right,MatchedGroups,MatchAcc2,StringRight),
	append(MatchAcc1,MatchAcc2,MatchAcc).

% Match repetition:

% Matches if string is empty
pre_match_rec([star(_id,constraint(RepeatCount,Min,_)),_],_MatchedGroups,[],[]) :- 
	RepeatCount >= Min.

pre_match_rec([star(Id,constraint(RepeatCount,Min,Max)),Left],MatchedGroups,[Match1|MatchRest],String) :-
	RepeatCount =< Max,
	msw(star(Id),continue),
	append(StringFirst,StringRest,String),
	pre_match_rec(Left,MatchedGroups,Match1,StringFirst),
	NextRepeatCount is RepeatCount + 1,
	pre_match_rec([star(Id,constraint(NextRepeatCount,Min,Max)),Left],MatchedGroups,MatchRest,StringRest).

% FIXME: TO BE TESTED (TOMORROW)
pre_match_rec([function,Goal], MatchedGroups, MatchAcc, String) :-
	Goal =.. [Functor|ArgList1],
	replace_backreference_arguments(ArgList1,MatchedGroups,ArgList2),
	% Add a "return" argument
	append(ArgList2,[Return],ArgList3),
	CallGoal =.. [Functor|ArgList3],
	call(CallGoal),
	RefExpr1 = [94|Return],
	reverse(RefExpr1,RefExpr2),
	RefExpr3 = [36|RefExpr2],
	reverse(RefExpr3,RefExpr4),!,
	re_compile(RefExpr4,[ungrouped(SubExpr)]),
	pre_match_rec(SubExpr,[],MatchAcc,String).
	
% Back references:
pre_match_rec([backref,Id], MatchedGroups, MatchAcc, String) :-
	extract_ref(Id,MatchedGroups,RefExpr),
	%write(extract_ref(Id,MatchedGroups,RefExpr)),nl,
	pre_match_rec(RefExpr,[],MatchAcc,String).

%% Match leaves:
pre_match_rec(SymbolCode,_,[SymbolCode],[SymbolCode]) :-
	integer(SymbolCode).
	
pre_match_rec(any(Id),_,[SymbolCode],[SymbolCode]) :-
	msw(any(Id), SymbolCode).
	
extract_ref(Id,[ReferencedGroup|RestGroups],SubExpr) :-
	length([ReferencedGroup|RestGroups],Id),
	% Insert ^ and $ to avoid building an re which also reconizes sorroundings
	RefExpr1 = [94|ReferencedGroup],
	reverse(RefExpr1,RefExpr2),
	RefExpr3 = [36|RefExpr2],
	reverse(RefExpr3,RefExpr4),
	!,
	% No need to label, it will only consist of (unnumbered) concats
	re_compile(RefExpr4,[ungrouped(SubExpr)]),
	!.
	
extract_ref(Id,[_|MatchedGroups],RefExpr) :-
	extract_ref(Id,MatchedGroups,RefExpr).

replace_backreference_arguments([],_MatchedGroups,[]).
replace_backreference_arguments([Arg|ArgS],MatchedGroups,[Arg2|Arg2S]) :-
	nth(Arg,MatchedGroups,Arg2),
	replace_backreference_arguments(ArgS,MatchedGroups,Arg2S).