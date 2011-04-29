:- table alphabet_codes/2.

% Full ascii alphabet
alphabet_codes(ascii,Codes) :-
	ascii_codes_rec(1,255,Codes).
	
% Simple alphabet consisting only of the letters a,b and c 
alphabet_codes(abc,Codes) :-
	ascii_codes_rec(97,99,Codes).
	
% A DNA alphabet
alphabet_codes(dna,Codes) :-
	alphabet_codes(dna_lowercase,SmallCodes),
	alphabet_codes(dna_uppercase,BigCodes),	
	append(SmallCodes,BigCodes,Codes).

% A DNA alphabet
alphabet_codes(dna_lowercase,Codes) :-
	atom_codes(agct,Codes).
	
% A DNA alphabet
alphabet_codes(dna_uppercase,Codes) :-
	atom_codes('AGCT',Codes).



% utility for generation of sequential ascii codes 
%ascii_codes_rec(+Min,+Max,Codes).
ascii_codes_rec(MinMax,MinMax,[MinMax]).
ascii_codes_rec(Min,Max,[Min|RestCodes]) :-
	NextCode is Min + 1, 
	ascii_codes_rec(NextCode,Max,RestCodes).
