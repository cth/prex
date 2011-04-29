%:- table revcmpl/2.

revcmpl(A,B) :-
	reverse(A,C),
	dna_seq_complement(C,B).

% dna_seq_complement(++NucleotideSequence,--ReverseComplementedNucleotideSequence)
dna_seq_complement([],[]).

dna_seq_complement([N|NucleotidesRest],[NC|NCRest]) :-
        dna_complement(N,NC),
        dna_seq_complement(NucleotidesRest,NCRest).

dna_complement(97,116).
dna_complement(116,97).
dna_complement(103,99).
dna_complement(99,103).


