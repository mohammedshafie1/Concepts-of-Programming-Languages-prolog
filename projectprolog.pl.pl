
main:-
	write('Welcome to Pro-Wordle!'),nl,
	write('----------------------'),nl,
	build_kb,nl,
	write('Done building the words database...'),nl,
	play.
	
build_kb:-
	write('Please enter a word and its category on separate lines:'),nl,
	read(Word),
	(
		Word = done;
		read(Category),
		assert(word(Word,Category)),
		build_kb
	).
	
	
play:-
	categories(L),
	write('The available categories are: '),write(L),nl,
	write('Choose a category: '),nl,
	read(C),
	category_check(L,C,New_Category),
	write('Choose a length:'),nl,
	read(Length), length_found_check(New_Category,Length,New_Length),setof(W,pick_word(W,New_Length,New_Category),All_words),random_member(Word,All_words),                           %retract(word(W,C)),
	write('Game started. '), 
	X1 is New_Length + 1,
	write('You have '),write(X1),write(' guesses.'),nl,nl,
	write('Enter a word composed of '),write(New_Length),write(' letters:'),nl,
	read(Input),nl,
	guessing_phase(Input,Word,X1,New_Length).
	

category_check(L1,C,NC):- member(C,L1),NC = C,!.
category_check(L1,_,NC):-
		write('This category does not exist.'),nl,
		write('Choose a category: '),nl,
		read(C1),
		category_check(L1,C1,NC).


length_found_check(C,Length,NL):- number(Length),available_length(Length), length_in_category(C,Length,NL),!.
length_found_check(C,Length,NL):- 
	\+number(Length),
	write('Please enter a number not a word'),nl,
	write('Choose a length'),nl,
	read(L),
	length_found_check(C,L,NL).
length_found_check(C,Length,NL):-
		number(Length),
		write('There are no words of this length. '),nl,
		write('Choose a length: '),nl,
		read(L),
		length_found_check(C,L,NL).
		
	
length_in_category(C,L,NL1):-
	word(W,C),
	string_length(W,L),
	NL1 = L.

length_in_category(C,L,NL1):-
	word(W,C),
	\+string_length(W,L),
	write('There are no words in '),write(C),write(' with length '),write(L),nl,
	write('Enter a new Length: '),nl,
	read(NewL),
	length_in_category(C,NewL,NL1).

guessing_phase(Input, Original_Word,_,_):-
	Input == Original_Word,nl,
	write('You Won !!!').
guessing_phase(Input, Original_Word,Guesses,_):-
	Input \==Original_Word,
	Guesses = 0,
	write('You Lost !'),nl,nl,
	write('The correct word was '),write(Original_Word).
guessing_phase(Input, Original_Word,Guesses,Length):-
	Input \== Original_Word,
	Guesses > 0,
	\+string_length(Input,Length),
	write('Word is not composed of '),write(Length),write(' letters. Try again'),nl,
	write('Remaining Guesses are '),write(Guesses),nl,nl,
	write('Enter a word composed of '),write(Length),write(' letters:'),nl,
	read(Input1),
	guessing_phase(Input1, Original_Word,Guesses,Length).

guessing_phase(Input, Original_Word,Guesses,Length):-
	word_exist(Input),
	Input \== Original_Word,
	Guesses > 0,
	string_length(Input,Length),
	string_chars(Original_Word,O),string_chars(Input,I),
	correct_letters(O,I,L1),
	correct_positions(O,I,L2),
	write('Correct letters are: '),write(L1),nl,
	write('Correct letters in correct positions are: '),write(L2),nl,
	G is Guesses - 1,
	(	G>0,
		write('Remaining Guesses are '),write(G),nl,nl,
		write('Enter a word composed of '),write(Length),write(' letters:'),nl,
		read(Input1),
		guessing_phase(Input1, Original_Word,G,Length); guessing_phase(Input, Original_Word,G,Length)
	).
		
guessing_phase(Input, Original_Word,Guesses,Length):-
	\+word_exist(Input),
	write('The word is not in the Knowledge Base'),nl,
	write('Remaining Guesses are '),write(Guesses),nl,nl,
	write('Enter a word composed of '),write(Length),write(' letters:'),nl,
	read(Input1),
	guessing_phase(Input1, Original_Word,Guesses,Length).
	




is_category(C):-
	word(_,C).

categories(L):-
	setof(C,is_category(C),L).


available_length(L):-
	word(W,_),
	string_length(W,L).


pick_word(W,L,C):-
	word(W,C),
	string_length(W,L).


correct_letters(L,L2,CL):- correct_lettersH(L,L2,[],CL).

correct_lettersH(_,[],A,A).
correct_lettersH(L1,[H|T],A,CL):-
	\+member(H,L1),
	correct_lettersH(L1,T,A,CL).

correct_lettersH(L1,[H|T],A,CL):-
	member(H,L1),
	\+member(H,A),
	append(A,[H],A1),
	correct_lettersH(L1,T,A1,CL).

correct_lettersH(L1,[H|T],A,CL):-
	member(H,L1),
	member(H,A),
	correct_lettersH(L1,T,A,CL).


correct_positions([],[],[]).	
correct_positions([H1|T1],[H2|T2],PL):-
	H1 \== H2,
	correct_positions(T1,T2,PL).
correct_positions([H1|T1],[H2|T2],[H1|PL1]):-
	H1 == H2,
	correct_positions(T1,T2,PL1).


word_exist(W):-
	word(W,_).