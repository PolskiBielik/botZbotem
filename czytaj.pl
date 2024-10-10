% Ustawienie kodowania UTF-8
:- set_prolog_flag(encoding, utf8).

% Import required libraries
:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).  % For file reading

% Declare dynamic predicates for all possible fields
:- dynamic tytul/1.
:- dynamic forma/1.
:- dynamic liczba_uczestnikow/1.
:- dynamic uczestnik/2.
:- dynamic prompt_systemowy/1.
:- dynamic zacznij_od_pytania/1.
:- dynamic liczba_wypowiedzi/1.
:- dynamic czas_trwania/1.

% Helper predicate to trim trailing carriage return '\r' if present
trim_cr(Codes, CleanCodes) :-
    (   append(CleanCodes, [0'\r], Codes) -> true
    ;   CleanCodes = Codes
    ).

% Main predicate to read and parse the file
parse_szablon_file(Filename) :-
    (   exists_file(Filename)  % Check if the file exists
    ->  format('File "~w" found, attempting to read...~n', [Filename]),
        % Read and display the content of the file for debugging
        read_file_to_string(Filename, Content, [encoding(utf8)]),
        format("File content:\n~s\n", [Content]),
        string_codes(Content, Codes),
        % Attempt to parse the file
        (   phrase(parse_szablon(Szablon), Codes)
        ->  writeln('Parsing successful!'),  % Indicate successful parsing
            print_szablon(Szablon),
            assert_facts(Szablon)
        ;   format('Error: Unable to parse the content of "~w". Please ensure the file format is correct.~n', [Filename])
        )
    ;   format('Error: File "~w" does not exist.~n', [Filename])
    ).

% DCG to parse the entire szablon.txt content
parse_szablon(szablon(Tytul, Forma, LiczbaUczestnikow, Uczestnicy, Prompt, Pytanie, LiczbaWypowiedzi, CzasTrwania)) -->
    "Tytuł: ", string_without("\n", TytulCodes), eol,  % Parse the title
    { writeln('Parsed Tytuł'),
      trim_cr(TytulCodes, CleanTytulCodes),
      atom_codes(Tytul, CleanTytulCodes)
    },
    "Forma: ", string_without("\n", FormaCodes), eol,  % Parse the form
    { writeln('Parsed Forma'),
      trim_cr(FormaCodes, CleanFormaCodes),
      atom_codes(Forma, CleanFormaCodes)
    },
    "Liczba uczestników: ", integer(LiczbaUczestnikow), eol,  % Parse the number of participants
    { writeln('Parsed Liczba uczestników') },
    "Uczestnik ", integer(Nr1), ": ", string_without("\n", Name1Codes), eol,  % Parse Uczestnik 1
    { atom_codes(Name1, Name1Codes),
      format('Parsed Uczestnik ~w: ~w~n', [Nr1, Name1])
    },
    "Uczestnik ", integer(Nr2), ": ", string_without("\n", Name2Codes), eol,  % Parse Uczestnik 2
    { atom_codes(Name2, Name2Codes),
      format('Parsed Uczestnik ~w: ~w~n', [Nr2, Name2]),
      Uczestnicy = [uczestnik(Nr1, Name1), uczestnik(Nr2, Name2)]
    },
    "Prompt systemowy: ", string_without("\n", PromptCodes), eol,  % Parse the system prompt
    { writeln('Parsed Prompt systemowy'),
      trim_cr(PromptCodes, CleanPromptCodes),
      atom_codes(Prompt, CleanPromptCodes)
    },
    "Zacznij od pytania: ", string_without("\n", PytanieCodes), eol,  % Parse the starting question
    { writeln('Parsed Zacznij od pytania'),
      trim_cr(PytanieCodes, CleanPytanieCodes),
      atom_codes(Pytanie, CleanPytanieCodes)
    },
    "Liczba wypowiedzi: ", integer(LiczbaWypowiedzi), eol,  % Parse the number of statements
    { writeln('Parsed Liczba wypowiedzi') },
    "Czas trwania: ", string_without("\n", CzasTrwaniaCodes), eol,  % Parse the duration
    { writeln('Parsed Czas trwania'),
      trim_cr(CzasTrwaniaCodes, CleanCzasTrwaniaCodes),
      atom_codes(CzasTrwania, CleanCzasTrwaniaCodes)
    },
    optional_eols.

% Allow any number of trailing end-of-line markers
optional_eols --> [].
optional_eols --> eol, optional_eols.

% Print the parsed szablon
print_szablon(szablon(Tytul, Forma, LiczbaUczestnikow, Uczestnicy, Prompt, Pytanie, LiczbaWypowiedzi, CzasTrwania)) :-
    format("Tytuł: ~w~n", [Tytul]),
    format("Forma: ~w~n", [Forma]),
    format("Liczba uczestników: ~w~n", [LiczbaUczestnikow]),
    format("Uczestnicy: ~w~n", [Uczestnicy]),
    format("Prompt systemowy: ~w~n", [Prompt]),
    format("Pytanie początkowe: ~w~n", [Pytanie]),
    format("Liczba wypowiedzi: ~w~n", [LiczbaWypowiedzi]),
    format("Czas trwania: ~w~n", [CzasTrwania]).

% Assert facts from the parsed szablon
assert_facts(szablon(Tytul, Forma, LiczbaUczestnikow, Uczestnicy, Prompt, Pytanie, LiczbaWypowiedzi, CzasTrwania)) :-
    % Assert simple facts
    (   retractall(tytul(_)),
        assertz(tytul(Tytul)),
        writeln('Fact asserted: tytul/1')
    ),
    (   retractall(forma(_)),
        assertz(forma(Forma)),
        writeln('Fact asserted: forma/1')
    ),
    (   retractall(liczba_uczestnikow(_)),
        assertz(liczba_uczestnikow(LiczbaUczestnikow)),
        writeln('Fact asserted: liczba_uczestnikow/1')
    ),
    (   retractall(prompt_systemowy(_)),
        assertz(prompt_systemowy(Prompt)),
        writeln('Fact asserted: prompt_systemowy/1')
    ),
    (   retractall(zacznij_od_pytania(_)),
        assertz(zacznij_od_pytania(Pytanie)),
        writeln('Fact asserted: zacznij_od_pytania/1')
    ),
    (   retractall(liczba_wypowiedzi(_)),
        assertz(liczba_wypowiedzi(LiczbaWypowiedzi)),
        writeln('Fact asserted: liczba_wypowiedzi/1')
    ),
    (   retractall(czas_trwania(_)),
        assertz(czas_trwania(CzasTrwania)),
        writeln('Fact asserted: czas_trwania/1')
    ),
    % Assert uczestnik/2 facts
    forall(member(Uczestnik, Uczestnicy),
           (   Uczestnik = uczestnik(Nr, Name),
               retractall(uczestnik(Nr, _)),
               assertz(uczestnik(Nr, Name)),
               format('Fact asserted: uczestnik(~w, ~w)/2~n', [Nr, Name])
           )).

% Parse command line arguments and execute
:- initialization(main, main).

main :-
    current_prolog_flag(argv, Argv),
    (   Argv = [Filename|_]
    ->  parse_szablon_file(Filename)
    ;   parse_szablon_file('szablon.txt')
    ),
    halt.