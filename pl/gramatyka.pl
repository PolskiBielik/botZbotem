:- module(gramatyka, [parsuj_szablon/3]).

% Ustawienie kodowania UTF-8
:- set_prolog_flag(encoding, utf8).

:- use_module(library(dcg/basics)).

% Reguły gramatyczne DCG do parsowania pliku szablon.txt

parsuj_szablon(szablon(Tytul, Forma, LiczbaUczestnikow, Uczestnicy, Prompt, Pytanie, LiczbaWypowiedzi, CzasTrwania)) -->
    { writeln('Starting to parse szablon...') },
    "Tytuł: ", string_without("\n", TytulCodes), gramatyka_eol,
    { gramatyka_usuń_cr(TytulCodes, CleanTytulCodes), atom_codes(Tytul, CleanTytulCodes), format('Parsed Tytuł: ~w\n', [Tytul]) },
    "Forma: ", string_without("\n", FormaCodes), gramatyka_eol,
    { gramatyka_usuń_cr(FormaCodes, CleanFormaCodes), atom_codes(Forma, CleanFormaCodes), format('Parsed Forma: ~w\n', [Forma]) },
    "Liczba uczestników: ", integer(LiczbaUczestnikow), gramatyka_eol,
    { format('Parsed Liczba uczestników: ~w\n', [LiczbaUczestnikow]) },
    linie_uczestnik(Uczestnicy),
    linie_prompt_systemowy(Prompt),
    "Zacznij od pytania: ", string_without("\n", PytanieCodes), gramatyka_eol,
    { gramatyka_usuń_cr(PytanieCodes, CleanPytanieCodes), atom_codes(Pytanie, CleanPytanieCodes), format('Parsed Zacznij od pytania: ~w\n', [Pytanie]) },
    "Liczba wypowiedzi: ", string_without("\n", LiczbaWypowiedziCodes), gramatyka_eol,
    { gramatyka_usuń_cr(LiczbaWypowiedziCodes, CleanLiczbaWypowiedziCodes), atom_codes(LiczbaWypowiedzi, CleanLiczbaWypowiedziCodes), format('Parsed Liczba wypowiedzi: ~w\n', [LiczbaWypowiedzi]) },
    "Czas trwania: ", string_without("\n", CzasTrwaniaCodes), gramatyka_eol,
    { gramatyka_usuń_cr(CzasTrwaniaCodes, CleanCzasTrwaniaCodes), atom_codes(CzasTrwania, CleanCzasTrwaniaCodes), format('Parsed Czas trwania: ~w\n', [CzasTrwania]) },
    opcjonalne_eol,
    { writeln('Finished parsing szablon.') }.

% Parse participants (Uczestnik 1, Uczestnik 2, etc.)
linie_uczestnik([uczestnik(Nr, Name) | Rest]) -->
    "Uczestnik ", integer(Nr), ": ", string_without("\n", NameCodes), gramatyka_eol,
    { gramatyka_usuń_cr(NameCodes, CleanNameCodes), atom_codes(Name, CleanNameCodes), format('Parsed Uczestnik: ~w - ~w\n', [Nr, Name]) },
    linie_uczestnik(Rest).
linie_uczestnik([]) --> [].


% Parse Prompt systemowy (Prompt systemowy 1, Prompt systemowy 2, etc.)
linie_prompt_systemowy([prompt(Nr, Name) | Rest]) -->
    "Prompt systemowy", optional_integer(Nr), ": ", string_without("\n", NameCodes), gramatyka_eol,
    { gramatyka_usuń_cr(NameCodes, CleanNameCodes), atom_codes(Name, CleanNameCodes), format('Prompt systemowy: ~w - ~w\n', [Nr, Name]) },
    linie_prompt_systemowy(Rest).
linie_prompt_systemowy([]) --> [].


% Allow any number of trailing end-of-line markers
opcjonalne_eol --> [].
opcjonalne_eol --> gramatyka_eol, opcjonalne_eol.

% Helper DCG for end of line
gramatyka_eol --> "\n".
gramatyka_eol --> "\r\n".

% Helper predicate to trim trailing carriage return '\r' if present
gramatyka_usuń_cr(Codes, CleanCodes) :-
    (   append(CleanCodes, [0'\r], Codes) -> true
    ;   CleanCodes = Codes
    ).

% Helper DCG for optional integer
optional_integer(Nr) --> " ", integer(Nr).
optional_integer(0) --> [].
