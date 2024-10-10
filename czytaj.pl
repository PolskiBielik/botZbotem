:- module(czytaj, [main/0]).

% Ustawienie kodowania UTF-8
:- set_prolog_flag(encoding, utf8).

% Import required libraries
:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).  % For file reading
:- use_module(pl/gramatyka).

% Deklaracja dynamicznych predykatów dla wszystkich możliwych pól
:- dynamic tytul/1.
:- dynamic forma/1.
:- dynamic liczba_uczestnikow/1.
:- dynamic uczestnik/2.
:- dynamic prompt_systemowy/2.
:- dynamic zacznij_od_pytania/1.
:- dynamic liczba_wypowiedzi/1.
:- dynamic czas_trwania/1.

% Pomocniczy predykat do usunięcia końcowego znaku powrotu karetki '\r', jeśli występuje
usuń_cr(Codes, CleanCodes) :-
    (   append(CleanCodes, [0'\r], Codes) -> true
    ;   CleanCodes = Codes
    ).

% Główny predykat do czytania i parsowania pliku
parsuj_szablon_plik(NazwaPliku) :-
    (   exists_file(NazwaPliku)  % Sprawdź, czy plik istnieje
    ->  format('Plik "~w" znaleziony, próba odczytu...~n', [NazwaPliku]),
        % Odczytaj i wyświetl zawartość pliku do celów debugowania
        read_file_to_string(NazwaPliku, Zawartosc, [encoding(utf8)]),
        format("Zawartość pliku:\n~s\n", [Zawartosc]),
        string_codes(Zawartosc, Kody),
        % Próba parsowania pliku
        (   phrase(gramatyka:parsuj_szablon(Szablon), Kody)
        ->  writeln('Parsowanie zakończone sukcesem!'),  % Wskaż, że parsowanie zakończyło się sukcesem
            wydrukuj_szablon(Szablon),
            dodaj_fakty(Szablon)
        ;   format('Błąd: Nie można sparsować zawartości pliku "~w". Upewnij się, że format pliku jest poprawny.~n', [NazwaPliku])
        )
    ;   format('Błąd: Plik "~w" nie istnieje.~n', [NazwaPliku])
    ).

% Pozwól na dowolną liczbę końcowych znaków nowej linii
opcjonalne_eol --> [].
opcjonalne_eol --> eol, opcjonalne_eol.

% Wydrukuj sparsowany szablon
wydrukuj_szablon(szablon(Tytul, Forma, LiczbaUczestnikow, Uczestnicy, Prompty, Pytanie, LiczbaWypowiedzi, CzasTrwania)) :-
    format("Tytuł: ~w~n", [Tytul]),
    format("Forma: ~w~n", [Forma]),
    format("Liczba uczestników: ~w~n", [LiczbaUczestnikow]),
    format("Uczestnicy: ~w~n", [Uczestnicy]),
    format("Prompty systemowe: ~w~n", [Prompty]),
    format("Pytanie początkowe: ~w~n", [Pytanie]),
    format("Liczba wypowiedzi: ~w~n", [LiczbaWypowiedzi]),
    format("Czas trwania: ~w~n", [CzasTrwania]).

% Dodaj fakty z sparsowanego szablonu
dodaj_fakty(szablon(Tytul, Forma, LiczbaUczestnikow, Uczestnicy, Prompty, Pytanie, LiczbaWypowiedzi, CzasTrwania)) :-
    % Dodaj proste fakty
    (   retractall(tytul(_)),
        assertz(tytul(Tytul)),
        writeln('Fakt dodany: tytul/1')
    ),
    (   retractall(forma(_)),
        assertz(forma(Forma)),
        writeln('Fakt dodany: forma/1')
    ),
    (   retractall(liczba_uczestnikow(_)),
        assertz(liczba_uczestnikow(LiczbaUczestnikow)),
        writeln('Fakt dodany: liczba_uczestnikow/1')
    ),
    (   retractall(zacznij_od_pytania(_)),
        assertz(zacznij_od_pytania(Pytanie)),
        writeln('Fakt dodany: zacznij_od_pytania/1')
    ),
    (   retractall(liczba_wypowiedzi(_)),
        assertz(liczba_wypowiedzi(LiczbaWypowiedzi)),
        writeln('Fakt dodany: liczba_wypowiedzi/1')
    ),
    (   retractall(czas_trwania(_)),
        assertz(czas_trwania(CzasTrwania)),
        writeln('Fakt dodany: czas_trwania/1')
    ),
    % Dodaj fakty uczestnik/2
    forall(member(Uczestnik, Uczestnicy),
           (   Uczestnik = uczestnik(Nr, Nazwa),
               retractall(uczestnik(Nr, _)),
               assertz(uczestnik(Nr, Nazwa)),
               format('Fakt dodany: uczestnik(~w, ~w)/2~n', [Nr, Nazwa])
           )),
    % Dodaj fakty prompt_systemowy/2
    forall(member(prompt(Nr, Prompt), Prompty),
           (   retractall(prompt_systemowy(Nr, _)),
               assertz(prompt_systemowy(Nr, Prompt)),
               format('Fakt dodany: prompt_systemowy(~w, ~w)/2~n', [Nr, Prompt])
           )).

% Parsuj argumenty wiersza poleceń i wykonaj
:- initialization(main, main).

main :-
    current_prolog_flag(argv, Argv),
    (   Argv = [NazwaPliku|_]
    ->  parsuj_szablon_plik(NazwaPliku)
    ;   parsuj_szablon_plik('szablon.txt')
    ),
    halt.
