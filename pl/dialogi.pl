% Ustawienie kodowania UTF-8
:- set_prolog_flag(encoding, utf8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dialog dialog botów
% Funkcja uruchamiająca dialog pomiędzy dwoma botami
%
dialog_botów :-
    bot1(Bot1),
    bot2(Bot2),
    pierwsze_pytanie(PierwszePytanie),
    licznik_rozmowy(Licznik),
    dialog_pomiędzy_botami(Bot1, Bot2, PierwszePytanie, Licznik).

% Funkcja prowadząca dialog pomiędzy dwoma botami, ograniczona liczbą wymian
dialog_pomiędzy_botami(_, _, _, 0) :- !.  % Warunek stopu po osiągnięciu limitu wymian
dialog_pomiędzy_botami(Bot1, Bot2, Zapytanie, Licznik) :-
    system_prompt_tekst(SystemPrompt),
    drukuj_pytanie(Bot1, Zapytanie),
    wyślij_zapytanie_openai(Zapytanie, SystemPrompt, Odpowiedź1),
    daj_zawartość(Odpowiedź1, Odpowiedź1Zawartość),
    drukuj_odpowiedź(Bot2, Odpowiedź1Zawartość),

    % Poproś WMJ o sformułowanie nowego pytania na podstawie odpowiedzi
    format(atom(AnalizaZapytania1), 
           'Biorąc pod uwagę następującą odpowiedź: "~w", sformułuj kolejne pytanie do dyskusji.', 
           [Odpowiedź1Zawartość]),
    wyślij_zapytanie_openai(AnalizaZapytania1, SystemPrompt, NowePytanie1),
    daj_zawartość(NowePytanie1, NowePytanie1Zawartość),
    drukuj_pytanie(Bot2, NowePytanie1Zawartość),
    wyślij_zapytanie_openai(NowePytanie1Zawartość, SystemPrompt, Odpowiedź2),

    % Przekaż nowe pytanie do drugiego bota
    daj_zawartość(Odpowiedź2, Odpowiedź2Zawartość),
    drukuj_odpowiedź(Bot1, Odpowiedź2Zawartość),

    % Poproś WMJ o sformułowanie nowego pytania na podstawie odpowiedzi
    format(atom(AnalizaZapytania2), 
           'Biorąc pod uwagę następującą odpowiedź: "~w", sformułuj kolejne pytanie do dyskusji.', 
           [Odpowiedź2Zawartość]),
    wyślij_zapytanie_openai(AnalizaZapytania2, SystemPrompt, NowePytanie2),
    daj_zawartość(NowePytanie2, NowePytanie2Zawartość),

    % Zmniejsz licznik i kontynuuj dialog
    NowyLicznik is Licznik - 1,
    dialog_pomiędzy_botami(Bot1, Bot2, NowePytanie2Zawartość, NowyLicznik).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dialog co to jest
% Funkcja rozpoczynająca dialog między dwoma botami
%
dialog_co_to_jest :-
    bot1(Bot1),
    bot2(Bot2),
    pierwsze_pytanie(PierwszePytanie),
    licznik_rozmowy(Licznik),
    co_to_jest(CoToJest),
    dialog_co_to_jest_pomiędzy_botami(Bot1, Bot2, PierwszePytanie, Licznik, CoToJest, []).

% Funkcja prowadząca dialog między botami, ograniczona liczbą wymian
dialog_co_to_jest_pomiędzy_botami(Bot1, Bot2, _, 0, CoToJest, Historia) :- !,
    % Wyświetl licznik
    format('Licznik: ~w~n', [0]),
    
    % Bot1 dokonuje ostatecznego zgadywania
    system_prompt_pytacza(PromptPytacza),
    format(atom(HistoriaTekst), '~w', [Historia]),
    format(atom(PolecenieZgadnij), 'Na podstawie poniższej historii pytań i odpowiedzi, spróbuj zgadnąć, co to jest: ~w', [HistoriaTekst]),
    
    % Wysyłanie zapytania o zgadywanie
    wyślij_zapytanie_openai(PolecenieZgadnij, PromptPytacza, ZgadnijJSON),
    (   daj_zawartość(ZgadnijJSON, ZgadnijSurowe)
    ->  normalizuj_pytanie(ZgadnijSurowe, Zgadnij),
        format('~w: Myślę, że to jest "~w".~n', [Bot1, Zgadnij]),
        nl,
        
        % Formatowanie potwierdzenia
        wyjasnienie_co_to_jest(CoToJest, Wyjasnienie),
        format(atom(PotwierdzeniePrompt), 
               'Zanalizuj, czy ta odpowiedź "~w" jest poprawną odpowiedzią na pytanie o obiekt "~w" wiedząc, że tak on jest tak opisany "~w". Odpowiedz tylko "tak" albo "nie".', 
               [Zgadnij, CoToJest, Wyjasnienie]),
        
        % Pobieranie system prompt dla odpowiadacza
        system_prompt_odpowiadacza(PromptSystemowy),
        
        % Wysyłanie potwierdzenia
        wyślij_zapytanie_openai(PotwierdzeniePrompt, PromptSystemowy, PotwierdzenieJSON),
        (   daj_zawartość(PotwierdzenieJSON, PotwierdzenieSurowe)
        ->  normalizuj_odpowiedz(PotwierdzenieSurowe, Potwierdzenie),
            drukuj_odpowiedź(Bot2, Potwierdzenie),
            (   Potwierdzenie = "tak"
            ->  format('~w: Zgadłem poprawnie!~n', [Bot1]),
                (   wyjasnienie_co_to_jest(CoToJest, Wyjasnienie)
                ->  format('~w: Świetnie! ~n"~w"~n to: ~w~n', [Bot2, Zgadnij, Wyjasnienie])
                ;   format('~w: Świetnie! ~n"~w"~n jest poprawną odpowiedzią.~n', [Bot2, Zgadnij])
                )
            ;   Potwierdzenie = "nie"
            ->  format('~w: Niestety, nie zgadłem. Poprawna odpowiedź to "~w".~n', [Bot1, CoToJest]),
                (   wyjasnienie_co_to_jest(CoToJest, Wyjasnienie)
                ->  format('~w: Niestety ta odpowiedź, ~n"~w"~n nie jest poprawną odpowiedzią. Poprawna odpowiedź to: ~n"~w". ~w~n', [Bot2, Zgadnij, CoToJest, Wyjasnienie])
                ;   format('~w: Niestety ta odpowiedź, ~n"~w"~n nie jest poprawną odpowiedzią. Poprawna odpowiedź to: ~n"~w".~n', [Bot2, Zgadnij, CoToJest])
                )
            ;   % Potwierdzenie = "nie wiem" lub inne
                format('~w: Nie jestem pewien, czy "~w" to poprawna odpowiedź.~n', [Bot2, Zgadnij])
            )
        ;   format('Błąd podczas potwierdzania zgadywania.~n'),
            fail
        )
    ;   format('Błąd podczas przetwarzania odpowiedzi zgadywania.~n'),
        fail
    ).

dialog_co_to_jest_pomiędzy_botami(Bot1, Bot2, Pytanie, Licznik, CoToJest, Historia) :-
    Licznik > 0,
    % Wyświetl licznik
    format('Licznik: ~w~n', [Licznik]),
    system_prompt_pytacza(PromptPytacza),
    system_prompt_odpowiadacza(PromptOdpowiadacza),
    drukuj_pytanie(Bot1, Pytanie),
    %
    % Bot2 odpowiada
    % Zmodyfikowano tutaj, aby użyć wyjaśnienia
    (   wyjasnienie_co_to_jest(CoToJest, Wyjasnienie)
    ->  format(atom(PolecenieOdpowiedź), 'Biorąc pod uwagę, że to chodzi o "~w", który jest: "~w", odpowiedz na pytanie: "~w". Odpowiedz tylko "tak" albo "nie".', [CoToJest, Wyjasnienie, Pytanie])
    ;   format(atom(PolecenieOdpowiedź), 'Biorąc pod uwagę, że to chodzi o "~w", odpowiedz na pytanie: "~w". Odpowiedz tylko "tak" albo "nie".', [CoToJest, Pytanie])
    ),
    wyślij_zapytanie_openai(PolecenieOdpowiedź, PromptOdpowiadacza, OdpowiedźJSON),
    (   daj_zawartość(OdpowiedźJSON, OdpowiedźSurowa)
    ->  % Upewnij się, że odpowiedź to "tak" lub "nie"
        normalizuj_odpowiedz(OdpowiedźSurowa, Odpowiedź),
        drukuj_odpowiedź(Bot2, Odpowiedź),
        %
        % Aktualizuj historię
        append(Historia, [Pytanie-Odpowiedź], NowaHistoria),
        %
        % Bot1 formułuje nowe pytanie na podstawie historii
        format(atom(HistoriaTekst), '~w', [NowaHistoria]),
        format(atom(PoleceniePytanie), 'Na podstawie poniższej historii pytań i odpowiedzi, zadaj kolejne pytanie dotyczące osoby lub obiektu, na które odpowiedź to "tak" lub "nie": ~w', [HistoriaTekst]),
        wyślij_zapytanie_openai(PoleceniePytanie, PromptPytacza, NowePytanieJSON),
        (   daj_zawartość(NowePytanieJSON, NowePytanieSurowe)
        ->  % Oczyść nowe pytanie
            normalizuj_pytanie(NowePytanieSurowe, NowePytanie),
            %
            % Zmniejsz licznik i kontynuuj dialog
            NowyLicznik is Licznik - 1,
            dialog_co_to_jest_pomiędzy_botami(Bot1, Bot2, NowePytanie, NowyLicznik, CoToJest, NowaHistoria)
        ;   format('Błąd podczas przetwarzania nowego pytania.~n'),
            fail
        )
    ;   format('Błąd podczas przetwarzania odpowiedzi.~n'),
        fail
    ).

% Normalizuj odpowiedź, aby upewnić się, że to "tak" lub "nie"
normalizuj_odpowiedz(OdpowiedźSurowa, Odpowiedź) :-
    string_lower(OdpowiedźSurowa, OdpowiedźMala),
    (   sub_string(OdpowiedźMala, _, _, _, "tak")
    ->  Odpowiedź = "tak"
    ;   sub_string(OdpowiedźMala, _, _, _, "nie")
    ->  Odpowiedź = "nie"
    ;   Odpowiedź = "nie wiem"
    ).

% Normalizuj pytanie, usuwając zbędne białe znaki
normalizuj_pytanie(PytanieSurowe, Pytanie) :-
    normalize_space(string(Pytanie), PytanieSurowe).
