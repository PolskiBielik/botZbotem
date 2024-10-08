% Potrzebne moduły
:- use_module(library(http/json)).
:- use_module(library(http/http_open)).

% Ustawienie kodowania UTF-8
:- set_prolog_flag(encoding, utf8).

% Fakty które możesz zmienić!

% Systemowe podpowiedzi (prompty)
system_prompt_pytacza('Zadajesz pytania dotyczące osoby lub obiektu, na które odpowiedź to "tak" lub "nie", w celu odgadnięcia, co to jest. Unikaj powtarzania pytań.').
system_prompt_odpowiadacza('Odpowiadasz na pytania tylko "tak" albo "nie". Jeśli pytanie nie jest pytaniem "tak" lub "nie", odpowiadasz "nie wiem".').

% Nazwy botów
bot1('Bot1').  % Pytacz
bot2('Bot2').  % Odpowiadacz

% Ustawienia rozmowy
co_to_jest('Bielik').  % Obiekt do odgadnięcia (np. 'Bielik' - gatunek orła)
pierwsze_pytanie('Czy to jest zwierzę?').
licznik_rozmowy(10).  % Ilość rund pytań
maksymalna_liczba_tokenów(200).
temperatura_dialogu(0.7).

% Wyjaśnienie obiektu
wyjasnienie_co_to_jest('Bielik', 'Bielik  gatunek orła to duży ptak drapieżny z rodziny jastrzębiowatych, występujący w Europie i Azji.').

% WMJ czat URL API
model_bielika('Bielik-11B-v2.3-Instruct.Q4_K_M').
url('http://127.0.0.1:8080/v1/chat/completions').

% Funkcja wysyłająca zapytanie używając OpenAI API i pobierająca odpowiedź
wyślij_zapytanie_openai(Zapytanie, PromptSystemowy, Odpowiedź) :-
    url(URL),
    maksymalna_liczba_tokenów(MaksTokeny),
    temperatura_dialogu(Temperatura),
    model_bielika(Model),
    Wiadomosci = [
        _{role: "system", content: PromptSystemowy},
        _{role: "user", content: Zapytanie}
    ],
    JsonZapytanie = _{
        model: Model,
        messages: Wiadomosci,
        max_tokens: MaksTokeny,
        temperature: Temperatura
    },
    atom_json_dict(JsonAtom, JsonZapytanie, []),
    catch(
        (
            setup_call_cleanup(
                http_open(URL, Strumien, [
                    method(post),
                    request_header('Content-Type'='application/json'),
                    post(atom(JsonAtom)),
                    cert_verify_hook(cert_accept_any)  % Dodane, jeśli używasz samopodpisanego certyfikatu
                ]),
                json_read_dict(Strumien, Odpowiedź),
                close(Strumien)
            )
        ),
        Error,
        (
            format('Błąd podczas wysyłania zapytania: ~w~n', [Error]),
            fail
        )
    ).

% Funkcje do wyświetlania

% Wyświetla pytanie bota
drukuj_pytanie(Bot, Pytanie) :-
    format('~w pyta: "~w"~n', [Bot, Pytanie]),
    nl.

% Wyświetla odpowiedź bota
drukuj_odpowiedź(Bot, Odpowiedź) :-
    format('~w odpowiada: "~w"~n', [Bot, Odpowiedź]),
    nl.

% Wyodrębnia treść z odpowiedzi API
daj_zawartość(OdpowiedźAPI, Zawartosc) :-
    (   get_dict(choices, OdpowiedźAPI, [Wybor | _]),
        get_dict(message, Wybor, Wiadomosc),
        get_dict(content, Wiadomosc, Zawartosc)
    ->  true
    ;   format('Błąd: Nie udało się wyodrębnić treści z odpowiedzi API.~n'),
        fail
    ).

% Funkcja rozpoczynająca dialog między dwoma botami
dialog_botow :-
    bot1(Bot1),
    bot2(Bot2),
    pierwsze_pytanie(PierwszePytanie),
    licznik_rozmowy(Licznik),
    co_to_jest(CoToJest),
    dialog_pomiedzy_botami(Bot1, Bot2, PierwszePytanie, Licznik, CoToJest, []).

% Funkcja prowadząca dialog między botami, ograniczona liczbą wymian
dialog_pomiedzy_botami(Bot1, Bot2, _, 0, CoToJest, Historia) :- !,
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
               'Zanalizuj czy ta odpowiedź "~w" jest poprawną odpowiedźią na pytanie o obiekt "~w" wiedząć że tak on jest tak opisany "~w". Odpowiedz tylko "tak" albo "nie".', 
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
                ->  format('~w: Świetnie! "~n~w~n" to: ~w~n', [Bot2, Zgadnij, Wyjasnienie])
                ;   format('~w: Świetnie! "~n~w~n" jest poprawną odpowiedzią.~n', [Bot2, Zgadnij])
                )
            ;   Potwierdzenie = "nie"
            ->  format('~w: Niestety, nie zgadłem. Poprawna odpowiedź to "~w".~n', [Bot1, CoToJest]),
                (   wyjasnienie_co_to_jest(CoToJest, Wyjasnienie)
                ->  format('~w: Niestety ta odpowiedź, "~n~w~n" nie jest poprawną odpowiedzią. Poprawna odpowiedź to: "~n~w". ~w~n', [Bot2, Zgadnij, CoToJest, Wyjasnienie])
                ;   format('~w: Niestety ta odpowiedź, "~n~w~n" nie jest poprawną odpowiedzią. Poprawna odpowiedź to: "~n~w".~n', [Bot2, Zgadnij, CoToJest])
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

dialog_pomiedzy_botami(Bot1, Bot2, Pytanie, Licznik, CoToJest, Historia) :-
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
            dialog_pomiedzy_botami(Bot1, Bot2, NowePytanie, NowyLicznik, CoToJest, NowaHistoria)
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

% Przykładowe wykonanie dialogu
przykladowy_dialog :-
    dialog_botow,
    halt.

% Automatyczne wykonanie po załadowaniu
:- przykladowy_dialog.
