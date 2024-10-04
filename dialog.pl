% Potrzebne moduły
:- use_module(library(http/json)).
:- use_module(library(http/http_open)).

% Ustawienie kodowania UTF-8
:- set_prolog_flag(encoding, utf8).

% Fakty które możesz zmienić!

% Definicja systemowego prompta
system_prompt_tekst('Jesteś znajomym, który prowadzi rozmowę, odpowiadaj krótko w jednym zdaniu').

% Nazwy botów
bot1('Bot1').
bot2('Bot2').

% Definicja rozmowy
pierwsze_pytanie('Jak się masz?').
licznik_rozmowy(5).
maksymalna_liczba_tokenów(100).
temperatura_dialogu(0.7).

% Bielik WMJ model 
model_bielika('Bielik-11B-v2.3-Instruct.Q4_K_M').

% WMJ czat url
url('http://127.0.0.1:8080/v1/chat/completions').

% Funkcja wysyłająca zapytanie używając OpenAI API i pobierająca odpowiedź
wyślij_zapytanie_openai(Zapytanie, Odpowiedź) :-
    url(URL),                               % Punkt końcowy API
    system_prompt_tekst(PromptSystemowy),   % Pobranie prompta systemowego
    maksymalna_liczba_tokenów(MaksTokeny),
    temperatura_dialogu(Temperatura),
    model_bielika(Model),
    Messages = [
        _{role: "system", content: PromptSystemowy},
        _{role: "user", content: Zapytanie}
    ],
    JsonZapytanie = _{
        model: Model,
        messages: Messages,
        max_tokens: MaksTokeny,
        temperature: Temperatura
    },
    atom_json_dict(JsonAtom, JsonZapytanie, []),
    catch(
        (
            setup_call_cleanup(
                http_open(URL, Strumień, [
                    method(post),
                    request_header('Content-Type'='application/json'),
                    post(atom(JsonAtom))
                ]),
                json_read_dict(Strumień, Odpowiedź),
                close(Strumień)
            )
        ),
        Error,
        (
            format('Błąd: ~w~n', [Error]),
            fail
        )
    ).

% Funkcja drukująca odpowiedź
drukuj_odpowiedź(Bot, Odpowiedź) :-
    format('~w odpowiada: "~w"~n', [Bot, Odpowiedź]),
    nl.

% Funkcja drukująca pytanie
drukuj_pytanie(Bot, Zapytanie) :-
    format('~w pyta: "~w"~n', [Bot, Zapytanie]),
    nl.

% Funkcja wyciągająca treść odpowiedzi z odpowiedzi API
daj_zawartość(Response, Zawartość) :-
    get_dict(choices, Response, [Choice | _]),
    get_dict(message, Choice, Message),
    get_dict(content, Message, Zawartość).

% Funkcja uruchamiająca dialog pomiędzy dwoma botami
dialog_botów :-
    bot1(Bot1),
    bot2(Bot2),
    pierwsze_pytanie(PierwszePytanie),
    licznik_rozmowy(Licznik),
    dialog_pomiędzy_botami(Bot1, Bot2, PierwszePytanie, Licznik).

% Funkcja prowadząca dialog pomiędzy dwoma botami, ograniczona liczbą wymian
dialog_pomiędzy_botami(_, _, _, 0) :- !.  % Warunek stopu po osiągnięciu limitu wymian
dialog_pomiędzy_botami(Bot1, Bot2, Zapytanie, Licznik) :-
    drukuj_pytanie(Bot1, Zapytanie),
    wyślij_zapytanie_openai(Zapytanie, Odpowiedź1),
    daj_zawartość(Odpowiedź1, Odpowiedź1Zawartość),
    drukuj_odpowiedź(Bot2, Odpowiedź1Zawartość),

    % Poproś WMJ o sformułowanie nowego pytania na podstawie odpowiedzi
    format(atom(AnalizaZapytania1), 
           'Biorąc pod uwagę następującą odpowiedź: "~w", sformułuj kolejne pytanie do dyskusji.', 
           [Odpowiedź1Zawartość]),
    wyślij_zapytanie_openai(AnalizaZapytania1, NowePytanie1),
    daj_zawartość(NowePytanie1, NowePytanie1Zawartość),
    drukuj_pytanie(Bot2, NowePytanie1Zawartość),
    wyślij_zapytanie_openai(NowePytanie1Zawartość, Odpowiedź2),

    % Przekaż nowe pytanie do drugiego bota
    daj_zawartość(Odpowiedź2, Odpowiedź2Zawartość),
    drukuj_odpowiedź(Bot1, Odpowiedź2Zawartość),

    % Poproś WMJ o sformułowanie nowego pytania na podstawie odpowiedzi
    format(atom(AnalizaZapytania2), 
           'Biorąc pod uwagę następującą odpowiedź: "~w", sformułuj kolejne pytanie do dyskusji.', 
           [Odpowiedź2Zawartość]),
    wyślij_zapytanie_openai(AnalizaZapytania2, NowePytanie2),
    daj_zawartość(NowePytanie2, NowePytanie2Zawartość),

    % Zmniejsz licznik i kontynuuj dialog
    NowyLicznik is Licznik - 1,
    dialog_pomiędzy_botami(Bot1, Bot2, NowePytanie2Zawartość, NowyLicznik).

% Przykładowe wywołanie dialogu
przykładowy_dialog :-
    dialog_botów,
    halt.

% Automatyczne uruchomienie przykładowego dialogu po załadowaniu
:- przykładowy_dialog.
