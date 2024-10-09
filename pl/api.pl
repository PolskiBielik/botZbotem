
% Potrzebne moduły
:- use_module(library(http/json)).
:- use_module(library(http/http_open)).

% Ustawienie kodowania UTF-8
:- set_prolog_flag(encoding, utf8).

% Fakty które możesz zmienić!

% Ustawienia
maksymalna_liczba_tokenów(200).
temperatura_dialogu(0.7).

% WMJ czat URL API
domyślny_model_bielika('Bielik-11B-v2.3-Instruct.Q4_K_M').
domyślny_url('http://127.0.0.1:8080/v1/chat/completions').

model_bielika('Bielik-11B-v2.3-Instruct.Q4_K_M').
url('http://127.0.0.1:8080/v1/chat/completions').

% Systemowe prompty
system_prompt_tak_albo_nie('Odpowiadasz na pytania tylko "tak" albo "nie". Jeśli pytanie nie jest pytaniem "tak" lub "nie", odpowiadasz "nie wiem".').


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

% Wyodrębnia treść z odpowiedzi API
daj_zawartość(OdpowiedźAPI, Zawartosc) :-
    (   get_dict(choices, OdpowiedźAPI, [Wybor | _]),
        get_dict(message, Wybor, Wiadomosc),
        get_dict(content, Wiadomosc, Zawartosc)
    ->  true
    ;   format('Błąd: Nie udało się wyodrębnić treści z odpowiedzi API.~n'),
        fail
    ).
