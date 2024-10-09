% Ustawienie kodowania UTF-8
:- set_prolog_flag(encoding, utf8).

% Potrzebne moduły
:- ['ładuj.pl'].

% Fakty które możesz zmienić!

% Definicja systemowego prompta
system_prompt_tekst('Jesteś znajomym, który prowadzi rozmowę, odpowiadaj krótko w jednym zdaniu').

% Nazwy botów
bot1('Bot1').
bot2('Bot2').

% Definicja rozmowy
pierwsze_pytanie('Jak się masz?').
licznik_rozmowy(5).

% Przykładowe wywołanie dialogu
przykładowy_dialog :-
    dialog_botów,
    halt.

% Automatyczne uruchomienie przykładowego dialogu po załadowaniu
:- przykładowy_dialog.
