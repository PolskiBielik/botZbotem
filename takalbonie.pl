% Ustawienie kodowania UTF-8
:- set_prolog_flag(encoding, utf8).

% Potrzebne moduły
:- ['ładuj.pl'].


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

% Wyjaśnienie obiektu
wyjasnienie_co_to_jest('Bielik', 'Bielik  gatunek orła to duży ptak drapieżny z rodziny jastrzębiowatych, występujący w Europie i Azji.').


% Przykładowe wykonanie dialogu
przykladowy_dialog :-
    dialog_co_to_jest,
    halt.

% Automatyczne wykonanie po załadowaniu
:- przykladowy_dialog.
