% Funkcje do wyświetlania

% Wyświetla pytanie bota
drukuj_pytanie(Bot, Pytanie) :-
    format('~w pyta: "~w"~n', [Bot, Pytanie]),
    nl.

% Wyświetla odpowiedź bota
drukuj_odpowiedź(Bot, Odpowiedź) :-
    format('~w odpowiada: "~w"~n', [Bot, Odpowiedź]),
    nl.
