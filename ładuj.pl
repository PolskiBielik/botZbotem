% Potrzebne moduły
:- use_module(library(http/json)).
:- use_module(library(http/http_open)).

% Ustawienie kodowania UTF-8
:- set_prolog_flag(encoding, utf8).

:- consult('pl/drukuj.pl').
:- consult('pl/api.pl').
:- consult('pl/prompty.pl').
:- consult('pl/dialogi.pl').
