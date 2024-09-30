# botZbotem
Jak bot z botem rozmawiał.

SWI-prolog implementacja dialogu botów przy użyciu Wielkiego Modelu Językowego (WMJ) Bielik.

## Instalacja SWI-Prolog

Na tej stronie znajdziesz instruckje jak sciągnąź SWI-Prolog w wersji dla Windows.

https://www.swi-prolog.org/Download.html

Pod Linuksem najłatwiej jest zainstalować pakiet jaki jest dostępny w dystrybucji którą używasz.

## Installacja narzędzi potrzebnych do uruchamiania WMJ Bielik.

### llama.cpp

Tutaj jest kod źródłowy llama.cpp który będziesz musiał samodzielnie skompilować (zawansowani).

https://github.com/ggerganov/llama.cpp

### ollama  

Ollama jest przyjaznym środowiskiem do obsługi WMJ takich jak Bielik.
Na tej stronie znajdziesz informację jak sciągnąć i zainstalować ollama na macOS, Linux i Windows.

https://ollama.com/download


## Installacja WMJ Bielik

WMJ Bielik jest dostępny w kilku binarnych formatach które są dostąpne na stronie HuggingFace.
To jaki format jest odpowiedni dla Ciebie zależy od tego jakim sprzętem dysponujesz i jakie narzędzia uzywasz.
Na przykład ta wersja jest dla llama.cpp:
Bielik-11B-v2.3-Instruct.Q4_K_M.gguf



# Uruchamianie

## llama.cpp

llama.cpp/llama-server  -m Bielik-11B-v2.3-Instruct.Q4_K_M.gguf --port 8080

## ollama

ollama run Bielik-11B-v2.3-Instruct.Q4_K_M


## dialog 

swipl -s dialog.pl

A pod linuksem

bzb.sh







