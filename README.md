# botZbotem

How bot talked to bot.

SWI-Prolog implementation of bot dialogues using the Large Language Model (LLM) Bielik.

## SWI-Prolog Installation

On this page, you will find instructions on how to download SWI-Prolog for Windows.

https://www.swi-prolog.org/Download.html

Under Linux, it is easiest to install the package available in your distribution.

## Installation of Tools Needed to Run LLM Bielik

### llama.cpp

Here is the llama.cpp source code that you will need to compile yourself (advanced users).

https://github.com/ggerganov/llama.cpp

### ollama

Ollama is a user-friendly environment for working with LLMs such as Bielik. On this page, you will find instructions on how to download and install ollama on macOS, Linux, and Windows.

https://ollama.com/download


## LLM Bielik Installation

LLM Bielik is available in several binary formats that can be found on the HuggingFace website. Which format is suitable for you depends on the hardware you have and the tools you use. For example, this version is for llama.cpp:

Bielik-11B-v2.3-Instruct.Q4_K_M.gguf

## Running

### llama.cpp

llama.cpp/llama-server -m Bielik-11B-v2.3-Instruct.Q4_K_M.gguf --port 8080

### ollama

ollama run Bielik-11B-v2.3-Instruct.Q4_K_M

## Dialog

### swipl

swipl -s dialog.pl

### bzb.sh (under Linux)

bzb.sh

## TakAlboNie

### swipl

swipl -s takalbonie.pl

### 10pytań.sh (under Linux)

10pytań.sh

This text has been translated from Polish to English by Bielik LLM from the original text in CZYTAJMNIE.md preserving Markdown format.

Ten tekst został przetłumaczony z języka polskiego na angielski przez Bielik WMJ z oryginalnego tekstu w CZYTAJMNIE.md, zachowując formatowanie Markdown.


