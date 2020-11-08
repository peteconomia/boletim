# Template do Boletim

Estrutura Básica:

* `boletim.tex` é o arquivo principal
* O diretório [`conteudo`](./conteudo/) guarda arquivos principais do boletim, capa, contracapa, preâmbulo (configurações), editorial e os capítulos.
* O diretório [`logos`](./logos/) contém imagens de todos os logos
* `Makefile` é um utilítário para compilar `boletim.tex` com bibliografia e outras configurações adicionais

Todos os conteudos são importados via `\input` no arquivo `boletim.tex`