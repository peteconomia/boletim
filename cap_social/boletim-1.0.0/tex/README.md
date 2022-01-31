# Template

Estrutura Básica:

* `boletim.tex` é o arquivo principal
* O diretório [`conteudo`](./conteudo/) guarda arquivos principais do boletim, capa, contracapa, preâmbulo (configurações), apresentação e os capítulos.
* O diretório [`logos`](./logos/) contém todas as logos
* `Makefile` é um utilítário para compilar `boletim.tex` com bibliografia e outras configurações adicionais

Para compilar o arquivo pdf compile `boletim.tex`:

```bash
make build
```

Compilar a versão final:

```bash
make publish
```