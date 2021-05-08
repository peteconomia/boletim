# Documentação

O Boletim de Conjuntura é escrito em RMarkdown, uma extensão do markdown para construir relatórios dinâmicos.

[RMarkdown](rmarkdown.md)

## Compilando o PDF

### Dependências

- [Pandoc](https://pandoc.org/installing.html)
- MikTex ou TeXLive

  - Windows: [miktex.org](https://miktex.org/)
  - Linux flavors: TeXLive

Um conjunto básico de pacotes R é necessário para compilar o pdf:

```r
install.packages(c("bookdown", "tinytex", "rmarkdown"))
```

Além desses é necessário instalar os pacotes usado em cada capítulo. Veja o arquivo `main.Rmd` dentro de cada pasta `cap_*`. Em geral os pacotes bases são:

- tidyverse
- [petknitr](https://github.com/peteconomia/petknitr)
- [petgg](https://github.com/peteconomia/petgg)
- [sidrar](https://cran.r-project.org/web/packages/sidrar/index.html)
- [ipeadatar](https://cran.r-project.org/web/packages/ipeadatar/index.html)
- [siconfir](https://github.com/pedrocastroo/siconfir)
- PNADcIBGE
- ecoseries
- kableExtra
- scales
- survey
- convey
- [archive](https://github.com/jimhester/archive)
- zoo

Execute o arquivo `build.R` ou pelo `R` console:
```r
bookdown::render_book("index.Rmd", output_format = "bookdown::pdf_book")
```

ou terminal:
```
make pdf
```

## Dados

| Tema                    | Frequência                | Fonte                    |
| ----------------------- | ------------------------- | ------------------------ |
| Fiscais/Contas Públicas | Bimestral e Quadrimestral | Siconfi/Tesouro Nacional |
| Emprego                 | Trimestral e Mensal       | CAGED, RAIS              |
| Social                  | Trimestal e Anual         | PNAD/IBGE                |
| Agricultura             | Mensal                    | SIDRA/IBGE               |
| Balança Comercial       | Trimestral e Mensal       | FIETO, MDIC              |
| PIB                     | Trimestral                | IBGE                     |


### Dados Fiscais/Contas Públicas

Os dados fiscais são extraíos do Siconfi/Tesouro Nacional, os de frequência bimestral estão no Relatório Resumido de Execução Orçamentária (RREO), nele é apresentado dados da execução orçamentária para todos os entes da federação, sendo possível obter dados à nivel de município. Dados de frequência quadrimestral estão contidos no Relatório de Gestão Fiscal (RGF). O RGF é menos detalhado que RREO, nele há demonstrativo da despesa com pessoal, dívida consolidada, operações de crédito, entre outros.

Publicações do Tesouro Nacional:
* [RREO](https://www.tesourotransparente.gov.br/publicacoes/rreo-em-foco-estados-e-municipios/2020/16)
* [RGF](https://www.tesourotransparente.gov.br/publicacoes/rgf-em-foco-estados-e-municipios/2020/27?ano_selecionado=2020)

### Emprego
Os dados dos empregos são extraidos do CAGED (Cadastro Geral de Empregados e Desempregados) que possuem frequência mensal submetidos a competência do Ministério do Trabalho, dele são apresentados informações sobre o emprego formal, como saldo de empregos, salários de contratação, perfil, idade, genêro. Também, são usados os dados da PNAD-Continua divulgadas pelo IBGE, com a frequência mensal, trimestral e anual, nela temos informações sobre a taxa de desemprego, rendimentos, ocupação e etc.

Publicações do Ministério do Trabalho e IBGE:
* [CAGED](http://pdet.mte.gov.br/novo-caged)
* [PNAD-Continua](https://www.ibge.gov.br/estatisticas/sociais/trabalho/9171-pesquisa-nacional-por-amostra-de-domicilios-continua-mensal.html?=&t=o-que-e)
* [Seguro-desemprego](http://pdet.mte.gov.br/seguro-desemprego)

### Indicadores Sociais

Apresentação dos principais indicadores sociais do estado do Tocantins, quais sejam, Taxa de Pobreza, Taxa de Extrema Pobreza e Índice de Gini. Os dados são divulgados pelo IBGE a partir de dados da PNAD Contínua e são disponibilizados a partir da divulgação do documento Síntese dos Indicadores Sociais.

* [Síntese dos Indicadores Sociais](https://www.ibge.gov.br/estatisticas/sociais/protecao-social/9221-sintese-de-indicadores-sociais.html?=&t=o-que-e)

### Agricultura
Os dados da agricultura são extraidos do IBGE, pelo SIDRA (Sistema IBGE de Recuperação Automática), que possuem a frequência variada, mas, frequentemente mensal. Nela, temos os dados da produção agricola, rendimento médio, plantio por hectáres, abates de bovinos, suinos, frangos, produção de leite e produção de ovos.

Publicações do SIDRA:

* [Abate](https://sidra.ibge.gov.br/home/abate/brasil)
* [Leite](https://sidra.ibge.gov.br/home/leite/brasil)
* [Produção de Ovos de Galinha](https://sidra.ibge.gov.br/home/pog/brasil)
* [Levantamento Sistemático da Produção Agrícola](https://sidra.ibge.gov.br/home/lspa/brasil)


### Balança Comercial
Os dados da balança comercial são extraídos diretamente do COMEX STAT(plataforma oficial do MDIC(Ministério da Indústria e Comércio)) e são disponibilizados dados mensais para todas as cidades, estados, produotos e setores necessários para a pesquisa!

* [Tocantins](http://comexstat.mdic.gov.br/pt/comex-vis)

## Convenções

### Estatísticas

* Dados desconhecidos/nulos: indicado por `...`
* O hífen (`-`) entre dois valores, como 2019-2020, indica um intervalo incluindo o primeiro e último valor
* O sinal de mais (`+`) no final de um valor, como 65+, indica valores maiores e igual a ele

### Gráficas

A padronização dos gráficos é feita pelo pacote [petgg](https://github.com/peteconomia/petgg)

* Título autoexplicativo

* Não é necessário título para os eixos, exceto para gráficos de dispersão

* Fonte do título, eixos e legenda é a mesma do texto

* Tamanho da fonte dos eixos e legendas são iguais

* Posição da legenda é abaixo do gráfico

* Título do gráfico inicia com letra maiúscula, a partir da primeira palavra inicia com letra minúscula (exceto para substantivos próprios). Mesma recomendação para tabelas

### Comandos LaTeX

* `\source{}` para colocar a fonte do gráfico ou tabela
* `\notes{}` para colocar notas no gráfico ou tabela
* `\footnote{}` para notas de rodapé
* `\abbr{}` ou `\acrshort{}` para usar uma sigla
  * A lista de siglas estão no arquivo [`siglas.tex`](../tex/conteudo/siglas.tex)

* `\trimestres` ou `\bimestres` para printar as abreviações dos trimestres ou bimestres. Utilize dentro de `\notes{}`