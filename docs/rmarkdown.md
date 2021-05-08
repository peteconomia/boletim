# RMarkdown

### Abreviações

|  RMarkdown   | LaTeX Output |
| :----------: | :----------: |
| `[@abbr:key]` | `\abbr{key}` |

### Referenciamento

| Descrição |   RMarkdown    |    LaTeX Output    |
| :-------: | :------------: | :----------------: |
|  Figuras  |  `[@fig:key]`   |  `\ref{fig:key}`   |
|  Tabelas  |  `[@tab:key]`   |  `\ref{tab:key}`   |
|  Quadros  |  `[@box:key]`   |    `\ref{key}`     |
| Equações  |   `[@eq:key]`   |   `\ref{eq:key}`   |
| Capítulo  |   `[@ch:key]`   |   `\ref{ch:key}`   |
|  Seções   |  `[@sec:key]`   |  `\ref{sec:key}`   |
| Subseções | `[@subsec:key]` | `\ref{subsec:key}` |

### Figuras
```r
```{r label, fig.cap = "Titulo", fig.subcap = "Subtítulo", fig.source = "IBGE", fig.notes = "O estado de São Paulo teve nota suspensa"}
ggplot(mpg) +
    geom_point(aes(x = displ, y = hwy, color = class))
```
```

Chunck opções:

- `label`: nome de identificação para referenciar
- `fig.cap`: título da figura
- `fig.subcap`: subtítulo da figura
- `fig.source`: fonte dos dados
- `fig.notes`: notas
- `wrap`: quando é necessário agrupar mais de uma figura utilize `wrap = "open"` no primeiro chunck e `wrap = "close"` no último chunck. Se for apenas uma figura utilize `wrap = TRUE`

Para referenciar a figura: `[@fig:label]`

### Tabelas

```r
```{r eco}
economics %>% 
  head() %>% 
  select(pce:unemploy) %>% 
  kableExtra::kbl(
    caption = "Exemplo de Tabela", 
    booktabs = TRUE
  ) %>% 
  kable_styling(full_width = T)
```
```
Para referenciar a tabela: `[@tab:eco]`

### Quadros

```markdown
::: {.smbox data-latex="[label={label}]{Titulo}"}
Duis ullamco cupidatat et sint dolor aliqua. Amet ad labore sunt esse in ad id nisi officia culpa. Proident fugiat laborum nisi laborum officia tempor excepteur ut veniam enim deserunt.
:::
```

Para referenciar o quadro: `[@box:label]`