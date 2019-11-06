Análisis de la constitución
================

``` r
library(dplyr)
library(tidyr)
library(forcats)
library(kableExtra)
library(readr)

constitucion <- read_csv("Planillas exportadas/constitucion.csv")
```

``` r
str(constitucion)
```

    ## Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame': 255 obs. of  4 variables:
    ##  $ cap    : chr  "Capitulo 1" "Capitulo 1" "Capitulo 1" "Capitulo 1" ...
    ##  $ nom_cap: chr  "bases de la institucionalidad" "bases de la institucionalidad" "bases de la institucionalidad" "bases de la institucionalidad" ...
    ##  $ art    : chr  "artículo 1" "artículo 1" "artículo 1" "artículo 1" ...
    ##  $ inciso : chr  "Las personas nacen libres e iguales en dignidad y derechos." "La familia es el núcleo fundamental de la sociedad." "El Estado reconoce y ampara a los grupos intermedios a través de los cuales se organiza y estructura la socieda"| __truncated__ "El Estado está al servicio de la persona humana y su finalidad es promover el bien común, para lo cual debe con"| __truncated__ ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   cap = col_character(),
    ##   ..   nom_cap = col_character(),
    ##   ..   art = col_character(),
    ##   ..   inciso = col_character()
    ##   .. )

``` r
constitucion <- constitucion %>% 
  mutate(
    cap = as_factor(cap),
    art = as_factor(art)
  )
```

``` r
constitucion %>% 
  group_by("Capítulo" = cap) %>% 
  summarise(
    "Artículos" = n_distinct(art),
    "Incisos" = n_distinct(inciso)
            ) %>% 
  kable(format = "html")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
Capítulo
</th>
<th style="text-align:right;">
Artículos
</th>
<th style="text-align:right;">
Incisos
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Capitulo 1
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 2
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
30
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 4
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
23
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 5
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 6
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 7
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 8
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 9
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 10
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 11
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 12
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 13
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 14
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
48
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 15
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Disposiciones transitorias
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
38
</td>
</tr>
</tbody>
</table>
