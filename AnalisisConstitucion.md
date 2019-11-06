Análisis de la constitución
================

Sobre este trabajo
------------------

En el siguiente documento realizaremos un ejercicio analítico (más cuantitativo que cualitativo) de la constitución política de Chile.

Primeros pasos
--------------

Para comenzar cargaremos las librerías a utilizar así como la planilla con la información de la constitución.

``` r
library(dplyr)
library(tidyr)
library(forcats)
library(kableExtra)
library(readr)
library(tidytext)

constitucion <- read_csv("Datos/constitucion.csv") %>% 
  # no incluiremos las disposiciones transitorias en este análisis
  filter(cap != "Disposiciones transitorias")
```

``` r
str(constitucion)
```

    ## Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame': 217 obs. of  4 variables:
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

Las variables correspondientes a los capítulos (`cap`) y artículos (`art`) de la constitución las cambiaremos de texto a factores para futuros análisis.

``` r
constitucion <- constitucion %>% 
  mutate(
    cap = as_factor(cap),
    art = as_factor(art)
  )
```

Análisis descriptivo general
----------------------------

En primer lugar haremos un simple análisis de cada capítulo de la constitución en termino de número de artículos, incisos, y palabras.

``` r
fila_total <- constitucion %>% 
  unnest_tokens(palabras, inciso, drop = FALSE) %>% 
  summarise(
    "Artículos" = n_distinct(art),
    "Incisos" = n_distinct(inciso),
    "Palabras" = n(),
    "Incisos/Artículos" = round(n_distinct(inciso)/n_distinct(art),1),
    "Palabras/Artículos" = round(n()/n_distinct(art),1)
            ) %>% 
  mutate("Capítulo" = "",
         "Nombre capítulo" = "Toda la constitución")

constitucion %>% 
  unnest_tokens(palabras, inciso, drop = FALSE) %>% 
  group_by("Capítulo" = cap,
           "Nombre capítulo" = nom_cap) %>% 
  summarise(
    "Artículos" = n_distinct(art),
    "Incisos" = n_distinct(inciso),
    "Palabras" = n(),
    "Incisos/Artículos" = round(n_distinct(inciso)/n_distinct(art),1),
    "Palabras/Artículos" = round(n()/n_distinct(art),1)
            ) %>% 
  bind_rows(fila_total) %>% 
  kable(format = "html")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
Capítulo
</th>
<th style="text-align:left;">
Nombre capítulo
</th>
<th style="text-align:right;">
Artículos
</th>
<th style="text-align:right;">
Incisos
</th>
<th style="text-align:right;">
Palabras
</th>
<th style="text-align:right;">
Incisos/Artículos
</th>
<th style="text-align:right;">
Palabras/Artículos
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Capitulo 1
</td>
<td style="text-align:left;">
bases de la institucionalidad
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
837
</td>
<td style="text-align:right;">
2.8
</td>
<td style="text-align:right;">
93.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 2
</td>
<td style="text-align:left;">
nacionalidad y ciudadania
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
886
</td>
<td style="text-align:right;">
3.3
</td>
<td style="text-align:right;">
98.4
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 3
</td>
<td style="text-align:left;">
de los derechos y deberes constitucionales
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
5064
</td>
<td style="text-align:right;">
1.2
</td>
<td style="text-align:right;">
1012.8
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 4
</td>
<td style="text-align:left;">
gobierno
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
3749
</td>
<td style="text-align:right;">
1.0
</td>
<td style="text-align:right;">
163.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 5
</td>
<td style="text-align:left;">
congreso nacional
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
6309
</td>
<td style="text-align:right;">
1.2
</td>
<td style="text-align:right;">
203.5
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 6
</td>
<td style="text-align:left;">
poder judicial
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
1382
</td>
<td style="text-align:right;">
1.9
</td>
<td style="text-align:right;">
197.4
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 7
</td>
<td style="text-align:left;">
ministerio publico
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
1062
</td>
<td style="text-align:right;">
1.0
</td>
<td style="text-align:right;">
118.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 8
</td>
<td style="text-align:left;">
tribunal constitucional
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2255
</td>
<td style="text-align:right;">
1.3
</td>
<td style="text-align:right;">
751.7
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 9
</td>
<td style="text-align:left;">
servicio electoral y justicia electoral
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
749
</td>
<td style="text-align:right;">
1.8
</td>
<td style="text-align:right;">
187.2
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 10
</td>
<td style="text-align:left;">
contraloria general de la republica
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
510
</td>
<td style="text-align:right;">
1.0
</td>
<td style="text-align:right;">
170.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 11
</td>
<td style="text-align:left;">
fuerzas armadas, de orden y seguridad publica
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
455
</td>
<td style="text-align:right;">
1.0
</td>
<td style="text-align:right;">
91.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 12
</td>
<td style="text-align:left;">
consejo de seguridad nacional
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
252
</td>
<td style="text-align:right;">
1.0
</td>
<td style="text-align:right;">
126.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 13
</td>
<td style="text-align:left;">
banco central
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
151
</td>
<td style="text-align:right;">
1.0
</td>
<td style="text-align:right;">
75.5
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 14
</td>
<td style="text-align:left;">
gobierno y administracion interior del estado
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:right;">
3036
</td>
<td style="text-align:right;">
2.5
</td>
<td style="text-align:right;">
159.8
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitulo 15
</td>
<td style="text-align:left;">
reforma de la constitucion
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
594
</td>
<td style="text-align:right;">
1.3
</td>
<td style="text-align:right;">
198.0
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
Toda la constitución
</td>
<td style="text-align:right;">
134
</td>
<td style="text-align:right;">
217
</td>
<td style="text-align:right;">
27291
</td>
<td style="text-align:right;">
1.6
</td>
<td style="text-align:right;">
203.7
</td>
</tr>
</tbody>
</table>
La mayoría de los capítulos tiene menos de 10 artículos con solo 3 que superan este número (*Capítulo 4: Gobierno*, *Capítulo 5: Congreso Nacional* y *Capítulo 14: Gobierno y Administración Interior del Estado*). El capítulo con mayor cantidad de palabras es el **Capítulo 5** pero el que tiene mayor cantidad de palabras por inciso es el **Capítulo 3 sobre derechos y deberes constitucionales**. El capítulo más "simple" (según estas métricas) es el **Capítulo 13 sobre el Banco Central**.

``` r
stop <- read_csv("Datos/stop.csv")

constitucion_palabra <- constitucion %>% 
  unnest_tokens(palabra, inciso) %>% 
  anti_join(stop) 

constitucion_palabra %>% 
  count(palabra, sort = TRUE)
```

    ## # A tibble: 3,335 x 2
    ##    palabra            n
    ##    <chr>          <int>
    ##  1 ley              280
    ##  2 presidente       177
    ##  3 podrã            147
    ##  4 caso             104
    ##  5 constitucional    97
    ##  6 repãºblica        88
    ##  7 ejercicio         83
    ##  8 tribunal          78
    ##  9 derecho           76
    ## 10 nacional          71
    ## # ... with 3,325 more rows
