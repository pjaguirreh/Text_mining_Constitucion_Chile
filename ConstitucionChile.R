library(rvest)
library(dplyr)
library(stringr)

##-----------
## CAPÍTULO 1
##-----------

web1 <- read_html("https://www.senado.cl/constitucion-politica-capitulo-i-bases-de-la-institucionalidad/senado/2012-01-16/093048.html")

cap1 <- web %>% html_nodes("strong~ p")

cap1_text <- cap1 %>% 
  html_text() %>% 
  tibble::enframe() %>% 
  mutate(value = str_replace(value, "Â", " "),
         value = str_replace(value, "Ã", "í"),
         value = str_replace(value, "Ã", "í"),
         value = str_replace(value, "Ã³", "ó"),
         value = str_replace(value, "í³", "ó"),
         value = str_replace(value, "Ã¡", "á"),
         value = str_replace(value, "í¡", "á"),
         value = str_replace(value, "íº", "ú"),
         value = str_replace(value, "í³", "ó"),
         value = str_replace(value, "Ãº", "ú"),
         value = str_replace(value, "í©", "é"),
         value = str_replace(value, "º.-", ""),
         value = str_replace(value, "º .-", ""),
         value = str_replace(value, " .-", "")) %>% 
  mutate(cap = "capítulo 1",
         nom_cap = "bases de la institucionalidad") %>% 
  mutate(art = case_when(
    name %in% c(1,2,3,5,6) ~ "artículo 1",
    name %in% c(9) ~ "artículo 2",
    name %in% c(12,13,14) ~ "artículo 3",
    name %in% c(17) ~ "artículo 4",
    name %in% c(20,21) ~ "artículo 5",
    name %in% c(24,25,26) ~ "artículo 6",
    name %in% c(29,30,31) ~ "artículo 7",
    name %in% c(34,35,36,37) ~ "artículo 8",
    name %in% c(40,41,42) ~ "artículo 9"
  )) %>% 
  filter(!is.na(art)) %>% 
  select(cap, nom_cap, art, texto = value)

rm(web, cap1)

##-----------
## CAPÍTULO 2
##-----------

web2 <- read_html("https://www.senado.cl/capitulo-ii-nacionalidad-y-ciudadania/senado/2012-01-16/093226.html")

cap2 <- web2 %>% html_nodes(".CUERPO p")

cap2_text <- cap2 %>% 
  html_text() %>% 
  tibble::enframe() %>% 
  mutate(value = str_replace(value, "Â", " "),
         value = str_replace(value, "Ã", "í"),
         value = str_replace(value, "Ã", "í"),
         value = str_replace(value, "Ã³", "ó"),
         value = str_replace(value, "í³", "ó"),
         value = str_replace(value, "Ã¡", "á"),
         value = str_replace(value, "í¡", "á"),
         value = str_replace(value, "í¡", "á"),
         value = str_replace(value, "íº", "ú"),
         value = str_replace(value, "í³", "ó"),
         value = str_replace(value, "Ãº", "ú"),
         value = str_replace(value, "í©", "é"),
         value = str_replace(value, "º.-", ""),
         value = str_replace(value, "º .-", ""),
         value = str_replace(value, " .-", ""),
         value = str_replace(value, ".-", ""),
         value = str_replace(value, "[0-9]+", "")) %>% 
  mutate(cap = "capítulo 2",
         nom_cap = "nacionalidad y ciudadanía") %>% 
  mutate(art = case_when(
    name %in% c(2:6) ~ "artículo 10",
    name %in% c(9:13) ~ "artículo 11",
    name %in% c(16) ~ "artículo 12",
    name %in% c(19:22) ~ "artículo 13",
    name %in% c(25,26) ~ "artículo 14",
    name %in% c(29,30) ~ "artículo 15",
    name %in% c(33:36) ~ "artículo 16",
    name %in% c(39:42) ~ "artículo 17",
    name %in% c(45:47) ~ "artículo 18"
  )) %>% 
  filter(!is.na(art)) %>% 
  select(cap, nom_cap, art, texto = value)

rm(web2, cap2)

##---
## CAPÍTULO 3
##---

web3 <- read_html("https://www.senado.cl/capitulo-iii-de-los-derechos-y-deberes-constitucionales/senado/2012-01-16/093413.html")

cap3 <- web3 %>% html_nodes(".CUERPO p")

cap3_text <- cap3 %>% 
  html_text() %>% 
  tibble::enframe() %>% 
  mutate(value = str_replace(value, "Â", " "),
         value = str_replace(value, "Ã", "í"),
         value = str_replace(value, "Ã", "í"),
         value = str_replace(value, "Ã³", "ó"),
         value = str_replace(value, "í³", "ó"),
         value = str_replace(value, "Ã¡", "á"),
         value = str_replace(value, "í¡", "á"),
         value = str_replace(value, "í¡", "á"),
         value = str_replace(value, "íº", "ú"),
         value = str_replace(value, "í³", "ó"),
         value = str_replace(value, "Ãº", "ú"),
         value = str_replace(value, "í©", "é"),
         value = str_replace(value, "º.-", ""),
         value = str_replace(value, "º .-", ""),
         value = str_replace(value, " .-", ""),
         value = str_replace(value, ".-", ""),
         value = str_replace(value, "[0-9]+", "")) %>% 
  mutate(cap = "capítulo 3",
         nom_cap = "de los derechos y deberes constitucionales") %>% 
  mutate(art = case_when(
    name %in% c(2:3) ~ "artículo 19",
    name %in% c(6) ~ "artículo 20",
    name %in% c(9) ~ "artículo 21",
    name %in% c(12) ~ "artículo 22",
    name %in% c(15) ~ "artículo 23"
  )) %>% 
  filter(!is.na(art)) %>% 
  select(cap, nom_cap, art, texto = value)

rm(web3, cap3)

##---
## CAPÍTULO 4
##---

web4 <- read_html("https://www.senado.cl/capitulo-iv-gobierno/senado/2012-01-16/094234.html")

cap4 <- web4 %>% html_nodes("p+ p")

cap4_text <- cap4 %>% 
  html_text() %>% 
  tibble::enframe() %>% 
  mutate(value = str_replace(value, "Â", " "),
         value = str_replace(value, "Ã", "í"),
         value = str_replace(value, "Ã", "í"),
         value = str_replace(value, "Ã³", "ó"),
         value = str_replace(value, "í³", "ó"),
         value = str_replace(value, "Ã¡", "á"),
         value = str_replace(value, "í¡", "á"),
         value = str_replace(value, "í¡", "á"),
         value = str_replace(value, "íº", "ú"),
         value = str_replace(value, "í³", "ó"),
         value = str_replace(value, "Ãº", "ú"),
         value = str_replace(value, "í©", "é"),
         value = str_replace(value, "º.-", ""),
         value = str_replace(value, "º .-", ""),
         value = str_replace(value, " .-", ""),
         value = str_replace(value, ".-", ""),
         value = str_replace(value, "[0-9]+", "")) %>% 
  mutate(cap = "capítulo 4",
         nom_cap = "gobierno") %>% 
  mutate(art = case_when(
    name %in% c(2) ~ "artículo 24",
    name %in% c(5) ~ "artículo 25",
    name %in% c(8) ~ "artículo 26",
    name %in% c(11) ~ "artículo 27",
    name %in% c(14) ~ "artículo 28",
    name %in% c(17) ~ "artículo 29",
    name %in% c(20) ~ "artículo 30",
    name %in% c(23) ~ "artículo 31",
    name %in% c(26) ~ "artículo 32",
    name %in% c(31) ~ "artículo 33",
    name %in% c(34) ~ "artículo 34",
    name %in% c(37) ~ "artículo 35",
    name %in% c(40) ~ "artículo 36",
    name %in% c(43) ~ "artículo 37",
    name %in% c(46) ~ "artículo 37 bis",
    name %in% c(51) ~ "artículo 38",
    name %in% c(56) ~ "artículo 39",
    name %in% c(59) ~ "artículo 40",
    name %in% c(62) ~ "artículo 41",
    name %in% c(65) ~ "artículo 42",
    name %in% c(68) ~ "artículo 43",
    name %in% c(71) ~ "artículo 44",
    name %in% c(74) ~ "artículo 45"
  )) %>% 
  filter(!is.na(art)) %>% 
  select(cap, nom_cap, art, texto = value)

rm(web4, cap4)

##---
## CAPÍTULO 5
##---

##---
## CAPÍTULO 6
##---

##---
## CAPÍTULO 7
##---

##---
## CAPÍTULO 8
##---

##---
## CAPÍTULO 9
##---

##---
## CAPÍTULO 10
##---

##---
## CAPÍTULO 11
##---

##---
## CAPÍTULO 12
##---

##---
## CAPÍTULO 13
##---

##---
## CAPÍTULO 14
##---

##---
## CAPÍTULO 15
##---
