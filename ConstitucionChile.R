library(rvest)
library(dplyr)
library(stringr)
library(readxl)

# Planilla con los links a la información de la consitución
links <- read_excel("links_constitucion.xlsx")

# Descarga de la información y limpieza general de caractéres especiales
for (i in 1:16){
  assign(paste0("cap", i, "_text"),
         links[i, 2][[1]] %>% 
           read_html() %>% 
           html_nodes(".CUERPO p") %>% 
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
           mutate(cap = links[i, 1][[1]],
                  nom_cap = links[i, 3][[1]])
  )
}

rm(links, i)

# Función para hacer arreglos finales
funcion_limpiar <- function (x){
  filter(x, !is.na(art)) %>% 
    select(cap, nom_cap, art, texto = value)
}

##-----------
## CAPÍTULO 1
##-----------

cap1_text <- cap1_text %>% 
  mutate(art = case_when(
    name %in% c(3,4,5,7, 8) ~ "artículo 1",
    name %in% c(11) ~ "artículo 2",
    name %in% c(14,15,16) ~ "artículo 3",
    name %in% c(19) ~ "artículo 4",
    name %in% c(22,23) ~ "artículo 5",
    name %in% c(26,27,28) ~ "artículo 6",
    name %in% c(31,32,33) ~ "artículo 7",
    name %in% c(36,37,38,39) ~ "artículo 8",
    name %in% c(42,43,44) ~ "artículo 9"
  )) %>% 
  funcion_limpiar()

##-----------
## CAPÍTULO 2
##-----------

cap2_text <- cap2_text %>% 
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
  funcion_limpiar()

##---
## CAPÍTULO 3
##---

cap3_text <- cap3_text %>% 
  mutate(art = case_when(
    name %in% c(2:3) ~ "artículo 19",
    name %in% c(6) ~ "artículo 20",
    name %in% c(9) ~ "artículo 21",
    name %in% c(12) ~ "artículo 22",
    name %in% c(15) ~ "artículo 23"
  )) %>% 
  funcion_limpiar()

##---
## CAPÍTULO 4
##---

cap4_text <- cap4_text %>% 
  mutate(art = case_when(
    name %in% c(3) ~ "artículo 24",
    name %in% c(6) ~ "artículo 25",
    name %in% c(9) ~ "artículo 26",
    name %in% c(12) ~ "artículo 27",
    name %in% c(15) ~ "artículo 28",
    name %in% c(18) ~ "artículo 29",
    name %in% c(21) ~ "artículo 30",
    name %in% c(24) ~ "artículo 31",
    name %in% c(27) ~ "artículo 32",
    name %in% c(32) ~ "artículo 33",
    name %in% c(35) ~ "artículo 34",
    name %in% c(38) ~ "artículo 35",
    name %in% c(41) ~ "artículo 36",
    name %in% c(44) ~ "artículo 37",
    name %in% c(47) ~ "artículo 37 bis",
    name %in% c(52) ~ "artículo 38",
    name %in% c(57) ~ "artículo 39",
    name %in% c(60) ~ "artículo 40",
    name %in% c(63) ~ "artículo 41",
    name %in% c(66) ~ "artículo 42",
    name %in% c(69) ~ "artículo 43",
    name %in% c(72) ~ "artículo 44",
    name %in% c(75) ~ "artículo 45"
  )) %>% 
  funcion_limpiar()

##---
## CAPÍTULO 5
##---

cap5_text <- cap5_text %>% 
  mutate(art = case_when(
    name %in% c(2) ~ "artículo 46",
    name %in% c(7) ~ "artículo 47",
    name %in% c(10) ~ "artículo 48",
    name %in% c(13,14) ~ "artículo 49",
    name %in% c(16) ~ "artículo 50",
    name %in% c(19) ~ "artículo 51",
    name %in% c(24:26) ~ "artículo 52",
    name %in% c(31) ~ "artículo 53",
    name %in% c(36) ~ "artículo 54",
    name %in% c(41) ~ "artículo 55",
    name %in% c(44) ~ "artículo 56",
    name %in% c(47,48) ~ "artículo 56 bis",
    name %in% c(53) ~ "artículo 57",
    name %in% c(56) ~ "artículo 58",
    name %in% c(59) ~ "artículo 59",
    name %in% c(62,63) ~ "artículo 60",
    name %in% c(66) ~ "artículo 61",
    name %in% c(69) ~ "artículo 62",
    name %in% c(74) ~ "artículo 63",
    name %in% c(77) ~ "artículo 64",
    name %in% c(82) ~ "artículo 65",
    name %in% c(85) ~ "artículo 66",
    name %in% c(88) ~ "artículo 67",
    name %in% c(91) ~ "artículo 68",
    name %in% c(94) ~ "artículo 69",
    name %in% c(97) ~ "artículo 70",
    name %in% c(100) ~ "artículo 71",
    name %in% c(103) ~ "artículo 72",
    name %in% c(106) ~ "artículo 73",
    name %in% c(109) ~ "artículo 74",
    name %in% c(112) ~ "artículo 75"
  )) %>% 
  funcion_limpiar()

##---
## CAPÍTULO 6
##---

cap6_text <- cap6_text %>% 
  mutate(art = case_when(
    name %in% c(2) ~ "artículo 76",
    name %in% c(5:11) ~ "artículo 77",
    name %in% c(14) ~ "artículo 78",
    name %in% c(17) ~ "artículo 79",
    name %in% c(20) ~ "artículo 80",
    name %in% c(23) ~ "artículo 81",
    name %in% c(26) ~ "artículo 82"
  )) %>% 
  funcion_limpiar()

##---
## CAPÍTULO 7
##---

cap7_text <- cap7_text %>% 
  mutate(art = case_when(
    name %in% c(2) ~ "artículo 83",
    name %in% c(5) ~ "artículo 84",
    name %in% c(8) ~ "artículo 85",
    name %in% c(11) ~ "artículo 86",
    name %in% c(14) ~ "artículo 87",
    name %in% c(17) ~ "artículo 88",
    name %in% c(20) ~ "artículo 89",
    name %in% c(23) ~ "artículo 90",
    name %in% c(26) ~ "artículo 91"
  )) %>% 
  funcion_limpiar()

##---
## CAPÍTULO 8
##---

cap8_text <- cap8_text %>% 
  mutate(art = case_when(
    name %in% c(2) ~ "artículo 92",
    name %in% c(5,6) ~ "artículo 93",
    name %in% c(9) ~ "artículo 94"
  )) %>% 
  funcion_limpiar()

##---
## CAPÍTULO 9
##---

cap9_text <- cap9_text %>% 
  mutate(art = case_when(
    name %in% c(2:5) ~ "artículo 94 bis",
    name %in% c(8) ~ "artículo 95",
    name %in% c(11) ~ "artículo 96",
    name %in% c(14) ~ "artículo 97"
  )) %>% 
  funcion_limpiar()

##---
## CAPÍTULO 10
##---

cap10_text <- cap10_text %>% 
  mutate(art = case_when(
    name %in% c(2) ~ "artículo 98",
    name %in% c(5) ~ "artículo 99",
    name %in% c(8) ~ "artículo 100"
  )) %>% 
  funcion_limpiar()

##---
## CAPÍTULO 11
##---

cap11_text <- cap11_text %>% 
  mutate(art = case_when(
    name %in% c(2) ~ "artículo 101",
    name %in% c(5) ~ "artículo 102",
    name %in% c(8) ~ "artículo 103",
    name %in% c(11) ~ "artículo 104",
    name %in% c(14) ~ "artículo 105"
  )) %>% 
  funcion_limpiar()

##---
## CAPÍTULO 12
##---

cap12_text <- cap12_text %>% 
  mutate(art = case_when(
    name %in% c(2) ~ "artículo 106",
    name %in% c(5) ~ "artículo 107"
  )) %>% 
  funcion_limpiar()

##---
## CAPÍTULO 13
##---

cap13_text <- cap13_text %>% 
  mutate(art = case_when(
    name %in% c(2) ~ "artículo 108",
    name %in% c(5) ~ "artículo 109"
  )) %>% 
  funcion_limpiar()

##---
## CAPÍTULO 14
##---

cap14_text <- cap14_text %>% 
  mutate(art = case_when(
    name %in% c(2) ~ "artículo 110",
    name %in% c(7:13) ~ "artículo 111",
    name %in% c(16) ~ "artículo 112",
    name %in% c(19:28) ~ "artículo 113",
    name %in% c(31) ~ "artículo 114",
    name %in% c(34,35) ~ "artículo 115",
    name %in% c(38,39) ~ "artículo 115 bis",
    name %in% c(45) ~ "artículo 116",
    name %in% c(47) ~ "artículo 117",
    name %in% c(52:54) ~ "artículo 118",
    name %in% c(57) ~ "artículo 119",
    name %in% c(60) ~ "artículo 120",
    name %in% c(63) ~ "artículo 121",
    name %in% c(66) ~ "artículo 122",
    name %in% c(71,72) ~ "artículo 123",
    name %in% c(75:81) ~ "artículo 124",
    name %in% c(84:86) ~ "artículo 125",
    name %in% c(89) ~ "artículo 126",
    name %in% c(94,95) ~ "artículo 126 bis"
  )) %>% 
  funcion_limpiar()

##---
## CAPÍTULO 15
##---

cap15_text <- cap15_text %>% 
  mutate(art = case_when(
    name %in% c(2) ~ "artículo 127",
    name %in% c(5,6) ~ "artículo 128",
    name %in% c(9) ~ "artículo 129"
  )) %>% 
  funcion_limpiar()

##---
## CAPÍTULO 16 - disposiciones transitorias
##---

cap16_text <- cap16_text %>% 
  mutate(art = case_when(
    name %in% c(2) ~ "1",
    name %in% c(5) ~ "2",
    name %in% c(8) ~ "3",
    name %in% c(11) ~ "4",
    name %in% c(14) ~ "5",
    name %in% c(17) ~ "6",
    name %in% c(20) ~ "7",
    name %in% c(23) ~ "8",
    name %in% c(26) ~ "9",
    name %in% c(29) ~ "10",
    name %in% c(32) ~ "11",
    name %in% c(35) ~ "12",
    name %in% c(38) ~ "13",
    name %in% c(41) ~ "14",
    name %in% c(44) ~ "15",
    name %in% c(47) ~ "16",
    name %in% c(50) ~ "17",
    name %in% c(53) ~ "18",
    name %in% c(56) ~ "19",
    name %in% c(59) ~ "20",
    name %in% c(62) ~ "21",
    name %in% c(65) ~ "22",
    name %in% c(68) ~ "23",
    name %in% c(71:74) ~ "24",
    name %in% c(77) ~ "25",
    name %in% c(80:82) ~ "26",
    name %in% c(85,86) ~ "27",
    name %in% c(89:93) ~ "28"
  )) %>% 
  funcion_limpiar()
