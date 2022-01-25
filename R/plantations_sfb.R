# Public forests: Serviço Florestal Brasileiro. Includes military areas.
# Cadastro Nacional de Florestas Públicas (CNFP)
# Data 2008 - 2020 https://www.florestal.gov.br/cadastro-nacional-de-florestas-publicas
# 2019 updated
#https://www.florestal.gov.br/cadastro-nacional-de-florestas-publicas/127-informacoes-florestais/cadastro-nacional-de-florestas-publicas-cnfp/1894-cadastro-nacional-de-florestas-publicas-atualizacao-2019

#Plantations
#Interactive panel with data
#https://snif.florestal.gov.br/pt-br/florestas-plantadas/452-painel-interativo-1a?tipo=tableau&modal=1

#packages
library(tidyverse)
#library(readxl)
library(sf)

#Load usefull references
bla_state_names <- c("Acre", "Amapá", "Amazonas", "Maranhão", 
                     "Mato Grosso", "Pará", "Tocantins", "Rondônia", "Roraima")
bla_state_siglas <- c("AC", "AP", "AM", "MA", 
                      "MT", "PA", "TO", "RO", "RR")
bla_state_capitals <- data.frame(name_muni = c("Manaus", "Macapá", "Porto Velho", "Rio Branco", 
                                               "Boa Vista",
                                               "São Luís", "Cuiabá", "Belém", "Palmas"), 
                                 codmun7 = c(1302603, 1600303, 1100205, 1200401, 
                                             1400100,
                                             2111300, 5103403, 1501402, 1721000)
)
# Municipality names, codes, polygons and areas from IBGE. Updated August 2020. Accessed 8 January 2022
# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html?=&t=downloads
ibge_muni <- "vector\\ninestate_muni.shp"
sf_ninestate_muni <- st_read(ibge_muni) %>% filter(SIGLA_UF %in% bla_state_siglas)
#States
ibge_states <- "vector\\ninestate_poly.shp"
sf_ninestate <- st_read(ibge_states) %>% filter(SIGLA_UF %in% bla_state_siglas)
sf_ninestate %>% ggplot() + geom_sf(aes(fill = SIGLA_UF))
sf_ninestate_outline <- st_union(sf_ninestate)

#Plantations
#Global plantations from global forest watch
#gfw_plant <- "E:\\gfw\\plantations\\Tree_plantations\\bla_Tree_plantations.shp"
#global_plantations <- st_make_valid(st_read(gfw_plant))
#maintain only those in borders of nine states
#bla_plantations <- st_intersection(st_transform(sf_ninestate_outline, st_crs(global_plantations)), 
#                global_plantations)
#export
#st_write(bla_plantations, "vector\\bla_plantations.shp")

bla_plantations <- st_read("vector\\bla_plantations.shp")
#load data
public_ap <- st_read("E:\\bla_public_lands\\2019\\AP.shp")
unique(public_ap$classe)
#2019
#[1] "GLEBAFED"    "UCEST"       "USO MILITAR" "UCFED"       "GLEBAEST"    "ASSENFED"   
#[7] "UCMUN"       "TI"          "TI / UCFED" 
#2017
#[1] "GLEBAFED"   "TI"         "AREAMILIT"  "TI / UCFED" "ASSENFED"   "UCEST"      "UCFED"     
#[8] "UCMUN"  
