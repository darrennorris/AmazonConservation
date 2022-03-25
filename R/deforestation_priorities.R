# Deforestation priorities
# Data obtained from 
# http://combateaodesmatamento.mma.gov.br/municipios-prioritarios
# pdf tables downloaded and converted to excel: https://www.pdftoexcelconverter.net/
#packages
library(tidyverse)
library(readxl)
library(sf)
library(stringi)

df_priority <- read_excel("data//priority_municipality2021.xlsx", 
                                   na = c("", "NA"), .name_repair = "universal")

df_gdppop_muni_02a19 <- read_excel("data//bla_municipality_gdppop_02a19.xlsx", 
                                   na = c("", "NA"), .name_repair = "universal") %>% 
  rename(gva_total = Valor.adicionado.bruto.total....a.preços.correntes...R..1.000.) %>% 
  rename(gva_services = Valor.adicionado.bruto.dos.Serviços...a.preços.correntes.....exceto.Administração..defesa..educação.e.saúde.públicas.e.seguridade.social...R..1.000.) %>%
  rename(gva_admin = Valor.adicionado.bruto.da.Administração..defesa..educação.e.saúde.públicas.e.seguridade.social....a.preços.correntes...R..1.000.)

# Municipality names, codes, polygons and areas from IBGE. Updated August 2020. Accessed 8 January 2022
# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html?=&t=downloads
sf_ninestate_muni <- st_read("vector/brazil_ninestate_municipalities/ninestate_muni.shp")

#Basic reference vectors
#Not obvious how to select state capitals
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

#Grajaú included on both Amazon and Cerrado lists
df_priority %>% 
  mutate(muni_inep = stri_trans_general(toupper(NOME), 
                                        "Latin-ASCII")) %>% 
  group_by(UF, muni_inep, priority_type) %>% 
  summarise(first_year = min(year_included)) %>%
  right_join(
df_gdppop_muni_02a19 %>% 
  group_by(uf_sigla, uf) %>% 
  summarise(count_muni = length(unique(codmun7))) %>% right_join(
    data.frame(sf_ninestate_muni), by = c("uf_sigla" = "SIGLA_UF")
  ) %>% select(uf_sigla, uf, CD_MUN, NM_MUN, AREA_KM2) %>% 
  mutate(muni_inep = stri_trans_general(toupper(NM_MUN), "Latin-ASCII")), 
by = c( "UF" = "uf_sigla", "muni_inep" = "muni_inep")
) %>% 
  mutate(priority_type = if_else(is.na(priority_type), 
                                 "not included", priority_type)) %>%
  crossing(year = 2002:2019) %>% 
  select(UF, uf, NM_MUN, year, priority_type, first_year) %>% 
  arrange(UF, NM_MUN)  %>% 
  filter(year==2019) %>%
  write.csv("muni_fixedpriority.csv", row.names = FALSE)
  write.csv("muni_fixedpriority_year.csv", row.names = FALSE)
