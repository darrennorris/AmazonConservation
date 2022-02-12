#rural credit Matriz de Dados do Crédito Rural (MDCR) http://www.bcb.gov.br/pt-br/#!/c/MICRRURAL/
library(tidyverse)
library(sf)
library(stringi)
library(readxl)
bla_state_names <- c("Acre", "Amapá", "Amazonas", "Maranhão", 
                     "Mato Grosso", "Pará", "Tocantins", "Rondônia", "Roraima")
bla_state_siglas <- c("AC", "AP", "AM", "MA", 
                      "MT", "PA", "TO", "RO", "RR")
bla_state_codes <- c("12", "16", "13", "21", "51", "15", "17", "11", "14")
bla_state_capitals <- data.frame(name_muni = c("Manaus", "Macapá", "Porto Velho", "Rio Branco", 
                                               "Boa Vista",
                                               "São Luís", "Cuiabá", "Belém", "Palmas"), 
                                 codmun7 = c(1302603, 1600303, 1100205, 1200401, 
                                             1400100,
                                             2111300, 5103403, 1501402, 1721000)
)
ibge_muni <- "vector\\ninestate_muni.shp"
sf_ninestate_muni <- st_read(ibge_muni) %>% filter(SIGLA_UF %in% bla_state_siglas)
sf_ninestate_muni %>% 
  mutate(muni_inep = stri_trans_general(toupper(NM_MUN), "Latin-ASCII")) -> sf_ninestate_muni

myfile <- "E:\\Contratos de Investimento por Município e Produto.csv"
rural_credit_muni <- read.csv(myfile, as.is=TRUE, encoding = "UTF-8")
unique(rural_credit_muni$cdEstado)
# join with IBGE. Codes do not match IBGE codes. Uses código cadmu .....
# vector of municipio names that are not shared
rural_credit_muni %>% 
  #sample_n(10000) %>%
  mutate(muni_inep = stri_trans_general(toupper(Municipio), "Latin-ASCII")) -> rural_credit_muni
rural_credit_muni %>%  group_by(muni_inep) %>% 
  summarise(count_state = length(unique(cdEstado))) %>% 
  filter(count_state == 1) %>% pull(muni_inep) -> muni_inep_4state

data.frame(sf_ninestate_muni) %>% 
  filter(muni_inep %in% all_of(muni_inep_4state)) %>% 
  select(!geometry) %>% left_join(
rural_credit_muni %>% 
  sample_n(100000) %>%
  group_by(cdEstado, Municipio, muni_inep) %>% 
  summarise(acount = n()) %>%
filter(muni_inep %in% all_of(muni_inep_4state))
) %>% 
  filter(!is.na(cdEstado)) %>% 
  #group_by(SIGLA_UF) %>% summarise(state_code_count = length(unique(cdEstado))) 
  group_by(SIGLA_UF, cdEstado) %>% summarise(acount = n()) %>% 
  ungroup() -> dfstatecode

#Get only for municipios in the Brazilian Legal Amazon
rural_credit_muni %>% 
  filter(cdEstado %in% all_of(as.numeric(dfstatecode$cdEstado))) -> bla_rural_credit
#Add state acronyms
bla_rural_credit %>% 
  left_join(dfstatecode %>% select(cdEstado, SIGLA_UF)) -> bla_rural_credit
bla_rural_credit %>% 
  mutate(muni_inep = if_else(Municipio=="FORTALEZA DO TABOCÃO", "TABOCAO", 
                             muni_inep)) -> bla_rural_credit
#Add IBGE muni codes and names  
bla_rural_credit %>% 
  left_join(data.frame(sf_ninestate_muni) %>% 
              select(!geometry), 
            by = c("SIGLA_UF" = "SIGLA_UF", "muni_inep" = "muni_inep")) -> bla_rural_credit

# total credit per year. 
# example with different programs: Assessing multidimensional sustainability www.pnas.org/cgi/doi/10.1073/pnas.1920998117
#999 has more than double of any other program
bla_rural_credit %>% 
  mutate(value_reais = as.numeric(gsub(",",".",VlCusteio))) %>%
  group_by(cdPrograma) %>% summarise(tot_reais = sum(value_reais, na.rm = TRUE)) %>% 
  arrange(desc(tot_reais))

#Export
df_gdppop_muni_02a19 <- read_excel("data//bla_municipality_gdppop_02a19.xlsx", 
                                   na = c("", "NA"),
                                   .name_repair = "universal")
df_gdppop_muni_02a19 %>% 
  group_by(uf_sigla, uf) %>% 
  summarise(count_muni = length(unique(codmun7))) %>% right_join(
    data.frame(sf_ninestate_muni), by = c("uf_sigla" = "SIGLA_UF")
  ) %>% select(uf_sigla, uf, CD_MUN, NM_MUN, AREA_KM2) %>%
  crossing(year = 2002:2019) %>% left_join(
bla_rural_credit %>% 
  mutate(value_reais = as.numeric(gsub(",",".",VlCusteio))) %>% 
  group_by(SIGLA_UF, NM_MUN, CD_MUN, AnoEmissao) %>% 
  summarise(credito_rural_tot_reais = sum(value_reais, na.rm = TRUE)), 
by = c("uf_sigla"="SIGLA_UF", "CD_MUN" = "CD_MUN", "NM_MUN" = "NM_MUN", 
       "year" = "AnoEmissao")
) %>% 
  arrange(uf_sigla, NM_MUN) %>% 
  write.csv("muni_fixed_ruralcredit_long.csv", row.names = FALSE)
