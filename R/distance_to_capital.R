#Distance to capital
library(tidyverse)
library(sf)

#Basic reference vectors
#Not obvious how to select state capitals
bla_state_names <- c("Acre", "Amapá", "Amazonas", "Maranhão", 
                     "Mato Grosso", "Pará", "Tocantins", "Rondônia", "Roraima")
bla_state_siglas <- c("AC", "AP", "AM", "MA", 
                      "MT", "PA", "TO", "RO", "RR")
bla_bla_state_capitals <- data.frame(name_muni = c("Manaus", "Macapá", "Porto Velho", "Rio Branco", 
                                               "Boa Vista",
                                               "São Luís", "Cuiabá", "Belém", "Palmas"), 
                                 codmun7 = c(1302603, 1600303, 1100205, 1200401, 
                                             1400100,
                                             2111300, 5103403, 1501402, 1721000)
)

# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html?=&t=downloads
ibge_muni <- "C:\\Users\\user\\Documents\\Articles\\2022_Norris_gdp_deforestation\\analysis\\br_municipios_20200807\\BR_Municipios_2019.shp"
sf_ninestate_muni <- st_read(ibge_muni) %>% filter(SIGLA_UF %in% bla_state_siglas)
#point locations for sector de census
#ibge_muni_location <- "C:\\Users\\user\\Documents\\Articles\\2022_Norris_gdp_deforestation\\analysis\\br_locations\\BR_Localidades_2010_v1.shp"
#sf_ninestate_muni_location <- st_read(ibge_muni_location) %>% 
#  filter()
#points of sedes
ibge_muni_location <- "C:\\Users\\user\\Documents\\Articles\\2022_Norris_gdp_deforestation\\analysis\\shapefile_sede_ibge_2015\\5570_municipios2015.shp"
st_read(ibge_muni_location) %>% 
  mutate(SIGLA_UF = toupper(UF)) %>% 
  filter(SIGLA_UF %in% bla_state_siglas) -> sf_ninestate_muni_location 

sf_ninestate_muni_location %>% 
  mutate(CD_MUN = as.character(GEOCODIGO_)) %>% select(CD_MUN) %>%
  left_join(data.frame(sf_ninestate_muni) %>% select(!geometry)) -> sf_ninestate_muni_location

file.choose()
bind_rows(
  st_distance(sf_ninestate_muni_location %>% 
                filter(SIGLA_UF == "AC"), 
              sf_ninestate_muni_location %>% 
                filter(SIGLA_UF == "AC", 
                       NM_MUN %in% bla_state_capitals$name_muni)) %>% tibble() %>% 
    mutate(dist_statecapital_km = as.numeric(`.` / 1000)) %>% 
    bind_cols(data.frame(sf_ninestate_muni_location) %>% 
                filter(SIGLA_UF == "AC") %>% select(SIGLA_UF, NM_MUN)), 
  st_distance(sf_ninestate_muni_location %>% 
                filter(SIGLA_UF == "AP"), 
              sf_ninestate_muni_location %>% 
                filter(SIGLA_UF == "AP", 
                       NM_MUN %in% bla_state_capitals$name_muni)) %>% tibble() %>% 
    mutate(dist_statecapital_km = as.numeric(`.` / 1000)) %>% 
    bind_cols(data.frame(sf_ninestate_muni_location) %>% 
                filter(SIGLA_UF == "AP") %>% select(SIGLA_UF, NM_MUN)), 
  st_distance(sf_ninestate_muni_location %>% 
                filter(SIGLA_UF == "AM"), 
              sf_ninestate_muni_location %>% 
                filter(SIGLA_UF == "AM", 
                       NM_MUN %in% bla_state_capitals$name_muni)) %>% tibble() %>% 
    mutate(dist_statecapital_km = as.numeric(`.` / 1000)) %>% 
    bind_cols(data.frame(sf_ninestate_muni_location) %>% 
                filter(SIGLA_UF == "AM") %>% select(SIGLA_UF, NM_MUN)), 
  st_distance(sf_ninestate_muni_location %>% 
                filter(SIGLA_UF == "MA"), 
              sf_ninestate_muni_location %>% 
                filter(SIGLA_UF == "MA", 
                       NM_MUN %in% bla_state_capitals$name_muni)) %>% tibble() %>% 
    mutate(dist_statecapital_km = as.numeric(`.` / 1000)) %>% 
    bind_cols(data.frame(sf_ninestate_muni_location) %>% 
                filter(SIGLA_UF == "MA") %>% select(SIGLA_UF, NM_MUN)), 
  st_distance(sf_ninestate_muni_location %>% 
                filter(SIGLA_UF == "MT"), 
              sf_ninestate_muni_location %>% 
                filter(SIGLA_UF == "MT", 
                       CD_MUN %in% as.character(bla_state_capitals$codmun7))) %>% tibble() %>% 
    mutate(dist_statecapital_km = as.numeric(`.` / 1000)) %>% 
    bind_cols(data.frame(sf_ninestate_muni_location) %>% 
                filter(SIGLA_UF == "MT") %>% select(SIGLA_UF, NM_MUN)), 
  st_distance(sf_ninestate_muni_location %>% 
                filter(SIGLA_UF == "PA"), 
              sf_ninestate_muni_location %>% 
                filter(SIGLA_UF == "PA", 
                       NM_MUN %in% bla_state_capitals$name_muni)) %>% tibble() %>% 
    mutate(dist_statecapital_km = as.numeric(`.` / 1000)) %>% 
    bind_cols(data.frame(sf_ninestate_muni_location) %>% 
                filter(SIGLA_UF == "PA") %>% select(SIGLA_UF, NM_MUN)), 
  st_distance(sf_ninestate_muni_location %>% 
                filter(SIGLA_UF == "RO"), 
              sf_ninestate_muni_location %>% 
                filter(SIGLA_UF == "RO", 
                       NM_MUN %in% bla_state_capitals$name_muni)) %>% tibble() %>% 
    mutate(dist_statecapital_km = as.numeric(`.` / 1000)) %>% 
    bind_cols(data.frame(sf_ninestate_muni_location) %>% 
                filter(SIGLA_UF == "RO") %>% select(SIGLA_UF, NM_MUN)), 
  st_distance(sf_ninestate_muni_location %>% 
                filter(SIGLA_UF == "RR"), 
              sf_ninestate_muni_location %>% 
                filter(SIGLA_UF == "RR", 
                       NM_MUN %in% bla_state_capitals$name_muni)) %>% tibble() %>% 
    mutate(dist_statecapital_km = as.numeric(`.` / 1000)) %>% 
    bind_cols(data.frame(sf_ninestate_muni_location) %>% 
                filter(SIGLA_UF == "RR") %>% select(SIGLA_UF, NM_MUN)), 
  st_distance(sf_ninestate_muni_location %>% 
                filter(SIGLA_UF == "TO"), 
              sf_ninestate_muni_location %>% 
                filter(SIGLA_UF == "TO", 
                       NM_MUN %in% bla_state_capitals$name_muni)) %>% tibble() %>% 
    mutate(dist_statecapital_km = as.numeric(`.` / 1000)) %>% 
    bind_cols(data.frame(sf_ninestate_muni_location) %>% 
                filter(SIGLA_UF == "TO") %>% select(SIGLA_UF, NM_MUN))
) %>% select(SIGLA_UF, NM_MUN, dist_statecapital_km) -> df_capital_dist

df_capital_dist %>% arrange(SIGLA_UF, NM_MUN) %>% 
  write.csv("muni_fixed_dist.csv", row.names = FALSE)
  
sf_ninestate_muni_location$long <- st_coordinates(sf_ninestate_muni_location)[,1] 
sf_ninestate_muni_location$lat <- st_coordinates(sf_ninestate_muni_location)[,2] 

data.frame(sf_ninestate_muni_location) %>% 
  select(!geometry) %>% arrange(SIGLA_UF, NM_MUN) %>% 
  write.csv("muni_fixed_coords.csv", row.names = FALSE)
