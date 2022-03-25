#Indigenous
# coverage by presence of aldeia

#Packages
library(plyr)
library(tidyverse)
library(sf)
library(units)
library(readxl)

#Basic reference vectors
bla_state_names <- c("Acre", "Amapá", "Amazonas", "Maranhão", 
                     "Mato Grosso", "Pará", "Tocantins", "Rondônia", "Roraima")
bla_state_siglas <- c("AC", "AP", "AM", "MA", 
                      "MT", "PA", "TO", "RO", "RR")
dfstates <- data.frame(bla_state_names, bla_state_siglas)

#Load 
ind_area <- "C:\\Users\\user\\Documents\\Articles\\2022_Norris_gdp_deforestation\\AmazonConservation\\vector\\ti_sirgas\\ti_sirgas.shp"
sf_bla_ind_area <-  st_make_valid(st_read(ind_area, 
                                      options = "ENCODING=WINDOWS-1252"))
Selvalid <- which(st_is_valid(sf_bla_ind_area)) #626 all valid

#Load IBGE polygons
#Municipal polygons
ibge_muni <- "C:\\Users\\user\\Documents\\Articles\\2022_Norris_gdp_deforestation\\analysis\\br_municipios_20200807\\BR_Municipios_2019.shp"
sf_ninestate_muni <- st_read(ibge_muni) %>% 
  filter(SIGLA_UF %in% all_of(bla_state_siglas)) %>% 
  st_transform(crs = st_crs(sf_bla_ind_area))
#States
ibge_states <- "vector\\ninestate_poly.shp"
sf_ninestate <- st_read(ibge_states) %>% 
  filter(SIGLA_UF %in% all_of(bla_state_siglas)) %>% 
  st_transform(crs = st_crs(sf_bla_ind_area))

#function to summarise gold mines per municiapity
get_indigenous <- function(x) {
  state_sigla <- x$bla_state_siglas
  # Two step intersect as laptop has not so much memory!
  #1 intersect with state polygon
  sf_ind <- st_intersection(sf_bla_ind_area, 
                                sf_ninestate %>% 
                                  filter(SIGLA_UF==state_sigla))
  #2 intersect with municipality polygons
  sf_ind_muni <- st_intersection(sf_ind, 
                                     sf_ninestate_muni %>% 
                                       filter(SIGLA_UF==state_sigla))
  
  # Projection for area calculations 
  # South_America_Albers_Equal_Area_Conic
  # Consistent with values provided by IBGE 2020.
  st_transform(sf_ind_muni, 
               "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs") %>%
    st_area() -> sf_ind_muni$indigenous_area_m2 
  sf_ind_muni %>%
    mutate(indigenous_area_km2 = set_units(indigenous_area_m2 , km^2)) -> sf_ind_muni

  # For each indigenous polygon identify municipality and
  # the area covered in municipality  
  rowcount <- nrow(data.frame(sf_ind_muni))
  
  if(rowcount>0){
    data.frame(sf_ind_muni) %>% 
      select(NM_UF, NM_MUN, AREA_KM2, 
             terrai_cod, terrai_nom, etnia_nome, indigenous_area_km2) %>% 
      group_by(NM_UF, NM_MUN, AREA_KM2) %>% 
      summarise(count_territory = length(unique(terrai_cod)), 
                count_ethnic = length(unique(etnia_nome)),
                indigenous_area_km2 = sum(as.numeric(indigenous_area_km2))) %>% 
      ungroup() %>% data.frame() -> df_ind_muni
  }else{
    df_ind_muni <- data.frame(NM_UF = x$bla_state_names, 
                                   NM_MUN = NA, 
                                   AREA_KM2 = NA, 
                              count_territory = 0, 
                              count_ethnic = 0, 
                              indigenous_area_km2 = 0)
  }
  df_ind_muni 
}  
#run
df_indigenous <- plyr::ddply(dfstates, .(bla_state_siglas), 
                            .fun = get_indigenous)

#Add area % of municipality and export
df_muni_year <- read_excel("data//bla_municipalities.xlsx", 
                           na = c("", "NA"),
                           sheet = "municipality_annual",
                           .name_repair = "universal")

df_muni_year %>% left_join(
  df_indigenous %>% 
    select(NM_UF, NM_MUN, count_territory, count_ethnic, 
           indigenous_area_km2), 
  by = c("state_name" = "NM_UF", muni_name = "NM_MUN")
) %>% 
  mutate(count_territory = replace_na(count_territory, 0), 
         count_ethnic = replace_na(count_ethnic, 0), 
         indigenous_area_km2 = replace_na(indigenous_area_km2, 0)) %>%
  mutate(indigenous_area_percent = (indigenous_area_km2/muni_area_km2)*100) %>% 
  select(state_ref, muni_name,year,count_territory, count_ethnic,
         indigenous_area_km2, indigenous_area_percent) %>%
  arrange(state_ref, muni_name, year) %>% 
  write.csv("muni_fixed_indigenous_long.csv", row.names = FALSE)

testout <- read.csv("muni_fixed_indigenous_long.csv")
testout %>% filter(year == 2019) %>% 
  arrange(state_ref, muni_name) %>% 
  write.csv("muni_fixed_indigenous.csv", row.names = FALSE)
