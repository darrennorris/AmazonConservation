#Packages
library(plyr)
library(tidyverse)
library(sf)
library(units)
library(readxl)

# Data needed for mining: 
# processses per capita and area covered per year per municipality/per capita

#Basic reference vectors
bla_state_names <- c("Acre", "Amapá", "Amazonas", "Maranhão", 
                     "Mato Grosso", "Pará", "Tocantins", "Rondônia", "Roraima")
bla_state_siglas <- c("AC", "AP", "AM", "MA", 
                      "MT", "PA", "TO", "RO", "RR")
dfstates <- data.frame(bla_state_names, bla_state_siglas)

#Load files needed
#s05 <- "C:\\Users\\user\\Documents\\ZEE_socioeco\\analises\\BRASIL_ativo\\BRASIL.shp"

#Only those that are actively mining
# contribuição financeira pelo aproveitamento econômico de bens minerais -CFEM
fase_cfem <- c("CONCESSÃO DE LAVRA", 
               "LAVRA GARIMPEIRA", 
               "REQUERIMENTO DE REGISTRO DE EXTRAÇÃO",
               "REGISTRO DE EXTRAÇÃO")
fase_cfem_lower <- tolower(fase_cfem)

#Active processes in Brazilian legal Amazon 
#216699 in Brazil, 3460 functioning in the Brazilian Legal Amazon
#sf_br_min <- st_make_valid(st_read(s05), 
#                           options = "ENCODING=WINDOWS-1252") %>% 
#  filter(UF %in% all_of(bla_state_siglas), FASE %in% all_of(fase_cfem))

#Manually correct one invalid geometry
#Selvalid <- which(st_is_valid(sf_br_min)) #3459
#sf_br_min[-Selvalid, ]
#plot(sf_br_min[-Selvalid, 'ANO'])
#st_write(sf_br_min, "sf_bla_min.shp")
#Load 
sf_bla_mine <-  st_make_valid(st_read("sf_bla_min.shp", 
                                      options = "ENCODING=WINDOWS-1252"))
Selvalid <- which(st_is_valid(sf_bla_mine)) #3460
#rm("sf_br_min")
#Group important substrates
mine_gold <- c("MINÉRIO DE OURO", "OURO", "OURO NATIVO")
mine_metal <- c("COBRE", "MINÉRIO DE COBRE", 
                "CROMO", "MINÉRIO DE PLATINA",
                "MINÉRIO DE ESTANHO", "ESTANHO", "CASSITERITA",
                "MINÉRIO DE FERRO", "FERRO", 
                "MINÉRIO DE MANGANÊS", "MANGANÊS", 
                "MINÉRIO DE NIÓBIO", "NIÓBIO", 
                "MINÉRIO DE NÍQUEL", "NÍQUEL", 
                "MINÉRIO DE TÂNTALO", "TÂNTALO", 
                "ALUMÍNIO", "BAUXITA"
)
mine_construction <- c("ARGILA", "ARGILA P/CER. VERMELH", 
                       "ARGILA REFRATÁRIA",
                       "CASCALHO", "SEIXOS", 
                       "GRANITO", "GRANITO P/ BRITA", 
                       "AREIA", "SAIBRO")
mine_calcium <- c("CALCÁRIO", "CALCÁRIO CALCÍTICO", "CALCÁRIO DOLOMÍTICO")
#add groups
sf_bla_mine %>% 
  mutate(mine_group = case_when(SUBS %in% mine_gold~"gold", 
                   SUBS %in% mine_metal~"metal", 
                   SUBS %in% mine_construction ~"construction", 
                   SUBS %in% mine_calcium ~"calcium", 
                   TRUE ~ NA_character_)) -> sf_bla_mine
table(sf_bla_mine$mine_group)
data.frame(sf_bla_mine) %>% select(!geometry) %>% 
  ggplot(aes(x=ANO)) + 
  geom_bar(aes(fill = FASE)) + 
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.position = "top")

data.frame(sf_bla_mine) %>% select(!geometry) %>% 
  group_by(SUBS) %>% 
  summarise(acount = length(unique(PROCESSO))) %>% 
  pull(SUBS)
#all processes have unique FASE
data.frame(sf_bla_mine) %>% select(!geometry) %>% 
  group_by(PROCESSO) %>% summarise(count_fase = length(unique(FASE))) %>% 
  arrange(desc(count_fase))
#None in Acre
data.frame(sf_bla_mine) %>% 
  filter(SUBS %in% c("MINÉRIO DE OURO", "OURO", "OURO NATIVO")) %>%
  group_by(UF) %>% 
  summarise(count_state = length(unique(NM_UF)))

data.frame(sf_bla_mine) %>%
  filter(!is.na(mine_group)) %>%
  mutate(year_final = 2019) %>% 
  group_by(mine_group, PROCESSO, AREA_HA) %>% 
  mutate(year = list(seq(from = ANO, to = year_final))) %>% 
  unnest(year) %>% 
  filter(year >= 2002) %>%
  select(mine_group, PROCESSO, AREA_HA, ANO, year) %>% 
  group_by(mine_group, year) %>% 
  summarise(count_process = length(unique(PROCESSO)), 
            area_km2 = sum(as.numeric(AREA_HA)))

#Load IBGE polygons
#Municipal polygons
ibge_muni <- "C:\\Users\\user\\Documents\\Articles\\2022_Norris_gdp_deforestation\\analysis\\br_municipios_20200807\\BR_Municipios_2019.shp"
sf_ninestate_muni <- st_read(ibge_muni) %>% 
  filter(SIGLA_UF %in% all_of(bla_state_siglas)) %>% st_transform(crs = st_crs(sf_bla_mine))
#States
ibge_states <- "vector\\ninestate_poly.shp"
sf_ninestate <- st_read(ibge_states) %>% 
  filter(SIGLA_UF %in% all_of(bla_state_siglas)) %>% st_transform(crs = st_crs(sf_bla_mine))

#function to summarise gold mines per municiapity
get_goldmines <- function(x) {
state_sigla <- x$bla_state_siglas
# Two step intersect as laptop has not so much memory!
#1 intersect with state polygon
sf_mine_ap <- st_intersection(sf_bla_mine, 
                                   sf_ninestate %>% 
                                     filter(SIGLA_UF==state_sigla))
#2 intersect with municipality polygons
sf_mine_muni_ap <- st_intersection(sf_mine_ap, 
                                        sf_ninestate_muni %>% 
                                          filter(SIGLA_UF==state_sigla))

# Projection for area calculations 
# South_America_Albers_Equal_Area_Conic
# Consistent with values provided by IBGE 2020.
st_transform(sf_mine_muni_ap, 
             "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs") %>%
  st_area() -> sf_mine_muni_ap$mine_area_m2 
sf_mine_muni_ap %>%
  mutate(mine_area_km2 = set_units(mine_area_m2, km^2)) -> sf_mine_muni_ap

#Group important substrates
mine_gold <- c("MINÉRIO DE OURO", "OURO", "OURO NATIVO")
mine_metal <- c("COBRE", "MINÉRIO DE COBRE", 
                "CROMO", "MINÉRIO DE PLATINA",
                "MINÉRIO DE ESTANHO", "ESTANHO", "CASSITERITA",
                "MINÉRIO DE FERRO", "FERRO", 
                "MINÉRIO DE MANGANÊS", "MANGANÊS", 
                "MINÉRIO DE NIÓBIO", "NIÓBIO", 
                "MINÉRIO DE NÍQUEL", "NÍQUEL", 
                "MINÉRIO DE TÂNTALO", "TÂNTALO", 
                "ALUMÍNIO", "BAUXITA"
                )
mine_construction <- c("ARGILA", "ARGILA P/CER. VERMELH", 
                       "ARGILA REFRATÁRIA",
                       "CASCALHO", "SEIXOS", 
                       "GRANITO", "GRANITO P/ BRITA", 
                       "AREIA", "SAIBRO")
mine_calcium <- c("CALCÁRIO", "CALCÁRIO CALCÍTICO", "CALCÁRIO DOLOMÍTICO")

# For each mining polygon identify municipality and
# the area covered in municipality  
data.frame(sf_mine_muni_ap) %>% 
  select(!geometry) %>% 
  filter(SUBS %in% all_of(mine_gold), ANO < 2020) -> dftmp

#export summaries
if(nrow(dftmp)>0){
  dftmp %>% 
  mutate(year_final = 2019) %>% 
  group_by(NM_UF, NM_MUN, AREA_KM2, PROCESSO) %>% 
  mutate(year = list(seq(from = ANO, to = year_final))) %>% 
  unnest(year) %>% 
  filter(year > 2002) %>%
  select(NM_UF, NM_MUN, AREA_KM2, PROCESSO, ANO, year, mine_area_km2) %>% 
  group_by(NM_UF, NM_MUN, AREA_KM2, year) %>% 
  summarise(count_gold_process = length(unique(PROCESSO)), 
            gold_area_km2 = sum(as.numeric(mine_area_km2))) %>% 
  ungroup() %>% data.frame() -> df_goldmine_muni
}else{
  df_goldmine_muni <- data.frame(NM_UF = x$bla_state_names, 
                                 NM_MUN = NA, 
                                 AREA_KM2 = NA, 
                                 year = NA, 
                                 count_gold_process = 0, 
                                 gold_area_km2 = 0)
}
df_goldmine_muni
}

#df_goldmines <- plyr::ddply(dfstates, .(bla_state_siglas), 
#                            .fun = get_goldmines)

get_mines <- function(x) {
  state_sigla <- x$bla_state_siglas
  # Two step intersect as laptop has not so much memory!
  #1 intersect with state polygon
  sf_mine <- st_intersection(sf_bla_mine, 
                                sf_ninestate %>% 
                                  filter(SIGLA_UF==state_sigla))
  #2 intersect with municipality polygons
  sf_mine_muni <- st_intersection(sf_mine, 
                                     sf_ninestate_muni %>% 
                                       filter(SIGLA_UF==state_sigla))
  
  # Projection for area calculations 
  # South_America_Albers_Equal_Area_Conic
  # Consistent with values provided by IBGE 2020.
  st_transform(sf_mine_muni, 
               "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs") %>%
    st_area() -> sf_mine_muni$mine_area_m2 
  sf_mine_muni %>%
    mutate(mine_area_km2 = set_units(mine_area_m2, km^2)) -> sf_mine_muni
  
  # For each mining polygon identify municipality and
  # the area covered in municipality  
  data.frame(sf_mine_muni) %>% 
    select(!geometry) %>% 
    filter(SUBS %in% c(mine_gold, mine_metal, 
                       mine_construction, mine_calcium), 
           ANO < 2020) -> dftmp
  
  #export summaries
  if(nrow(dftmp)>0){
    dftmp %>% 
      filter(!is.na(mine_group)) %>%
      mutate(year_final = 2019) %>% 
      group_by(NM_UF, NM_MUN, AREA_KM2, 
               mine_group, PROCESSO, AREA_HA) %>% 
      mutate(year = list(seq(from = ANO, to = year_final))) %>% 
      unnest(year) %>% 
      filter(year >= 2002) %>%
      select(NM_UF, NM_MUN, AREA_KM2, mine_group,  
             PROCESSO, AREA_HA, ANO, year, mine_area_km2) %>% 
      group_by(NM_UF, NM_MUN, AREA_KM2, mine_group,  year) %>% 
      summarise(count_process = length(unique(PROCESSO)), 
                mine_area_km2 = sum(as.numeric(mine_area_km2))) %>% 
      ungroup() %>% data.frame() -> df_mine_muni
  }else{
    df_mine_muni <- data.frame(NM_UF = x$bla_state_names, 
                                   NM_MUN = NA, 
                                   AREA_KM2 = NA, 
                               mine_group = NA,
                                   year = NA, 
                                   count_process = 0, 
                                   mine_area_km2 = 0)
  }
  df_mine_muni
}

df_mines <- plyr::ddply(dfstates, .(bla_state_siglas), 
                        .fun = get_mines)
table(df_mines$year)
df_mines %>% 
  pivot_wider(id_cols = c(NM_UF, NM_MUN, year), 
              names_from = mine_group, 
              values_from = c(count_process, mine_area_km2), 
              values_fill = 0) -> df_mines_wide

#Add population and export
df_muni_year <- read_excel("data//bla_municipalities.xlsx", 
                           na = c("", "NA"),
                           sheet = "municipality_annual",
                           .name_repair = "universal")
names(df_mines_wide)
df_muni_year %>% left_join(
  df_mines_wide,
  by = c("state_name" = "NM_UF", muni_name = "NM_MUN", "year" = "year")) %>%
  mutate(count_process_construction = replace_na(count_process_construction, 0), 
         count_process_gold = replace_na(count_process_gold, 0), 
         count_process_metal = replace_na(count_process_metal, 0), 
         count_process_calcium = replace_na(count_process_calcium, 0), 
         mine_area_km2_construction = replace_na(mine_area_km2_construction, 0), 
         mine_area_km2_gold = replace_na(mine_area_km2_gold, 0), 
         mine_area_km2_metal = replace_na(mine_area_km2_metal, 0), 
         mine_area_km2_calcium = replace_na(mine_area_km2_calcium, 0)
         ) %>%
  mutate(mine_area_km2_construction_percapita = mine_area_km2_construction / tot_pop, 
         mine_area_km2_gold_percapita = mine_area_km2_gold / tot_pop, 
         mine_area_km2_metal_percapita = mine_area_km2_metal / tot_pop,
         mine_area_km2_calcium_percapita = mine_area_km2_calcium / tot_pop, 
         process_construction_p1000 = (count_process_construction/tot_pop) * 1000, 
         process_gold_p1000 = (count_process_gold/tot_pop) * 1000,
         process_metal_p1000 = (count_process_metal/tot_pop) * 1000,
         process_calcium_p1000 = (count_process_calcium/tot_pop) * 1000
  ) %>%
  arrange(state_ref, muni_name, year) %>% 
  write.csv("muni_fixed_mine_long.csv", row.names = FALSE)


#Mining
mutate(SUBS_simples = 
         case_when(SUBS %in% c("MINÉRIO DE OURO", "OURO", "OURO NATIVO") ~"OURO", 
                   SUBS %in% c("COBRE", "MINÉRIO DE COBRE") ~"COBRE", 
                   SUBS %in% c("MINÉRIO DE ESTANHO", "ESTANHO") ~"ESTANHO", 
                   SUBS %in% c("MINÉRIO DE FERRO", "FERRO") ~"FERRO", 
                   SUBS %in% c("MINÉRIO DE MANGANÊS", "MANGANÊS") ~"MANGANÊS", 
                   SUBS %in% c("MINÉRIO DE NIÓBIO", "NIÓBIO") ~"NIÓBIO", 
                   SUBS %in% c("MINÉRIO DE NÍQUEL", "NÍQUEL") ~"NÍQUEL", 
                   SUBS %in% c("MINÉRIO DE TÂNTALO", "TÂNTALO") ~"TÂNTALO", 
                   SUBS %in% c("ÁGUA MINERAL", "ÁGUA POTÁVEL DE MESA") ~"ÁGUA", 
                   SUBS %in% c("ARGILA", "ARGILA P/CER. VERMELH") ~"ARGILA", 
                   SUBS %in% c("ALUMÍNIO", "BAUXITA") ~"ALUMÍNIO/BAUXITA", 
                   SUBS %in% c("CASCALHO", "SEIXOS") ~"CASCALHO/SEIXOS", 
                   SUBS %in% c("GRANITO", "GRANITO P/ BRITA") ~"GRANITO",
                   SUBS %in% c("AREIA", "SAIBRO") ~"AREIA/SAIBRO",
                   TRUE~ as.character(SUBS))) 

dfmin %>% mutate(SUBS_main = 
                   case_when(!(SUBS_simples %in% c( "OURO", "CAULIM", "FERRO", "CROMO", "GRANITO")) ~"outros", 
                             TRUE~ as.character(SUBS_simples))) -> dfmin