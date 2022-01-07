# tidy data downloaded by "..._download.R" for further processsing
library(tidyverse)
library(readxl)
library(sf)
library(nngeo)
library(scales)

# Pop
# Join all pop years
bind_rows(df_pop_2000a2004, df_pop_2005a2009, 
          df_pop_2010a2014, df_pop_2015a2019, df_pop_2020a2024) %>% 
  rename(  mun_cod = `Município (Código)`, 
           pop_total = Valor, 
           cod_pop = `Variável (Código)`, 
           name_pop = `Variável`) -> df_pop_2000a2021 

#to get bla municipalities
df_gdp_muni_02a19 <- read_excel("data//ibge_gdp//bla_gdp_municipality_2002_2019.xlsx", 
                                .name_repair = "universal") %>% 
  rename(mun_cod = `Código.do.Município`)

bla_muni <- unique(df_gdp_muni_02a19$mun_cod)
df_pop_2000a2021 %>% 
  filter(mun_cod %in% bla_muni) -> df_bla_pop_02a19
write.csv(df_bla_pop_02a19, "bla_municipality_pop_02a19.csv", row.names = FALSE)

# Join all pib years
bind_rows(df_pib_2000a2004, df_pib_2005a2009, 
          df_pib_2010a2014, df_pib_2015a2019) %>% 
  rename(cod_pib = `Variável (Código)`) %>% 
  left_join(df_pib_vars, by = c("cod_pib" = "cod")) %>% 
  rename(mun_cod = `Município (Código)`, 
         name_pib = `desc`, 
         pib_total = Valor) %>% 
  mutate(Ano = `Ano (Código)`) %>%  select(-`Ano (Código)`) -> df_pib_2002a2018
unique(df_pib_2002a2018$Ano)

# Join all variables
df_pop_2000a2021 %>% 
  left_join(df_pib_2002a2018, by = c("mun_cod" = "mun_cod", "Ano" = "Ano")) %>% 
  mutate(ano = as.numeric(Ano)) %>%
  select(mun_cod, ano, pop_total, pib_total) -> dfmuni

#include 10 year census data
#  pop_tot = mulhertot + homemtot
censu <- read_excel("pnud_municipios.xlsx", sheet = "pnud_municipios", 
              .name_repair = "universal", na = c("", "NA")) %>% 
  filter(ano %in% c(1991, 2000, 2010))

dfmuni %>% 
  rename(codmun7 = mun_cod) %>% 
  mutate(type = 'annual', 
         pesorur = NA, 
         pesourb = NA, 
         urban_percent = NA) %>%
  bind_rows(
censu %>% 
  mutate(codmun7 = as.character(codmun7), 
         pop_total = pesotot,
         pib_total = NA, 
         type = "decadal", 
         urban_percent = pesourb/pesotot)) %>% 
  select(codmun7,ano, type, pop_total, 
         pesorur, pesourb, urban_percent, pib_total)  -> dfall

#presidents https://en.wikipedia.org/wiki/List_of_presidents_of_Brazil
pres_names <- c("Collor do Mello", "Franco", "Cardoso", "Lula", 
                "Rousseff", "Temer", "Bolsonaro")
pres_start <- c("15/03/1990", "02/10/1992", "01/01/1995", "01/03/2003", 
                "01/01/2011", "12/05/2016", "01/01/2019")
pres_end <- c("01/10/1992", "31/12/1994", 
              "31/12/2002", "31/12/2010", "11/05/2016", 
              "31/12/2018", "31/12/2021")
pres_party <- c("PRN", "PMDB", "PSDB", "PT", "PT", "MDB", "PSL/PL")
pres_party_direction <- c(NA, "center", "right-wing", "left_wing", 
                          "left-wing", "center", "right-wing")
df_presidents <- data.frame(pres_names = pres_names,
                            pres_party = pres_party, 
                            pres_party_direction = pres_party_direction,
                            pres_start = pres_start, 
                            pres_end = pres_end)

#Poligonos municipios Amapa
s01 <- "C:\\Users\\user\\Documents\\Articles\\2022_Norris_gdp_deforestation\\analysis\\lista_de_municipios_da_Amazonia_Legal_2020_SHP\\Amazonia_Legal_2020.shp"
s02 <- "C:\\Users\\user\\Documents\\Articles\\2022_Norris_gdp_deforestation\\analysis\\lista_de_municipios_da_Amazonia_Legal_2020_SHP\\Mun_Amazonia_Legal_2020.shp"
s03 <- "C:\\Users\\user\\Documents\\Articles\\2022_Norris_gdp_deforestation\\analysis\\lista_de_municipios_da_Amazonia_Legal_2020_SHP\\Sede_Mun_Amazonia_Legal_2020.shp"
#load files
sf::st_read(s01) %>% 
  st_make_valid() -> sf_lba_poly
sf::st_read(s02) %>% 
  st_make_valid() -> sf_lba
sf::st_read(s03) %>% 
  st_make_valid() -> sf_lba_sedes
#Lines Legal Brazilian Amazon
sf_lba_line <- st_cast(sf_lba, "MULTILINESTRING")

data.frame(sf_lba) %>% left_join(dfall, by = c("CD_MUN" = "codmun7")) %>% 
  mutate(pop_dens_km = pop_total / AREA_TOT) %>% 
  select(CD_UF, NM_UF, CD_MUN, NM_MUN, AREA_TOT, ano, 
         type, pop_total, pop_dens_km, urban_percent, pib_total) %>% 
  arrange(ano, NM_UF, NM_MUN) -> df_lba 

#State capitals
df_lba %>% 
  filter(ano == 2010) %>% 
  group_by(NM_UF) %>% 
  summarise(pop_max = max(pop_total)) %>% 
  left_join(df_lba %>% filter(ano == 2010), 
            by = c( "pop_max" = "pop_total")) -> state_capitals

#Distance to state capital
names_municipios <- unique(sf_lba_sedes$NM_MUN)
names_states <- sort(unique(sf_lba_sedes$NM_UF))

#[1] "Acre"        "Amapá"       "Amazonas"    "Maranhão"    "Mato Grosso" "Pará"       
#[7] "Rondônia"    "Roraima"     "Tocantins"
# would be better to redo using municipality codes as names are not unique
bind_rows(
st_distance(sf_lba_sedes %>% 
                  filter(NM_UF == "Acre"), 
                sf_lba_sedes %>% 
                  filter(NM_UF == "Acre", 
                         NM_MUN %in% state_capitals$NM_MUN)) %>% tibble() %>% 
  mutate(dist_statecapital_km = as.numeric(`.` / 1000)) %>% 
  bind_cols(data.frame(sf_lba_sedes) %>% 
              filter(NM_UF == "Acre") %>% select(NM_UF, NM_MUN)), 
st_distance(sf_lba_sedes %>% 
              filter(NM_UF == "Amapá"), 
            sf_lba_sedes %>% 
              filter(NM_UF == "Amapá", 
                     NM_MUN %in% state_capitals$NM_MUN)) %>% tibble() %>% 
  mutate(dist_statecapital_km = as.numeric(`.` / 1000)) %>% 
  bind_cols(data.frame(sf_lba_sedes) %>% 
              filter(NM_UF == "Amapá") %>% select(NM_UF, NM_MUN)), 
st_distance(sf_lba_sedes %>% 
              filter(NM_UF == "Amazonas"), 
            sf_lba_sedes %>% 
              filter(NM_UF == "Amazonas", 
                     NM_MUN %in% state_capitals$NM_MUN)) %>% tibble() %>% 
  mutate(dist_statecapital_km = as.numeric(`.` / 1000)) %>% 
  bind_cols(data.frame(sf_lba_sedes) %>% 
              filter(NM_UF == "Amazonas") %>% select(NM_UF, NM_MUN)), 
st_distance(sf_lba_sedes %>% 
              filter(NM_UF == "Maranhão"), 
            sf_lba_sedes %>% 
              filter(NM_UF == "Maranhão", 
                     NM_MUN %in% state_capitals$NM_MUN)) %>% tibble() %>% 
  mutate(dist_statecapital_km = as.numeric(`.` / 1000)) %>% 
  bind_cols(data.frame(sf_lba_sedes) %>% 
              filter(NM_UF == "Maranhão") %>% select(NM_UF, NM_MUN)), 
st_distance(sf_lba_sedes %>% 
              filter(NM_UF == "Mato Grosso"), 
            sf_lba_sedes %>% 
              filter(NM_UF == "Mato Grosso", 
                     CD_MUN %in% state_capitals$CD_MUN)) %>% tibble() %>% 
  mutate(dist_statecapital_km = as.numeric(`.` / 1000)) %>% 
  bind_cols(data.frame(sf_lba_sedes) %>% 
              filter(NM_UF == "Mato Grosso") %>% select(NM_UF, NM_MUN)), 
st_distance(sf_lba_sedes %>% 
              filter(NM_UF == "Pará"), 
            sf_lba_sedes %>% 
              filter(NM_UF == "Pará", 
                     NM_MUN %in% state_capitals$NM_MUN)) %>% tibble() %>% 
  mutate(dist_statecapital_km = as.numeric(`.` / 1000)) %>% 
  bind_cols(data.frame(sf_lba_sedes) %>% 
              filter(NM_UF == "Pará") %>% select(NM_UF, NM_MUN)), 
st_distance(sf_lba_sedes %>% 
              filter(NM_UF == "Rondônia"), 
            sf_lba_sedes %>% 
              filter(NM_UF == "Rondônia", 
                     NM_MUN %in% state_capitals$NM_MUN)) %>% tibble() %>% 
  mutate(dist_statecapital_km = as.numeric(`.` / 1000)) %>% 
  bind_cols(data.frame(sf_lba_sedes) %>% 
              filter(NM_UF == "Rondônia") %>% select(NM_UF, NM_MUN)), 
st_distance(sf_lba_sedes %>% 
              filter(NM_UF == "Roraima"), 
            sf_lba_sedes %>% 
              filter(NM_UF == "Roraima", 
                     NM_MUN %in% state_capitals$NM_MUN)) %>% tibble() %>% 
  mutate(dist_statecapital_km = as.numeric(`.` / 1000)) %>% 
  bind_cols(data.frame(sf_lba_sedes) %>% 
              filter(NM_UF == "Roraima") %>% select(NM_UF, NM_MUN)), 
st_distance(sf_lba_sedes %>% 
              filter(NM_UF == "Tocantins"), 
            sf_lba_sedes %>% 
              filter(NM_UF == "Tocantins", 
                     NM_MUN %in% state_capitals$NM_MUN)) %>% tibble() %>% 
  mutate(dist_statecapital_km = as.numeric(`.` / 1000)) %>% 
  bind_cols(data.frame(sf_lba_sedes) %>% 
              filter(NM_UF == "Tocantins") %>% select(NM_UF, NM_MUN))
) %>% select(NM_UF, NM_MUN, dist_statecapital_km) -> df_capital_dist

dfdist <- data.frame(unlist(nndist))
dfdist$type <- row.names(dfdist)
#name i want
myseq <- paste("dist",seq(2,120, by=2) , sep="")
dfdist %>% filter(type %in% myseq ) %>% 
  pull(unlist.nndist.) %>% summary()


#Find urban mucicipalities 
df_lba %>% left_join(
df_lba %>% filter(ano == 2010) %>% 
  mutate(urban_50_flag = if_else(urban_percent >= 0.5, 1,0), 
         urban_70_flag = if_else(urban_percent > .70, 1, 0), 
         capital_flag = if_else(CD_MUN %in% state_capitals$CD_MUN, 1, 0)) %>% 
  mutate(urban = if_else(urban_50_flag ==1, "Yes", "No"), 
         `state capital` = if_else(capital_flag ==1,"Yes", "No")) %>%
  select(CD_MUN, urban_50_flag, urban_70_flag, urban, capital_flag, `state capital`)
) -> df_lba

# check why Mojuí dos Campos 1504752 is NA for capital flag
# 

allyears_labels <- c("1991", "1992", "1993", "1994", 
              "1995","1996", "1997", "1998", "1999", 
              "2000","2001", "2002", "2003", "2004", 
             "2005", "2006", "2007", "2008", "2009", 
             "2010", "2011", "2012", "2013", "2014", 
             "2015", "2016", "2017", "2018", "2019", "2020", "2021")
plot_labels = c("1991", "1995","2000", "2005", "2010", "2015", "2020")

expand.grid(ano = c(1991:2021), NM_UF = unique(df_lba$NM_UF)) %>% 
                        mutate(allyears_labels = factor(ano)) -> df_allyears

df_lba %>% right_join(df_allyears) %>%
  filter(!is.na(capital_flag)) %>% 
  ggplot(aes(x= allyears_labels, y = pop_total, 
             colour = urban)) + 
  geom_violin(aes(colour =  urban)) +
  geom_point(aes(shape = `state capital`), 
             position=position_jitterdodge(), alpha = 0.4) + 
  scale_x_discrete("Year", drop = FALSE, 
                   breaks = plot_labels, labels = plot_labels) +
  scale_y_continuous("Number of people (millions)", 
                     breaks = c(0, 1000000, 2000000),
                     labels = unit_format(unit = "M", 
                                          scale = 1e-6, 
                                          accuracy = 0.1)) +         
  facet_wrap(~ NM_UF) + 
  labs(title = "Population growth in the Legal Brazilian Amazon", 
  caption = "Source: Instituto Brasileiro de Geografia e Estatística (https://sidra.ibge.gov.br/, accessed 11 December 2021)"
  ) + 
  theme(plot.title.position = "plot", 
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0), 
        axis.text.x = element_text(angle = 45)) + 
  theme(legend.position="top") 

df_lba %>% 
  filter(!is.na(urban_percent), ano ==2010) %>% pull(urban_percent) %>% summary()

df_lba %>% 
  filter(!is.na(urban_percent)) %>% 
  mutate(urban_50_flag = if_else(urban_percent >= 0.5, 1,0)) %>%
  group_by(ano) %>% 
  summarise(urban_median = median(urban_percent), 
            municpality_count = length(unique(CD_MUN)), 
            urban_municipality = sum(urban_50_flag)) -> df_urban_sum
  
# Urban %
df_lba %>% filter(!is.na(urban_percent), ano == 2010, urban=="No") %>% 
  mutate(rural_pop = pop_total *(1-urban_percent)) %>%
  group_by(ano) %>% 
  summarise(acount = length(unique(CD_MUN)), 
            tot_area = sum(AREA_TOT), 
            tot_pop = sum(pop_total), 
            rur_pop = sum(rural_pop))
censu %>% filter(ano == 2010, codmun7 %in% as.numeric(unique(df_lba$CD_MUN))) %>% 
  group_by(ano) %>% 
  summarise(acount = length(unique(codmun7)), 
            tot_pop = sum(pesorur) + sum(pesourb), 
            rur_pop = sum(pesorur))

df_lba %>% 
  filter(!is.na(urban_percent)) %>%
  ggplot(aes(x = urban_percent*100)) + 
  geom_histogram() + 
  geom_vline(data = df_urban_sum, 
             aes(xintercept = urban_median *100), linetype = "dashed") +
  scale_x_continuous("Urban population (percentage of municipality total)") +
  facet_wrap(~ano) + 
  labs(title = "urban population distribution in the Legal Brazilian Amazon", 
       y = "Number of municipalities",
       caption = "Source: Instituto Brasileiro de Geografia e Estatística (https://sidra.ibge.gov.br/, accessed 11 December 2021)"
  ) + 
  theme(plot.title.position = "plot", 
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0))

# Rural municipalities
sf_lba %>% right_join(df_lba %>% filter(!is.na(urban_percent)) %>% 
                             mutate(rural_percent = (1 - urban_percent)*100) %>% 
                             select(ano, CD_MUN, rural_percent), 
                           by = c("CD_MUN" = "CD_MUN")
                           ) %>% 
  ggplot() + 
  geom_sf(data = sf_lba_poly, colour = "black", size=1) +
  #geom_sf(data = sf_lba, fill="grey70") +
  #geom_sf(data = sf_lba_line, aes(colour = NM_UF), size=0.3, show.legend = "line") +
  geom_sf(aes(fill = rural_percent, colour = rural_percent), size = 0.05) + 
  scale_x_continuous(breaks = seq(-75, -46.8, by = 10)) +
  scale_fill_gradient2(midpoint = 50) +
  facet_wrap(~ano)  + 
  theme(legend.position="bottom") + 
  guides(colour = "none", 
         fill = guide_colourbar(title.position = "left")) -> map_rural_percent

#map_rural_percent # large takes a while to load
tiff("map_rural_percent.tif", width = 15, height = 7, units = "cm", res = 600,
     compression = "lzw")
map_rural_percent + theme(text = element_text(size = 8))
dev.off()

#plot
df_lba %>% 
  filter(ano == 2010) %>%
  ggplot(aes(x = factor(ano), y = urban_percent, 
         colour = factor(urban_flag))) + 
  geom_violin(aes(colour =  factor(urban_flag))) + 
  geom_point(position=position_jitterdodge(), alpha = 0.4) +
  facet_wrap(~ NM_UF) 

df_lba %>% 
  ggplot(aes(x= factor(ano), y = pop_total, 
             colour = factor(urban_flag))) + 
  geom_violin(aes(colour =  factor(urban_flag))) +
  geom_point(position=position_jitterdodge(), alpha = 0.4) + 
  facet_wrap(~ NM_UF)  

df_lba %>% 
  filter(ano ==2021) %>%
  group_by(NM_UF) %>% 
    summarise(tot_pop_estado = sum(pop_total), 
              area_estado = sum(AREA_TOT)) %>% 
  ungroup() %>% 
  mutate(pop_dens_km_estado = tot_pop_estado / area_estado) 

#
df_lba %>% 
  ggplot(aes(x=ano, y = pop_total, colour = factor(urban_flag))) + 
  geom_point() + 
  stat_smooth() + 
  facet_wrap(~ NM_UF)

df_lba %>% 
  ggplot(aes(x=ano, y = pop_dens_km, colour = factor(urban_flag))) + 
  geom_point() + 
  stat_smooth() + 
  #facet_wrap(~ NM_UF, scales = "free_y")
facet_wrap(~ NM_UF)

#tidy GDP data
#From https://www.ibge.gov.br/estatisticas/economicas/contas-nacionais/9088-produto-interno-bruto-dos-municipios.html?=&t=sobre
#https://ftp.ibge.gov.br/Pib_Municipios/2019/base/base_de_dados_2002_2009_xls.zip
#https://ftp.ibge.gov.br/Pib_Municipios/2019/base/base_de_dados_2010_2019_xls.zip
#1999 - 2002 for conversion factor
#https://biblioteca.ibge.gov.br/index.php/biblioteca-catalogo?view=detalhes&id=25968
# "cd" has the data
#https://biblioteca.ibge.gov.br/visualizacao/livros/liv5968_cd.zip

df_gdp_muni_02a19 <- read_excel("data//ibge_gdp//bla_gdp_municipality_2002_2019.xlsx", 
                                .name_repair = "universal") %>% 
  rename(codmun7 = `Código.do.Município`)
unique(df_gdp_muni_02a19$year) #18 years 2002 - 2019
#df_gdp_muni_99a02 <- read_excel("data//ibge_gdp//bla_gdp_municipality_1999_2002.xlsx", 
#                                .name_repair = "universal")

#conversion factor for 2000, 2001?? check using 2002
#Use only 2002 - 2019. conversion is not straight forward
munitest <- sample(unique(df_gdp_muni_99a02$name_muni), 10)
df_gdp_muni_99a02 %>% 
  filter(year==2002, name_muni %in% munitest) %>% 
  mutate(gdp_old = gdp_percapita_reais, 
         gva_agri_old = gva_agri) %>% 
  select(year, uf_sigla, name_muni, gva_agri_old, gdp_old) %>% 
  left_join(
    df_gdp_muni_02a19 %>% 
      filter(year==2002, name_muni %in% munitest) %>% 
      mutate(gdp_new = gdp_percapita_reais, 
             gva_agri_new = gva_agri) %>% 
      select(year, uf_sigla, name_muni, gva_agri_new, gdp_new)
  ) %>% 
  mutate(gva_conv = gva_agri_old / gva_agri_new, 
         gdp_conv = gdp_old / gdp_new)

#Population for gva per capita
df_pop_muni_02a19 <- read_excel("data//bla_municipality_pop_02a19.xlsx", 
                                .name_repair = "universal") %>% 
  rename(year = Ano) %>% 
  mutate(pop_total = as.numeric(pop_total))
bla_codes <- unique(df_pop_muni_02a19$mun_cod)  
unique(df_pop_muni_02a19$year)
df_pop_muni_02a19 %>% filter(is.na(pop_total)) %>% select(`Município`, year) %>% 
  arrange(`Município`, year)

#include 10 year census data
#  pop_tot = mulhertot + homemtot
censu <- read_excel("data//pnud_municipios.xlsx", sheet = "pnud_municipios", 
                    .name_repair = "universal", na = c("", "NA")) %>% 
  filter(ano %in% c(2000, 2010))

df_pop_muni_02a19 %>% 
  rename(codmun7 = mun_cod) %>% 
  select(year, codmun7, pop_total) %>%
  bind_rows(
    censu %>% 
      filter(codmun7 %in% bla_codes) %>%
      mutate(pop_total = pesotot,
             year = ano) %>% 
      select(year, codmun7, pop_total) 
  ) %>% 
  pivot_wider(id_cols = codmun7, names_from = year, values_from = pop_total) %>%
  left_join(df_gdp_muni_02a19 %>% 
              mutate(codmun7 = `Código.do.Município`) %>%
              select(codmun7, uf_sigla, uf, name_muni) %>% 
              group_by(codmun7, uf_sigla, uf, name_muni) %>% summarise(acount = n()) %>% 
              ungroup()) %>% select(!acount) %>% 
  pivot_longer(cols = !c("codmun7", "uf_sigla", "uf", "name_muni"), 
               names_to = "year", values_to = "tot_pop") %>% 
  mutate(year = as.numeric(year)) %>% arrange(codmun7, year) -> df_pop_muni_00a19

df_gdp_muni_02a19 %>% left_join(df_pop_muni_00a19 %>% select(year, codmun7, tot_pop), 
                                by = c( "year" = "year", "codmun7" = "codmun7")) -> df_gdp_muni_02a19
#gva per capita
df_gdp_muni_02a19 %>% 
  mutate(gva_agri_percapita_reais = (gva_agri*1000) / tot_pop) -> df_gdp_muni_02a19
write.csv(df_gdp_muni_02a19, 
          "bla_municipality_gdppop_02a19.csv", row.names = FALSE)

