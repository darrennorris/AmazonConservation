#Forest loss municipality

#packages
library(plyr)
library(tidyverse)
library(readxl)
library(sf)

#IBGE Municipality polygons
longname <- "vector/brazil_ninestate_municipalities/ninestate_muni.shp"
sf_munis <- sf::st_read(longname) 
#States
sf_instate <- st_read("vector//ninestate_poly.shp")


# Forest loss, transition from cover change
# Get IBGE muni codes associated with file names
df_muni_ref <- read_excel("missing_munis.xlsx")
df_legend <- read_excel("data/mapbiomas_6_legend.xlsx")
df_legend %>% filter(type_class == "antropic") %>% 
  pull(class_description) -> human_class

get_files <- function(folder_name = NA) {
  library(tidyverse)
  folder_location <- folder_name
  in_files <- list.files(folder_location, 
                          pattern = ".csv", full.names = TRUE)
  data.frame(folder_id = folder_location, file_id = in_files) %>%  
    group_by(folder_id, file_id) %>% 
    summarise(file_count = n()) %>% 
    ungroup() -> df_muni_csv
  return(df_muni_csv)
}
infolder <- "mapbiomas_ge/trans/"
df_muni_csv <- get_files(folder_name = infolder)
import_files <- function(x){
  infile <- x$file_id
  myfile <- read.csv(infile, as.is = TRUE, encoding = "UTF-8")
  myfile 
}
df_trans_missing <- plyr::adply(df_muni_csv, .margins = 1, 
            .fun = import_files)

df_trans_missing %>% filter(file_count==1) %>% 
  select(!c(.geo, file_count, system.index)) %>%
  distinct() %>% 
  mutate(file_trans = str_replace(file_id, infolder, "")) %>% 
  left_join(df_muni_ref) -> df_trans_ibge
#check join worked
df_trans_ibge %>% 
  filter(is.na(AREA_KM2)) %>% 
  group_by(SIGLA_UF, file_id) %>% summarise(count_missing = n())

df_trans_ibge %>% 
  filter(from_class %in% c("Forest Formation", "Savanna Formation"), 
         to_class %in% all_of(human_class)) %>% 
  mutate(CD_MUN = as.character(CD_MUN), 
         year = as.numeric(str_sub(band, -4, -1)), 
         from_class = 
           str_trim(str_replace(from_class, " Formation", ""))) %>% 
  mutate(from_class = 
           paste(tolower(from_class), "_loss_km2", sep = "")) %>%
  group_by(CD_MUN, year, from_class) %>% 
  summarise(area_km2 = sum(area)) %>% 
  filter(!is.na(area_km2)) %>%
  pivot_wider(names_from = from_class, values_from = area_km2, 
              names_expand = TRUE, values_fill = 0) %>% 
  mutate(tot_loss_km2 = forest_loss_km2 + savanna_loss_km2) %>% 
  left_join(read_excel("data/bla_municipalities.xlsx", 
                        sheet = "municipality_annual") %>% 
               rename(CD_MUN = muni_code) %>% 
              filter(year==2019) %>% 
              mutate(CD_MUN = as.character(CD_MUN)) %>%
               select(CD_MUN, forestcover_1985med_km2)
             ) %>% 
  mutate(tot_loss_percent = 
           (tot_loss_km2 /forestcover_1985med_km2 ) *100) -> df_loss_missing
#update with lag values
df_loss_missing %>% 
  #lag values
  group_by(CD_MUN) %>%
  mutate(lag01_lossarea_km2 = lag(tot_loss_km2, order_by = year), 
         lag02_lossarea_km2 = lag(tot_loss_km2, n=2, order_by = year), 
         lag03_lossarea_km2 = lag(tot_loss_km2, n=3, order_by = year), 
         lag04_lossarea_km2 = lag(tot_loss_km2, n=4, order_by = year), 
         lag05_lossarea_km2 = lag(tot_loss_km2, n=5, order_by = year), 
         lag06_lossarea_km2 = lag(tot_loss_km2, n=6, order_by = year), 
         lag07_lossarea_km2 = lag(tot_loss_km2, n=7, order_by = year), 
         lag08_lossarea_km2 = lag(tot_loss_km2, n=8, order_by = year), 
         lag09_lossarea_km2 = lag(tot_loss_km2, n=9, order_by = year), 
         lag10_lossarea_km2 = lag(tot_loss_km2, n=10, order_by = year)) %>% 
  ungroup() %>% 
  # Loss as % of 1985 forest area.
  mutate(lag01_lossarea_per = round((lag01_lossarea_km2 / forestcover_1985med_km2)*100,3), 
         lag02_lossarea_per = round((lag02_lossarea_km2 / 
                                       forestcover_1985med_km2)*100,3), 
         lag03_lossarea_per = round((lag03_lossarea_km2 / 
                                       forestcover_1985med_km2)*100,3), 
         lag04_lossarea_per = round((lag04_lossarea_km2 / 
                                       forestcover_1985med_km2)*100,3), 
         lag05_lossarea_per = round((lag05_lossarea_km2 / forestcover_1985med_km2)*100,3), 
         lag06_lossarea_per = round((lag06_lossarea_km2 / forestcover_1985med_km2)*100,3), 
         lag07_lossarea_per = round((lag07_lossarea_km2 / forestcover_1985med_km2)*100,3), 
         lag08_lossarea_per = round((lag08_lossarea_km2 / forestcover_1985med_km2)*100,3), 
         lag09_lossarea_per = round((lag09_lossarea_km2 / forestcover_1985med_km2)*100,3), 
         lag10_lossarea_per = round((lag10_lossarea_km2 / 
                                       forestcover_1985med_km2)*100,3)) -> df_loss_missing


# Rhett data --------------------------------------------------------------

# Transition as a proportion of municipality area.
#Mapbiomas
dfmapbiomas_transition_muni <- read_excel("data//Mapbiomas-Brazil-transition.xlsx", 
                                          sheet = "Sheet1", 
                                          .name_repair = "universal") %>% 
  filter(state %in% bla_state_names)

#Include earlier for lag values
cols_transition <- c(paste("..",1991:2019,".", 1992:2020, sep=""))
dfmapbiomas_transition_muni %>% 
  filter(from_level_2 %in% c("Forest Formation", "Savanna Formation"), 
         to_level_0 =="Anthropic") %>% 
  select(state, city, from_level_1, from_level_2, 
         to_level_0, to_level_2, all_of(cols_transition)) -> dfmapbiomas_forest_transition_muni

#773 municipalities
dfmapbiomas_forest_transition_muni %>% 
  group_by(state) %>% summarise(acount = length(unique(city))) %>% pull(acount) %>% sum()
#state and city names give a match to IBGE data except in four
#df_muni_cagr %>% filter(codmun7 == 5104526) #Ipiranga do Norte
#df_muni_cagr %>% filter(codmun7 == 5104542) #Itanhangá
#df_muni_cagr %>% filter(codmun7 == 1504752) # Mojuí dos Campos
#df_muni_cagr %>% filter(codmun7 == 5104542) #Fortaleza do Tabocão

#long format with annual totals
dfmapbiomas_forest_transition_muni %>% 
  select(state, city, from_level_2, all_of(cols_transition)) %>% 
  pivot_longer(cols = starts_with(".."), names_to = "ayear", names_prefix ="..", 
               values_to = "total_forestcover_loss") %>% 
  group_by(state, city, from_level_2, ayear) %>% 
  summarise(area_km2 = sum(total_forestcover_loss, na.rm=TRUE)/100) %>% 
  ungroup() %>%
  mutate(year = as.numeric(substr(ayear,6,11)), 
         from_class = if_else(from_level_2 == "Savanna Formation", 
                               "savanna", "forest")) %>% 
  mutate(from_class = 
           paste(tolower(from_class), "_loss_km2", sep = "")) -> dfmapbiomas_forest_transition_muni_long

#Update with muni code and 1985 cover
dfmapbiomas_forest_transition_muni_long %>% 
  left_join(read_excel("data/bla_municipalities.xlsx", 
                       sheet = "municipality_annual") %>% 
              rename(CD_MUN = muni_code) %>% 
              filter(year==2019) %>% 
              mutate(CD_MUN = as.character(CD_MUN)) %>%
              select(state_name, muni_name, CD_MUN, forestcover_1985med_km2), 
            by = c("state"="state_name", "city" = "muni_name")) -> dfmapbiomas_forest_transition_muni_long

#Update with lag values
dfmapbiomas_forest_transition_muni_long %>% 
  group_by(CD_MUN, year, from_class, forestcover_1985med_km2) %>% 
  summarise(area_km2 = sum(area_km2, na.rm = TRUE)) %>%
  pivot_wider(id_cols = c(CD_MUN, forestcover_1985med_km2, year), 
              names_from = from_class, values_from = area_km2) %>% 
  mutate(tot_loss_km2 = (replace_na(forest_loss_km2,0) + 
                                 replace_na(savanna_loss_km2,0))) %>% 
  mutate(tot_loss_percent = 
           (tot_loss_km2 / forestcover_1985med_km2) *100) %>%
  group_by(CD_MUN) %>%
  mutate(lag01_lossarea_km2 = lag(tot_loss_km2, order_by = year), 
         lag02_lossarea_km2 = lag(tot_loss_km2, n=2, order_by = year), 
         lag03_lossarea_km2 = lag(tot_loss_km2, n=3, order_by = year), 
         lag04_lossarea_km2 = lag(tot_loss_km2, n=4, order_by = year), 
         lag05_lossarea_km2 = lag(tot_loss_km2, n=5, order_by = year), 
         lag06_lossarea_km2 = lag(tot_loss_km2, n=6, order_by = year), 
         lag07_lossarea_km2 = lag(tot_loss_km2, n=7, order_by = year), 
         lag08_lossarea_km2 = lag(tot_loss_km2, n=8, order_by = year), 
         lag09_lossarea_km2 = lag(tot_loss_km2, n=9, order_by = year), 
         lag10_lossarea_km2 = lag(tot_loss_km2, n=10, order_by = year)) %>% 
  mutate(lag01_lossarea_per = round((lag01_lossarea_km2 / forestcover_1985med_km2)*100,3), 
         lag02_lossarea_per = round((lag02_lossarea_km2 / forestcover_1985med_km2)*100,3), 
         lag03_lossarea_per = round((lag03_lossarea_km2 / forestcover_1985med_km2)*100,3), 
         lag04_lossarea_per = round((lag04_lossarea_km2 / forestcover_1985med_km2)*100,3), 
         lag05_lossarea_per = round((lag05_lossarea_km2 / forestcover_1985med_km2)*100,3), 
         lag06_lossarea_per = round((lag06_lossarea_km2 / forestcover_1985med_km2)*100,3), 
         lag07_lossarea_per = round((lag07_lossarea_km2 / forestcover_1985med_km2)*100,3), 
         lag08_lossarea_per = round((lag08_lossarea_km2 / forestcover_1985med_km2)*100,3), 
         lag09_lossarea_per = round((lag09_lossarea_km2 / forestcover_1985med_km2)*100,3), 
         lag10_lossarea_per = round((lag10_lossarea_km2 / 
                                       forestcover_1985med_km2)*100,3)) -> df_loss_rhett

sort(names(df_loss_missing)); sort(names(df_loss_rhett))
rbind(df_loss_missing, df_loss_rhett) %>% 
  distinct() -> df_loss_all
length(unique(df_loss_all$CD_MUN))  

data.frame(sf_munis) %>% select(!geometry) %>% 
  crossing(year = 2002:2019) %>% left_join(df_loss_all) %>% 
  arrange(SIGLA_UF, NM_MUN) %>% 
  write.csv("muni_fixed_lossyear.csv", row.names = FALSE)

#
dfmapbiomas_forest_transition_muni_long %>% 
  filter(year %in% c(2002:2019))  %>% 
  group_by(state, city, cover_class) %>% 
  summarise(area_km2 = sum(area_km2, na.rm = TRUE)) %>%
  pivot_wider(id_cols = c(state, city), 
              names_from = cover_class, values_from = area_km2) %>% 
  mutate(tot_transition_km2 = (replace_na(forest,0) + replace_na(savanna,0))) %>%
  left_join(df_muni_cagr, 
            by = c("state" = "uf",  "city"="NM_MUN")) %>% 
  filter(!is.na(gdp_start)) -> df_muni_cagr_mapbiomas #769

#Add usefull flags 
df_muni_cagr_mapbiomas %>% 
  left_join(data.frame(sf_ninestate_muni) %>% select(-geometry) %>% 
              mutate(codmun7 = as.numeric(CD_MUN))) %>% 
  left_join(df_gdppop_muni_02a19 %>% 
              filter(year==2019) %>% 
              mutate(gdp_percapita_usd = gdp_percapita_reais / 3.946, 
                     flag_metropolitan = if_else(is.na(Região.Metropolitana), 
                                                 "other", "metropolitan"), 
                     flag_urban = if_else(is.na(Nome.Concentração.Urbana), "rural", "urban"), 
                     flag_capital = if_else(codmun7 %in% bla_state_capitals$codmun7, 
                                            "yes", "no"))) %>% 
  mutate(tot_transition_percent = (tot_transition_km2 / AREA_KM2)*100) -> df_figgrowth

#Export
#write.csv(df_figgrowth, "data\\bla_municipality_gdp_forestloss.csv", row.names = FALSE)

df_figgrowth %>%  
ggplot(aes(x=tot_transition_km2, y = cagr_gdp_percapita)) + 
  geom_point(aes(colour=flag_urban, shape=flag_capital)) + 
  stat_smooth(method = "gam") + 
  scale_shape_discrete("state capital") +
  scale_colour_discrete("urban\nconcentration", labels=c("no", "yes")) +
  facet_wrap(~uf, scales = "free_x", ncol=1) +
  labs(title = "(A) GDP growth 2002 - 2019", 
       y="compound annual growth rate GDP per capita", 
       x = bquote('forest cover transition'~(km^2))) + 
  theme(text = element_text(size = 16), 
         plot.title.position = "plot", 
        #legend.position="top"
         legend.position="none") -> fig_gdpgrowth

png(file = "figures//fig_gdpgrowth.png", bg = "white", type = c("cairo"), 
    width=2200, height=9000, res = 600)
fig_gdpgrowth
dev.off()

df_figgrowth %>%  
  ggplot(aes(x=tot_transition_percent, y = cagr_gdp_percapita)) + 
  geom_point(aes(colour=flag_urban, shape=flag_capital)) + 
  stat_smooth(method = "gam") + 
  scale_shape_discrete("state capital") +
  scale_colour_discrete("urban\nconcentration", labels=c("no", "yes")) +
  facet_wrap(~uf, ncol=1) +
  labs(title = "(C) GDP growth 2002 - 2019", 
       y="compound annual growth rate GDP per capita", 
       x = "forest cover transition (%)") + 
  theme(plot.title = element_text(color = "white"),
        axis.title.y = element_text(color = "white"),
        text = element_text(size = 16),
        plot.title.position = "plot", 
        #legend.position="top"
        legend.position="none") -> fig_gdpgrowth_percent

png(file = "figures//fig_gdpgrowth_percent.png", bg = "white", type = c("cairo"), 
    width=2200, height=9000, res = 600)
fig_gdpgrowth_percent
dev.off()

df_figgrowth %>%  
  ggplot(aes(x=tot_transition_km2, y = cagr_gva_agri_percapita)) + 
  geom_point(aes(colour=flag_urban, shape=flag_capital)) + 
  stat_smooth(method = "gam") + 
  scale_shape_discrete("state capital") +
  scale_colour_discrete("urban\nconcentration", labels=c("no", "yes")) +
  facet_wrap(~uf, scales = "free_x", ncol=1) +
  labs(title = "(B) GVA growth 2002 - 2019", 
       y="compound annual growth rate agricultural GVA per capita", 
       x = bquote('forest cover transition'~(km^2))) + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        #legend.position="top"
        legend.position="none") -> fig_gvagrowth

png(file = "figures//fig_gvagrowth.png", bg = "white", type = c("cairo"), 
    width=2200, height=9000, res = 600)
fig_gvagrowth
dev.off()

df_figgrowth %>%  
  mutate(tot_transition_percent = (tot_transition_km2 / AREA_KM2)*100) %>%
  ggplot(aes(x=tot_transition_percent, y = cagr_gva_agri_percapita)) + 
  geom_point(aes(colour=flag_urban, shape=flag_capital)) + 
  stat_smooth(method = "gam") + 
  scale_shape_discrete("state capital") +
  scale_colour_discrete("urban\nconcentration", labels=c("no", "yes")) +
  facet_wrap(~uf, ncol=1) +
  labs(title = "(d) GVA growth 2002 - 2019", 
       y="compound annual growth rate agricultural GVA per capita", 
       x = "forest cover transition (%)") + 
  theme(plot.title = element_text(color = "white"),
        axis.title.y = element_text(color = "white"), 
        text = element_text(size = 16), 
        plot.title.position = "plot", 
        #legend.position="top"
        legend.position="none") -> fig_gvagrowth_percent

png(file = "figures//fig_gvagrowth_percent.png", bg = "white", type = c("cairo"), 
    width=2200, height=9000, res = 600)
fig_gvagrowth_percent
dev.off()


#Bivariate maps
df_figgrowth

sf_ninestate_muni

#
dfbi_class <- bi_class(df_figgrowth, x = tot_transition_percent, y = cagr_gdp_percapita, 
                 style = "quantile", dim = 3)
dfbi_class_gva_agri <- bi_class(df_figgrowth, x = tot_transition_percent, 
                                y = cagr_gva_agri_percapita, 
                       style = "quantile", dim = 3)

sf_ninestate_muni %>% left_join(dfbi_class) %>% 
  ggplot()+
  geom_sf(aes(fill = bi_class), 
          color = "white", size = 0.1, show.legend = FALSE) + 
  bi_scale_fill(pal = "DkBlue", dim = 3, na.value = "grey50") + 
  #scale_x_continuous(breaks = seq(-75, -44, by = 10)) +   +
  coord_sf(crs = 4326, datum = NA) +
  theme_bw() + 
  #bi_theme() +
  #labs(title = "Forest loss and economic development 2002 - 2019")  + 
  theme(plot.title.position = "plot", 
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0)) -> bimap_gdp

sf_ninestate_muni %>% left_join(dfbi_class_gva_agri) %>% 
  ggplot()+
  geom_sf(aes(fill = bi_class), 
          color = "white", size = 0.1, show.legend = FALSE) + 
  bi_scale_fill(pal = "DkBlue", dim = 3, na.value = "grey50") + 
  #scale_x_continuous(breaks = seq(-75, -44, by = 10)) +   +
  coord_sf(crs = 4326, datum = NA) +
  theme_bw() + 
  #bi_theme() +
  #labs(title = "Forest loss and economic development 2002 - 2019")  + 
  theme(plot.title.position = "plot", 
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0)) -> bimap_gva

#Follow code example from help
legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "forest loss",
                    ylab = "GDP gain",
                    size = 16)
legend_gva <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "forest loss",
                    ylab = "GVA gain",
                    size = 16)
#?draw_plot
figbi_gdp <- cowplot::ggdraw() +
  cowplot::draw_plot(bimap_gdp, 0, 0, 1, 1) +
  cowplot::draw_plot(legend, 0.7, .71, 0.35, 0.25) + 
  draw_text("(D)", x = 0.03, y = 0.95, size = 30)
figbi_gva <- cowplot::ggdraw() +
  cowplot::draw_plot(bimap_gva, 0, 0, 1, 1) +
  cowplot::draw_plot(legend_gva, 0.7, .71, 0.35, 0.25) + 
  draw_text("(E)", x = 0.03, y = 0.95, size = 30)
#fig3b <- cowplot::ggdraw() +
#  cowplot::draw_plot(bimap_intensity, 0, 0, 1, 1) +
#  cowplot::draw_plot(legend, 0.7, .65, 0.35, 0.25)

png(file = "figures//fig_bimap_gdp.png", bg = "white", type = c("cairo"), 
    width=8000, height=6000, res = 600)
figbi_gdp
dev.off()

png(file = "figures//fig_bimap_gva.png", bg = "white", type = c("cairo"), 
    width=8000, height=6000, res = 600)
figbi_gva
dev.off()

#INPE deforestation
#States and municipality names all in capitals without accents.
#"geocode" is codmun7
dfinpe_forestloss_muni <- read_excel("data//INPE_Amazon_Cerrado.xlsx", 
                                sheet = "Amazon-municipality", 
                                .name_repair = "universal") %>% 
  rename(area_km2 = areakm, 
         codmun7 = geocode_ibge) %>%
  mutate(biome = "Amazon") %>% 
  select(biome, year, state, municipality, codmun7, area_km2) %>% 
  rbind(read_excel("data//INPE_Amazon_Cerrado.xlsx", 
                   sheet = "Cerrado-municipality", 
                   .name_repair = "universal") %>% 
          filter(state %in% c("MATO GROSSO", "TOCANTINS", "MARANHÃO", "RONDÔNIA")) %>% 
          rename(area_km2 = areakm, 
                 codmun7 = geocode_ibge) %>%
          mutate(biome = "Cerrado") %>% 
          select(biome, year, state, municipality, codmun7, area_km2) 
        )
#some municipalities have different biomes
dfinpe_forestloss_muni %>% group_by(codmun7) %>% 
  summarise(count_biome = length(unique(biome))) %>% arrange(desc(count_biome))

df_gdppop_muni_02a19 %>% left_join(
dfinpe_forestloss_muni %>% 
  group_by(year, codmun7, biome) %>% 
  summarise(area_km2 = sum(area_km2, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = biome, values_from = area_km2) %>% 
  rename(inpe_loss_cerrado_km2 = Cerrado, 
         inpe_loss_amazon_km2 = Amazon) %>% 
  mutate(inpe_loss_total_km2 = replace_na(inpe_loss_cerrado_km2, 0) + 
           replace_na(inpe_loss_amazon_km2, 0))
) -> df_gdppop_muni_02a19inpe

#Hansen forest loss
#Hansen
dfhansen_forestloss_primary <- read_excel("data//brazil_gfw_loss_2021_hansen.xlsx", 
                                          sheet = "primary_loss_admin2", 
                                          .name_repair = "universal") %>% 
  filter(admin1 %in% bla_states)

dfhansen_forestloss_primary[,-c(1,2,5)] %>% 
  pivot_longer(!c(admin1, admin2), names_to = "ayear", names_prefix ="...", 
               values_to = "primary_loss_ha") %>% 
  mutate(year = as.numeric(ayear)) -> dfhansen_forestloss_primary_long



#Difference in coverage
dfmapbiomas_transition_muni %>% group_by(state) %>% 
  rename(state_mapbiomas = state) %>%
  summarise(count_muni_mapbiomas = length(unique(city))) %>% 
  bind_cols(dfinpe_forestloss_muni %>% group_by(state) %>% 
              summarise(count_muni_inpe = length(unique(codmun7)))) %>% 
  bind_cols(dfhansen_forestloss_primary %>% group_by(admin1) %>% 
              summarise(count_muni_hansen = length(unique(admin2))))

### Where / Why Hansen increased in 2016/2017
#Check coverage is same - yes municipality count is same
dfinpe_forestloss_muni %>% group_by(state) %>% 
  summarise(count_muni_inpe = length(unique(codmun7))) %>% 
  bind_cols(
    dfhansen_forestloss_primary_long %>% 
      filter(year %in% c(2002:2015)) %>%
              group_by(admin1) %>% 
              summarise(count_muni_hansen15 = length(unique(admin2)))
  ) %>% 
  bind_cols(
    dfhansen_forestloss_primary_long %>% 
      filter(year %in% c(2016:2017)) %>%
      group_by(admin1) %>% 
      summarise(count_muni_hansen17 = length(unique(admin2)))
  )

#Check annual means per state. Roraima, Maranhão, Tocantins, Para
dfhansen_forestloss_primary_long %>% 
  filter(year %in% c(2010:2015)) %>%
  group_by(admin1) %>% 
  summarise(mean_loss10_15 = mean(primary_loss_ha, na.rm = FALSE)) %>% 
  bind_cols(
    dfhansen_forestloss_primary_long %>% 
      filter(year %in% c(2016:2017)) %>%
      group_by(admin1) %>% 
      summarise(mean_loss16_17 = mean(primary_loss_ha, na.rm = FALSE))
  ) %>% 
  bind_cols(
    dfhansen_forestloss_primary_long %>% 
      filter(year %in% c(2018:2020)) %>%
      group_by(admin1) %>% 
      summarise(mean_loss18_20 = mean(primary_loss_ha, na.rm = FALSE))
  ) %>%
  mutate(change_mean = round(((mean_loss16_17 - mean_loss10_15) / mean_loss10_15) *100,1)) %>% 
  select(admin1...1, mean_loss10_15, mean_loss16_17, mean_loss18_20, change_mean) %>% 
  arrange(desc(change_mean))

#Roraima
#Check annual means per municipality: Cantá, Rorainópolis and Iracema
dfhansen_forestloss_primary_long %>% 
  filter(year %in% c(2010:2015), admin1 == "Roraima") %>%
  group_by(admin2) %>% 
  summarise(mean_loss10_15 = mean(primary_loss_ha, na.rm = FALSE)) %>% 
  right_join(
    dfhansen_forestloss_primary_long %>% 
      filter(year %in% c(2016:2017, admin1 == "Roraima")) %>%
      group_by(admin2) %>% 
      summarise(mean_loss16_17 = mean(primary_loss_ha, na.rm = FALSE))
  ) %>% 
  right_join(
    dfhansen_forestloss_primary_long %>% 
      filter(year %in% c(2018:2020), admin1 == "Roraima") %>%
      group_by(admin2) %>% 
      summarise(mean_loss18_20 = mean(primary_loss_ha, na.rm = FALSE))
  ) %>%
  mutate(change_mean = round(((mean_loss16_17 - mean_loss10_15) / mean_loss10_15) *100,1)) %>%
  arrange(desc(mean_loss16_17))
