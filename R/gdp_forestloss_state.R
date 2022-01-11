#Forest loss

#packages
library(tidyverse)
library(readxl)
library(gridExtra)
library(psych)

#State level
#GDP (IBGE 4 january 2022 "Especiais" com os dados apresentados no informativo
# https://biblioteca.ibge.gov.br/index.php/biblioteca-catalogo?view=detalhes&id=2101873
# https://biblioteca.ibge.gov.br/visualizacao/livros/liv101873_informativo.pdf
#excel files
# https://ftp.ibge.gov.br/Contas_Regionais/2019/xls/Especiais_2010_2019_xls.zip
# https://ftp.ibge.gov.br/Contas_Regionais/1985_a_2003/Especiais/

df_gdp_state <- read_excel("data//bla_gdp2002_2019.xlsx", 
           .name_repair = "universal") %>% 
  pivot_longer(!uf, names_to = "ayear", names_prefix ="...", 
               values_to = "gdp_reais_millions") %>% 
  mutate(ayear = as.numeric(ayear))
df_gdp_state %>% filter(ayear==2007)
df_gdp_state %>% pull(ayear) %>% unique()

#correct 2002 PIB values using IPCA at 2.676. I.e 10 in 2002 is 26.76 in 2018 

#Population (from ""ibge_sidrar_download.R)
df_pop_state <- read_excel("data//bla_state_pop_1991_2021.xlsx", 
                     .name_repair = "universal")
df_pop_state %>% filter(Ano=="2007") #missing population data for 2007
df_pop_state %>% pull(ayear) %>% unique()
#https://www.ibge.gov.br/estatisticas/sociais/populacao/9103-estimativas-de-populacao.html?edicao=17283&t=downloads
#2021 estimates from https://www.ibge.gov.br/estatisticas/sociais/populacao/9103-estimativas-de-populacao.html?=&t=resultados





df_gdp_state %>% right_join(df_pop_state) %>% 
  mutate(gdp_per_capita = (gdp_reais_millions * 1000000) / total_pop) -> df_state_gdp

# GDP per capita across the Brazilian Legal Amazon
# Conversion to US$ based on 2019 annual rate:
# https://www.irs.gov/individuals/international-taxpayers/yearly-average-currency-exchange-rates
df_state_gdp %>% filter(ayear==2007)

df_state_gdp %>% filter(!is.na(gdp_per_capita)) %>%
  group_by(ayear) %>% 
  summarise(tot_pop = sum(total_pop, na.rm = TRUE), 
            tot_gdp = sum(gdp_reais_millions, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(gdp_per_capita_reais = (tot_gdp * 1000000) / tot_pop) %>% 
  mutate(gdp_per_capita_usd = gdp_per_capita_reais / 3.946) -> df_bla_gdp
df_bla_gdp %>% pull(gdp_per_capita_reais)%>% max() -> gdp_per_capita_reais_max 
#[1] 22762.05
df_bla_gdp %>% pull(gdp_per_capita_usd)%>% max() -> gdp_per_capita_usd_max 
#[1] 5768.387

#INPE
dfinpe_forestloss <- read_excel("data//INPE_Amazon_Cerrado.xlsx", sheet = "Amazon-total", 
                              .name_repair = "universal") %>% 
  mutate(biome = "Amazon", 
         area_km2 = `area.km²`) %>% 
  select(biome, year, area_km2) %>% 
  rbind(read_excel("data//INPE_Amazon_Cerrado.xlsx", sheet = "Cerrado-State", 
           .name_repair = "universal") %>% 
  filter(uf %in% c("MATO GROSSO", "TOCANTINS", "MARANHÃO", "RONDÔNIA")) %>% 
  group_by(year) %>% 
  summarise(`area.km²` = sum(`area.km²`)) %>% 
  mutate(biome = "Cerrado", 
         area_km2 = `area.km²`) %>% 
  select(biome, year, area_km2))

dfinpe_forestloss %>% 
  group_by(year) %>% 
  arrange(year) %>% 
  summarise(area_tot_km2 = sum(area_km2)) %>% 
  mutate(area_lag1 = lag(area_tot_km2, n = 1L), 
         area_lag2 = lag(area_tot_km2, n = 2L)) %>% 
  pivot_longer(!year, names_to = "lag_year", values_to = "area_km2") %>% 
  group_by(year) %>% 
  summarise(area_km2_3y = median(area_km2)) -> dfinpe_forestloss_3yr
  
dfinpe_forestloss %>% group_by(year) %>% 
  summarise(area = sum(area_km2, na.rm=TRUE)) %>% pull(area)%>% max() -> forestloss_max
# 40340.38 km2
#http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/prodes 
# 2004 = 27772 km2 and 20221 = 13235
dfinpe_forestloss %>% 
  filter(year ==2004) #1 Amazon   2004   27772
dfinpe_forestloss %>% 
  filter(year ==2021) #Amazon  2021    13235

axis_trans <- forestloss_max / gdp_per_capita_usd_max
df_forestloss_labels <- data.frame(year = c(2004, 2019), 
                                   yaxis_value = forestloss_max +(forestloss_max*0.05),
                                   label_values = c(round(forestloss_max,0), 
                                                     round(gdp_per_capita_usd_max,0)))
df_forestloss_labels
mycols <- c("Amazon" = "darkorchid1", "Cerrado" = "darkmagenta")
dfinpe_forestloss %>% 
  filter(year >1999) %>%
  ggplot(aes(x=year, y=area_km2)) + 
  geom_col(aes(fill = biome), colour="blue") + 
  geom_line(data = dfinpe_forestloss_3yr, aes(x=year, y = area_km2_3y), 
            colour ="grey60", size = 1.5) +
  geom_line(data = dfinpe_forestloss_3yr, aes(x=year, y = area_km2_3y), 
            colour ="yellow", linetype = "dashed", size = 1.1) +
  geom_line(data = df_bla_gdp, 
            aes(x=ayear, y=gdp_per_capita_usd*axis_trans), size=2, color="black") + 
  scale_y_continuous(#for the second y-axis
  sec.axis = sec_axis(~./axis_trans,#divided by transformation rate, in order to be represented based on the first y-axis
                      name = "GDP per capita (US$)")) + 
  scale_x_continuous(limits = c(1999.5, 2020.5))  + 
  scale_fill_manual("deforestation per biome:", values = mycols) + 
  geom_label(data = df_forestloss_labels, 
             aes(x= year, y = yaxis_value, label = label_values), 
             colour = c("blue", "black")) + 
  labs(y = bquote('deforestation'~(km^2)))  + 
  #theme_bw() +
  theme(legend.position = "top", legend.box = "horizontal") -> fig_deforestation_gdp
fig_deforestation_gdp

png(file = "figures//figure_deforestation.png", bg = "white", type = c("cairo"), 
    width=5000, height=3000, res = 600)
fig_deforestation_gdp + theme(text = element_text(size = 16))
dev.off()

bla_states <- c("Acre", "Amapá", "Amazonas", "Maranhão", 
                "Mato Grosso", "Pará", "Tocantins", "Rondônia", "Roraima")
#Hansen
dfhansen_forestloss_primary <- read_excel("data//brazil_gfw_loss_2021_hansen.xlsx", 
                                  sheet = "primary_loss_admin1", 
                                .name_repair = "universal") %>% 
  filter(admin1 %in% bla_states)
dfhansen_coverloss <- read_excel("data//brazil_gfw_loss_2021_hansen.xlsx", 
                                          sheet = "tree_cover_loss_admin1", 
                                          .name_repair = "universal") %>% 
  filter(admin1 %in% bla_states)


#Long format
dfhansen_forestloss_primary[,-c(1,2,4)] %>% 
  pivot_longer(!c(admin1, admin2), names_to = "ayear", names_prefix ="...", 
                            values_to = "primary_loss") %>% 
                 mutate(ayear = as.numeric(ayear)) %>% right_join(
dfhansen_coverloss[,-c(1,2,4)] %>% 
  pivot_longer(!admin1, names_to = "ayear", names_prefix ="...", 
               values_to = "total_forestcover_loss") %>% 
  mutate(ayear = as.numeric(ayear))
                 ) %>% 
  mutate(forestcover_loss = total_forestcover_loss - primary_loss) %>% 
  group_by(ayear) %>% 
  summarise(total_forestcover_loss_km2 = sum(total_forestcover_loss, na.rm = TRUE)/100, 
            primary_loss_km2 = sum(primary_loss, na.rm = TRUE)/100, 
            forestcover_loss_km2 = sum(forestcover_loss, na.rm = TRUE)/100) %>% 
  mutate(forestcover_loss_km2 = if_else(forestcover_loss_km2==0, total_forestcover_loss_km2, 
                                        forestcover_loss_km2)) %>% 
  pivot_longer(!ayear, names_to = "forest_loss", 
               values_to = "area_km2") %>% 
  filter(forest_loss != "total_forestcover_loss_km2") %>% 
  mutate(area_km2 = if_else(area_km2 ==0, NA_real_, area_km2), 
         year = ayear)  -> dfhansen_long
# 
dfhansen_long %>% 
  group_by(year) %>% 
  arrange(year) %>% 
  summarise(area_tot_km2 = sum(area_km2, na.rm = TRUE)) %>% 
  mutate(area_lag1 = lag(area_tot_km2, n = 1L), 
         area_lag2 = lag(area_tot_km2, n = 2L)) %>% 
  pivot_longer(!year, names_to = "lag_year", values_to = "area_km2") %>% 
  group_by(year) %>% 
  summarise(area_km2_3y = median(area_km2)) -> dfhansen_forestloss_3yr
#  
dfhansen_long %>% group_by(ayear) %>% 
  summarise(area = sum(area_km2, na.rm=TRUE)) %>% pull(area)%>% max() -> hansenloss_max
axis_trans_hansen <- hansenloss_max / gdp_per_capita_usd_max
df_hansen_labels <- data.frame(year = c(2016, 2019), 
                               yaxis_value = hansenloss_max +(hansenloss_max*0.05),
                               label_values = c(round(hansenloss_max,0), 
                                                round(gdp_per_capita_usd_max,0)))
df_hansen_labels
mycols_hansen <- c("primary_loss_km2" = "darkorange", 
                   "forestcover_loss_km2" = "darkgoldenrod1")  
dfhansen_long %>%
ggplot(aes(x=year, y=area_km2)) + 
  geom_col(aes(fill = forest_loss), colour="blue") + 
  geom_line(data = df_bla_gdp, 
            aes(x=ayear, y=gdp_per_capita_usd*axis_trans_hansen), size=2, color="black") + 
  scale_y_continuous(#for the second y-axis
    sec.axis = sec_axis(~./axis_trans_hansen,#divided by transformation rate, in order to be represented based on the first y-axis
                        name = "GDP per capita (US$)")) + 
  scale_x_continuous(limits = c(1999.5, 2020.5))  + 
  scale_fill_manual("forest loss:", values = mycols_hansen, 
                    labels = c("primary forest", "tree cover")) + 
  geom_label(data = df_hansen_labels, 
             aes(x= year, y = yaxis_value, label = label_values), 
             colour = c("blue", "black")) + 
  labs(y = bquote('forest cover loss'~(km^2)))  +
  theme(legend.position = "top", legend.box = "horizontal") -> fig_forestloss_gdp

png(file = "figures//figure_forestloss.png", bg = "white", type = c("cairo"), 
    width=5000, height=3000, res = 600)
fig_forestloss_gdp + theme(text = element_text(size = 16))
dev.off()

#Mapbiomas
dfmapbiomas_transition <- read_excel("data//Mapbiomas-Brazil-transition.xlsx", 
                                          sheet = "Sheet1", 
                                          .name_repair = "universal") %>% 
  filter(state %in% bla_states)
#unique(dfmapbiomas_landcover$state)
#Follow Hansen, 2000 is reference year to calculate annual losses from 2000 to 2020

cols_transition <- c(paste("..",1990:2019,".", 1991:2020, sep=""))
unique(dfmapbiomas_transition)
dfmapbiomas_transition %>% 
  filter(from_level_1=="1. Forest", to_level_0 =="Anthropic") %>% 
  select(state, city, from_level_1, from_level_2, 
         to_level_0, to_level_2, cols_transition) -> dfmapbiomas_forest_transition

dfmapbiomas_forest_transition %>% 
  select(from_level_2, cols_transition) %>% 
  pivot_longer(!from_level_2, names_to = "ayear", names_prefix ="..", 
               values_to = "total_forestcover_loss") %>% 
  group_by(from_level_2, ayear) %>% 
  summarise(area_km2 = sum(total_forestcover_loss, na.rm=TRUE)/100) -> dfmapbiomas_forest_transition_long
#transition from "flooded forest class is only 3.103197 km, combine with forest 
dfmapbiomas_forest_transition_long %>% 
  filter(from_level_2 == "Flooded Forest") %>% pull(area_km2) %>% sum(na.rm=TRUE)
#[1] 3.103197

# 3 year median
dfmapbiomas_forest_transition_long %>% 
  filter(from_level_2 != "Magrove") %>% 
  mutate(year = as.numeric(substr(ayear,6,11)), 
         cover_class = if_else(from_level_2 == "Savanna Formation", "savanna", 
                               "forest")) %>%  
  group_by(year) %>% 
  arrange(year) %>% 
  summarise(area_tot_km2 = sum(area_km2, na.rm = TRUE)) %>% 
  mutate(area_lag1 = lag(area_tot_km2, n = 1L), 
         area_lag2 = lag(area_tot_km2, n = 2L)) %>% 
  pivot_longer(!year, names_to = "lag_year", values_to = "area_km2") %>% 
  group_by(year) %>% 
  summarise(area_km2_3y = median(area_km2)) -> dfmapbiomas_forestloss_3yr

dfmapbiomas_forest_transition_long %>% 
  filter(from_level_2 != "Magrove") %>% 
  group_by(ayear) %>%
  summarise(area = sum(area_km2, na.rm=TRUE)) %>% 
  pull(area) %>% max() -> mapbiomasloss_max #[1] 46185.84

axis_trans_mapbiomas <- mapbiomasloss_max / gdp_per_capita_usd_max
df_mapbiomas_labels <- data.frame(year = c(2004, 2019), 
                               yaxis_value = mapbiomasloss_max +(mapbiomasloss_max*0.05),
                               label_values = c(round(mapbiomasloss_max,0), 
                                                round(gdp_per_capita_usd_max,0)))
df_mapbiomas_labels
mycols_mapbiomas <- c("savanna" = "deeppink4", 
                   "forest" = "deeppink") 

dfmapbiomas_forest_transition_long %>% 
  filter(from_level_2 != "Magrove") %>% 
  mutate(year = as.numeric(substr(ayear,6,11)), 
         cover_class = if_else(from_level_2 == "Savanna Formation", "savanna", 
                               "forest")) %>% 
  ggplot(aes(x=year, y=area_km2)) + 
  geom_col(aes(fill = cover_class), colour="blue") + 
  geom_line(data = df_bla_gdp, 
            aes(x=ayear, y=gdp_per_capita_usd*axis_trans_mapbiomas), size=2, color="black") + 
  scale_y_continuous(#for the second y-axis
    sec.axis = sec_axis(~./axis_trans_mapbiomas,#divided by transformation rate, in order to be represented based on the first y-axis
                        name = "GDP per capita (US$)")) + 
  scale_x_continuous(limits = c(1999.5, 2020.5))  + 
  scale_fill_manual("cover transition:", values = mycols_mapbiomas, 
                    labels = c("savanna", "forest")) + 
  geom_label(data = df_mapbiomas_labels, 
             aes(x= year, y = yaxis_value, label = label_values), 
             colour = c("blue", "black")) + 
  labs(y = bquote('cover transition'~(km^2)))  +
  theme(legend.position = "top", legend.box = "horizontal") -> fig_covertransition_gdp
fig_covertransition_gdp

png(file = "figures//fig_covertransition_gdp.png", bg = "white", type = c("cairo"), 
    width=5000, height=3000, res = 600)
fig_covertransition_gdp + theme(text = element_text(size = 16))
dev.off()

#mapbiomas transition to naural grasslands (e.g. includes fire impacts)
unique(dfmapbiomas_transition$to_level_2)
dfmapbiomas_transition %>% 
  filter(to_level_0 =="Anthropic") %>% pull(to_level_2) %>% unique() -> anthropic_cover
all_loss <- c(anthropic_cover, 
              "Grassland", "Non Forest Natural Formation", "Other Non Forest Natural Formation")

dfmapbiomas_transition %>% 
  filter(from_level_2 %in% c("Forest Formation", "Savanna Formation"), 
         to_level_2 %in% all_loss) %>% 
  select(state, city, from_level_1, from_level_2, 
         to_level_0, to_level_2, cols_transition) %>%
  select(from_level_2, cols_transition) %>% 
  pivot_longer(!from_level_2, names_to = "ayear", names_prefix ="..", 
               values_to = "total_forestcover_loss") %>% 
  group_by(from_level_2, ayear) %>% 
  summarise(area_km2 = sum(total_forestcover_loss, na.rm=TRUE)/100) %>% 
  mutate(year = as.numeric(substr(ayear,6,11)), 
         cover_class = if_else(from_level_2 == "Savanna Formation", 
                               "savanna", "forest")) -> dfmapbiomas_forest_transitionall_long

# 3 year median
dfmapbiomas_forest_transitionall_long %>% 
  group_by(year) %>% 
  arrange(year) %>% 
  summarise(area_tot_km2 = sum(area_km2, na.rm = TRUE)) %>% 
  mutate(area_lag1 = lag(area_tot_km2, n = 1L), 
         area_lag2 = lag(area_tot_km2, n = 2L)) %>% 
  pivot_longer(!year, names_to = "lag_year", values_to = "area_km2") %>% 
  group_by(year) %>% 
  summarise(area_km2_3y = median(area_km2)) -> dfmapbiomas_forestlossall_3yr

dfmapbiomas_forest_transitionall_long %>% 
  group_by(ayear) %>%
  summarise(area = sum(area_km2, na.rm=TRUE)) %>% 
  pull(area) %>% max() -> mapbiomasloss_all_max #[1] 47198.18

axis_trans_mapbiomasall <- mapbiomasloss_all_max / gdp_per_capita_usd_max
df_mapbiomasall_labels <- data.frame(year = c(2004, 2019), 
                                  yaxis_value = mapbiomasloss_all_max +(mapbiomasloss_all_max*0.05),
                                  label_values = c(round(mapbiomasloss_all_max,0), 
                                                   round(gdp_per_capita_usd_max,0)))
df_mapbiomasall_labels
mycols_mapbiomas <- c("savanna" = "deeppink4", 
                      "forest" = "deeppink") 

#Combined figure
dfinpe_forestloss %>% 
  filter(year >1999) %>%
  ggplot(aes(x=year, y=area_km2)) + 
  geom_col(aes(fill = biome), colour="blue") + 
  geom_line(data = dfinpe_forestloss_3yr, aes(x=year, y = area_km2_3y), 
            colour ="grey60", size = 1.5) +
  geom_line(data = dfinpe_forestloss_3yr, aes(x=year, y = area_km2_3y), 
            colour ="yellow", linetype = "dashed", size = 1.1) +
  geom_line(data = df_bla_gdp, 
            aes(x=ayear, y=gdp_per_capita_usd*axis_trans), size=2, color="black") + 
  scale_y_continuous(limits = c(0, 50000), 
                     #for the second y-axis
    sec.axis = sec_axis(~./axis_trans,#divided by transformation rate, in order to be represented based on the first y-axis
                        name = "GDP per capita (US$)")) + 
  scale_x_continuous(limits = c(1999.5, 2020.5))  + 
  scale_fill_manual("deforestation:", values = mycols) + 
  geom_label(data = df_forestloss_labels, 
             aes(x= year, y = yaxis_value, label = label_values), 
             colour = c("blue", "black")) + 
  labs(title = "(A)", 
       y = bquote('deforestation'~(km^2)))  +
  theme(text = element_text(size = 16), 
        plot.title.position = "plot",
        legend.position = "top", legend.box = "horizontal") -> figa_deforestation_gdp

dfhansen_long %>%
  ggplot(aes(x=year, y=area_km2)) + 
  geom_col(aes(fill = forest_loss), colour="blue") + 
  geom_line(data = dfhansen_forestloss_3yr, aes(x=year, y = area_km2_3y), 
            colour ="grey60", size = 1.5) +
  geom_line(data = dfhansen_forestloss_3yr, aes(x=year, y = area_km2_3y), 
            colour ="yellow", linetype = "dashed", size = 1.1) +
  geom_line(data = df_bla_gdp, 
            aes(x=ayear, y=gdp_per_capita_usd*axis_trans_hansen), size=2, color="black") + 
  scale_y_continuous(limits = c(0, 50000), 
                     #for the second y-axis
    sec.axis = sec_axis(~./axis_trans_hansen,#divided by transformation rate, in order to be represented based on the first y-axis
                        name = "GDP per capita (US$)")) + 
  scale_x_continuous(limits = c(1999.5, 2020.5))  + 
  scale_fill_manual("forest loss:", values = mycols_hansen, 
                    labels = c("primary forest", "tree cover")) + 
  geom_label(data = df_hansen_labels, 
             aes(x= year, y = yaxis_value, label = label_values), 
             colour = c("blue", "black")) + 
  labs(title = "(B)", 
       y = bquote('forest cover loss'~(km^2)))  +
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position = "top", legend.box = "horizontal") -> figb_forestloss_gd

dfmapbiomas_forest_transition_long %>% 
  filter(from_level_2 != "Magrove") %>% 
  mutate(year = as.numeric(substr(ayear,6,11)), 
         cover_class = if_else(from_level_2 == "Savanna Formation", "savanna", 
                               "forest")) %>% 
  ggplot(aes(x=year, y=area_km2)) + 
  geom_col(aes(fill = cover_class), colour="blue") + 
  geom_line(data = dfmapbiomas_forestloss_3yr, aes(x=year, y = area_km2_3y), 
            colour ="grey60", size = 1.5) +
  geom_line(data = dfmapbiomas_forestloss_3yr, aes(x=year, y = area_km2_3y), 
            colour ="yellow", linetype = "dashed", size = 1.1) +
  geom_line(data = df_bla_gdp, 
            aes(x=ayear, y=gdp_per_capita_usd*axis_trans_mapbiomas), size=2, color="black") + 
  scale_y_continuous(limits = c(0, 50000), 
                     #for the second y-axis
    sec.axis = sec_axis(~./axis_trans_mapbiomas,#divided by transformation rate, in order to be represented based on the first y-axis
                        name = "GDP per capita (US$)")) + 
  scale_x_continuous(limits = c(1999.5, 2020.5))  + 
  scale_fill_manual("cover transition:", values = mycols_mapbiomas, 
                    labels = c("savanna\nto anthropic", "forest\nto anthropic")) + 
  geom_label(data = df_mapbiomas_labels, 
             aes(x= year, y = yaxis_value, label = label_values), 
             colour = c("blue", "black")) + 
  labs(title = "(C)", 
       y = bquote('cover transition'~(km^2)))  +
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position = "top", legend.box = "horizontal") -> figc_covertransition_gdp

dfmapbiomas_forest_transitionall_long %>% 
  ggplot(aes(x=year, y=area_km2)) + 
  geom_col(aes(fill = cover_class), colour="blue") + 
  geom_line(data = dfmapbiomas_forestlossall_3yr, aes(x=year, y = area_km2_3y), 
            colour ="grey60", size = 1.5) +
  geom_line(data = dfmapbiomas_forestlossall_3yr, aes(x=year, y = area_km2_3y), 
            colour ="yellow", linetype = "dashed", size = 1.1) +
  geom_line(data = df_bla_gdp, 
            aes(x=ayear, y=gdp_per_capita_usd*axis_trans_mapbiomas), size=2, color="black") + 
  scale_y_continuous(limits = c(0, 50000), 
                     #for the second y-axis
                     sec.axis = sec_axis(~./axis_trans_mapbiomasall,#divided by transformation rate, in order to be represented based on the first y-axis
                                         name = "GDP per capita (US$)")) + 
  scale_x_continuous(limits = c(1999.5, 2020.5))  + 
  scale_fill_manual("cover transition:", values = mycols_mapbiomas, 
                    labels = c("savanna\nto other", "forest\nto other")) + 
  geom_label(data = df_mapbiomasall_labels, 
             aes(x= year, y = yaxis_value, label = label_values), 
             colour = c("blue", "black")) + 
  labs(title = "(D)", 
       y = bquote('cover transition'~(km^2)))  +
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position = "top", legend.box = "horizontal") -> figd_covertransitionall_gdp

png(file = "figures//fig_compositecoverchange_gdp.png", bg = "white", type = c("cairo"), 
    width=5000, height=8000, res = 600)
gridExtra::grid.arrange(figa_deforestation_gdp, 
                        figb_forestloss_gd, 
                        figc_covertransition_gdp, ncol = 1)
dev.off()

#correlation between measures
df_bla_gdp %>% mutate(year = ayear) %>% 
  select(year, tot_pop, tot_gdp, gdp_per_capita_reais, gdp_per_capita_usd) %>% 
  left_join(
dfhansen_long %>% 
  group_by(year) %>% summarise(area_km2_hansen = sum(area_km2, na.rm = TRUE)) %>% 
  left_join(
dfinpe_forestloss %>% 
  group_by(year) %>% summarise(area_km2_inpe = sum(area_km2, na.rm = TRUE)) 
) %>% left_join(
dfmapbiomas_forest_transition_long %>% 
  filter(from_level_2 != "Magrove") %>% 
  mutate(year = as.numeric(substr(ayear,6,11))) %>% 
  group_by(year) %>% summarise(area_km2_mapbiomas = sum(area_km2, na.rm = TRUE))
)
) -> df_bla_summary

psych::pairs.panels(df_bla_summary[, c("gdp_per_capita_usd", "area_km2_hansen", 
                                       "area_km2_inpe", "area_km2_mapbiomas")], 
                    method = "spearman")
