#GDP education municipality 2000 - 2010
#packages
library(tidyverse)
library(readxl)
library(gridExtra)
library(psych)
library(tidymodels)
library(MuMIn) # AICc function
library(sf)
library(viridisLite)
library(viridis)
library(psych)
library(cowplot)
library(biscale) #for bivariate map colours
library(scales)

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

# Municipality names, codes, polygons and areas from IBGE. Updated August 2020. Accessed 8 January 2022
# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html?=&t=downloads
ibge_muni <- "vector\\ninestate_muni.shp"
sf_ninestate_muni <- st_read(ibge_muni) %>% filter(SIGLA_UF %in% bla_state_siglas)
#States
ibge_states <- "vector\\ninestate_poly.shp"
sf_ninestate <- st_read(ibge_states) %>% filter(SIGLA_UF %in% bla_state_siglas)
sf_ninestate %>% ggplot() + geom_sf(aes(fill = SIGLA_UF))

#GDP etc 
#GDP per capita and GVA by agriculture per capita 2002 - 2019 (from ibge_sidrar_tidy)
df_gdppop_muni_02a19 <- read_excel("data//bla_municipality_gdppop_02a19.xlsx", 
                                   na = c("", "NA"),
                                   .name_repair = "universal")

# compound annual growth rates per municipality 2000 - 2010
#First 3 years there are 805, last 808
data.frame(sf_ninestate_muni) %>% select(-geometry) %>% 
  mutate(codmun7 = as.numeric(CD_MUN)) %>% 
  right_join(
    (df_gdppop_muni_02a19 %>% 
       filter(year %in% c(2002, 2003, 2004)) %>% 
       group_by(uf_sigla, uf, codmun7) %>% 
       summarise(gdp_start = median(gdp_percapita_reais, na.rm=TRUE), 
                 gva_agri_start = median(gva_agri_percapita_reais, na.rm=TRUE)) %>% 
       ungroup() %>%
       left_join(df_gdppop_muni_02a19 %>% 
                   filter(year %in% c(2009, 2010, 2011)) %>% 
                   group_by(uf_sigla, uf, codmun7) %>% 
                   summarise(gdp_end = median(gdp_percapita_reais, na.rm=TRUE), 
                             gva_agri_end = median(gva_agri_percapita_reais, na.rm=TRUE))) %>% 
       ungroup() %>%
       mutate(gdp_divide = gdp_end / gdp_start, 
              gva_agri_divide = gva_agri_end / gva_agri_start) %>% 
       mutate(gdp_power = gdp_divide^(1/10), 
              gva_agri_power = gva_agri_divide^(1/10)) %>% 
       mutate(cagr_gdp_percapita = gdp_power -1, 
              cagr_gva_agri_percapita = gva_agri_power -1) %>% 
       select(uf_sigla, uf, codmun7, gdp_start, gdp_end, gdp_divide, gdp_power, cagr_gdp_percapita, 
              gva_agri_start, gva_agri_end, gva_agri_divide, gva_agri_power, cagr_gva_agri_percapita)
    ), by = c("codmun7" = "codmun7", "SIGLA_UF" = "uf_sigla") 
  ) -> df_muni_cagr

#census 1991, 2000, 2010 from Atlas do Brasil
df_census <- read_excel("data//Atlas 2013_municipal, estadual e Brasil.xlsx", 
                                   sheet="MUN 91-00-10",
                                   na = c("", "NA"),
                                   .name_repair = "universal") %>% 
  filter(UF %in% all_of(as.numeric(sf_ninestate$CD_UF)))

#Association with GDP per capita and HDI 
df_census %>% filter(ANO==2010) %>% 
  select(Codmun7,IDHM_R) %>% 
  left_join(df_muni_cagr %>% select(uf, codmun7, gdp_end), 
            by=c("Codmun7"="codmun7")) %>%
  filter(!is.na(uf)) %>%
  group_by(uf) %>%
  nest() %>% 
  mutate(
   # fit = map(data, ~ lm(IDHM_R ~ gdp_end, data = .x)),
    fit = map(data, ~ mgcv::gam(IDHM_R ~ s(gdp_end), data = .x)),
    #models = lapply(data, function(df) lm(IDHM_R ~ gdp_end, data = df)),
    glanced = map(fit, glance), 
    R.square = map_dbl(fit, ~ summary(.)$r.sq)
  ) %>%
  unnest(glanced) %>% select(uf, R.square) %>% 
  left_join( df_census %>% filter(ANO==2010) %>% 
               select(Codmun7, IDHM, IDHM_E, IDHM_L, IDHM_R) %>% 
               left_join(df_muni_cagr %>% select(uf, codmun7, gdp_end), 
                         by=c("Codmun7"="codmun7")) %>% 
               filter(!is.na(uf)) %>% 
               group_by(uf) %>% 
               summarise(gdp_end = min(gdp_end), 
                         IDHM_R = 0.2) 
             )%>% 
  mutate(mylabel = paste("r2:", round(R.square,2))) -> df_gdp_hdi_labels

#can also use 
#unnest() %>%
#select(-data, -fit)

df_census %>% filter(ANO==2010) %>% 
  select(Codmun7, IDHM, IDHM_E, IDHM_L, IDHM_R) %>% 
  left_join(df_muni_cagr %>% select(uf, codmun7, gdp_end), 
            by=c("Codmun7"="codmun7")) %>% 
  filter(!is.na(uf)) %>%
  ggplot(aes(x = gdp_end, y = IDHM_R)) + 
  geom_point(aes(colour=uf)) + 
  stat_smooth(method="gam", aes(colour=uf)) + 
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +
  geom_label(data = df_gdp_hdi_labels, aes(label = mylabel), 
             hjust = "left",
             nudge_x = 100) + 
  facet_wrap(~uf, scales = "free_x") + 
  labs(x = "GDP per capita 2010 (thousands of reais)", 
       y = "HDI income") + 
  theme(legend.position="none") -> fig_gdp_hdi
png(file = "figures//fig_gdp_hdi.png", bg = "white", type = c("cairo"), 
    width=4000, height=3000, res = 600)
fig_gdp_hdi
dev.off()  

# compound annual growth rate of education
data.frame(sf_ninestate_muni) %>% select(-geometry) %>% 
  mutate(codmun7 = as.numeric(CD_MUN)) %>% 
  right_join(
     df_census %>% 
       rename(codmun7 = Codmun7) %>%
       filter(ANO %in% c(2000)) %>% 
       group_by(UF, codmun7) %>% 
       summarise(edu_start = min(IDHM_E, na.rm=TRUE))
       ) %>% 
       ungroup() %>%
       left_join(
         df_census %>% 
           rename(codmun7 = Codmun7) %>%
           filter(ANO %in% c(2010)) %>% 
           group_by(UF, codmun7) %>% 
           summarise(edu_end = min(IDHM_E, na.rm=TRUE))
       ) %>% 
       ungroup() %>%
       mutate(edu_divide = edu_end / edu_start) %>% 
       mutate(edu_power = edu_divide^(1/10)) %>% 
       mutate(cagr_edu = edu_power -1) %>% 
  mutate(cagr_edu_inverse = cagr_edu * -1) -> df_muni_cagr_edu

df_muni_cagr_edu %>% filter(is.na(cagr_edu))  #0


#Forest loss
# Transition as a proportion of municipality area.
#Mapbiomas
dfmapbiomas_transition_muni <- read_excel("data//Mapbiomas-Brazil-transition.xlsx", 
                                          sheet = "Sheet1", 
                                          .name_repair = "universal") %>% 
  filter(state %in% bla_state_names)

cols_transition <- c(paste("..",1999:2019,".", 2000:2020, sep=""))
dfmapbiomas_transition_muni %>% 
  filter(from_level_2 %in% c("Forest Formation", "Savanna Formation"), 
         to_level_0 =="Anthropic") %>% 
  select(state, city, from_level_1, from_level_2, 
         to_level_0, to_level_2, all_of(cols_transition)) -> dfmapbiomas_forest_transition_muni
#long format with annual totals
dfmapbiomas_forest_transition_muni %>% 
  select(state, city, from_level_2, all_of(cols_transition)) %>% 
  pivot_longer(cols = starts_with(".."), names_to = "ayear", names_prefix ="..", 
               values_to = "total_forestcover_loss") %>% 
  group_by(state, city, from_level_2, ayear) %>% 
  summarise(area_km2 = sum(total_forestcover_loss, na.rm=TRUE)/100) %>%
  mutate(year = as.numeric(substr(ayear,6,11)), 
         cover_class = if_else(from_level_2 == "Savanna Formation", 
                               "savanna", "forest")) -> dfmapbiomas_forest_transition_muni_long

dfmapbiomas_forest_transition_muni_long %>% 
  filter(year %in% c(2000:2010))  %>% 
  group_by(state, city, cover_class) %>% 
  summarise(area_km2 = sum(area_km2, na.rm = TRUE)) %>%
  pivot_wider(id_cols = c(state, city), 
              names_from = cover_class, values_from = area_km2) %>% 
  mutate(tot_transition_km2 = (replace_na(forest,0) + replace_na(savanna,0))) %>%
  #join with gdp data
  left_join(df_muni_cagr, 
            by = c("state" = "uf",  "city"="NM_MUN")) %>% 
  filter(!is.na(gdp_start)) -> df_muni_cagr_mapbiomas #769

#Add useful flags 
df_muni_cagr_mapbiomas %>% 
  left_join(data.frame(sf_ninestate_muni) %>% select(-geometry) %>% 
              mutate(codmun7 = as.numeric(CD_MUN))) %>% 
  left_join(df_muni_cagr_edu %>% select(codmun7, cagr_edu, cagr_edu_inverse)
    ) %>%
  left_join(df_gdppop_muni_02a19 %>% 
              filter(year==2019) %>% 
              mutate(gdp_percapita_usd = gdp_percapita_reais / 3.946, 
                     flag_metropolitan = if_else(is.na(Região.Metropolitana), 
                                                 "other", "metropolitan"), 
                     flag_urban = if_else(is.na(Nome.Concentração.Urbana), "rural", "urban"), 
                     flag_capital = if_else(codmun7 %in% bla_state_capitals$codmun7, 
                                            "yes", "no"))) %>% 
  mutate(tot_transition_percent = (tot_transition_km2 / AREA_KM2)*100) -> df_figgrowth_2000_2010

write.csv(df_figgrowth_2000_2010, "data//df_figgrowth_2000_2010.csv", row.names = FALSE)
#Bivariate maps
#Makes big files
# classes
dfbi_class <- bi_class(df_figgrowth_2000_2010, 
                       x = tot_transition_percent, y = cagr_gdp_percapita, 
                       style = "quantile", dim = 3)
dfbi_class_gva_agri <- bi_class(df_figgrowth_2000_2010, 
                                x = tot_transition_percent, 
                                y = cagr_gva_agri_percapita, 
                                style = "quantile", dim = 3) 
dfbi_class_gdp_edu <- bi_class(df_figgrowth_2000_2010, 
                       x = cagr_edu_inverse, y = cagr_gdp_percapita, 
                       style = "quantile", dim = 3)
dfbi_class_gva_agri_edu <- bi_class(df_figgrowth_2000_2010, 
                                x = cagr_edu_inverse, 
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

sf_ninestate_muni %>% left_join(dfbi_class_gdp_edu) %>% 
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
        plot.caption = element_text(hjust = 0)) -> bimap_gdp_edu

sf_ninestate_muni %>% left_join(dfbi_class_gva_agri_edu) %>% 
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
        plot.caption = element_text(hjust = 0)) -> bimap_gva_edu

#Follow code example from help
legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "forest loss",
                    ylab = "GDP gain",
                    size = 20)
legend_gva <- bi_legend(pal = "DkBlue",
                        dim = 3,
                        xlab = "forest loss",
                        ylab = "GVA gain",
                        size = 20)
legend_gdp_edu <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "less education",
                    ylab = "GDP gain",
                    size = 20)
legend_gva_edu <- bi_legend(pal = "DkBlue",
                        dim = 3,
                        xlab = "less education",
                        ylab = "GVA gain",
                        size = 20)

#?draw_plot
figbi_gdp <- cowplot::ggdraw() +
  cowplot::draw_plot(bimap_gdp, 0, 0, 1, 1) +
  cowplot::draw_plot(legend, 0.7, .71, 0.35, 0.25) + 
  draw_text("(A)", x = 0.03, y = 0.95, size = 30)
figbi_gva <- cowplot::ggdraw() +
  cowplot::draw_plot(bimap_gva, 0, 0, 1, 1) +
  cowplot::draw_plot(legend_gva, 0.7, .71, 0.35, 0.25) + 
  draw_text("(B)", x = 0.03, y = 0.95, size = 30)
figbi_gdp_edu <- cowplot::ggdraw() +
  cowplot::draw_plot(bimap_gdp_edu, 0, 0, 1, 1) +
  cowplot::draw_plot(legend_gdp_edu, 0.7, .71, 0.35, 0.25) + 
  draw_text("(B)", x = 0.03, y = 0.95, size = 30)
figbi_gva_edu <- cowplot::ggdraw() +
  cowplot::draw_plot(bimap_gva_edu, 0, 0, 1, 1) +
  cowplot::draw_plot(legend_gva_edu, 0.7, .71, 0.35, 0.25) + 
  draw_text("(D)", x = 0.03, y = 0.95, size = 30)

png(file = "figures//fig_bimap_gdp_2000_2010.png", bg = "white", type = c("cairo"), 
    width=8000, height=6000, res = 600)
figbi_gdp
dev.off()

png(file = "figures//fig_bimap_gva_2000_2010.png", bg = "white", type = c("cairo"), 
    width=8000, height=6000, res = 600)
figbi_gva
dev.off()

png(file = "figures//fig_bimap_gdp_edu_2000_2010.png", bg = "white", type = c("cairo"), 
    width=8000, height=6000, res = 600)
figbi_gdp_edu
dev.off()
png(file = "figures//fig_bimap_gva_edu_2000_2010.png", bg = "white", type = c("cairo"), 
    width=8000, height=6000, res = 600)
figbi_gva_edu
dev.off()

#remove large objects
rm("bimap_gdp")
rm("bimap_gva")
rm("bimap_gdp_edu")
rm("bimap_gva_edu")
rm("figbi_gdp_edu")
rm("figbi_gva_edu")

save.image("~/Articles/2022_Norris_gdp_deforestation/AmazonConservation/gdp_humancapital.RData")
