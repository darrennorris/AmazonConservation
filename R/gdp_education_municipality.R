#GDP education municipality 2000 - 2010
#packages
library(tidyverse)
library(readxl)
library(stringi)
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
) %>% mutate(muni_upper = toupper(name_muni)) %>% 
  mutate(muni_inep = stri_trans_general(muni_upper, "Latin-ASCII"))

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

#PNAD 2016 - 2019 from ibge_sidrar_download
df_pnad_muni <- read_excel("data//edu_completo_25anos.xlsx", 
                        sheet="edu_complete_25y_municipality",
                        na = c("", "NA"),
                        .name_repair = "universal") %>% 
  filter(Município..Código. %in% all_of(as.numeric(sf_ninestate_muni$CD_MUN)))

df_pnad_state <- read_excel("data//edu_completo_25anos.xlsx", 
                           sheet="edu_complete_25y_state",
                           na = c("", "NA"),
                           .name_repair = "universal") %>% 
  filter(Unidade.da.Federação..Código. %in% all_of(as.numeric(sf_ninestate$CD_UF)))

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

# Municipality. Compound annual growth rate of education 2000 - 2010
data.frame(sf_ninestate_muni) %>% select(-geometry) %>% 
  mutate(codmun7 = as.numeric(CD_MUN)) %>% 
  right_join(
     df_census %>% 
       rename(codmun7 = Codmun7) %>%
       filter(ANO %in% c(2000)) %>% 
       group_by(UF, codmun7) %>% 
       summarise(edu_start = min(IDHM_E, na.rm=TRUE), 
                 edu_uni_start = min(T_SUPER25M, na.rm=TRUE))
       ) %>% 
       ungroup() %>%
       left_join(
         df_census %>% 
           rename(codmun7 = Codmun7) %>%
           filter(ANO %in% c(2010)) %>% 
           group_by(UF, codmun7) %>% 
           summarise(edu_end = min(IDHM_E, na.rm=TRUE), 
                     edu_uni_end = min(T_SUPER25M, na.rm=TRUE))
       ) %>% 
       ungroup() %>%
       mutate(edu_divide = edu_end / edu_start, 
              edu_uni_divide = if_else(edu_uni_start ==0, edu_uni_end, 
                                       (edu_uni_end / edu_uni_start))) %>% 
       mutate(edu_power = edu_divide^(1/10), 
              edu_uni_power = edu_uni_divide^(1/10)) %>% 
       mutate(cagr_edu = edu_power -1, 
              cagr_edu_uni = edu_uni_power -1) %>% 
  mutate(cagr_edu_inverse = cagr_edu * -1, 
         cagr_edu_uni_inverse = cagr_edu_uni * -1) -> df_muni_cagr_edu

df_muni_cagr_edu %>% filter(is.na(cagr_edu), is.na(cagr_edu_uni))  #0

#correlation
df_muni_cagr_edu %>% 
  ggplot(aes(x=cagr_edu_uni, y=cagr_edu)) + 
  geom_point() + stat_smooth(method="lm")
cor.test(df_muni_cagr_edu$cagr_edu, df_muni_cagr_edu$cagr_edu_uni) #0.24

# Municipality, n = 9. Compound annual growth rate of education 2000 - 2019
data.frame(sf_ninestate_muni) %>% select(-geometry) %>% 
  mutate(codmun7 = as.numeric(CD_MUN)) %>% 
  right_join(
    df_census %>% 
      rename(codmun7 = Codmun7) %>%
      filter(ANO %in% c(2000)) %>% 
      group_by(UF, codmun7) %>% 
      summarise(edu_uni_start = min(T_SUPER25M, na.rm=TRUE))
  ) %>% 
  ungroup() %>%
  right_join(
    df_pnad_muni %>% 
      rename(codmun7 = Município..Código.) %>%
      filter(Sexo == "Total", Nível.de.instrução =="Superior completo", 
             Unidade.de.Medida != "Mil pessoas") %>% 
      group_by(codmun7) %>% 
      summarise(edu_uni_end = median(Valor, na.rm=TRUE))
  ) %>% 
  ungroup() %>%
  mutate(edu_uni_divide = if_else(edu_uni_start ==0, edu_uni_end, 
                                  (edu_uni_end / edu_uni_start))) %>% 
  mutate(edu_uni_power = edu_uni_divide^(1/20)) %>% 
  mutate(cagr_edu_uni = edu_uni_power -1) %>% 
  mutate(cagr_edu_uni_inverse = cagr_edu_uni * -1) -> df_muni_cagr_edu_uni_2000_2019


# Forest loss
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
  left_join(df_muni_cagr_edu %>% select(codmun7, cagr_edu, cagr_edu_inverse, 
                                        cagr_edu_uni, cagr_edu_uni_inverse)
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
save.image("~/Articles/2022_Norris_gdp_deforestation/AmazonConservation/gdp_humancapital.RData")

#
df_muni_cagr_edu_uni_2000_2019 %>% 
  left_join(df_muni_cagr) %>%
  ggplot(aes(x=cagr_edu_uni, y= cagr_gdp_percapita )) + 
  geom_point() + stat_smooth(method="lm") 
df_muni_cagr_edu_uni_2000_2019 %>% 
  left_join(df_muni_cagr) %>%
  ggplot(aes(x=cagr_edu_uni, y= cagr_gva_agri_percapita )) + 
  geom_point() + stat_smooth(method="lm")


df_gdppop_muni_02a19 %>% 
  mutate(flag_capital = if_else(codmun7 %in% bla_state_capitals$codmun7, 
                                "state capital", "other")) %>%
  ggplot(aes(x=gva_agri_percapita_reais/3.946, y=gdp_percapita_reais/3.946)) + 
  geom_point(aes(colour=factor(year))) + 
  stat_smooth(aes(colour=factor(year)), method="lm", se=FALSE) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +
  scale_x_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +
  scale_color_viridis_d() + 
  facet_wrap(~flag_capital) +
  labs(title= "(A)", 
       x = "GVA agriculture per capita (US$)", 
       y = "GDP per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position="bottom", 
        legend.title = element_blank()) + 
  guides(col = guide_legend(nrow = 2))-> fig_eco_agri_2002_2019

png(file = "figures//fig_eco_agri_2002_2019.png", bg = "white", type = c("cairo"), 
    width=6000, height=3500, res = 600)
fig_eco_agri_2002_2019
dev.off()  

df_gdppop_muni_02a19 %>% 
  left_join(dfmapbiomas_forest_transition_muni_long %>% 
              group_by(year, state, city) %>% 
              summarise(area_km2 = sum(area_km2, na.rm=TRUE)), 
            by = c("year"="year", "uf"="state", "name_muni"="city")) %>% 
  left_join(data.frame(sf_ninestate_muni) %>% select(-geometry) %>% 
              mutate(codmun7 = as.numeric(CD_MUN))) %>% 
  mutate(tot_transition_percent = (area_km2 / AREA_KM2)*100, 
         flag_capital = if_else(codmun7 %in% bla_state_capitals$codmun7, 
                                       "state capital", "other")) %>% 
  ggplot(aes(x=tot_transition_percent, y=gdp_percapita_reais/3.946)) + 
  geom_point(aes(colour=factor(year))) + 
  stat_smooth(aes(colour=factor(year)), method="lm", se=FALSE) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +
  scale_color_viridis_d() +
  facet_wrap(~flag_capital) +
  labs(title= "(B)", 
       x = "Forest transition (%)", 
       y = "GDP per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position="bottom", 
        legend.title = element_blank()) + 
  guides(col = guide_legend(nrow = 2)) -> fig_eco_forest_2002_2019

png(file = "figures//fig_eco_forest_2002_2019.png", bg = "white", type = c("cairo"), 
    width=6000, height=3500, res = 600)
fig_eco_forest_2002_2019
dev.off()   

#School pass rates
df_inep_school <- read_excel("data\\inep_school_passrates.xlsx", 
                             .name_repair = "universal")
df_inep_school %>% 
  filter(ANO > 1999, ANO < 2020, dep=="Estadual") %>%
  group_by(ANO, SIGLA, muni_inep) %>% 
  summarise(total_schools = length(unique(school_idcode)), 
            pass_rate_per = median(pass_rate_percent, na.rm = TRUE),
            pass_rate_sd = sd(pass_rate_percent, na.rm = TRUE)
  ) -> df_muni_school


df_gdppop_muni_02a19 %>% 
  mutate(muni_upper = toupper(name_muni)) %>% 
  mutate(muni_inep = stri_trans_general(muni_upper, "Latin-ASCII"), 
         SIGLA = uf_sigla) %>% left_join(df_muni_school %>% 
                                       rename(year = ANO), 
                                       by = c("year" = "year", "SIGLA" = "SIGLA", 
                                              "muni_inep" = "muni_inep")) %>% 
  mutate(flag_capital = if_else(muni_inep %in% bla_state_capitals$muni_inep, 
                                "state capital", "other"), 
         school_percapita = total_schools / tot_pop) %>% 
  filter(!is.na(school_percapita)) %>%
  ggplot(aes(x=pass_rate_per, y=gdp_percapita_reais/3.946)) + 
  geom_point(aes(colour=factor(year))) + 
  stat_smooth(aes(colour=factor(year)), method="lm", se=FALSE) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3, 
                                                  accuracy = 1.0)) +
  scale_color_viridis_d() +
  facet_wrap(~flag_capital) +
  labs(title= "(C)", 
       x = "school pass rate (%)", 
       y = "GDP per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position="bottom", 
        legend.title = element_blank()) + 
  guides(col = guide_legend(nrow = 2)) -> fig_eco_school_2002_2019

png(file = "figures//fig_eco_school_2002_2019.png", bg = "white", type = c("cairo"), 
    width=6000, height=3500, res = 600)
fig_eco_school_2002_2019
dev.off()

df_gdppop_muni_02a19 %>% 
  left_join(df_census %>% 
              filter(ANO %in% c(2000, 2010)) %>% 
              rename(codmun7 = Codmun7, 
                     year = ANO) %>% 
              mutate(year = if_else(year==2000, 2002, 2010)) %>%
              select(year, codmun7, T_SUPER25M) %>%
              bind_rows(df_pnad_muni %>% 
                          rename(codmun7 = Município..Código., 
                                 T_SUPER25M = Valor, 
                                 year = Ano) %>%
                          filter(Sexo == "Total", Nível.de.instrução =="Superior completo", 
                                 Unidade.de.Medida != "Mil pessoas") %>%
                          select(year, codmun7, T_SUPER25M) 
              ), 
            by = c("year"="year", "codmun7"="codmun7")) %>% 
  left_join(data.frame(sf_ninestate_muni) %>% select(-geometry) %>% 
              mutate(codmun7 = as.numeric(CD_MUN))) %>% 
  filter(!is.na(T_SUPER25M)) %>% 
  mutate(flag_capital = if_else(codmun7 %in% bla_state_capitals$codmun7, 
                         "state capital", "other")) %>%
  ggplot(aes(x=T_SUPER25M, y=gdp_percapita_reais/3.946)) + 
  geom_point(aes(colour=factor(year))) + 
  stat_smooth(aes(colour=factor(year)), method="lm", se=FALSE) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3, 
                                                  accuracy = 1.0)) +
  scale_color_viridis_d() +
  facet_wrap(~flag_capital) +
  labs(title= "(C)", 
       x = "university qualification (%)", 
       y = "GDP per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position="bottom", 
        legend.title = element_blank()) + 
  guides(col = guide_legend(nrow = 2)) -> fig_eco_edu_2002_2019

png(file = "figures//fig_eco_edu_2002_2019.png", bg = "white", type = c("cairo"), 
    width=6000, height=3500, res = 600)
fig_eco_edu_2002_2019
dev.off()

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
dfbi_class_gdp_edu_uni <- bi_class(df_figgrowth_2000_2010, 
                               x = cagr_edu_uni_inverse, y = cagr_gdp_percapita, 
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

sf_ninestate_muni %>% left_join(dfbi_class_gdp_edu_uni) %>% 
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
        plot.caption = element_text(hjust = 0)) -> bimap_gdp_edu_uni

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
legend_gdp_edu_uni <- bi_legend(pal = "DkBlue",
                            dim = 3,
                            xlab = "less qualification",
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
figbi_gdp_edu_uni <- cowplot::ggdraw() +
  cowplot::draw_plot(bimap_gdp_edu_uni, 0, 0, 1, 1) +
  cowplot::draw_plot(legend_gdp_edu_uni, 0.7, .71, 0.35, 0.25) + 
  draw_text("(C)", x = 0.03, y = 0.95, size = 30)
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

png(file = "figures//fig_bimap_gdp_edu_uni_2000_2010.png", bg = "white", type = c("cairo"), 
    width=8000, height=6000, res = 600)
figbi_gdp_edu_uni
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
