#Forest loss municipality

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
ibge_muni <- "C:\\Users\\user\\Documents\\Articles\\2022_Norris_gdp_deforestation\\analysis\\br_municipios_20200807\\BR_Municipios_2019.shp"
sf_ninestate_muni <- st_read(ibge_muni) %>% filter(SIGLA_UF %in% bla_state_siglas)
#Export
#st_write(sf_ninestate_muni, "vector\\ninestate_muni.shp")

#States
ibge_states <- "C:\\Users\\user\\Documents\\Articles\\2022_Norris_gdp_deforestation\\analysis\\br_unidades_da_federacao\\BR_UF_2019.shp"
sf_ninestate <- st_read(ibge_states) %>% filter(SIGLA_UF %in% bla_state_siglas)
sf_ninestate %>% ggplot() + geom_sf(aes(fill = SIGLA_UF))
#Export
#st_write(sf_ninestate, "vector\\ninestate_poly.shp")

#GDP per capita and GVA by agriculture per capita 2002 - 2019 (from ibge_sidrar_tidy)
df_gdppop_muni_02a19 <- read_excel("data//bla_municipality_gdppop_02a19.xlsx", 
                                   na = c("", "NA"), .name_repair = "universal")
#Export gva for industry
df_gdppop_muni_02a19 %>% 
  group_by(uf_sigla, uf) %>% 
  summarise(count_muni = length(unique(codmun7))) %>% right_join(
    data.frame(sf_ninestate_muni), by = c("uf_sigla" = "SIGLA_UF")
  ) %>% select(uf_sigla, uf, CD_MUN, NM_MUN, AREA_KM2) %>%
  crossing(year = 2002:2019) %>% left_join(
     df_gdppop_muni_02a19,  
     by = c("uf_sigla" = "uf_sigla", "year" = "year", "NM_MUN" = "name_muni")) %>%
arrange(uf_sigla, NM_MUN, year) %>% 
  write.csv("muni_fixed_gva_industry_long.csv", row.names = FALSE)

#Salary etc
df_salary_muni_06a19 <- read_excel("data//muni_salary.xlsx", 
                                   na = c("", "NA"),
                                   .name_repair = "universal")

df_gdppop_muni_02a19 %>% 
  group_by(uf_sigla, uf) %>% 
  summarise(count_muni = length(unique(codmun7))) %>% right_join(
    data.frame(sf_ninestate_muni), by = c("uf_sigla" = "SIGLA_UF")
  ) %>% select(uf_sigla, uf, CD_MUN, NM_MUN, AREA_KM2) %>%
  crossing(year = as.character(2002:2019)) %>% left_join(
df_salary_muni_06a19 %>% 
  mutate(CD_MUN = as.character(Município..Código.), 
         year = as.character(Ano)) %>%
  filter(CD_MUN %in% sf_ninestate_muni$CD_MUN) %>% 
  mutate(income_metric = case_when(Variável == "Número de unidades locais" ~ "count_local_units", 
                                   Variável == "Número de empresas e outras organizações atuantes" ~"count_active_companies", 
                                   Variável == "Pessoal ocupado total" ~ "employed_total", 
                                   Variável == "Pessoal ocupado assalariado" ~"employed_informal", 
                                   Variável == "Pessoal assalariado médio"~"mean_employed_informal", 
                                   Variável == "Salários e outras remunerações" ~"total_salaries", 
                                   Variável == "Salário médio mensal" ~ "min_salary_mean", 
                                   Variável == "Salário médio mensal em reais" ~"salary_mean_reais",
                                   TRUE ~ NA_character_ ) 
  ) %>% select(CD_MUN, year, income_metric, Valor) %>% 
  pivot_wider(id_cols = c(CD_MUN, year), 
              names_from = income_metric, values_from = Valor)
  ) %>% arrange(uf_sigla, NM_MUN) %>% 
  write.csv("muni_fixed_income_long.csv", row.names = FALSE)

#Interpolate 2007 (to do......)
df_gdppop_muni_02a19 %>% 
  select(year, uf_sigla, uf, codmun7, name_muni, 
         tot_pop, gdp_percapita_reais, gva_agri_percapita_reais) %>%
  arrange(uf_sigla, uf, name_muni, year) %>% 
  mutate(lag_pop = lag(tot_pop), 
         lead_pop = lead(tot_pop), 
         lag_gva = lag(gva_agri_percapita_reais), 
         lead_gva = lead(gva_agri_percapita_reais)) %>% 
  mutate(pop_new = if_else(is.na(tot_pop), round(((lag_pop + lead_pop)/2),0), 
                           tot_pop), 
         gva_new = if_else(is.na(gva_agri_percapita_reais), 
                           round(((lag_gva + lead_gva)/2),2), 
                           gva_agri_percapita_reais)) -> df_gdppop_muni_02a19_nona

#Economic growth over time for each municipality
df_gdppop_muni_02a19 %>% 
ggplot(aes(x=gdp_percapita_reais)) + geom_histogram()

table(df_gdppop_muni_02a19$Região.Metropolitana)

df_gdppop_muni_02a19 %>% 
  filter(year==2019) %>% 
  mutate(gdp_percapita_usd = gdp_percapita_reais / 3.946, 
         flag_metropolitan = if_else(is.na(Região.Metropolitana), 
                                     "other", "metropolitan"), 
         flag_urban = if_else(is.na(Nome.Concentração.Urbana), "rural", "urban")) %>% 
  group_by(flag_urban) %>% summarise(acount = n())

df_gdppop_muni_02a19 %>% 
  filter(year==2019) %>% 
  mutate(gdp_percapita_usd = gdp_percapita_reais / 3.946, 
         flag_metropolitan = if_else(is.na(Região.Metropolitana), 
                                     "other", "metropolitan"), 
         flag_urban = if_else(is.na(Nome.Concentração.Urbana), "rural", "urban"), 
         flag_capital = if_else(codmun7 %in% bla_state_capitals$codmun7, 
                                "yes", "no")) %>%
  ggplot(aes(x =uf_sigla, y=tot_pop)) + 
  geom_point(aes(shape = flag_capital)) +
  geom_boxplot(outlier.shape = NA) + 
  scale_y_continuous("Number of people (millions)", 
                     breaks = c(0, 1000000, 2000000),
                     labels = unit_format(unit = "M", 
                                          scale = 1e-6, 
                                          accuracy = 0.1)) +
  scale_shape_discrete("state capital", labels = c("no", "yes")) +
  facet_wrap(~flag_urban) +
  labs(title = "(A) population in 2019", 
       y="population", 
       x = "state") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position="top") -> fig_pop

df_gdppop_muni_02a19 %>% 
  filter(year==2019) %>% 
  mutate(gdp_percapita_usd = gdp_percapita_reais / 3.946, 
         flag_metropolitan = if_else(is.na(Região.Metropolitana), 
                                     "other", "metropolitan"), 
         flag_urban = if_else(is.na(Nome.Concentração.Urbana), "rural", "urban"), 
         flag_capital = if_else(codmun7 %in% bla_state_capitals$codmun7, 
                                "yes", "no")) %>%
  ggplot(aes(x =uf_sigla, y=gdp_percapita_usd)) +
  geom_point(aes(shape = flag_capital)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_shape_discrete("state capital", labels = c("no", "yes")) +
  facet_wrap(~flag_urban) +
  labs(title = "(B) GDP per capita in 2019", 
       y="GDP per capita (US$)", 
       x = "state") + 
  theme( text = element_text(size = 16), 
         plot.title.position = "plot", 
         legend.position="top") -> fig_gdp

png(file = "figures//fig_rural_urban.png", bg = "white", type = c("cairo"), 
    width=5000, height=4000, res = 600)
gridExtra::grid.arrange(fig_pop, fig_gdp, ncol = 1)
dev.off()

# compound annual growth rates per municipality
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
              filter(year %in% c(2017, 2018, 2019)) %>% 
              group_by(uf_sigla, uf, codmun7) %>% 
              summarise(gdp_end = median(gdp_percapita_reais, na.rm=TRUE), 
                        gva_agri_end = median(gva_agri_percapita_reais, na.rm=TRUE))) %>% 
   ungroup() %>%
  mutate(gdp_divide = gdp_end / gdp_start, 
         gva_agri_divide = gva_agri_end / gva_agri_start) %>% 
  mutate(gdp_power = gdp_divide^(1/18), 
         gva_agri_power = gva_agri_divide^(1/18)) %>% 
  mutate(cagr_gdp_percapita = gdp_power -1, 
         cagr_gva_agri_percapita = gva_agri_power -1) %>% 
  select(uf_sigla, uf, codmun7, gdp_start, gdp_end, gdp_divide, gdp_power, cagr_gdp_percapita, 
         gva_agri_start, gva_agri_end, gva_agri_divide, gva_agri_power, cagr_gva_agri_percapita)
), by = c("codmun7" = "codmun7", "SIGLA_UF" = "uf_sigla") 
) -> df_muni_cagr


#Model economic trends for 808 municipalities
#Adapt from https://www.tidymodels.org/learn/develop/broom/ to add AICc
myglance.lm <- function(x, ...) {
  with(
    summary(x),
    tibble::tibble(
      r.squared = r.squared,
      adj.r.squared = adj.r.squared,
      sigma = sigma,
      statistic = fstatistic["value"],
      p.value = pf(
        fstatistic["value"],
        fstatistic["numdf"],
        fstatistic["dendf"],
        lower.tail = FALSE
      ),
      df = fstatistic["numdf"],
      logLik = as.numeric(stats::logLik(x)),
      AIC = stats::AIC(x),
      AICc = MuMIn::AICc(x),
      BIC = stats::BIC(x),
      deviance = stats::deviance(x),
      df.residual = df.residual(x),
      nobs = stats::nobs(x)
    )
  )
}
#Run models
df_gdppop_muni_02a19 %>%
  select(year, uf_sigla, codmun7, name_muni, gdp_percapita_reais)%>% 
  nest(data = -c(uf_sigla, codmun7, name_muni)) %>% 
  mutate(
    fit = map(data, ~ lm(gdp_percapita_reais ~ year, data = .x)),
    tidied = map(fit, tidy), 
    glanced = map(fit, myglance.lm)
  ) -> regressions_gdp
regressions_gdp

df_gdppop_muni_02a19 %>% 
  filter(!is.na(gva_agri_percapita_reais)) %>%
  select(uf_sigla, codmun7, name_muni, gdp_percapita_reais, 
         gva_agri_percapita_reais)%>% 
  nest(data = -c(uf_sigla, codmun7, name_muni)) %>% 
  mutate(
    fit = map(data, ~ lm(gdp_percapita_reais ~ gva_agri_percapita_reais, data = .x)),
    tidied = map(fit, tidy), 
    glanced = map(fit, myglance.lm)
  ) -> regressions_gva_agri
regressions_gva_agri

#combine both "tidied" and "glanced" (univariate models)
# Variable summary. Hack to join summaries for these univariate models
regressions_gdp %>%
  unnest(tidied) %>% 
  filter(term != '(Intercept)') %>% 
  left_join(regressions_gdp %>% 
              unnest(glanced), 
            by = c("uf_sigla" = "uf_sigla", 
                   "codmun7" = "codmun7", 
                   "name_muni" = "name_muni",
                   "data" = "data", "fit" = "fit") 
  ) -> df_regression_gdp_out

regressions_gva_agri %>%
  unnest(tidied) %>% 
  filter(term != '(Intercept)') %>% 
  left_join(regressions_gva_agri %>% 
              unnest(glanced), 
            by = c("uf_sigla" = "uf_sigla", 
                   "codmun7" = "codmun7", 
                   "name_muni" = "name_muni",
                   "data" = "data", "fit" = "fit") 
  ) -> df_regressions_gva_agri_out

#need to check what r2 are actually showing.....(to do.....)
df_regression_gdp_out %>% group_by(uf_sigla) %>% 
  summarise(median_r2 = median(r.squared, na.rm = TRUE)) -> df_medianr2_gdp
df_regressions_gva_agri_out %>% group_by(uf_sigla) %>% 
  summarise(median_r2 = median(r.squared, na.rm = TRUE)) -> df_medianr2_gva_agri

df_regression_gdp_out %>% 
  ggplot(aes(x=estimate, y= r.squared)) + 
  geom_point(aes(colour = uf_sigla)) + 
  geom_hline(data = df_medianr2_gdp, aes(yintercept = median_r2)) +
  stat_smooth(aes(colour = uf_sigla), method="gam") + 
  scale_y_continuous(limits = c(0,1), breaks = c(0, 0.5, 1)) +
  facet_wrap(~uf_sigla) + 
  theme(legend.position = "none")

df_regressions_gva_agri_out %>% 
  ggplot(aes(x=estimate, y= r.squared)) + 
  geom_point(aes(colour = uf_sigla)) + 
  geom_hline(data = df_medianr2_gva_agri, aes(yintercept = median_r2)) +
  stat_smooth(aes(colour = uf_sigla), method="gam") + 
  scale_y_continuous(limits = c(0,1), breaks = c(0, 0.5, 1)) +
  facet_wrap(~uf_sigla) + 
  theme(legend.position = "none")

#Forest loss
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
dfmapbiomas_forest_transition_muni %>% 
  group_by(state, city) %>% summarise(acount = n()) %>% ungroup() -> df_muni_mapbiomas
df_muni_mapbiomas %>% select(-acount) %>% left_join(df_muni_cagr, 
                                by = c("state" = "uf",  "city"="NM_MUN")) %>% 
  filter(is.na(gdp_start)) %>% select(state, city)
#Missing 4 (all formally installed after 2002)
df_muni_cagr %>% filter(codmun7 == 5104526) #Ipiranga do Norte
df_muni_cagr %>% filter(codmun7 == 5104542) #Itanhangá
df_muni_cagr %>% filter(codmun7 == 1504752) # Mojuí dos Campos
df_muni_cagr %>% filter(codmun7 == 5104542) #Fortaleza do Tabocão

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

#Export
#Add state names and year (18 * 808 = 14544)
df_gdppop_muni_02a19 %>% 
  group_by(uf_sigla, uf) %>% 
  summarise(count_muni = length(unique(codmun7))) %>% right_join(
    data.frame(sf_ninestate_muni), by = c("uf_sigla" = "SIGLA_UF")
  ) %>% select(uf_sigla, uf, CD_MUN, NM_MUN, AREA_KM2) %>% 
  crossing(year = 2002:2019) %>% left_join(
#
df_gdppop_muni_02a19_nona %>% 
  select(year, uf_sigla, uf, codmun7, name_muni, 
         pop_new, gdp_percapita_reais, gva_new), 
by = c("uf_sigla" = "uf_sigla", "uf" = "uf", 
       "year" = "year", "NM_MUN" = "name_muni") 
) %>% left_join(#
dfmapbiomas_forest_transition_muni_long %>% 
  filter(year %in% c(2001:2019)) %>% 
  group_by(state, city, year, cover_class) %>% 
  summarise(area_km2 = sum(area_km2, na.rm = TRUE)) %>%
  pivot_wider(id_cols = c(state, city, year), 
              names_from = cover_class, values_from = area_km2) %>% 
  mutate(tot_transition_km2 = (replace_na(forest,0) + replace_na(savanna,0))), 
by = c("uf" = "state", "year" = "year", "NM_MUN" = "city") 
) %>% select(!codmun7) %>% 
  mutate(tot_transition_km2 = 
           if_else(tot_transition_km2==0, NA_real_, tot_transition_km2)) %>%
  mutate(tot_transition_km2_percent = (tot_transition_km2 / AREA_KM2)*100) %>%
  arrange(uf_sigla, NM_MUN) %>% 
  write.csv("muni_fixed_year.csv", row.names = FALSE)

#export with lag values
df_gdppop_muni_02a19 %>% 
  group_by(uf_sigla, uf) %>% 
  summarise(count_muni = length(unique(codmun7))) %>% right_join(
    data.frame(sf_ninestate_muni), by = c("uf_sigla" = "SIGLA_UF")
  ) %>% select(uf_sigla, uf, CD_MUN, NM_MUN, AREA_KM2) %>% 
  crossing(year = 2002:2019) %>% left_join(
dfmapbiomas_forest_transition_muni_long %>% 
  group_by(state, city, year, cover_class) %>% 
  summarise(area_km2 = sum(area_km2, na.rm = TRUE)) %>%
  pivot_wider(id_cols = c(state, city, year), 
              names_from = cover_class, values_from = area_km2) %>% 
  mutate(tot_transition_km2 = (replace_na(forest,0) + replace_na(savanna,0))) %>% 
  group_by(state, city) %>%
arrange(state, city) %>% 
  mutate(lag01_lossarea_km2 = lag(tot_transition_km2, order_by = year), 
         lag02_lossarea_km2 = lag(tot_transition_km2, n=2, order_by = year), 
         lag03_lossarea_km2 = lag(tot_transition_km2, n=3, order_by = year), 
         lag04_lossarea_km2 = lag(tot_transition_km2, n=4, order_by = year), 
         lag05_lossarea_km2 = lag(tot_transition_km2, n=5, order_by = year), 
         lag06_lossarea_km2 = lag(tot_transition_km2, n=6, order_by = year), 
         lag07_lossarea_km2 = lag(tot_transition_km2, n=7, order_by = year), 
         lag08_lossarea_km2 = lag(tot_transition_km2, n=8, order_by = year), 
         lag09_lossarea_km2 = lag(tot_transition_km2, n=9, order_by = year), 
         lag10_lossarea_km2 = lag(tot_transition_km2, n=10, order_by = year)) %>% 
  #select(state, city, year, lag10_lossarea_km2) %>% 
  filter(year >=2002), 
by = c("uf" = "state", "year" = "year", "NM_MUN" = "city") 
) %>% 
  mutate(lag01_lossarea_per = round((lag01_lossarea_km2 / AREA_KM2)*100,3), 
         lag02_lossarea_per = round((lag02_lossarea_km2 / AREA_KM2)*100,3), 
         lag03_lossarea_per = round((lag03_lossarea_km2 / AREA_KM2)*100,3), 
         lag04_lossarea_per = round((lag04_lossarea_km2 / AREA_KM2)*100,3), 
         lag05_lossarea_per = round((lag05_lossarea_km2 / AREA_KM2)*100,3), 
         lag06_lossarea_per = round((lag06_lossarea_km2 / AREA_KM2)*100,3), 
         lag07_lossarea_per = round((lag07_lossarea_km2 / AREA_KM2)*100,3), 
         lag08_lossarea_per = round((lag08_lossarea_km2 / AREA_KM2)*100,3), 
         lag09_lossarea_per = round((lag09_lossarea_km2 / AREA_KM2)*100,3), 
         lag10_lossarea_per = round((lag10_lossarea_km2 / AREA_KM2)*100,3)) %>% 
  arrange(uf_sigla, NM_MUN) %>% 
  write.csv("muni_fixed_lagloss.csv", row.names = FALSE)

#filter(year %in% c(2002:2019)) %>% 
  
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
