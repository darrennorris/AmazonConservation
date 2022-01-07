#Forest loss municipality

#packages
library(tidyverse)
library(readxl)
library(gridExtra)
library(psych)
library(tidymodels)
library(MuMIn) # AICc function

#GDP per capita and GVA by agriculture per capita 2002 - 2019 (from ibge_sidrar_tidy)
df_gdppop_muni_02a19 <- read_excel("data//bla_municipality_gdppop_02a19.xlsx", 
                                   na = c("", "NA"),
                                .name_repair = "universal")
#Not obvious how to select state capitals
state_capitals <- data.frame(name_muni = c("Manaus", "Macapá", "Porto Velho", "Rio Branco", 
                             "Boa Vista",
                             "São Luís", "Cuiabá", "Belém", "Palmas"), 
                    codmun7 = c(1302603, 1600303, 1100205, 1200401, 
                                1400100,
                                2111300, 5103403, 1501402, 1721000)
) 			


#Interpolate 2007 (to do......)

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
         flag_capital = if_else(codmun7 %in% state_capitals$codmun7, 
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
         flag_capital = if_else(codmun7 %in% state_capitals$codmun7, 
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
# Variable summary. Hack to pull together summaries for these univariate models
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
bla_states <- c("Acre", "Amapá", "Amazonas", "Maranhão", 
                "Mato Grosso", "Pará", "Tocantins", "Rondônia", "Roraima")
#Hansen
dfhansen_forestloss_primary <- read_excel("data//brazil_gfw_loss_2021_hansen.xlsx", 
                                          sheet = "primary_loss_admin2", 
                                          .name_repair = "universal") %>% 
  filter(admin1 %in% bla_states)

dfhansen_forestloss_primary[,-c(1,2,5)] %>% 
  pivot_longer(!c(admin1, admin2), names_to = "ayear", names_prefix ="...", 
               values_to = "primary_loss_ha") %>% 
  mutate(year = as.numeric(ayear)) -> dfhansen_forestloss_primary_long

#Mapbiomas
dfmapbiomas_transition_muni <- read_excel("data//Mapbiomas-Brazil-transition.xlsx", 
                                     sheet = "Sheet1", 
                                     .name_repair = "universal") %>% 
  filter(state %in% bla_states)

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
