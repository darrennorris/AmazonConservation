library(tidyverse)
library(readxl)
library(scales)
library(mgcv)
library(stringi)


#google: r cran purtest example tutorial interpretation
#stationarity https://kevinkotze.github.io/ts-6-tut/
df_muni <- read_excel("data//bla_municipalities.xlsx", 
                                   na = c("", "NA"),
                                   sheet = "municipality_fixed_ref",
                                   .name_repair = "universal")
df_muni_year <- read_excel("data//bla_municipalities.xlsx", 
                      na = c("", "NA"),
                      sheet = "municipality_annual",
                      .name_repair = "universal")

#Basic reference vectors
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

#Summary
summary(df_muni$muni_area_km2)
df_muni %>% filter(!is.na(forest_2019_km2)) %>% 
  pull(muni_area_km2) %>% summary()


#2019 summaries. reference levels .......
df_muni_year %>% 
  filter(!is.na(tot_forest_cover_2019_percent), year == "2019") %>% 
  ggplot(aes(x=tot_forest_cover_2019_percent, y=salary_mean_reais/3.946)) + 
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_y_continuous(lim=c(0,1100), 
                     breaks = c(0,200, 400, 600, 800, 1000)) + 
  labs(title= "(A)", 
       x = "forest cover 2019 (% of municipality area)", 
       y = "mean monthly salary (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot") -> fig_2019_salary_forest
fig_2019_salary_forest
#Export
png(file = "figures//fig_2019_salary_forest.png", bg = "white", type = c("cairo"), 
    width=4000, height=4000, res = 600)
fig_2019_salary_forest
dev.off()

df_muni_year %>% 
  filter(!is.na(tot_forest_cover_2019_percent), year == "2019") %>% 
  ggplot(aes(x=tot_forest_cover_2019_percent, 
             y=(employed_informal/employed_total)*100)) + 
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_y_continuous(lim=c(0, 100)) +
  labs(title= "(B)", 
       x = "forest cover 2019 (% of municipality area)", 
       y = "informal employment (% of workforce)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot") -> fig_2019_informal_forest
fig_2019_informal_forest
#Export
png(file = "figures//fig_2019_informal_forest.png", bg = "white", type = c("cairo"), 
    width=4000, height=4000, res = 600)
fig_2019_informal_forest
dev.off()

df_muni_year %>% 
  filter(!is.na(tot_forest_cover_2019_percent), year == "2019") %>% 
  ggplot(aes(x=tot_forest_cover_2019_percent, y=gdp_percapita_reais/3.946)) + 
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +  
  labs(title= "(C)", 
       x = "forest cover 2019 (% of municipality area)", 
       y = "GDP per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot") -> fig_2019_gdp_forest
fig_2019_gdp_forest
#Export
png(file = "figures//fig_2019_gdp_forest.png", bg = "white", type = c("cairo"), 
    width=4000, height=4000, res = 600)
fig_2019_gdp_forest
dev.off()

df_muni_year %>% 
  filter(!is.na(tot_forest_cover_2019_percent), year == "2019") %>% 
  ggplot(aes(x=tot_forest_cover_2019_percent, 
             y=gva_agri_percapita_reais/3.946)) + 
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +  
  labs(title= "(D)", 
       x = "forest cover 2019 (% of municipality area)", 
       y = "GVA agriculture per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot") -> fig_2019_gva_forest
fig_2019_gva_forest  
#Export
png(file = "figures//fig_2019_gva_forest.png", bg = "white", type = c("cairo"), 
    width=4000, height=4000, res = 600)
fig_2019_gva_forest
dev.off()

#forest loss
df_muni %>% 
  left_join(df_muni_year %>% 
              filter(year == "2019") %>% 
              select(muni_name, salary_mean_reais)) %>%
  filter(!is.na(tot_loss_percent)) %>% 
  ggplot(aes(x=tot_loss_percent, y=salary_mean_reais/3.946)) + 
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_y_continuous(lim=c(0,1100), 
                     breaks = c(0,200, 400, 600, 800, 1000)) + 
  labs(title= "(E)", 
       x = "forest loss 2002-2019 (% of municipality area)", 
       y = "mean monthly salary (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot") -> fig_2019_salary_forestloss
fig_2019_salary_forestloss

#Export
png(file = "figures//fig_2019_salary_forestloss.png", bg = "white", type = c("cairo"), 
    width=4000, height=4000, res = 600)
fig_2019_salary_forestloss
dev.off()
#informal
df_muni %>% 
  left_join(df_muni_year %>% 
              filter(year == "2019") %>% 
              mutate(informal_per = (employed_informal/employed_total)*100) %>%
              select(muni_name, informal_per)) %>%
  filter(!is.na(tot_loss_percent)) %>% 
  ggplot(aes(x=tot_loss_percent, y=informal_per)) + 
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_y_continuous(lim=c(0, 100)) +
  labs(title= "(F)", 
       x = "forest loss 2002-2019 (% of municipality area)", 
       y = "informal employment (% of workforce)") +
  theme(text = element_text(size = 16), 
        plot.title.position = "plot") -> fig_2019_informal_forestloss
  fig_2019_informal_forestloss

#Export
png(file = "figures//fig_2019_informal_forestloss.png", bg = "white", type = c("cairo"), 
    width=4000, height=4000, res = 600)
fig_2019_informal_forestloss
dev.off()

df_muni %>% 
  filter(!is.na(tot_forest_cover_2019_percent)) %>% 
  ggplot(aes(x=tot_loss_percent, 
             y=gdp_percapita_reais_2019/3.946)) + 
  geom_point() + 
  stat_smooth(method="gam", method.args = list(family = "tw")) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +  
  labs(title= "(G)", 
       x = "forest loss 2002-2019 (% of municipality area)", 
       y = "GDP per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot") -> fig_2019_gdp_forestloss
fig_2019_gdp_forestloss
#Export
png(file = "figures//fig_2019_gdp_forestloss.png", bg = "white", type = c("cairo"), 
    width=4000, height=4000, res = 600)
fig_2019_gdp_forestloss
dev.off()
#GVA agri forestloss
df_muni %>% 
  left_join(df_muni_year %>% 
              filter(year == "2019") %>% 
              select(muni_name, gva_agri_percapita_reais)) %>%
  filter(!is.na(tot_forest_cover_2019_percent)) %>% 
  ggplot(aes(x=tot_loss_percent, 
             y=gva_agri_percapita_reais/3.946)) + 
  geom_point() + 
  stat_smooth(method="gam", method.args = list(family = "tw")) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +  
  labs(title= "(H)", 
       x = "forest loss 2002-2019 (% of municipality area)", 
       y = "GVA agriculture per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot") -> fig_2019_gva_forestloss
fig_2019_gva_forestloss  
#Export
png(file = "figures//fig_2019_gva_forestloss.png", bg = "white", type = c("cairo"), 
    width=4000, height=4000, res = 600)
fig_2019_gva_forestloss
dev.off()


#Annual patterns
#757 municipalities
df_muni_year %>% 
  select(muni_name, muni_area_km2, year, gdp_percapita_reais, tot_loss_percent, 
         school_per1000, superior_course_per1000, pg_per1000) %>% 
  filter(!is.na(tot_loss_percent), !is.na(school_per1000)) %>% 
  group_by(muni_name, muni_area_km2) %>% 
  summarise(acount = n(), 
            count_year = length(unique(year))) %>% 
  arrange(desc(count_year)) %>% 
  ungroup() %>% pull(muni_area_km2) %>% sum() #[1] 5030055

df_muni_year %>% 
  select(state_name, muni_name, muni_area_km2, year, 
         gdp_percapita_reais, tot_loss_percent, 
         school_per1000, superior_course_per1000, pg_per1000) %>% 
  filter(!is.na(tot_loss_percent), !is.na(school_per1000)) %>% 
  ggplot(aes(x=year, y = school_per1000)) + 
  geom_point() + 
  #stat_smooth(method="gam") + 
  stat_smooth(aes(colour = state_name), method="gam")


df_muni_year %>%
ggplot(aes(x=gva_agri_percapita_reais/3.946, y=gdp_percapita_reais/3.946)) + 
  geom_point(aes(colour=factor(year))) + 
  stat_smooth(aes(colour=factor(year)), method="lm", se=FALSE) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +
  scale_x_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +
  scale_color_viridis_d() + 
  facet_wrap(~flag_urban) +
  labs(title= "(A)", 
       x = "GVA agriculture per capita (US$)", 
       y = "GDP per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position="bottom", 
        legend.title = element_blank()) + 
  guides(col = guide_legend(nrow = 2))-> fig_gdp_agri_2002_2019
fig_gdp_agri_2002_2019

#Export
png(file = "figures//fig_gdp_agri_2002_2019.png", bg = "white", type = c("cairo"), 
    width=6000, height=3500, res = 600)
fig_gdp_agri_2002_2019
dev.off()

df_muni_year %>%
  ggplot(aes(x=tot_loss_percent, y=gdp_percapita_reais/3.946)) + 
  geom_point(aes(colour=factor(year))) + 
  stat_smooth(aes(colour=factor(year)), method="lm", se=FALSE) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +
  scale_color_viridis_d() +
  facet_wrap(~flag_urban) +
  labs(title= "(B)", 
       x = "Forest transition (%)", 
       y = "GDP per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position="bottom", 
        legend.title = element_blank()) + 
  guides(col = guide_legend(nrow = 2)) -> fig_gdp_forest_2002_2019
fig_gdp_forest_2002_2019
#Export
png(file = "figures//fig_gdp_forest_2002_2019.png", bg = "white", type = c("cairo"), 
    width=6000, height=3500, res = 600)
fig_gdp_forest_2002_2019
dev.off()

df_muni_year %>%
  ggplot(aes(x=tot_loss_percent, y=gva_agri_percapita_reais/3.946)) + 
  geom_point(aes(colour=factor(year))) + 
  stat_smooth(aes(colour=factor(year)), method="lm", se=FALSE) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) +
  scale_color_viridis_d() +
  facet_wrap(~flag_urban) +
  labs(title= "(C)", 
       x = "Forest transition (%)", 
       y = "GVA agriculture per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position="bottom", 
        legend.title = element_blank()) + 
  guides(col = guide_legend(nrow = 2)) -> fig_agri_forest_2002_2019
fig_agri_forest_2002_2019
#Export
png(file = "figures//fig_agri_forest_2002_2019.png", bg = "white", type = c("cairo"), 
    width=6000, height=3500, res = 600)
fig_agri_forest_2002_2019
dev.off()

df_muni_year %>%
  #ggplot(aes(x=pass_rate_per, y=gdp_percapita_reais/3.946)) + 
  ggplot(aes(x=school_per1000, y=gdp_percapita_reais/3.946)) + 
  geom_point(aes(colour=factor(year))) + 
  stat_smooth(aes(colour=factor(year)), method="lm", se=FALSE) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3, 
                                                  accuracy = 1.0)) +
  scale_color_viridis_d() +
  facet_wrap(~flag_urban) +
  labs(title= "(D)", 
       x = "schools per 1000", 
       y = "GDP per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position="bottom", 
        legend.title = element_blank()) + 
  guides(col = guide_legend(nrow = 2)) -> fig_gdp_school_2002_2019
fig_gdp_school_2002_2019
#Export
png(file = "figures//fig_gdp_school_2002_2019.png", bg = "white", type = c("cairo"), 
    width=6000, height=3500, res = 600)
fig_gdp_school_2002_2019
dev.off()

df_muni_year %>%
  ggplot(aes(x= superior_course_per1000, y=gdp_percapita_reais/3.946)) + 
  geom_point(aes(colour=factor(year))) + 
  stat_smooth(aes(colour=factor(year)), method="lm", se=FALSE) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3, 
                                                  accuracy = 1.0)) +
  scale_color_viridis_d() +
  facet_wrap(~flag_urban) +
  labs(title= "(E)", 
       x = "post secondary courses per 1000", 
       y = "GDP per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position="bottom", 
        legend.title = element_blank()) + 
  guides(col = guide_legend(nrow = 2)) -> fig_gdp_ps_2002_2019
fig_gdp_ps_2002_2019
#Export
png(file = "figures//fig_gdp_ps_2002_2019.png", bg = "white", type = c("cairo"), 
    width=6000, height=3500, res = 600)
fig_gdp_ps_2002_2019
dev.off()

df_muni_year %>%
  mutate(pg_per1000 = count_pg_course/(tot_pop/1000)) %>%
  ggplot(aes(x=pg_per1000, y=gdp_percapita_reais/3.946)) + 
  geom_point(aes(colour=factor(year))) + 
  stat_smooth(aes(colour=factor(year)), method="lm", se=FALSE) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3, 
                                                  accuracy = 1.0)) +
  scale_color_viridis_d() +
  facet_wrap(~flag_urban) +
  labs(title= "(F)", 
       x = "post graduate courses per 1000", 
       y = "GDP per capita (US$)") + 
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position="bottom", 
        legend.title = element_blank()) + 
  guides(col = guide_legend(nrow = 2)) -> fig_gdp_pg_2002_2019
fig_gdp_pg_2002_2019
#Export
png(file = "figures//fig_gdp_pg_2002_2019.png", bg = "white", type = c("cairo"), 
    width=6000, height=3500, res = 600)
fig_gdp_pg_2002_2019
dev.off()

df_muni %>% 
  filter(!is.na(tot_forest_cover_2019_percent)) %>%
ggplot(aes(x = tot_forest_cover_2019_percent, y = gdp_percapita_reais_2019)) +
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_y_continuous("GDP per capita (thousands Reais)", 
                     labels = unit_format(unit = "K", 
                                          scale = 1e-3, 
                                          accuracy = 0.1)) +
  labs(title = "GDP and forest cover 2019", 
       x = "remaining forest cover (% of municipality)") + 
  theme(plot.title.position = "plot") -> fig_gdp_variation
fig_gdp_variation
png(file = "figures//fig_gdp_variation.png", bg = "white", type = c("cairo"), 
    width=4000, height=4000, res = 600)
fig_gdp_variation + theme(text = element_text(size = 16))
dev.off()
  
df_muni %>% 
  filter(!is.na(tot_forest_cover_2019_percent)) %>%
  ggplot(aes(x = school_per1000, y = gdp_percapita_reais_2019)) +
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_y_continuous("GDP per capita (thousands Reais)", 
                     labels = unit_format(unit = "K", 
                                          scale = 1e-3, 
                                          accuracy = 0.1)) +
  labs(title = "GDP and forest cover 2019", 
       x = "schools per 1000") + 
  theme(plot.title.position = "plot")   

df_muni %>% 
  filter(!is.na(tot_forest_cover_2019_percent)) %>%
  ggplot(aes(x = pass_rate_per, y = gdp_percapita_reais_2019)) +
  geom_point() + 
  stat_smooth(method="gam") + 
  scale_y_continuous("GDP per capita (thousands Reais)", 
                     labels = unit_format(unit = "K", 
                                          scale = 1e-3, 
                                          accuracy = 0.1)) +
  labs(title = "GDP and forest cover 2019", 
       x = "school pass rate") + 
  theme(plot.title.position = "plot") 

#models
#mgcv timeseries
#https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/
#https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
#https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/

var_response <- c("gdp_percapita_reais")
var_timeconstant <- c("state_name", "muni_name", "muni_area_km2", "dist_statecapital_km", 
         "flag_urban")
var_timevary <- c("year","pop_dens_km2", "tot_loss_percent", "gva_agri_percapita_reais",
                  "school_per1000", "superior_course_per1000", "pg_per1000")
var_lags <- c("lag01_lossarea_per", "lag02_lossarea_per", "lag03_lossarea_per", 
              "lag04_lossarea_per", "lag05_lossarea_per", "lag06_lossarea_per", 
              "lag07_lossarea_per", "lag08_lossarea_per", "lag09_lossarea_per", 
              "lag10_lossarea_per")
df_muni_year %>% 
  filter(!is.na(tot_loss_percent), !is.na(school_per1000), 
         !is.na(superior_course_per1000), !is.na(pg_per1000), 
         dist_statecapital_km >0) %>% 
  select(all_of(var_response), all_of(var_timeconstant), all_of(var_timevary), 
         all_of(var_lags)) %>% 
  mutate(tot_loss3y_percent = lag01_lossarea_per + 
           lag02_lossarea_per + lag03_lossarea_per, 
         tot_loss5y_percent = lag01_lossarea_per + 
           lag02_lossarea_per + lag03_lossarea_per + 
           lag04_lossarea_per + lag05_lossarea_per, 
         adate = as.Date(paste(year,"-01", "-01", sep="")), 
         format = c("%Y-%m-%d")) -> dfgam
which(is.na(dfgam)[,3]) #
dfgam$muni_namef <- as.factor(dfgam$muni_name) 
dfgam$state_namef <- as.factor(dfgam$state_name)
dfgam$flag_urbanf <- as.factor(dfgam$flag_urban)

#Subset to develop models
unique(dfgam$state_name)
dfgam[which(dfgam$gdp_percapita_reais == max(dfgam$gdp_percapita_reais)), 
            'state_name']
dfgam[which(dfgam$gdp_percapita_reais == min(dfgam$gdp_percapita_reais)), 
      'state_name']
dfgam %>% 
  filter(state_name %in% c("Amapá", "Pará", 
                           "Maranhão")) -> dfgam_test

table(dfgam$flag_urban)
#library(corrgram) -18 (lag 1,2,3)
corrgram(dfgam[, c(var_response, "tot_loss_percent", var_lags)],
         lower.panel=panel.pts, upper.panel=panel.conf,
         diag.panel=panel.density)
cor.test(dfgam$gdp_percapita_reais, 
         dfgam$school_per1000) #0.07020494
cor.test(dfgam$gdp_percapita_reais, 
         dfgam$superior_course_per1000) #- 0.004 NS
cor.test(dfgam$gdp_percapita_reais, 
         dfgam$pg_per1000) #0.078

#Model
#Need to use log as there are (5 or so) extreme outlier gdp_percapita values 
model_00 <- gam(log(gdp_percapita_reais) ~ year*flag_urbanf +
                  s(pop_dens_km2) +
                  s(tot_loss5y_percent) +
                  s(gva_agri_percapita_reais) +
                  s(school_per1000) + 
                   s(pg_per1000) + 
                  s(dist_statecapital_km, by = state_namef), 
                 data = dfgam, 
                method="REML")
gam.check(model_00) #test has same distributions
summary(model_00)
plot(model_00, scale = 0)

#
ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B", 
             maxIter = 99, msMaxIter = 99)
model_01 <- gamm(log(gdp_percapita_reais) ~ year*flag_urbanf +
                   s(pop_dens_km2) +
                   s(tot_loss5y_percent) +
                   s(gva_agri_percapita_reais) +
                   s(school_per1000) + 
                   s(pg_per1000) + 
                   s(dist_statecapital_km, by = state_namef), 
                 data = dfgam_test, 
                 method="REML")
summary(model_01$lme)
library(forecast)
arma_res <- auto.arima(resid(model_01$lme, type = "normalized"),
                       stationary = TRUE, seasonal = FALSE)

arma_res$coef
#       ar1        ar2        ar3        ma1        ma2 
#0.2820864 -0.3996548  0.8024361  0.5823226  0.9371343 

#AR ...
model_01_ar3 <- gamm(log(gdp_percapita_reais) ~ year*flag_urbanf + 
                       s(pop_dens_km2) +
                       s(tot_loss5y_percent) +
                       s(gva_agri_percapita_reais) +
                       s(school_per1000) + 
                       s(pg_per1000) + 
                       s(dist_statecapital_km, by = state_namef), 
                         data = dfgam, 
                         method="REML",
                   correlation = corARMA(form = ~ 1|year, p = 3, q = 2), 
                 control = ctrl)
saveRDS(model_01_ar3, "model_01_ar3.rds")
model_01_ar3 <- readRDS("model_01_ar3.rds")
summary(model_01_ar3$lme) 
summary(model_01_ar3$gam)

#residuals
anova(model_01$lme, model_01_ar4$lme)
res_gam <- resid(model_00, type = "deviance")
res_gamm <- resid(model_01$lme, type = "normalized")
#res_gamm_ar4 <- resid(model_01_ar4$lme, type = "normalized")
res_gamm_ar4 <- resid(model_01_ar4$gam, type = "deviance")

dfgam$m01_res_gam <- res_gam
dfgam$m01_res_gamm <- res_gamm


library(timetk)
dfgam %>%
  group_by(state_name, muni_name) %>%
  tk_acf_diagnostics(
    .date_var = year,
    .value = m01_res_gamm, 
    .lags = 11
  ) -> tidy_acf

#export as .png  250 * 1000
tidy_acf %>% 
  ggplot(aes(x = lag, y = ACF, color = state_name, 
             group = state_name)) +
  # Add horizontal line a y=0
  geom_hline(yintercept = 0) +
  # Plot autocorrelations
  geom_point(size = 2) +
  geom_segment(aes(xend = lag, yend = 0), size = 1) +
  # Add cutoffs
  geom_line(aes(y = .white_noise_upper), color = "black", 
            linetype = 2) +
  geom_line(aes(y = .white_noise_lower), color = "black", 
            linetype = 2) +
  # Add facets
  facet_wrap(~ state_name, ncol = 1) +
  # Aesthetics
  expand_limits(y = c(-1, 1)) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) + labs(
    title = "AutoCorrelation (ACF)",
    subtitle = "GAMM residuals", 
    x = "lag (year)"
  )

#https://www.kaggle.com/janiobachmann/time-series-i-an-introductory-start/script
dfgam %>%
  group_by(state_name, muni_name) %>% 
  plot_acf_diagnostics(
    date,
    AAPL,
    .ccf_vars = GOOGL,
    .show_ccf_vars_only = TRUE,
    .interactive=FALSE, 
    .line_color = "black",
    .point_color =palette_light()[[2]],
    .line_size = 1.5,
    .title = "Cross Correlation of Technology Stocks"
  ) 
