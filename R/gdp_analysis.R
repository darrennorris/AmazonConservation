library(tidyverse)
library(readxl)
library(scales)
library(mgcv)
df_muni <- read_excel("data//bla_municipalities.xlsx", 
                                   na = c("", "NA"),
                                   sheet = "municipality_fixed_ref",
                                   .name_repair = "universal")
df_muni_year <- read_excel("data//bla_municipalities.xlsx", 
                      na = c("", "NA"),
                      sheet = "municipality_annual",
                      .name_repair = "universal")

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
