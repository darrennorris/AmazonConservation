#Perspective
#packages
library(tidyverse)
library(readxl)
dfgam <- readRDS("dfgam.rds") #13710 obs. 55 vars
dfgam$log_gdp_percapita_reais <- log(dfgam$gdp_percapita_reais)

#GDP increase
dfgam %>% group_by(year) %>% 
  summarise(gdp_median_reais = median(gdp_percapita_reais), 
            gdp_median_usd = median(gdp_percapita_reais) / 3.946) %>% 
  filter(year %in% c(2002, 2019)) %>% 
  mutate(lead_gdp = lead(gdp_median_usd)) %>% 
  mutate(gdp_diff = lead_gdp - gdp_median_usd, 
         gdp_inc = lead_gdp/gdp_median_usd)
#Forest loss
count_muni <- length(unique(dfgam$muni_factor))
dfgam %>% 
  group_by(state_name, muni_name, muni_area_km2) %>% 
  summarise(acount = n()) %>% ungroup() %>% 
  pull(muni_area_km2) %>% sum() -> tot_muni_area_km2
dfgam %>% 
  group_by(state_name, muni_name, muni_area_km2, forestcover_1985_km2) %>% 
  summarise(acount = n()) %>% ungroup() %>% 
  pull(forestcover_1985_km2) %>% sum() -> tot_forestcover_1985_km
dfgam %>% pull(tot_loss_km2) %>% sum() -> tot_loss_km2_02a19#507434.7 km2
#loss in relation to muni area
round((tot_loss_km2_02a19 / tot_muni_area_km2) * 100, 3) #10.238
# loss in relation to 1985 forest cover
round((tot_loss_km2_02a19 / tot_forestcover_1985_km) * 100, 3) #12.032

# Select municipalities with less and more forest cover
dfgam %>% 
  group_by(state_name, muni_name, muni_area_km2, 
           forestcover_1985_km2, forestcover_1985_percent) %>% 
  summarise(acount = n()) %>% ungroup() %>% 
  pull(forestcover_1985_percent) %>% hist()

# 54 municipalities with low cover in 1985
dfgam %>% filter(forestcover_1985_percent <=40, 
                 indigenous_area_percent < 50) %>%
  pull(muni_factor) %>% as.character() %>% unique() -> n40#54
dfgam %>% filter(forestcover_1985_percent <=30, 
                 indigenous_area_percent < 50) %>%
  pull(muni_factor) %>% as.character() %>% unique() -> n30#28

dfgam %>% 
  filter(forestcover_1985_percent <=40, indigenous_area_percent < 50) %>%
  group_by(state_name, muni_name, muni_area_km2, process_gold_p1000,
           dist_statecapital_km, indigenous_area_percent, 
           forestcover_1985_percent, tot_forest_cover_2019_percent) %>% 
  summarise(median_gold_p1000 = median(process_gold_p1000), 
            median_pop_dens = median(pop_dens_km2), 
            median_industry = median(gva_industry_percent),
            acount = n()) %>% ungroup() -> df_muni_cover40
keep_states <- unique(df_muni_cover40$state_name) 
# "Amapá"       "Amazonas"    "Maranhão"    "Mato Grosso" "Pará"       
 #[6] "Roraima"     "Tocantins" 
summary(df_muni_cover40$dist_statecapital_km) #25 - 753 km 187 median
summary(df_muni_cover40$muni_area_km2) # #79 - 12535 km2 median 1201
summary(df_muni_cover40$indigenous_area_percent) # mean = 0.8, max = 21
summary(df_muni_cover40$median_gold_p1000) #0
summary(df_muni_cover40$median_pop_dens) # median = 11, max = 334
summary(df_muni_cover40$median_industry) # median = 4.6, max = 41.5

# 327
dfgam %>% 
  filter(forestcover_1985_percent >=60, indigenous_area_percent <= 21, 
         dist_statecapital_km <= 753, muni_area_km2 <= 12535, 
         state_name %in% keep_states) %>%
  group_by(state_name, muni_name, muni_area_km2, 
           dist_statecapital_km, indigenous_area_percent, 
           forestcover_1985_percent, tot_forest_cover_2019_percent) %>% 
  summarise(median_gold_p1000 = median(process_gold_p1000), 
            median_pop_dens = median(pop_dens_km2),
            median_industry = median(gva_industry_percent),
            acount = n()) %>% 
  filter(acount ==18, 
         median_gold_p1000==0, 
         median_pop_dens <=350, 
         median_industry <= 42) %>% 
  ungroup() -> df_muni_cover60

df_muni_cover40 %>% mutate(trees = "few") %>% 
  bind_rows(df_muni_cover60  %>% mutate(trees = "many")) -> dfmatched
library(Hmisc)
options(digits=3)
par(mfrow = c(3, 2)) # 450 * 600
histbackback(df_muni_cover40$muni_area_km2, 
             df_muni_cover60$muni_area_km2, probability=TRUE, 
             xlab = c("<=40",">=60"), main = "municipality size (km2)")
histbackback(df_muni_cover40$dist_statecapital_km, 
             df_muni_cover60$dist_statecapital_km, probability=TRUE, 
             xlab = c("<=40",">=60"), main = "distance to state capital (km)")
histbackback(df_muni_cover40$median_pop_dens, 
             df_muni_cover60$median_pop_dens, probability=TRUE, 
             xlab = c("<=40",">=60"), main = "population density")
histbackback(df_muni_cover40$indigenous_area_percent, 
             df_muni_cover60$indigenous_area_percent, probability=TRUE, 
             xlab = c("<=40",">=60"), main = "indigenous area (%)")
histbackback(df_muni_cover40$median_industry, 
             df_muni_cover60$median_industry, probability=TRUE, 
             xlab = c("<=40",">=60"), main = "industry  value contribution (%)")
histbackback(df_muni_cover40$tot_forest_cover_2019_percent, 
             df_muni_cover60$tot_forest_cover_2019_percent, probability=TRUE, 
             xlab = c("<=40",">=60"), main = "forest cover 2019 (%)") 

#Analysis with matched groups
#matched subset
dfgam %>% 
  right_join(dfmatched %>% select(state_name, muni_name, trees)) -> dfgam_matched
dfgam_matched$cover_group <- as.factor(dfgam_matched$trees)
levels(dfgam_matched$cover_group) <- c("forest cover <=40%", 
                                       "forest cover >=60%")

dfgam_matched %>% 
  ggplot(aes(x=year, y=gdp_percapita_reais / 3.946)) + 
  geom_point() + 
  stat_smooth(method = "lm") + 
  facet_wrap(~cover_group) + 
  scale_y_continuous("GDP per capita (US$)", 
                     labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) + 
  
