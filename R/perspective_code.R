#Perspective
#packages
library(tidyverse)
library(readxl)
library(gridExtra)
library(mgcv)

#load data
dfgam <- readRDS("dfgam.rds") #13710 obs. 63 vars
dfgam$log_gdp_percapita_reais <- log(dfgam$gdp_percapita_reais)
dfgam$gdp_percapita_usd = dfgam$gdp_percapita_reais / 3.946

#GDP increase over time
dfgam %>% group_by(year) %>% 
  summarise(gdp_median_reais = median(gdp_percapita_reais), 
            gdp_median_usd = median(gdp_percapita_reais) / 3.946) %>% 
  filter(year %in% c(2002, 2019)) %>% 
  mutate(lead_gdp = lead(gdp_median_usd)) %>% 
  mutate(gdp_diff = lead_gdp - gdp_median_usd, 
         gdp_inc = lead_gdp/gdp_median_usd)
#Forest loss
count_muni <- length(unique(dfgam$muni_factor)) #763
dfgam %>% 
  group_by(state_name, muni_name, muni_area_km2) %>% 
  summarise(acount = n()) %>% ungroup() %>% 
  pull(muni_area_km2) %>% sum() -> tot_muni_area_km2 #4956340
dfgam %>% 
  group_by(state_name, muni_name, muni_area_km2, forestcover_1985_km2) %>% 
  summarise(acount = n()) %>% ungroup() %>% 
  pull(forestcover_1985_km2) %>% sum() -> tot_forestcover_1985_km
dfgam %>% pull(tot_loss_km2) %>% sum() -> tot_loss_km2_02a19#507434.7 km2
#loss in relation to muni area
round((tot_loss_km2_02a19 / tot_muni_area_km2) * 100, 3) #10.238
# loss in relation to 1985 forest cover
round((tot_loss_km2_02a19 / tot_forestcover_1985_km) * 100, 3) #12.032


#Correlations
cor.test(dfgam$gdp_percapita_reais, dfgam$tot_loss_km2) #0.019
cor.test(dfgam$log_gdp_percapita_reais, dfgam$tot_loss_km2) #0.05301267
cor.test(dfgam$min_salary_mean, dfgam$tot_loss_km2) #0.1546216

# Figure 1
dfgam %>% 
  group_by(year) %>%
  summarise(area = sum(tot_loss_km2, na.rm=TRUE)) %>% 
  pull(area) %>% max() -> mapbiomasloss_all_max #[1] 44877.9
dfgam %>% 
  group_by(year) %>%
  summarise(area = median(gdp_percapita_usd, na.rm=TRUE)) %>% 
  pull(area) %>% max() -> gdp_percapita_usd_median_max #[1] 3569.88

axis_trans_mapbiomasall <- mapbiomasloss_all_max / gdp_percapita_usd_median_max

dfgam %>% 
  group_by(year) %>%
  summarise(area = median(min_salary_mean, na.rm=TRUE)) %>% 
  filter(!is.na(area)) %>%
  pull(area) %>% max() -> min_salary_mean_median_max #[1] 1.9
axis_trans_mapbiomassal <- mapbiomasloss_all_max / min_salary_mean_median_max

# try gini on salary
library(ineq)
dfgam %>% 
  group_by(year) %>%
  summarise(gini_ineq = ineq::ineq(min_salary_mean))

df_mapbiomasall_labels <- data.frame(year = c(2004, 2019), 
                                     yaxis_value = mapbiomasloss_all_max +(mapbiomasloss_all_max*0.05),
                                     label_values = c(round(mapbiomasloss_all_max,0), 
                                                      round(gdp_percapita_usd_median_max,0)), 
                                     label_values_salary = c(round(mapbiomasloss_all_max,0), 
                                                      round(min_salary_mean_median_max,2)))
df_mapbiomasall_labels
mycols_mapbiomas <- c("savanna" = "deeppink4", 
                      "forest" = "deeppink")

dfgam %>% 
  group_by(year) %>% 
  summarise(tot_loss_km2 = sum(tot_loss_km2), 
            gdp_per_capita_usd = median(gdp_percapita_usd)) %>%
ggplot(aes(x=year, y=tot_loss_km2)) + 
  geom_col(fill="deeppink", colour="blue") + 
  #geom_line(data = dfmapbiomas_forestloss_3yr, aes(x=year, y = area_km2_3y), 
  #          colour ="grey60", size = 1.5) +
  #geom_line(data = dfmapbiomas_forestloss_3yr, aes(x=year, y = area_km2_3y), 
  #          colour ="yellow", linetype = "dashed", size = 1.1) +
  geom_line(aes(x=year, y=gdp_per_capita_usd*axis_trans_mapbiomasall), 
            size=2, color="black") + 
  scale_y_continuous(limits = c(0, 50000), 
                     #for the second y-axis
                     sec.axis = sec_axis(~./axis_trans_mapbiomasall,#divided by transformation rate, in order to be represented based on the first y-axis
                                         name = "GDP per capita (US$)")) + 
  scale_x_continuous(limits = c(2001.5, 2019.5))  + 
  geom_label(data = df_mapbiomasall_labels, 
             aes(x= year, y = yaxis_value, label = label_values), 
             colour = c("blue", "black")) + 
  labs(title = "(A)", 
       y = bquote('forest loss'~(km^2)))  +
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position = "top", legend.box = "horizontal") -> figa_loss_gdp
figa_loss_gdp

#salary
dfgam %>% 
  group_by(year) %>% 
  summarise(tot_loss_km2 = sum(tot_loss_km2), 
            min_salary_median = median(min_salary_mean, na.rm = TRUE)) %>%
  ggplot(aes(x=year, y=tot_loss_km2)) + 
  geom_col(fill="deeppink", colour="blue") + 
  #geom_line(data = dfmapbiomas_forestloss_3yr, aes(x=year, y = area_km2_3y), 
  #          colour ="grey60", size = 1.5) +
  #geom_line(data = dfmapbiomas_forestloss_3yr, aes(x=year, y = area_km2_3y), 
  #          colour ="yellow", linetype = "dashed", size = 1.1) +
  geom_line(aes(x=year, y=min_salary_median*axis_trans_mapbiomassal), 
            size=2, color="black") + 
  scale_y_continuous(limits = c(0, 50000), 
                     #for the second y-axis
                     sec.axis = sec_axis(~./axis_trans_mapbiomassal,#divided by transformation rate, in order to be represented based on the first y-axis
                                         name = "salary")) + 
  scale_x_continuous(limits = c(2001.5, 2019.5))  + 
  geom_label(data = df_mapbiomasall_labels, 
             aes(x= year, y = yaxis_value, label = label_values_salary), 
             colour = c("blue", "black")) + 
  labs(title = "(B)", 
       y = bquote('forest loss'~(km^2)))  +
  theme(text = element_text(size = 16), 
        plot.title.position = "plot", 
        legend.position = "top", legend.box = "horizontal") -> figa_loss_salary
figa_loss_salary

png(file = "figures//fig_loss_gdp_salary.png", 
    bg = "white", type = c("cairo"), 
    width=5000, height=5300, res = 600)
gridExtra::grid.arrange(figa_loss_gdp, 
                        figa_loss_salary, ncol = 1)
dev.off()

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

# 132
dfgam %>% 
  filter(forestcover_1985_percent >=60, indigenous_area_percent <= 21, 
         dist_statecapital_km <= 753, muni_area_km2 <= 12535, 
         tot_forest_cover_2019_percent >=60,
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
# <= 50%, 124
dfgam %>% 
  filter(forestcover_1985_percent >=60, indigenous_area_percent <= 21, 
         dist_statecapital_km <= 753, muni_area_km2 <= 12535, 
         tot_forest_cover_2019_percent <=50,
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
  ungroup() -> df_muni_cover60less

df_muni_cover40 %>% mutate(trees = "few") %>% 
  bind_rows(df_muni_cover60  %>% mutate(trees = "many")) %>% 
  bind_rows(df_muni_cover60less %>% mutate(trees = "many_loss")) -> dfmatched


#Analysis with matched groups
#matched subset
dfgam %>% 
  right_join(dfmatched %>% select(state_name, muni_name, trees)) -> dfgam_matched
dfgam_matched$cover_group <- as.factor(dfgam_matched$trees)
levels(dfgam_matched$cover_group) <- c("forest cover <=40%", 
                                       "forest cover >=60%", 
                                       "forest cover >=60%\nwith loss")

dfgam_matched %>% 
  filter(!is.na(min_salary_mean)) %>%
  ggplot(aes(x=year, y=gdp_percapita_reais / 3.946)) + 
  geom_point() + 
  stat_smooth(method = "lm") + 
  facet_wrap(~cover_group) + 
  scale_y_continuous("GDP per capita (US$)", 
                     labels = scales::unit_format(unit = "k", 
                                                  scale = 1e-3)) + 
  labs(title = "(A)") +
  theme(plot.title.position = "plot") -> fig_GDP_matched
fig_GDP_matched

dfgam_matched %>% 
  filter(!is.na(min_salary_mean)) %>%
  ggplot(aes(x=year, y=min_salary_mean)) + 
  geom_point() + 
  stat_smooth(method = "lm") + 
  facet_wrap(~cover_group) + 
  scale_y_continuous("minimum salary") + 
  labs(title = "(B)") +
  theme(plot.title.position = "plot") -> fig_salary_matched
fig_salary_matched
#Export
png(file = "figures//fig_economic_matched.png", 
    bg = "white", type = c("cairo"), 
    width=3000, height=3000, res = 600)
grid.arrange(fig_GDP_matched, fig_salary_matched, nrow = 2)
dev.off()

#Explain
# values for tweedie and AR1 calculated in "gdp_bams.R"
dfgam_matched %>% 
  filter(!is.na(min_salary_mean)) %>% 
  arrange(muni_factor, year) %>% 
  group_by(muni_factor) %>%
  mutate(start_year = min(year)) %>% 
  mutate(start_event = year== start_year) %>% 
  ungroup() -> dfgam_matched_model
#GDP 
aov <- lm(log_gdp_percapita_reais~cover_group, data = dfgam_matched_model)
summary(aov) 
anova(aov) #marginally significant

myctrl <- list(keepData = TRUE, trace = TRUE)  
bam_loss_01 <- bam(log_gdp_percapita_reais~ 
                 cover_group +
                 #Spatial smooth
                 s(long, lat) + 
                 #Spatial proximity
                 s(dist_statecapital_km, state_namef, bs='fs', m=1) + 
                 #Time
                 s(year, cover_group, bs='fs', m=1) +
                 #s(year, by = state_namef) +
                 s(yearf, bs = "re") +
                 #Random 
                 s(state_namef, bs="re") + 
                 s(muni_factor, bs="re")+ 
                 s(cover_group, bs="re"), 
               #AR1 residual errors
               rho=0.893, AR.start = dfgam_matched_model$start_event, 
               family=Tweedie(1.99),
               method = "fREML",
               discrete = TRUE,
               data = dfgam_matched_model, 
               control = myctrl)   
summary(bam_loss_01)
plot(bam_loss_01, scale = 0, all.terms = TRUE)

#Salary
bam_loss_02 <- bam(min_salary_mean ~ 
                     cover_group +
                     #Spatial smooth
                     s(long, lat) + 
                     #Spatial proximity
                     s(dist_statecapital_km, state_namef, bs='fs', m=1) + 
                     #Time
                     s(year, cover_group, bs='fs', m=1) +
                     #s(year, by = state_namef) +
                     s(yearf, bs = "re") +
                     #Random 
                     s(state_namef, bs="re") + 
                     s(muni_factor, bs="re")+ 
                     s(cover_group, bs="re"), 
                   #AR1 residual errors
                   rho=0.843, AR.start = dfgam_matched_model$start_event, 
                   family=Tweedie(1.337),
                   method = "fREML",
                   discrete = TRUE,
                   data = dfgam_matched_model, 
                   control = myctrl)   
summary(bam_loss_02)
plot(bam_loss_02, scale = 0, all.terms = TRUE)


#4 other essentials to a standard of living
df_muni <- read_excel("data//bla_municipalities.xlsx", 
                      na = c("", "NA"),
                      sheet = "municipality_fixed_ref",
                      .name_repair = "universal")
df_muni %>% 
  right_join(dfmatched %>% 
               select(state_name, muni_name, trees)) -> df_muni_matched
df_muni_matched$cover_group <- as.factor(df_muni_matched$trees)
levels(df_muni_matched$cover_group) <- c("forest cover <=40%", 
                                         "forest cover >=60%", 
                                         "forest cover >=60%\nwith loss")

#sanitation
df_muni_matched %>% 
  ggplot(aes(x = cover_group, y = flag_sanitation_plan)) + 
  geom_violin() +
  scale_y_continuous("sanitation plan approved", 
                     breaks = c(0,1), labels = c("no", "yes")) +
  geom_jitter(height = 0.03, width=0.3) + 
  labs(title = "(A)") +
  theme(plot.title.position = "plot", 
        axis.title.x = element_blank()) -> fig_essential_sani
fig_essential_sani
#internet
df_muni_matched %>% 
  ggplot(aes(x = cover_group, y = flag_int_complete_cover)) + 
  geom_violin() + 
  scale_y_continuous("internet connection", 
                     breaks = c(0,1), labels = c("no", "yes")) +
  geom_jitter(height = 0.03, width=0.3) +
  labs(title = "(B)") +
  theme(plot.title.position = "plot", 
        axis.title.x = element_blank()) -> fig_essential_inter
fig_essential_inter

#Export
png(file = "figures//fig_essentials_matched.png", 
    bg = "white", type = c("cairo"), 
    width=3000, height=2800, res = 600)
grid.arrange(fig_essential_sani, fig_essential_inter, nrow = 2)
dev.off()

#Supplemental material
#correlations with time varying covariates
#Pairs panel with human readable names
pairs_vars_gdp <- c('log_gdp_percapita_reais', 'tot_loss_km2')
dfgam %>%
  select(all_of(pairs_vars_gdp)) %>%
  rename(GDP = log_gdp_percapita_reais, forest_loss = tot_loss_km2) %>%
  psych::pairs.panels()

pairs_vars_salary <- c('min_salary_mean', 'tot_loss_km2')
dfgam %>%
  select(all_of(pairs_vars_salary)) %>%
  rename(salary = min_salary_mean, forest_loss = tot_loss_km2) %>%
  filter(!is.na(salary)) %>%
  psych::pairs.panels()


library(Hmisc)
png(file = "figures//fig_back2back.png", 
    bg = "white", type = c("cairo"), 
    width=1500, height=7000, res = 600)
par(mfrow = c(6, 1))
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
dev.off()

#loss
png(file = "figures//fig_back2backloss.png", 
    bg = "white", type = c("cairo"), 
    width=1500, height=7000, res = 600)
par(mfrow = c(6, 1))
histbackback(df_muni_cover40$muni_area_km2, 
             df_muni_cover60less$muni_area_km2, probability=TRUE, 
             xlab = c("<=40",">=60 loss"), main = "municipality size (km2)")
histbackback(df_muni_cover40$dist_statecapital_km, 
             df_muni_cover60less$dist_statecapital_km, probability=TRUE, 
             xlab = c("<=40",">=60 loss"), main = "distance to state capital (km)")
histbackback(df_muni_cover40$median_pop_dens, 
             df_muni_cover60less$median_pop_dens, probability=TRUE, 
             xlab = c("<=40",">=60 loss"), main = "population density")
histbackback(df_muni_cover40$indigenous_area_percent, 
             df_muni_cover60less$indigenous_area_percent, probability=TRUE, 
             xlab = c("<=40",">=60 loss"), main = "indigenous area (%)")
histbackback(df_muni_cover40$median_industry, 
             df_muni_cover60less$median_industry, probability=TRUE, 
             xlab = c("<=40",">=60 loss"), main = "industry  value contribution (%)")
histbackback(df_muni_cover40$tot_forest_cover_2019_percent, 
             df_muni_cover60less$tot_forest_cover_2019_percent, probability=TRUE, 
             xlab = c("<=40",">=60 loss"), main = "forest cover 2019 (%)") 
dev.off()