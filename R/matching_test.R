#test matching
library(tidyverse)
library(readxl)
library(gridExtra)
library(scales)
library(psych)
library(sf)

df_muni <- read_excel("data//bla_municipalities.xlsx", 
                      na = c("", "NA"),
                      sheet = "municipality_fixed_ref",
                      .name_repair = "universal")

df_muni_year <- read_excel("data//bla_municipalities.xlsx", 
                           na = c("", "NA"),
                           sheet = "municipality_annual",
                           .name_repair = "universal")
 #Priority or monitoring before 2017. 73 included between 2008 - 2012
df_muni%>% 
  filter(!is.na(forestcover_1985med_km2), dist_statecapital_km > 0, 
         !is.na(first_year)) %>% 
  summarise(count_state = length(unique(state_name)), 
            count_muni = length(unique(muni_name)), 
            min_included = min(first_year),
            max_included = max(first_year),
            maxcov_2002_per = max(forestcover_2002_percent_85),
            cov_2002_per = median(forestcover_2002_percent_85), 
            cov_2012_per = median(forestcover_2012_percent_85), 
            cov_2019_per = median(forestcover_2019_percent_85)) %>% data.frame()
#count_state count_muni min_included max_included maxcov_2002_per cov_2002_per
#1           8         91         2008         2021        100.3787     90.47293
#cov_2012_per cov_2019_per
#1     81.86155     77.94102

#Simple criteria to start
df_muni%>% 
  filter(!is.na(forestcover_1985med_km2), dist_statecapital_km > 0, 
         !is.na(first_year)) %>% 
  pull(dist_statecapital_km) %>% max() -> ref_dist

df_muni%>% 
  filter(!is.na(forestcover_1985med_km2), dist_statecapital_km > 0, 
         !is.na(first_year)) %>% 
  pull(indigenous_area_percent) %>% summary()

df_muni%>% 
  filter(!is.na(forestcover_1985med_km2), dist_statecapital_km > 0, 
         !is.na(first_year)) %>% 
  pull(state_name) %>% unique() -> ref_states

#Municipalites created during the time
df_muni_year %>% 
  filter(is.na(gdp_percapita_reais)) %>% pull(muni_name) %>% unique() -> new_muni

# 240 municipalites
df_muni%>% 
  filter(!is.na(forestcover_1985med_km2), dist_statecapital_km > 0, 
         is.na(first_year), forestcover_2002_percent_85 >77, 
         !is.na(gdp_percapita_reais_2019), 
         ! (muni_name %in% all_of(new_muni)),
         forestcover_2019_percent_85 >77, 
         indigenous_area_percent < 25,
         state_name %in% all_of(ref_states), 
         dist_statecapital_km < ref_dist) %>% 
  summarise(count_state = length(unique(state_name)), 
            count_muni = length(unique(muni_name)), 
            maxcov_2002_per = max(forestcover_2002_percent_85),
            cov_2002_per = median(forestcover_2002_percent_85), 
            cov_2012_per = median(forestcover_2012_percent_85), 
            cov_2019_per = median(forestcover_2019_percent_85)) %>% data.frame()
#count_state count_muni maxcov_2002_per cov_2002_per cov_2012_per cov_2019_per
#1           8        241         133.136       97.447     93.11857     89.83285


# Anuual patterns parallel?
df_muni_year %>% 
  filter(!is.na(forestcover_1985med_km2), dist_statecapital_km > 0, 
         !is.na(first_year), year <2013) %>% 
  ggplot(aes(x=year, y = gdp_percapita_reais)) + 
  geom_point() + 
  geom_line(aes(group=muni_name), linetype = "dashed") + 
  stat_smooth(method = "gam", aes(group= state_name), 
              colour = "white", size=2, se = FALSE) +
  stat_smooth(method = "gam", aes(colour = state_name), se = FALSE) + 
  scale_x_continuous(breaks = c(2002, 2004, 2006, 2008, 2010, 2012)) +
  scale_color_viridis_d("state") +
  labs(title = "GDP per capita", 
       subtitle = "Priority monitoriing list (n = 91)",
       y = "GDP per capita R$") + 
  theme(plot.title.position = "plot") -> fig_gdp_priority
fig_gdp_priority

df_muni_year %>% 
  filter(!is.na(forestcover_1985med_km2), dist_statecapital_km > 0, 
         is.na(first_year), forestcover_2002_percent_85 >77, 
         year < 2013, 
         ! (muni_name %in% all_of(new_muni)),
         indigenous_area_percent < 25,
         forestcover_2019_percent_85 >77, 
         state_name %in% all_of(ref_states), 
         dist_statecapital_km < ref_dist) %>% 
  ggplot(aes(x=year, y = gdp_percapita_reais)) + 
  geom_point() + 
  geom_line(aes(group=muni_name), linetype = "dashed") + 
  stat_smooth(method = "gam", aes(group= state_name), 
              colour = "white", size=2, se = FALSE) +
  stat_smooth(method = "gam", aes(colour = state_name), se = FALSE) + 
  scale_x_continuous(breaks = c(2002, 2004, 2006, 2008, 2010, 2012)) +
  scale_color_viridis_d("state") +
  labs(title = "GDP per capita", 
       subtitle = "Not priority monitoriing list (n = 241)",
       y = "GDP per capita R$") + 
  theme(plot.title.position = "plot") -> fig_gdp_notpriority
fig_gdp_notpriority

# By state
df_muni_year %>% 
  filter(!is.na(forestcover_1985med_km2), dist_statecapital_km > 0, 
         !is.na(first_year), 
         #first_year < 2017, 
         year <2013) %>% 
  ggplot(aes(x=year, y = gdp_percapita_reais)) + 
  geom_point() + 
  geom_line(aes(group=muni_name), linetype = "dashed") + 
  stat_smooth(method = "gam", aes(group= state_name), 
              colour = "white", size=2, se = FALSE) +
  stat_smooth(method = "gam", se = FALSE) + 
  scale_x_continuous(breaks = c(2002, 2004, 2006, 2008, 2010, 2012)) + 
  scale_y_continuous(labels = scales::unit_format(unit = "K", 
                                                  scale = 1e-3)) +
  scale_color_viridis_d("state") +
  facet_wrap(~state_name, ncol = 1, scales = "free_y") +
  labs(title = "GDP per capita", 
       subtitle = "Priority monitoriing list (n = 91)",
       y = "GDP per capita R$") + 
  theme(plot.title.position = "plot", 
        legend.position= "none") -> fig_gdp_priority_long
fig_gdp_priority_long


df_muni_year %>% 
  filter(!is.na(forestcover_1985med_km2), dist_statecapital_km > 0, 
         is.na(first_year), forestcover_2002_percent_85 >77, 
         year < 2013, 
         ! (muni_name %in% all_of(new_muni)),
         indigenous_area_percent < 25,
         forestcover_2019_percent_85 >77, 
         state_name %in% all_of(ref_states), 
         dist_statecapital_km < ref_dist) %>% 
  ggplot(aes(x=year, y = gdp_percapita_reais)) + 
  geom_point() + 
  geom_line(aes(group=muni_name), linetype = "dashed") + 
  stat_smooth(method = "gam", aes(group= state_name), 
              colour = "white", size=2, se = FALSE) +
  stat_smooth(method = "gam", se = FALSE) + 
  scale_x_continuous(breaks = c(2002, 2004, 2006, 2008, 2010, 2012)) + 
  scale_y_continuous(labels = scales::unit_format(unit = "K", 
                                                  scale = 1e-3)) +
  scale_color_viridis_d("state") +
  facet_wrap(~state_name, ncol = 1, scales = "free_y") +
  labs(title = "GDP per capita", 
       subtitle = "Not priority monitoriing list (n = 240)",
       y = "GDP per capita R$") + 
  theme(plot.title.position = "plot", 
        legend.position= "none") -> fig_gdp_notpriority_long
fig_gdp_notpriority_long

#Export
png(file = "figures//fig_test_match.png", 
    bg = "white", type = c("cairo"), 
    width=5000, height=10000, res = 600)
gridExtra::grid.arrange(fig_gdp_priority_long, 
                        fig_gdp_notpriority_long, ncol = 2)
dev.off()

pdf(file = "figures//fig_test_match.pdf", 
    width=8, height=15)
gridExtra::grid.arrange(fig_gdp_priority_long, 
                        fig_gdp_notpriority_long, ncol = 2)
dev.off()


