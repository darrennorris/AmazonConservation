#gdp GAM models
#packages
library(tidyverse)
library(mgcv)
library(stringi)
library(timetk)

memory.limit(30000)#needed to speed up models and run gam.check

#Uses dfgam from "gdp_analysis.R"
dfgam <- readRDS("dfgam.rds")
plot(dfgam$gva_industry_percent, dfgam$gdp_percapita_reais)

#GAMM AR ...
dfgam$muni_factor <- paste(dfgam$state_name,dfgam$muni_name, sep = "_")
dfgam$muni_factor <- as.factor(dfgam$muni_factor)
ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B", 
             maxIter = 99, msMaxIter = 99, keepData = TRUE)
model_01_ar1 <- gamm(log(gdp_percapita_reais) ~ year*flag_urbanf +
                       pres_groupf + 
                       s(year, by = state_namef, k=5, m=1, bs="tp") +
                       s(gva_agri_percapita_reais) + 
                       s(gva_industry_percent) +
                       s(pop_dens_km2) +
                       s(tot_loss5y_percent) +
                       s(school_per1000) + 
                       s(pg_per1000) + 
                       s(dist_statecapital_km, by = state_namef) + 
                       s(state_namef, bs="re"), 
                     correlation = corARMA(form = ~ year|muni_factor, p = 1), 
                     data = dfgam, 
                     method="REML", 
                     control = ctrl)
saveRDS(model_01_ar1, "model_01_ar1.rds")
model_01_ar1 <- readRDS("model_01_ar1.rds")
summary(model_01_ar1$lme)
anova(model_01_ar1$lme)
summary(model_01_ar1$gam) #r2 = 0.925
gam.check(model_01_ar1$gam) #problem with residual > 1

#Residuals
#AR1
res_gamm_ar1_lme <- resid(model_01_ar1$lme, type = "normalized")
res_gamm_ar1_gam <- resid(model_01_ar1$gam, type = "deviance")
hist(res_gamm_ar1_lme) #problem with residual > 10
hist(res_gamm_ar1_gam) #problem with residual > 0.6
df_ar1 <- model_01_ar1$lme$data[,1:13] %>% 
  separate(muni_factor, into = c("state_name", "muni_name"), 
           sep = "_", remove = FALSE)
df_ar1$m01_res_gamm_ar1_lme <- res_gamm_ar1_lme
df_ar1$m01_res_gamm_ar1_gam <- res_gamm_ar1_gam


#log(gdp) 8.9, 9, 10, 11, 12
#Maranhão_Santo Antônio dos Lopes lme residual 21.77
df_ar1 %>% filter(m01_res_gamm_ar1_lme > 10) %>% 
  pull(m01_res_gamm_ar1_lme) %>% length() #4
df_ar1 %>% filter(m01_res_gamm_ar1_gam > 1) %>% 
  pull(m01_res_gamm_ar1_gam) %>% length() # 28
df_ar1 %>% filter(m01_res_gamm_ar1_gam > 1) %>% 
  arrange(desc(m01_res_gamm_ar1_gam))
#summary of high residual
df_ar1 %>% filter(m01_res_gamm_ar1_gam > 1) %>% 
  group_by(state_name) %>% 
  summarise(count_high = n(), 
            count_muni_high = length(unique(muni_name)), 
            max_high = max(m01_res_gamm_ar1_gam),
            median_high = median(m01_res_gamm_ar1_gam)) %>% 
  arrange(desc(median_high))

# Temporal autocorrelation
# Only 4 with less than 18 years. Mojuí dos Campos fewest (7 years).
df_ar1 %>% 
  group_by(state_name, muni_name) %>% 
  summarise(year_count = length(unique(year)), 
            year_min = min(year), 
            year_max = max(year)) %>% 
  arrange(year_count)

#Temporal autocorrelation
df_ar1 %>%
  group_by(state_namef, dist_statecapital_km) %>%
  tk_acf_diagnostics(
    .date_var = year,
    .value = m01_res_gamm_ar1_lme, 
    .lags = 11
  ) -> tidy_acf

#export as .png  250 * 1000
tidy_acf %>% 
  ggplot(aes(x = lag, y = ACF, color = state_namef, 
             group = state_namef)) +
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
  facet_wrap(~ state_namef, ncol = 1) +
  # Aesthetics
  expand_limits(y = c(-1, 1)) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) + labs(
    title = "AutoCorrelation (ACF)",
    subtitle = "GAMM AR(1) residuals", 
    x = "lag (year)"
  )

#Spatial pattern in residuals
#AR1

#Map with polygons
sf_ninestate_muni %>% left_join(
  df_ar1 %>%
    group_by(state_name, muni_name) %>% 
    summarise(median_resid = median(m01_res_gamm_ar1_lme), 
              sd_resid = sd(m01_res_gamm_ar1_lme)) %>% 
    left_join(dfstates, 
              by = c("state_name" = "bla_state_names")), 
  by = c("SIGLA_UF"="bla_state_siglas" ,"NM_MUN"="muni_name")
) %>% 
  filter(!is.na(median_resid)) %>%
  ggplot() + geom_sf(aes(fill = median_resid)) + 
  scale_fill_viridis_c("residual")

#Spatial autocorrelation
#Distance matrix from locations of the mayors office
##763
bla_city %>% left_join(data.frame(sf_ninestate_muni) %>% 
                         select(!geometry), 
                       by = c("CD_GEOCODM"="CD_MUN")) %>% 
  right_join(df_ar1 %>%
               group_by(state_name, muni_name) %>% 
               summarise(median_resid = median(m01_res_gamm_ar1_lme), 
                         sd_resid = sd(m01_res_gamm_ar1_lme)) %>% 
               left_join(dfstates, 
                         by = c("state_name" = "bla_state_names")), 
             by = c("SIGLA_UF" = "bla_state_siglas", "NM_MUN" = "muni_name")) %>% 
  st_transform(crs=3395)-> sf_point_residuals

library(geoR)
coords<-matrix(0,nrow(sf_point_residuals),2) 
coords[,1]<-st_coordinates(sf_point_residuals)[,'X'] 
coords[,2]<-st_coordinates(sf_point_residuals)[,'Y']   
#Median
gb<-list(data=sf_point_residuals$median_resid, coords=coords)
myvar <- variog(gb,max.dist = 1000000)
mye <- variog.mc.env(gb, obj.var = myvar)
#scale to km
mye$u <- mye$u / 1000
myvar$u <- myvar$u/1000
myvar$bins.lim <- myvar$bins.lim/1000
myvar$max.dist <- myvar$max.dist/1000
myvar$uvec <- myvar$uvec/1000
plot(myvar, var.lines=TRUE, envelope.obj = mye, xlab = "distance (km)")

#SD
gb<-list(data=sf_point_residuals$sd_resid, coords=coords)
myvar <- variog(gb, max.dist = 1000000)
mye <- variog.mc.env(gb, obj.variog = myvar)
#scale to km
mye$u <- mye$u / 1000
myvar$u <- myvar$u/1000
myvar$bins.lim <- myvar$bins.lim/1000
myvar$max.dist <- myvar$max.dist/1000
myvar$uvec <- myvar$uvec/1000
plot(myvar, var.lines=TRUE, envelope.obj = mye, xlab = "distance (km)")
