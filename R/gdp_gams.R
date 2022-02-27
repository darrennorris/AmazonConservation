#gdp GAM models
#packages
library(tidyverse)
library(mgcv)
library(stringi)
library(timetk)
library(gratia)

#extra memory to speed up models, pairs panel and  gam.check
memory.limit(30000)

#Uses dfgam from "gdp_analysis.R"
dfgam <- readRDS("dfgam.rds") #13710 obs. 38 vars
plot(dfgam$gva_industry_percent, dfgam$gdp_percapita_reais)
#length(unique(dfgam$muni_factor)) #763 municipalities
# 4956340 km2
#dfgam %>% group_by(state_name, muni_name) %>% 
#  summarise(area_km2 = max(muni_area_km2)) %>% pull(area_km2) %>% sum()

#correlations with time varying covariates
#names(dfgam)
psych::pairs.panels(dfgam[, c('gdp_percapita_reais',
                              'gva_agri_percapita_reais', 
                              'gva_industry_percent', 
                              'pop_dens_km2', 
                              'tot_loss5y_percent', 
                              'school_per1000', 
                              'pg_per1000')])

#GAMM ...
#without AR
model_01 <- gamm(log(gdp_percapita_reais) ~ year*main_sectorf +
                       pres_groupf + 
                       s(year, by = state_namef, k=5, m=1, bs="tp") +
                       s(gva_agri_percapita_reais, by =main_sectorf) + 
                       s(gva_industry_percent, by =main_sectorf) +
                       s(pop_dens_km2) +
                       s(tot_loss5y_percent, by =main_sectorf) +
                       s(school_per1000) + 
                       s(pg_per1000) + 
                       s(dist_statecapital_km, by = state_namef) + 
                       s(state_namef, bs="re"), 
                     data = dfgam, 
                     method="REML")
hist(resid(model_01$gam, type = "deviance"))
summary(model_01$gam) #r2 95.9. everything significant!
appraise(model_01$gam)
plot(model_01$gam, scale = 0, all.terms = TRUE)
saveRDS(model_01, "model_01.rds")
model_01 <- readRDS("model_01.rds")

#with AR
ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B", 
             maxIter = 99, msMaxIter = 99, keepData = TRUE)
model_01_ar1 <- gamm(log(gdp_percapita_reais) ~ year*main_sectorf +
                       pres_groupf + 
                       s(year, by = state_namef, k=5, m=1, bs="tp") +
                       s(gva_agri_percapita_reais, by =main_sectorf) + 
                       s(gva_industry_percent) +
                       s(pop_dens_km2) +
                       s(tot_loss5y_percent, by =main_sectorf) +
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
summary(model_01_ar1$lme) #check correlation structure
summary(model_01_ar1$gam) #r2 = 0.925
anova(model_01$lme, model_01_ar1$lme)

gam.check(model_01_ar1$gam) #problem with residual > 1
appraise(model_01_ar1$gam)

# Below not working
# Variance Inflation Factor https://github.com/samclifford/mgcv.helper/blob/master/R/vif.gam.R
 vif.gam <- function(object){
  
  obj.sum <- mgcv::summary.gam(object)
  
  s2 <- object$sig2 # estimate of standard deviation of residuals
  X <- object$model # data used to fit the model
  n <- nrow(X) # how many observations were used in fitting?
  v <- -1 # omit the intercept term, it can't inflate variance
  varbeta <- obj.sum$p.table[v,2]^2 # variance in estimates
  selected_col <- row.names(obj.sum$p.table)[v]
  selected_col <- gsub("TRUE", "", selected_col)
  varXj <- apply(X=X[, selected_col],MARGIN=2, var) # variance of all the explanatory variables
  VIF <- varbeta/(s2/(n-1)*1/varXj) # the variance inflation factor, obtained by rearranging
  # var(beta_j) = s^2/(n-1) * 1/var(X_j) * VIF_j
  
  VIF.df <- tibble::tibble(variable=names(VIF),
                           vif=VIF)
  
  return(VIF.df)
}
# vif.gam(model_01_ar1$gam) #Error in `[.data.frame`(X, , selected_col) : undefined columns selected
#mod01 <- model_01_ar1$gam
#vif.gam(mod01) #Error in `[.data.frame`(X, , selected_col) : undefined columns selected

#Residuals
#AR1
res_gamm_ar1_lme <- resid(model_01_ar1$lme, type = "normalized")
res_gamm_ar1_gam <- resid(model_01_ar1$gam, type = "deviance")
hist(res_gamm_ar1_lme) #problem with residual > 10
hist(res_gamm_ar1_gam) #problem with residual > 1
df_ar1 <- model_01_ar1$lme$data[,1:15] %>% 
  separate(muni_factor, into = c("state_name", "muni_name"), 
           sep = "_", remove = FALSE)
df_ar1$m01_res_gamm_ar1_lme <- res_gamm_ar1_lme
df_ar1$m01_res_gamm_ar1_gam <- res_gamm_ar1_gam

#log(gdp) 8.9, 9, 10, 11, 12
#Pará_Jacareacanga, Maranhão_Davinópolis, Santo Antônio dos Lopes 
df_ar1 %>% filter(m01_res_gamm_ar1_lme > 10) %>% 
  pull(m01_res_gamm_ar1_lme) %>% length() #4
df_ar1 %>% filter(m01_res_gamm_ar1_lme > 10) %>% 
  arrange(desc(m01_res_gamm_ar1_lme))
#GAm residuals
#Maranhão_Davinópolis, Santo Antônio dos Lopes, 
#Pará_Canaã dos Carajás, Barcarena
df_ar1 %>% filter(m01_res_gamm_ar1_gam > 1) %>% 
  pull(m01_res_gamm_ar1_gam) %>% length() # 24
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
