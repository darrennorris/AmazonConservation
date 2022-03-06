#gdp GAM models
#packages
library(tidyverse)
library(mgcv)
library(stringi)
library(timetk)
library(gratia)
library(sf)

#guides
#bam
#https://jacolienvanrij.com/Tutorials/GAMM.html#model-terms-partial-effects
#http://jacolienvanrij.com/PupilAnalysis/SupplementaryMaterials-2.html

#extra memory to speed up models, pairs panel, gam.check etc
memory.limit(80000)

#Uses dfgam from "gdp_analysis.R"
dfgam <- readRDS("dfgam.rds") #13710 obs. 55 vars
dfgam$log_gdp_percapita_reais <- log(dfgam$gdp_percapita_reais)
#include start for AR.start
dfgam %>% 
arrange(muni_factor, year) %>% 
  group_by(muni_factor) %>%
  mutate(start_year = min(year)) %>% 
  mutate(start_event = year== start_year) %>% 
  ungroup() -> dfgam

#random smooths adjust the trend of a numeric predictor 
#in a nonlinear way: s(Time, Subject, bs="fs", m=1).
myctrl <- list(keepData = TRUE, trace = TRUE)  
bam_000 <- bam(log_gdp_percapita_reais~ 
                #spatial smooth
                s(long, lat) + 
                #Spatial proximity
                s(dist_statecapital_km, by = state_namef) +
                #random temporal smooth. 3.2 GB
                #s(year, muni_namef, bs='fs', m=1) + 
                #time varying covariates
                s(tot_loss5y_percent) +
                s(school_per1000) +
                s(process_gold_p1000) +
                s(gva_agri_percapita_reais), 
              #AR1 residual errors
              rho=0.9, AR.start = dfgam$start_event,
              method = "fREML",
              discrete = TRUE,
              data = dfgam, 
              control = myctrl)   
res_bam_ar1_000 <- resid(bam_000, type = "deviance")
hist(res_bam_ar1_000) #
summary(bam_000) #0.618
plot(bam_000, scale = 0, all.terms = TRUE)
saveRDS(bam_000, "bam_000.rds")
bam_000 <- readRDS("bam_000.rds")

#residals not great for below
bam_00 <- bam(log_gdp_percapita_reais~ 
                #spatial smooth
                s(long, lat) + 
                #Spatial proximity
                s(dist_statecapital_km, by = state_namef) +
                #random temporal smooth. 3.2 GB
                s(year, muni_namef, bs='fs', m=1) + 
                #time varying covariates
                s(tot_loss5y_percent, by  = state_namef) +
                s(school_per1000, by  = state_namef) +
                s(process_gold_p1000) +
                s(gva_agri_percapita_reais, by  = state_namef), 
              #AR1 residual errors
              rho=0.9, AR.start = dfgam$start_event,
              method = "fREML",
              discrete = TRUE,
              data = dfgam, 
              control = myctrl)         
hist(resid(bam_00, type = "deviance")) #
summary(bam_00) #0.618
plot(bam_00, scale = 0, all.terms = TRUE)
saveRDS(bam_00, "bam_00.rds")
bam_00 <- readRDS("bam_00.rds")

dfgam$res_bam_ar1 <- resid(bam_00, type = "deviance")
#Temporal autocorrelation
dfgam %>%
  group_by(state_namef, dist_statecapital_km) %>%
  tk_acf_diagnostics(
    .date_var = year,
    .value = res_bam_ar1, 
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

tidy_acf %>% 
  ggplot(aes(x = lag, y = PACF, color = state_namef, 
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
    title = "Partial ACF (PACF)",
    subtitle = "GAMM AR(1) residuals", 
    x = "lag (year)"
  )

#Spatial autocorrelation
#Semivariograms
#Distance matrix from locations of the mayors office
##763
#City points
ibge_city <- "vector//brazil_cities//BR_Localidades_2010_v1.shp"
sf_city <- st_read(ibge_city, options = "ENCODING=WINDOWS-1252") %>% 
  filter(NM_CATEGOR == "CIDADE", CD_GEOCODM %in% all_of(sf_ninestate_muni$CD_MUN))
moji <- data.frame(CD_GEOCODM = "1504752", NM_MUNICIP = "Mojuí dos Campos", 
                   LONG = -54.6431, LAT = -2.68472, ALT = 84)
pt1 <- st_point(c(-54.6431, -2.68472))
moji$geometry <- st_sfc(pt1)
sf_moji <- st_as_sf(moji)
#Add moji missing from 2010 data
sf_city %>% select(CD_GEOCODM, NM_MUNICIP, LONG, LAT, ALT) %>% 
  bind_rows(sf_moji) -> bla_city

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

#Below nnotes and testing
#Include spatial and temporal autocorrelation
#get tweedie p. takes few minutes even for simple example
#gam_null_tw <- gam(log(gdp_percapita_reais) ~ 1,
#                   family = tw,
#                  data = dfgam)
#gam_null_tw$family$family #p=1.99
#appraise for simle tweedie bam gobbles all memory and locks
#bam_01 <- bam(log_gdp_percapita_reais~ s(gva_agri_percapita_reais), 
#              family=Tweedie(1.99), 
#              method = "REML",
#              data = dfgam)
#hist(resid(bam_01, type = "deviance"))
#summary(bam_01) #0.563
#plot(bam_01, scale = 0, all.terms = TRUE)
#appraise(bam_01)