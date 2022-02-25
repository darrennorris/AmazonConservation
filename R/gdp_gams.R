#gdp GAM models
#packages
library(tidyverse)
library(mgcv)
library(stringi)
library(timetk)

memory.limit(30000)#needed to speed up models and run gam.check

#Uses dfgam from "gdp_analysis.R"
dfgam <- readRDS("dfgam.rds")

#GAMM AR ...
dfgam$muni_factor <- paste(dfgam$state_name,dfgam$muni_name, sep = "_")
dfgam$muni_factor <- as.factor(dfgam$muni_factor)
ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B", 
             maxIter = 99, msMaxIter = 99, keepData = TRUE)
model_01_ar1 <- gamm(log(gdp_percapita_reais) ~ year*flag_urbanf +
                       pres_groupf + 
                       s(year, by = state_namef, k=5, m=1, bs="tp") +
                       s(gva_agri_percapita_reais) + 
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
summary(model_01_ar1$gam) #r2 = 0.833
gam.check(model_01_ar1$gam) #problem with residual > 1

#Residuals
#AR1
res_gamm_ar1_lme <- resid(model_01_ar1$lme, type = "normalized")
res_gamm_ar1_gam <- resid(model_01_ar1$gam, type = "deviance")
hist(res_gamm_ar1_lme) #problem with residual > 10
hist(res_gamm_ar1_gam) #problem with residual > 1
df_ar1 <- model_01_ar1$lme$data[,1:13] %>% 
  separate(muni_factor, into = c("state_name", "muni_name"), 
           sep = "_", remove = FALSE)
df_ar1$m01_res_gamm_ar1_lme <- res_gamm_ar1_lme
df_ar1$m01_res_gamm_ar1_gam <- res_gamm_ar1_gam
hist(df_ar1$`log(gdp_percapita_reais)`)

#log(gdp) 8.9, 9, 10, 11, 12
#Maranhão_Santo Antônio dos Lopes lme residual 21.77
df_ar1 %>% filter(m01_res_gamm_ar1_lme > 10) %>% 
  pull(m01_res_gamm_ar1_lme) %>% length() #8
df_ar1 %>% filter(m01_res_gamm_ar1_gam > 1) %>% 
  pull(m01_res_gamm_ar1_gam) %>% length() # 248
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
    title = "Partial AutoCorrelation (PACF)",
    subtitle = "GAMM AR(1) residuals", 
    x = "lag (year)"
  )

