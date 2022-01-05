#Forest loss

#packages
library(tidyverse)
library(readxl)

#State level
#GDP (IBGE 4 january 2022 "Especiais" com dados apresentados no informativo
# https://biblioteca.ibge.gov.br/index.php/biblioteca-catalogo?view=detalhes&id=2101873
# https://biblioteca.ibge.gov.br/visualizacao/livros/liv101873_informativo.pdf
#excel files
# https://ftp.ibge.gov.br/Contas_Regionais/2019/xls/Especiais_2010_2019_xls.zip)

df_gdp_state <- read_excel("data//bla_gdp2002_2019.xlsx", 
           .name_repair = "universal") %>% 
  pivot_longer(!uf, names_to = "ayear", names_prefix ="...", 
               values_to = "gdp_reais_millions") %>% 
  mutate(ayear = as.numeric(ayear))
#Population (from ""ibge_sidrar_download.R)
df_pop_state <- read_excel("data//bla_state_pop_1991_2021.xlsx", 
                     .name_repair = "universal")
df_gdp_state %>% right_join(df_pop_state) %>% 
  mutate(gdp_per_capita = (gdp_reais_millions * 1000000) / total_pop) -> df_state_gdp

# GDP per capita across the Brazilian Legal Amazon
# Conversion to US$ based on 2019 annual rate:
# https://www.irs.gov/individuals/international-taxpayers/yearly-average-currency-exchange-rates
df_state_gdp %>% filter(!is.na(gdp_per_capita)) %>%
  group_by(ayear) %>% 
  summarise(tot_pop = sum(total_pop, na.rm = TRUE), 
            tot_gdp = sum(gdp_reais_millions, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(gdp_per_capita_reais = (tot_gdp * 1000000) / tot_pop) %>% 
  mutate(gdp_per_capita_usd = gdp_per_capita_reais / 3.946) -> df_bla_gdp
df_bla_gdp %>% pull(gdp_per_capita_reais)%>% max() -> gdp_per_capita_reais_max 
#[1] 22762.05
df_bla_gdp %>% pull(gdp_per_capita_usd)%>% max() -> gdp_per_capita_usd_max 
#[1] 5768.387

#INPE
dfinpe_forestloss <- read_excel("data//INPE_Amazon_Cerrado.xlsx", sheet = "Amazon-total", 
                              .name_repair = "universal") %>% 
  mutate(biome = "Amazon", 
         area_km2 = `area.km²`) %>% 
  select(biome, year, area_km2) %>% 
  rbind(read_excel("data//INPE_Amazon_Cerrado.xlsx", sheet = "Cerrado-State", 
           .name_repair = "universal") %>% 
  filter(uf %in% c("MATO GROSSO", "TOCANTINS", "MARANHÃO", "RONDÔNIA")) %>% 
  group_by(year) %>% 
  summarise(`area.km²` = sum(`area.km²`)) %>% 
  mutate(biome = "Cerrado", 
         area_km2 = `area.km²`) %>% 
  select(biome, year, area_km2))
dfinpe_forestloss %>% group_by(year) %>% 
  summarise(area = sum(area_km2, na.rm=TRUE)) %>% pull(area)%>% max() -> forestloss_max
# 40340.38 km2


axis_trans <- forestloss_max / gdp_per_capita_usd_max
df_forestloss_labels <- data.frame(year = c(2004, 2019), 
                                   yaxis_value = forestloss_max +(forestloss_max*0.05),
                                   label_values = c(round(forestloss_max,0), 
                                                     round(gdp_per_capita_usd_max,0)))
df_forestloss_labels
mycols <- c("Amazon" = "darkorchid1", "Cerrado" = "darkmagenta")
dfinpe_forestloss %>% 
  filter(year >1999) %>%
  ggplot(aes(x=year, y=area_km2)) + 
  geom_col(aes(fill = biome), colour="blue") + 
  geom_line(data = df_bla_gdp, 
            aes(x=ayear, y=gdp_per_capita_usd*axis_trans), size=2, color="black") + 
  scale_y_continuous(#for the second y-axis
  sec.axis = sec_axis(~./axis_trans,#divided by transformation rate, in order to be represented based on the first y-axis
                      name = "GDP per capita (US$)")) + 
  scale_x_continuous(limits = c(1999.5, 2020.5))  + 
  scale_fill_manual("deforestation per biome:", values = mycols) + 
  geom_label(data = df_forestloss_labels, 
             aes(x= year, y = yaxis_value, label = label_values), 
             colour = c("blue", "black")) + 
  labs(y = bquote('deforestation'~(km^2)))  +
  theme(legend.position = "top", legend.box = "horizontal") -> fig_deforestation_gdp
fig_deforestation_gdp

png(file = "figures//figure_deforestation.png", bg = "white", type = c("cairo"), 
    width=5000, height=3000, res = 600)
fig_deforestation_gdp + theme(text = element_text(size = 16))
dev.off()

bla_states <- c("Acre", "Amapá", "Amazonas", "Maranhão", 
                "Mato Grosso", "Pará", "Tocantins", "Rondônia", "Roraima")
#Hansen
dfhansen_forestloss_primary <- read_excel("data//brazil_gfw_loss_2021_hansen.xlsx", 
                                  sheet = "primary_loss_admin1", 
                                .name_repair = "universal") %>% 
  filter(admin1 %in% bla_states)
dfhansen_coverloss <- read_excel("data//brazil_gfw_loss_2021_hansen.xlsx", 
                                          sheet = "tree_cover_loss_admin1", 
                                          .name_repair = "universal") %>% 
  filter(admin1 %in% bla_states)


#Long format
dfhansen_forestloss_primary[,-c(1,2,4)] %>% 
  pivot_longer(!admin1, names_to = "ayear", names_prefix ="...", 
                            values_to = "primary_loss") %>% 
                 mutate(ayear = as.numeric(ayear)) %>% right_join(
dfhansen_coverloss[,-c(1,2,4)] %>% 
  pivot_longer(!admin1, names_to = "ayear", names_prefix ="...", 
               values_to = "total_forestcover_loss") %>% 
  mutate(ayear = as.numeric(ayear))
                 ) %>% 
  mutate(forestcover_loss = total_forestcover_loss - primary_loss) %>% 
  group_by(ayear) %>% 
  summarise(total_forestcover_loss_km2 = sum(total_forestcover_loss, na.rm = TRUE)/100, 
            primary_loss_km2 = sum(primary_loss, na.rm = TRUE)/100, 
            forestcover_loss_km2 = sum(forestcover_loss, na.rm = TRUE)/100) %>% 
  mutate(forestcover_loss_km2 = if_else(forestcover_loss_km2==0, total_forestcover_loss_km2, 
                                        forestcover_loss_km2)) %>% 
  pivot_longer(!ayear, names_to = "forest_loss", 
               values_to = "area_km2") %>% 
  filter(forest_loss != "total_forestcover_loss_km2") %>% 
  mutate(area_km2 = if_else(area_km2 ==0, NA_real_, area_km2), 
         year = ayear)  -> dfhansen_long

#  
dfhansen_long %>% group_by(ayear) %>% 
  summarise(area = sum(area_km2, na.rm=TRUE)) %>% pull(area)%>% max() -> hansenloss_max
axis_trans_hansen <- hansenloss_max / gdp_per_capita_usd_max
df_hansen_labels <- data.frame(year = c(2016, 2019), 
                               yaxis_value = hansenloss_max +(hansenloss_max*0.05),
                               label_values = c(round(hansenloss_max,0), 
                                                round(gdp_per_capita_usd_max,0)))
df_hansen_labels
mycols_hansen <- c("primary_loss_km2" = "darkorange", 
                   "forestcover_loss_km2" = "darkgoldenrod1")  
dfhansen_long %>%
ggplot(aes(x=year, y=area_km2)) + 
  geom_col(aes(fill = forest_loss), colour="blue") + 
  geom_line(data = df_bla_gdp, 
            aes(x=ayear, y=gdp_per_capita_usd*axis_trans_hansen), size=2, color="black") + 
  scale_y_continuous(#for the second y-axis
    sec.axis = sec_axis(~./axis_trans_hansen,#divided by transformation rate, in order to be represented based on the first y-axis
                        name = "GDP per capita (US$)")) + 
  scale_x_continuous(limits = c(1999.5, 2020.5))  + 
  scale_fill_manual("forest loss:", values = mycols_hansen, 
                    labels = c("primary forest", "tree cover")) + 
  geom_label(data = df_hansen_labels, 
             aes(x= year, y = yaxis_value, label = label_values), 
             colour = c("blue", "black")) + 
  labs(y = bquote('forest cover loss'~(km^2)))  +
  theme(legend.position = "top", legend.box = "horizontal") -> fig_forestloss_gdp

png(file = "figures//figure_forestloss.png", bg = "white", type = c("cairo"), 
    width=5000, height=3000, res = 600)
fig_forestloss_gdp + theme(text = element_text(size = 16))
dev.off()
