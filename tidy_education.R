#Tidy education
library(sidrar)
library(tidyverse)
library(readxl)
library(sf)

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
)

# Municipality names, codes, polygons and areas from IBGE. Updated August 2020. Accessed 8 January 2022
# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html?=&t=downloads
ibge_muni <- "C:\\Users\\user\\Documents\\Articles\\2022_Norris_gdp_deforestation\\analysis\\br_municipios_20200807\\BR_Municipios_2019.shp"
sf_ninestate_muni <- st_read(ibge_muni) %>% filter(SIGLA_UF %in% bla_state_siglas)
municipality_codes_text <- sf_ninestate_muni %>% pull(CD_MUN) %>% unique()
municipality_codes_number <- sf_ninestate_muni %>% pull(CD_MUN) %>% 
  unique() %>% as.numeric()

##7269	Pessoas de 25 anos ou mais de idade, por sexo e grupamentos de nível 
# de instrução
df_edu_completo_25anos <- get_sidra(x = 7269,
                                    geo = "City", 
                                    period = years_2015a2019,
                                    header = TRUE,
                                    format = 4)
unique(df_edu_completo_25anos[,15]) #[5] "Superior completo"
length(unique(df_edu_completo_25anos[,6])) # only 27 capitals

#decadal
# t_super25m	% de 25 anos ou mais com superior completo	
#Percentual da população de 25 anos ou mais com superior completo	
#Razão entre a população de 25 anos ou mais de idade que concluiu pelo menos 
#a graduação do ensino superior e o total de pessoas nesta faixa etária
#multiplicado por 100.

df_census <- read_excel("data//pnud_municipios.xlsx", 
           .name_repair = "universal") %>% 
  filter(ano %in% c(2000,2010), codmun7 %in% all_of(municipality_codes_number))

#Correlation of 25 year old or over with superior complete and education index
#R2
df_census %>% 
  filter(!is.na(idhm_e)) %>%
  select(ano, nome, codmun7, t_super25m,idhm_e)%>% 
  nest(data = -c(ano,nome)) %>% 
  mutate(
    fit = map(data, ~ lm(idhm_e ~ t_super25m, data = .x)),
    tidied = map(fit, tidy), 
    glanced = map(fit, glance)
  ) -> regressions_edu
regressions_edu

regressions_edu %>%
  unnest(tidied) %>% 
  filter(term != '(Intercept)') %>% 
  left_join(regressions_edu %>% 
              unnest(glanced), 
            by = c("ano" = "ano", 
                   "nome" = "nome", 
                   "data" = "data", "fit" = "fit") 
  ) -> df_regressions_edu_out

# summaries for plotting labels
df_regressions_edu_out %>% select(ano, nome, r.squared, p.value.x) %>% 
  right_join(
df_census %>% 
  group_by(ano, nome) %>% 
  summarise(idhm_e = median(idhm_e, na.rm = TRUE), 
            t_super25m = median(t_super25m, na.rm = TRUE))) %>% 
  mutate(mylabel = paste(ano, " r2: ", 
                         round(r.squared,2),sep="")) -> df_edu_labels

df_census %>% 
  ggplot(aes(x=t_super25m, y=idhm_e)) + 
  geom_point(aes(colour=factor(ano))) + 
  stat_smooth(method="lm", aes(colour=factor(ano))) + 
  scale_colour_discrete("decade:") +
  facet_wrap(~nome) + 
  geom_label(data = df_edu_labels, aes(label = mylabel), nudge_x = 10) +
  labs(x = "25 years or older with university education (%)", 
       y = "human development index - education") +
  theme(legend.position="top") -> fig_edu

png(file = "figures//figure_edu.png", bg = "white", type = c("cairo"), 
    width=4000, height=4000, res = 600)
fig_edu + theme(text = element_text(size = 16))
dev.off()

#Association of maternity  and education index
#R2
df_census %>% 
  filter(!is.na(idhm_e)) %>%
  select(ano, nome, codmun7, t_m15a17cf,idhm_e)%>% 
  nest(data = -c(ano,nome)) %>% 
  mutate(
    fit = map(data, ~ lm(idhm_e ~ t_m15a17cf, data = .x)),
    tidied = map(fit, tidy), 
    glanced = map(fit, glance)
  ) -> regressions_edu_child
regressions_edu_child

regressions_edu_child %>%
  unnest(tidied) %>% 
  filter(term != '(Intercept)') %>% 
  left_join(regressions_edu_child %>% 
              unnest(glanced), 
            by = c("ano" = "ano", 
                   "nome" = "nome", 
                   "data" = "data", "fit" = "fit") 
  ) -> df_regressions_edu_child_out

# summaries for plotting labels
df_regressions_edu_child_out %>% select(ano, nome, r.squared, p.value.x) %>% 
  right_join(
    df_census %>% 
      group_by(ano, nome) %>% 
      summarise(idhm_e = median(idhm_e, na.rm = TRUE), 
                t_m15a17cf = median(t_m15a17cf, na.rm = TRUE))) %>% 
  mutate(mylabel = paste(ano, " r2: ", 
                         round(r.squared,2),sep="")) -> df_edu_child_labels
df_census %>% 
  ggplot(aes(x=t_m15a17cf, y=idhm_e)) + 
  geom_point(aes(colour=factor(ano))) + 
  stat_smooth(method="lm", aes(colour=factor(ano))) + 
  scale_colour_discrete("decade:") +
  facet_wrap(~nome) + 
  geom_label(data = df_edu_child_labels, aes(label = mylabel), nudge_x = 10) +
  labs(x = "10 - 14 with child", 
       y = "human development index - education") +
  theme(legend.position="top")

#https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/saeb


#bind decades and annual together
df_census %>% 
  mutate(year = ano, 
         uni_complete_25 = t_super25m, 
         codmun7 = as.character(codmun7)) %>%
  select(codmun7, year, uni_complete_25) -> df_edu_decadal
df_edu_completo_25anos %>% 
  mutate(codmun7 = `Município (Código)`, 
         uni_complete_25 = Valor, 
         name_edu = `Nível de instrução`, 
         year = as.numeric(Ano)) %>% 
  filter(name_edu == "Superior completo", Sexo == "Total", 
         Variável == "Distribuição percentual das pessoas de 25 anos ou mais de idade") %>% 
  select(codmun7, year, uni_complete_25) -> df_edu_annual

 %>% 
  bind_rows(

) -> df_edu_2000_2019

df_edu_2000_2019 %>% group_by(codmun7) %>% 
  summarise(count_years = length(unique(df_edu_2000_2019))) %>% 
  filter(count_years == 5)
