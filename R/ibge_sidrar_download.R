# https://sidra.ibge.gov.br/
# https://cran.r-project.org/web/packages/sidrar/vignettes/Introduction_to_sidrar.html
# devtools::install_github("rpradosiqueira/sidrar")
library(sidrar)
library(tidyverse)
library(sf)

# Years to download
myyears <- c("2000","2001", "2002", "2003", "2004", 
             "2005", "2006", "2007", "2008", "2009", 
             "2010", "2011", "2012", "2013", "2014", 
             "2015", "2016", "2017", "2018", "2019", "2020", "2021")
# Grouped to help make API requests polite
years_2000a2004 <- c("2000","2001", "2002", "2003", "2004")
years_2005a2009 <- c("2005", "2006", "2007", "2008", "2009")
years_2010a2014 <- c("2010", "2011", "2012", "2013", "2014")
years_2015a2019 <- c("2015", "2016", "2017", "2018", "2019")
years_2020a2024 <- c("2020", "2021", "2022", "2023", "2024")

#Cadastro Central de Empresas
#Salary table 1685 replaced 454 . By year due to download limit.
df_salary_2006 <- get_sidra(x = 1685, 
                              geo = "City", 
                              period = "2006", 
                              header = TRUE) 
df_salary_2007 <- get_sidra(x = 1685, 
                            geo = "City", 
                            period = "2007", 
                            header = TRUE) 
df_salary_2008 <- get_sidra(x = 1685, 
                            geo = "City", 
                            period = "2008", 
                            header = TRUE) 
df_salary_2009 <- get_sidra(x = 1685, 
                            geo = "City", 
                            period = "2009", 
                            header = TRUE) 
df_salary_2010 <- get_sidra(x = 1685, 
                            geo = "City", 
                            period = "2010", 
                            header = TRUE) 
df_salary_2011 <- get_sidra(x = 1685, 
                            geo = "City", 
                            period = "2011", 
                            header = TRUE) 
df_salary_2012 <- get_sidra(x = 1685, 
                            geo = "City", 
                            period = "2012", 
                            header = TRUE)
df_salary_2013 <- get_sidra(x = 1685, 
                            geo = "City", 
                            period = "2013", 
                            header = TRUE)
df_salary_2014 <- get_sidra(x = 1685, 
                            geo = "City", 
                            period = "2014", 
                            header = TRUE)
df_salary_2015 <- get_sidra(x = 1685, 
                            geo = "City", 
                            period = "2015", 
                            header = TRUE)
df_salary_2016 <- get_sidra(x = 1685, 
                            geo = "City", 
                            period = "2016", 
                            header = TRUE)
df_salary_2017 <- get_sidra(x = 1685, 
                            geo = "City", 
                            period = "2017", 
                            header = TRUE)
df_salary_2018 <- get_sidra(x = 1685, 
                            geo = "City", 
                            period = "2018", 
                            header = TRUE)
df_salary_2019 <- get_sidra(x = 1685, 
                            geo = "City", 
                            period = "2019", 
                            header = TRUE)

df_salary <- rbind(df_salary_2006, df_salary_2007, df_salary_2008, df_salary_2009,
 df_salary_2010, df_salary_2011, df_salary_2012, df_salary_2013,
df_salary_2014, df_salary_2015, df_salary_2016, df_salary_2017,
df_salary_2018, df_salary_2019)
write.csv(df_salary, "df_salary.csv", row.names = FALSE)

#Population
df_pop_2000a2004 <- get_sidra(x = 6579, 
                              geo = "City", 
                              period = years_2000a2004, 
                              header = TRUE) 
df_pop_2005a2009 <- get_sidra(x = 6579, 
                              geo = "City", 
                              period = years_2005a2009, 
                              header = TRUE)
df_pop_2010a2014 <- get_sidra(x = 6579, 
                              geo = "City", 
                              period = years_2010a2014, 
                              header = TRUE)
df_pop_2015a2019 <- get_sidra(x = 6579, 
                              geo = "City", 
                              period = years_2015a2019, 
                              header = TRUE)
df_pop_2020a2024 <- get_sidra(x = 6579, 
                              geo = "City", 
                              period = years_2020a2024, 
                              header = TRUE)
#Censu 
df_pop_muni_censu <- get_sidra(x = 202,
                                period = c("2000"),
                                geo = "City", 
                                header = TRUE)

#Pop by state
df_pop_state <- get_sidra(x = 6579,
                             period = myyears,
                             geo = "State", 
                             header = TRUE,
                             format = 4)
unique(df_pop_state$Ano) #missing 2007
#Censu 
df_pop_state_censu <- get_sidra(x = 202,
                          period = c("1991", "2000", "2010"),
                          geo = "State", 
                          header = TRUE,
                          format = 4)
unique(df_pop_state_censu[,"Unidade da Federação"])
bla_states <- c("Acre", "Amapá", "Amazonas", "Maranhão", 
                "Mato Grosso", "Pará", "Tocantins", "Rondônia", "Roraima")

rbind(df_pop_state_censu %>% 
  filter(Sexo == "Total", `Situação do domicílio` == "Total", 
         `Unidade da Federação` %in% bla_states) %>% 
  mutate(uf = `Unidade da Federação`, 
         ayear = Ano,
         total_pop = Valor) %>% 
  select(uf, ayear, total_pop), 
df_pop_state %>%
  filter(`Unidade da Federação` %in% bla_states) %>% 
  mutate(uf = `Unidade da Federação`, 
         ayear = Ano,
         total_pop = Valor) %>% 
  select(uf, ayear, total_pop) 
) -> bla_state_pop_1991_2021
  
write.csv(bla_state_pop_1991_2021, "bla_state_pop_1991_2021.csv", row.names = FALSE)
#PIB 
# Plus
# https://www.ibge.gov.br/estatisticas/economicas/contas-nacionais/9054-contas-regionais-do-brasil.html?edicao=17236&t=downloads
info_pib_muni <- info_sidra(5938, wb = FALSE)
df_pib_vars <- info_pib_muni$variable
df_pib_vars %>% filter(cod == "37")
# 37 Produto Interno Bruto a preços correntes (Mil Reais)
df_pib_2000a2004 <-  get_sidra(x = 5938, variable = 37,
                             geo = "City", 
                             period = years_2000a2004,
                             header = TRUE,
                             format = 1)
min(df_pib_2000a2004$`Ano (Código)`)
df_pib_2005a2009 <-  get_sidra(x = 5938, variable = 37,
                               geo = "City", 
                               period = years_2005a2009,
                               header = TRUE,
                               format = 1)
df_pib_2010a2014 <-  get_sidra(x = 5938, variable = 37,
                               geo = "City", 
                               period = years_2010a2014,
                               header = TRUE,
                               format = 1)
df_pib_2015a2019 <-  get_sidra(x = 5938, variable = 37,
                               geo = "City", 
                               period = years_2015a2019,
                               header = TRUE,
                               format = 1)
df_pib_2020a2024 <-  get_sidra(x = 5938, variable = 37,
                                   geo = "City", 
                                   period = years_2020a2024,
                                   header = TRUE,
                                   format = 1)
max(df_pib_2015a2019$`Ano (Código)`) #2018
#PIB agri
#Examples:
#https://greenjurisdictions.org/
#https://greenjurisdictions.org/jurisdictional_profile?region=3216,states#perfil_prev2  
df_pib_vars %>% filter(cod == "513")
#Valor adicionado bruto a preços correntes da agropecuária (Mil Reais)
df_pibagri_2000a2004 <-  get_sidra(x = 5938, variable = 513,
                               geo = "City", 
                               period = years_2000a2004,
                               header = TRUE,
                               format = 1)
df_pibagri_2005a2009 <-  get_sidra(x = 5938, variable = 513,
                               geo = "City", 
                               period = years_2005a2009,
                               header = TRUE,
                               format = 1)
df_pibagri_2010a2014 <-  get_sidra(x = 5938, variable = 513,
                               geo = "City", 
                               period = years_2010a2014,
                               header = TRUE,
                               format = 1)

df_pibagri_2015a2019 <-  get_sidra(x = 5938, variable = 513,
                               geo = "City", 
                               period = years_2015a2019,
                               header = TRUE,
                               format = 1)
# No data for 2020
df_pibagri_2020a2024 <-  get_sidra(x = 5938, variable = 513,
                                   geo = "City", 
                                   period = years_2020a2024,
                                   header = TRUE,
                                   format = 1)
# Índices de Preços ao Consumidor
get_sidra(x = 1419,
          variable = 63,
          period = c("last" = 12),
          geo = "City",
          geo.filter = 5002407,
          classific = "c315",
          category = list(7169),
          header = FALSE,
          format = 4)


#Gini index per state 2018
search_sidra(c("gini"))
"Tabela 5939 - Índice de Gini do produto interno bruto a preços correntes e do valor 
adicionado bruto a preços correntes por atividade econômica - Referência 2010" 
?se
df_gini_ref2010 <- get_sidra(x = 5939,
          variable = 529,
          period = myyears,
          geo = "State", 
          header = TRUE,
          format = 4)
"Tabela 7435 - Índice de Gini do rendimento domiciliar per capita, a preços médios do ano"
df_gini_renddom <- get_sidra(x = 7435,
          geo = "State", 
          period = myyears,
          header = TRUE,
          format = 1)

#PIB
search_sidra(c("pib"))
# "Tabela 6784 - Produto Interno Bruto, Produto Interno Bruto per capita, População residente e Deflator"                                           
# "Tabela 6795 - Indicador 9.4.1 - Emissão de CO2 pelo PIB"                                                                                         
# "Tabela 6840 - Indicador 2.a.1 - Índice de orientação agrícola para a despesa pública"                                                            
# "Tabela 7223 - Indicador 17.1.1 - Total das receitas do Governo em percentagem do PIB, por fonte" 


#Education
#A taxa de escolarização é o percentual de estudantes de determinada faixa etária no total de pessoas dessa mesma faixa etária.
info_edu_muni <- info_sidra(7138, wb = FALSE)
df_edu_vars <- info_edu_muni$variable
df_edu_vars %>% filter(cod == "37")
# taxa
df_edu_taxa16 <- get_sidra(x = 7138,
                             geo = "City", 
                             period = "2016",
                             header = TRUE,
                             format = 4)
unique(df_edu_taxa16$Variável)

#Tabela 7267 - Pessoas de 14 anos ou mais de idade, por sexo e grupamentos de nível de instrução
info_edu_muni <- info_sidra(7267, wb = FALSE)
df_edu_vars <- info_edu_muni$variable
df_edu_vars %>% filter(cod == "37")

# taxa
#7267	Pessoas de 14 anos ou mais de idade, por sexo e grupamentos de nível de instrução
df_edu_completo <- get_sidra(x = 7267,
                           geo = "City", 
                           period = years_2015a2019,
                           header = TRUE,
                           format = 4)

#7269	Pessoas de 25 anos ou mais de idade, por sexo e grupamentos de nível de instrução
# t_super25m	% de 25 anos ou mais com superior completo	Percentual da população de 25 anos ou mais com superior completo	Razão entre a população de 25 anos ou mais de idade que concluiu pelo menos a graduação do ensino superior e o total de pessoas nesta faixa etária multiplicado por 100.

df_edu_completo_25anos <- get_sidra(x = 7269,
                             geo = "City", 
                             period = years_2015a2019,
                             header = TRUE,
                             format = 4)
unique(df_edu_completo_25anos[,2]) #[5] "Município"
unique(df_edu_completo_25anos[,6]) #[5] 27 municipalities
unique(df_edu_completo_25anos[,15]) #[5] "Superior completo"
mun_codes <- as.character(unique(sf_ninestate_muni$CD_MUN))
which(unique(df_edu_completo_25anos[,6]) %in% mun_codes) #9
df_edu_completo_25anos_state <- get_sidra(x = 7269,
                                    geo = "State", 
                                    period = years_2015a2019,
                                    header = TRUE,
                                    format = 4)
write.csv(df_edu_completo_25anos, "df_edu_completo_25anos.csv", row.names = FALSE)
write.csv(df_edu_completo_25anos_state, "df_edu_completo_25anos_state.csv", row.names = FALSE)
