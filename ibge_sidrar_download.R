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

#PIB 
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
