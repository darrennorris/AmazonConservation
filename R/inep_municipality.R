
# Educação infantil = creche (to 3) e pré-escola (4-5)
# Ensino medio = 15 - 17 (pre university). Com internet para alunos??
# "INDICESC" Taxa de insucesso por município (Reprovação+Abandono)
# IEM00042 Taxa de Reprovação no Ensino Médio (year before)
# IEM00047 Taxa de Abandono no Ensino Médio (year before)
# Educação profissional ?

#packages
library(plyr)
library(tidyverse)
library(readxl)
library(stringi)
library(sf)
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

ibge_muni <- "vector\\ninestate_muni.shp"
sf_ninestate_muni <- st_read(ibge_muni) %>% filter(SIGLA_UF %in% bla_state_siglas)
#2000 - 2020
#https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/indicadores-educacionais/taxas-de-rendimento
#get file names
data.frame(aid = 1:18, 
                           mypath = "E:\\edu_inep\\rendimento\\data",
                           mynames = list.files(path = "E:\\edu_inep\\rendimento\\data", 
                                                full.names = FALSE) 
) %>% 
  mutate(full_path = paste(mypath, mynames, sep="\\")) %>% 
  mutate(aid = full_path)%>% 
  mutate(flag_name_INDICESC = str_detect(mynames, "INDICESC"), 
         flag_name_xlsx = str_detect(mynames, ".xls")) %>% 
  filter(flag_name_INDICESC | flag_name_xlsx) -> dfnames_data

count_sheets <- function(x){
  if(x$flag_name_xlsx) {
    infile <- x$full_path
    sheet_list <- excel_sheets(infile) 
    n_sheets <- length(sheet_list)
    dfcount <- data.frame(excel_sheet_count = n_sheets)
  }
  if(x$flag_name_INDICESC){
    dfcount <- data.frame(excel_sheet_count = 0)
  }
  dfcount
}

ddply(dfnames_data, .(mynames), .fun= count_sheets) %>% 
  separate(mynames ,into = c("b","c"), 
           sep = "2",remove = FALSE, extra = "merge") %>% 
  mutate(ayear = as.numeric(paste("2", substr(c,0,3), sep=""))) %>% 
  select(mynames, ayear, excel_sheet_count) %>% 
  left_join(dfnames_data) %>%
  mutate(flag_name_excel6 = 
           str_detect(excel_sheet_count, "6"), 
         flag_name_excel1 = 
           str_detect(excel_sheet_count, "1")
  ) %>% arrange(ayear) -> dfnames_data

#Annual pass rates per school
inep_get_ensinomedio <- function(x) {
  if(x$flag_name_INDICESC){
  indicesc_cols <- c("MASCARA", "ANO", "CODMUNIC", "UF", "SIGLA", 
                     "MUNIC", "DEP", "LOC", "CODFUNC", 
                     "IEM00037","IEM00042", "IEM00047")
  col_types = c(rep("text", 9))
  #Includes urban / rural etc
  schools_medio <- read_delim(x$full_path, delim = "|", 
                            col_select = all_of(indicesc_cols),
                            col_types = cols(.default = "c"), 
                            escape_double = FALSE, 
                            trim_ws = TRUE) %>% 
    filter(CODFUNC == "Ativo", SIGLA %in% all_of(bla_state_siglas), !is.na(IEM00037),
           !is.na(IEM00042), !is.na(IEM00047), 
           !(DEP %in% c("Particular", "Privada","'Particular")))

  #format to return. Early versions report previous years pass rates
  school_medio_out <- data.frame(flag_type = "early_2000_2003", 
                         ANO = as.numeric(schools_medio$ANO) -1,
                         UF = schools_medio$UF, 
                         SIGLA = schools_medio$SIGLA, 
                         MUNIC = schools_medio$MUNIC, 
                         dep = schools_medio$DEP,
                         school_idcode = schools_medio$MASCARA, 
                         pass_rate_percent = as.numeric(schools_medio$IEM00037),
                         fail_rate_percent = as.numeric(schools_medio$IEM00042), 
                         abandon_rate_percent = as.numeric(schools_medio$IEM00047)
                         )
  }
  
  if(x$ayear %in% c(2007, 2008, 2009, 2010)){
    
    #Load sheet
    schools_medio  <- read_excel(x$full_path, na = c("", "--"),
                                 sheet = 1, 
                                 col_types = "text",
                                 .name_repair = "universal") %>% 
      filter(UF %in% all_of(bla_state_siglas), !is.na(Rede)) %>% 
      filter(!(Rede %in% c("Particular", "","Privada","'Particular"))) %>% 
      filter(!is.na(Total.Abandono...Médio), !is.na(Total..Reprovação.Médio), 
             !is.na(Total..Aprovação.Médio))
    
    #format to return
    school_medio_out <- data.frame(flag_type = "recent_2007_2010", 
                                   ANO = schools_medio$Ano,
                                   UF = NA, 
                                   SIGLA = schools_medio$UF, 
                                   MUNIC = schools_medio$Nome.do.Município, 
                                   dep = schools_medio$Rede,
                                   school_idcode = schools_medio$Código.da.Escola, 
                                   pass_rate_percent = as.numeric(schools_medio$Total..Aprovação.Médio),
                                   fail_rate_percent = as.numeric(schools_medio$Total..Reprovação.Médio), 
                                   abandon_rate_percent = as.numeric(schools_medio$Total.Abandono...Médio) 
    )
  }
  
  if(x$ayear %in% c(2011,2012)){
    
    #Load sheet
    schools_medio  <- read_excel(x$full_path, na = c("", "--"),
                                 sheet = 1, 
                                 col_types = "text",
                                 .name_repair = "universal") %>% 
      filter(UF %in% all_of(bla_state_siglas), !is.na(Rede)) %>% 
      filter(!(Rede %in% c("Particular", "","Privada","'Particular"))) %>% 
      filter(!is.na(Total.Aprovação.no.Ens..Médio), !is.na(Total.Reprovação.no.Ens..Médio), 
             !is.na(Total.Abandono.no.Ens..Médio))
    
    #format to return
    school_medio_out <- data.frame(flag_type = "recent_2011_2012", 
                                   ANO = schools_medio$Ano,
                                   UF = NA, 
                                   SIGLA = schools_medio$UF, 
                                   MUNIC = schools_medio$Nome.do.Município, 
                                   dep = schools_medio$Rede,
                                   school_idcode = schools_medio$Código.da.Escola, 
                                   pass_rate_percent = as.numeric(schools_medio$Total.Aprovação.no.Ens..Médio),
                                   fail_rate_percent = as.numeric(schools_medio$Total.Reprovação.no.Ens..Médio), 
                                   abandon_rate_percent = as.numeric(schools_medio$Total.Abandono.no.Ens..Médio) 
    )
  }
  
  if(x$ayear %in% c(2013:2014)) {
    #format to return
    #Load sheet
    schools_medio  <- read_excel(x$full_path, na = c("", "--"),
                                 sheet = 1, 
                                 col_types = "text",
                                 .name_repair = "universal") %>% 
      filter(UF %in% all_of(bla_state_siglas), !is.na(Dependência.Administrativa)) %>% 
      filter(!(Dependência.Administrativa %in% c("Particular", "","Privada","'Particular"))) %>% 
      filter(!is.na(Total.Aprovação.no.Ens..Médio), !is.na(Total.Reprovação.no.Ens..Médio), 
             !is.na(Total.Abandono.no.Ens..Médio))
    
    #format to return
    school_medio_out <- data.frame(flag_type = "recent_2013_2014", 
                                   ANO = schools_medio$Ano,
                                   UF = NA, 
                                   SIGLA = schools_medio$UF, 
                                   MUNIC = schools_medio$Nome.do.Município, 
                                   dep = schools_medio$Dependência.Administrativa,
                                   school_idcode = schools_medio$Código.da.Escola, 
                                   pass_rate_percent = as.numeric(schools_medio$Total.Aprovação.no.Ens..Médio),
                                   fail_rate_percent = as.numeric(schools_medio$Total.Reprovação.no.Ens..Médio), 
                                   abandon_rate_percent = as.numeric(schools_medio$Total.Abandono.no.Ens..Médio) 
    )
  }

  if(x$ayear %in% c(2015:2020)){
    #column names in correct sequence
first_columns <- c("Ano",  "Região",  "UF", "Código.do.Município", "Nome.do.Município",
                   "Código.da.Escola", "Nome.da.Escola", "Localização" , "Rede") 
#Load sheet
schools_medio  <- read_excel(x$full_path, na = c("", "--"),
               sheet = 1, col_names = FALSE, skip=9,
               col_types = "text",
               .name_repair = "universal") %>% 
      select(1:9, 22, 40, 58) %>%
      stats::setNames(c(all_of(first_columns), "aprov", "reprov", "aband")) %>% 
   filter(UF %in% all_of(bla_state_siglas), !is.na(Rede),
          !is.na(aprov), !is.na(reprov), !is.na(aband)) %>% 
  filter(!(Rede %in% c("Particular", "","Privada","'Particular")))

    #format to return
    school_medio_out <- data.frame(flag_type = "recent_2015_2020", 
                                   ANO = schools_medio$Ano,
                                   UF = NA, 
                                   SIGLA = schools_medio$UF, 
                                   MUNIC = schools_medio$Nome.do.Município, 
                                   dep = schools_medio$Rede,
                                   school_idcode = schools_medio$Código.da.Escola, 
                                   pass_rate_percent = as.numeric(schools_medio$aprov),
                                   fail_rate_percent = as.numeric(schools_medio$reprov), 
                                   abandon_rate_percent = as.numeric(schools_medio$aband) 
    )
  }
  
  school_medio_out 
}
#Get school data
df_inep_schools_2000_2020 <- plyr::ddply(dfnames_data, .(aid), 
                                             .fun = inep_get_ensinomedio)
#Tidy data
#unique(df_inep_schools_2000_2020$dep) #"Estadual"  "Municipal" "Federal"
df_inep_schools_2000_2020 %>% 
  mutate(muni_upper = toupper(MUNIC)) %>% 
  mutate(muni_inep = stri_trans_general(muni_upper, "Latin-ASCII")) -> df_inep_schools_2000_2020

length(unique(df_inep_schools_2000_2020$MUNIC)) #1623
length(unique(df_inep_schools_2000_2020$muni_inep)) #824
sort(unique(df_inep_schools_2000_2020$muni_inep))

df_inep_schools_2000_2020 %>% group_by(MUNIC) %>% 
  summarise(count_year = length(unique(ANO))) %>% arrange(MUNIC)
  
df_inep_schools_2000_2020 %>% 
  mutate(muni_upper = toupper(MUNIC)) %>% 
  mutate(muni_inep = stri_trans_general(muni_upper, "Latin-ASCII")) %>%
  group_by(muni_inep) %>% 
  summarise(count_year = length(unique(ANO))) %>% arrange(muni_inep)

df_inep_schools_2000_2020 %>% 
  mutate(tot_percent = (pass_rate_percent + fail_rate_percent + 
                          abandon_rate_percent)) %>% arrange(tot_percent)
  
df_inep_schools_2000_2020 %>% group_by(ANO, flag_type) %>% 
  summarise(count_uf = length(unique(SIGLA)), 
            count_muni = length(unique(MUNIC)), 
            total_schools = length(unique(school_idcode)), 
            pass_rate_per = median(pass_rate_percent, na.rm = TRUE),
            pass_rate_sd = sd(pass_rate_percent, na.rm = TRUE)
            )
df_inep_schools_2000_2020 %>% 
  filter(dep=="Estadual") %>%
  group_by(ANO, flag_type) %>% 
  summarise(count_uf = length(unique(SIGLA)), 
            count_muni = length(unique(MUNIC)), 
            total_schools = length(unique(school_idcode)), 
            pass_rate_per = median(pass_rate_percent, na.rm = TRUE),
            pass_rate_sd = sd(pass_rate_percent, na.rm = TRUE)
  )


df_inep_schools_2000_2020 %>% filter(ANO==2020) %>% pull(pass_rate_percent)
df_inep_schools_2000_2020 %>% filter(ANO==2010)

df_inep_schools_2000_2020 %>% 
  filter(!is.na(ANO)) %>% 
  #filter(!(ANO==2020)) %>%
  ggplot(aes(x=ANO, y=pass_rate_percent)) + 
  geom_boxplot(aes(fill=dep), outlier.shape = NA) + 
  scale_x_discrete() +
  facet_wrap(~SIGLA)

Export
names_out <- c("aid", "flag_type", "ANO", "UF", "SIGLA" , "MUNIC", "muni_inep", 
               "dep", "school_idcode",  "pass_rate_percent", "fail_rate_percent", 
               "abandon_rate_percent" )
write.csv(df_inep_schools_2000_2020[ ,names_out], 
          "inep_school_passrates.csv", row.names = FALSE)

#summary by municipality
data.frame(sf_ninestate_muni) %>% 
  mutate(muni_upper = toupper(NM_MUN)) %>% 
  mutate(muni_inep = stri_trans_general(muni_upper, "Latin-ASCII"), 
         SIGLA = SIGLA_UF) %>% left_join(
df_inep_schools_2000_2020 %>% 
  filter(dep=="Estadual") %>%
  filter(ANO =="2019") %>% 
  group_by(SIGLA, muni_inep) %>% 
  summarise(total_schools = length(unique(school_idcode)), 
            pass_rate_per = median(pass_rate_percent, na.rm = TRUE),
            pass_rate_sd = sd(pass_rate_percent, na.rm = TRUE)) 
) -> df_muni_schools
#
df_muni_schools %>% 
  select(!geometry) %>% 
  arrange(SIGLA_UF, NM_MUN) %>% 
  write.csv("muni_fixed_inep.csv", row.names = FALSE)

#add forest loss
data.frame(sf_ninestate_muni) %>% mutate(CD_MUN = as.numeric(CD_MUN)) %>% left_join(
read_excel("data\\bla_municipality_gdp_forestloss.xlsx", 
           .name_repair = "universal") %>% 
  select(CD_MUN, SIGLA_UF, cagr_gdp_percapita, cagr_gva_agri_percapita, 
         forest,	savanna,	tot_transition_km2, tot_transition_percent)

) %>% 
  select(!geometry) %>% 
  arrange(SIGLA_UF, NM_MUN) %>% 
  write.csv("muni_fixed_forestloss.csv", row.names = FALSE)

## ignore below all notes
file.choose()
read_excel("E:\\edu_inep\\rendimento\\data\\tx_rend_escolas_2014.xlsx", 
           .name_repair = "universal", n_max=3) %>% names()

# first for each .zip get data files
dfnames <- data.frame(inep_year = c(2000:2003, 2007:2020) , 
                      mypath = "E:\\edu_inep\\rendimento",
                      mynames = list.files(path = "E:\\edu_inep\\rendimento", 
                                           pattern = "\\.zip$",
                                           full.names = FALSE) 
) %>% 
  mutate(full_path = paste(mypath, mynames, sep="\\")) %>% 
  mutate(aid = full_path)
inep_get_names <- function(x){
  zip_files <- unzip(x$full_path, list=TRUE) 
  zip_files <- data.frame(full_path = x$full_path, zip_files)
}
#get names of files with data, grouped by summary process
plyr::ddply(dfnames, .(inep_year, mynames, aid), 
            .fun = inep_get_names) %>% 
  mutate(flag_name_INDICESC = str_detect(Name, "INDICESC"), 
         flag_name_xlsx = str_detect(Name, ".xls")) %>% 
  filter(flag_name_INDICESC | flag_name_xlsx) -> df_inep_datafiles
#Unzip data files (cant get into excel files with "unz")
myunzip <- function(x) {
  unzip(zipfile = x$full_path, 
        files= x$Name, 
        exdir="E:\\edu_inep\\rendimento\\data", 
        junkpaths = TRUE, overwrite=TRUE)
}
#plyr::d_ply(df_inep_datafiles, .(aid), .fun = myunzip)

#excel files 2007:
#Names for columns 2007 to 2010
read_excel("E:\\edu_inep\\rendimento\\data\\tx_rend_escolas_2010.xlsx", 
           .name_repair = "universal") %>% names()

#Names for columns 2012
dfnames_data %>% filter(ayear ==2012)  %>% pull(full_path) %>% 
  excel_sheets() -> sheet_names_2012
dfnames_data %>% filter(ayear ==2012) %>% pull(full_path) -> path_2012
read_excel(path_2012, 
           sheet = sheet_names_2012[1], skip=6, n_max=10, 
           .name_repair = "universal") %>% names() -> colnames_2012a
colnames_2012a[1:10]
read_excel(path_2012, 
           sheet = sheet_names_2012[1], skip=7, n_max=8, 
           .name_repair = "universal") %>% names() -> colnames_2012b
colnames_2012b[11:63]
colnames_2012 <- c(colnames_2012a[1:10], colnames_2012b[11:63])

myoldcols = c("Total.Abandono.no.Ens..Médio", "Total.Abandono...Médio")
read_excel(path_2012, na = c("", "--"),
           sheet = 1, col_names = FALSE, skip=9,
           col_types = "text",
           .name_repair = "universal") %>% 
  select(1:9, 22, 40, 58) %>%
  stats::setNames(c(colnames_2012a[1:9], "aprov", "reprov", "aband")) %>% 
  filter(UF %in% all_of(bla_state_siglas), Rede != "Particular") %>%
  filter(!is.na(aprov)) -> test2012
unique(test2012$Rede) 
test2012 %>% 
  mutate(aprov = as.numeric(aprov), 
         reprov = as.numeric(reprov),
         aband = as.numeric(aband)) %>%
  mutate(tot_percent = aprov + reprov + aband) %>% 
  arrange(tot_percent) %>% 
  select(5:13)



#Names for columns 2020
dfnames_data %>% filter(ayear ==2020)  %>% pull(full_path) %>% 
  excel_sheets() -> sheet_names_2020
dfnames_data %>% filter(ayear ==2020) %>% pull(full_path) -> path_2020
read_excel(path_2020, 
           sheet = sheet_names_2020[1], skip=6, n_max=10, 
           .name_repair = "universal") %>% names() -> colnames_2020a
colnames_2020a[1:10]
read_excel(path_2020, 
           sheet = sheet_names_2020[1], skip=7, n_max=8, 
           .name_repair = "universal") %>% names() -> colnames_2020b
colnames_2020b[11:63]
colnames_2020 <- c(colnames_2020a[1:10], colnames_2020b[11:63])
read_excel(path_2020, na = c("", "--"),
           sheet = 1, skip=9, n_max=20, 
           col_names = colnames_2020, 
           col_types = "text",
           .name_repair = "universal") %>% 
  mutate(ens_med_abn = if("Total.Abandono.no.Ens..Médio" %in% colnames_2020) 
    Total.Abandono.no.Ens..Médio else Total.Abandono...Médio, 
    ens_med_repr = if("Total.Reprovação.no.Ens..Médio" %in% colnames_2020) 
      Total.Reprovação.no.Ens..Médio else Total..Reprovação.Médio) %>% 
  select(colnames_2020a[c(1,3,5)], Total.Abandono.no.Ens..Médio, ens_med_abn, ens_med_repr) 


data.frame(a2012 = colnames_2012b[11:63], a2020 = colnames_2020b[11:63])
data.frame(a2012 = colnames_2012a, a2020 = colnames_2020a)

dfnames_data %>% filter(ayear ==2007)  %>% pull(full_path) %>% 
  excel_sheets() -> sheet_names_2007
dfnames_data %>% filter(ayear ==2007) %>% pull(full_path) -> path_2007
read_excel(path_2007, 
           sheet = sheet_names_2007[1], skip=6, n_max=10, 
           .name_repair = "universal") %>% names() -> colnames_2007a
colnames_2007a[1:10]
read_excel(path_2007, 
           sheet = sheet_names_2007[1], skip=7, n_max=8, 
           .name_repair = "universal") %>% names() -> colnames_2007b
colnames_2007b[11:63]
colnames_2007 <- c(colnames_2007a[1:10], colnames_2007b[11:63])

myoldcols = c("Total.Abandono.no.Ens..Médio", "Total.Abandono...Médio")
read_excel(path_2007, 
           sheet = 1, skip=9,
           col_names = colnames_2007, 
           col_types = "text",
           .name_repair = "universal") %>% 
  mutate(ens_med_abn = if("Total.Abandono.no.Ens..Médio" %in% colnames(.)) 
    Total.Abandono.no.Ens..Médio else Total.Abandono...Médio, 
    ens_med_repr = if("Total.Reprovação.no.Ens..Médio"%in% colnames(.)) 
      Total.Reprovação.no.Ens..Médio else Total..Reprovação.Médio) %>% 
  select(colnames_2007a[1:9], ens_med_abn, ens_med_repr) -> test2007
unique(test2007$Rede)
test2007 %>% 
  filter(!is.na(Rede)) %>%
filter(!(Rede %in% c("Particular", "","Privada","'Particular"))) %>% 
  pull(Rede) %>% unique()


#Names for columns 2007 to 2010
dfnames_data %>% filter(ayear ==2011)  %>% pull(full_path) %>% 
  excel_sheets() -> sheet_names_2011
dfnames_data %>% filter(ayear ==2011) %>% pull(full_path) -> path_2011
read_excel(path_2011, 
           sheet = sheet_names_2011[1], skip=6, n_max=10, 
           .name_repair = "universal") %>% names() -> colnames_2011a
colnames_2011a[1:10]
read_excel(path_2011, 
           sheet = sheet_names_2011[1], skip=7, n_max=8, 
           .name_repair = "universal") %>% names() -> colnames_2011b
colnames_2011b[11:63]
colnames_2011 <- c(colnames_2011a[1:10], colnames_2011b[11:63])
read_excel(path_2011, 
           sheet = 1, skip=9, na = c("", "--"),
           col_names = colnames_2011, 
           col_types = "text",
           .name_repair = "universal") %>% 
  mutate(ens_med_abn = if("Total.Abandono.no.Ens..Médio" %in% colnames(.)) 
    Total.Abandono.no.Ens..Médio else Total.Abandono...Médio, 
    ens_med_repr = if("Total.Reprovação.no.Ens..Médio"%in% colnames(.)) 
      Total.Reprovação.no.Ens..Médio else Total..Reprovação.Médio) %>% 
  select(colnames_2011a[1:9], ens_med_abn, ens_med_repr) -> test2011
unique(test2011$Rede)
test2011 %>% 
  filter(!is.na(Rede), !is.na(ens_med_repr)) %>%
  filter(!(Rede %in% c("Particular", "","Privada","'Particular"))) %>% pull(UF) %>% unique()

df_inep_schools_2000_2020 %>% 
  filter(SIGLA %in% c("RO", "AC", "AM", "RR", "PA", "AP", "TO"), ANO == 2011)


#Summaries for each municipality
inep_escola_summary <- function(x) {
  if(x$flag_name_INDICESC){
    indicesc_cols <- c("MASCARA", "ANO", "CODMUNIC", "UF", "SIGLA", 
                       "MUNIC", "DEP", "LOC", "CODFUNC", 
                       "IEM00042", "IEM00047")
    col_types = c(rep("text", 9))
    #Includes urban / rural etc
    school_fail <- read_delim(x$full_path, delim = "|", 
                              col_select = all_of(indicesc_cols),
                              col_types = cols(.default = "c"), 
                              escape_double = FALSE, 
                              trim_ws = TRUE) %>% 
      filter(SIGLA %in% all_of(bla_state_siglas), 
             !is.na(IEM00042), !is.na(IEM00047), DEP != "Particular")
    #summary number of schools and failure rates
    school_fail %>% 
      mutate(IEM00042 = as.numeric(IEM00042), 
             IEM00047 = as.numeric(IEM00047)) %>%
      group_by(ANO, UF, SIGLA, MUNIC) %>% 
      summarise(count_schools_medio = length(unique(MASCARA)), 
                fail_rate_percent = median(IEM00042, na.rm = TRUE) + 
                  median(IEM00047, na.rm = TRUE), 
                tot_abn_reprov = sum(IEM00042, na.rm = TRUE) + 
                  sum(IEM00047, na.rm= TRUE)) %>% 
      mutate(fail_rate_percent2 = tot_abn_reprov/count_schools_medio) %>% 
      ungroup() %>% data.frame()-> muni_fail
    #format to return
    muni_out <- data.frame(flag_type = "early_2000_2003", 
                           ANO = muni_fail$ANO,
                           UF = muni_fail$UF, 
                           SIGLA = muni_fail$SIGLA, 
                           MUNIC = muni_fail$MUNIC, 
                           count_schools_medio = muni_fail$count_schools_medio, 
                           fail_rate_percent = muni_fail$fail_rate_percent, 
                           fail_rate_percent2 = muni_fail$fail_rate_percent2
    )
  }
  
  if(x$flag_name_excel6){
    
    #Column names for excel sheets 2007 to 2011
    read_excel(x$full_path, sheet = 1, skip=6, n_max=1, 
               .name_repair = "universal") %>% names() -> colnames_a
    read_excel(x$full_path, sheet = 1, skip=7, n_max=1, 
               .name_repair = "universal") %>% names() -> colnames_b
    colnames_excel6 <- c(colnames_a[1:10], colnames_b[11:63])
    
    #load data
    sheet01 <- read_excel(x$full_path, na = c("", "--"),
                          sheet = 1, skip=9, 
                          col_names = colnames_excel6, 
                          col_types = "text", 
                          .name_repair = "universal") %>% 
      filter(UF %in% all_of(bla_state_siglas), Rede != "Particular") %>% 
      mutate(ens_med_abn = if("Total.Abandono.no.Ens..Médio" %in% colnames_b[11:63]) 
        Total.Abandono.no.Ens..Médio else Total.Abandono...Médio, 
        ens_med_repr = if("Total.Reprovação.no.Ens..Médio" %in% colnames_b[11:63]) 
          Total.Reprovação.no.Ens..Médio else Total..Reprovação.Médio, 
        ens_med_aprov = if("Total.Aprovação.no.Ens..Médio" %in% colnames_b[11:63]) 
          Total.Aprovação.no.Ens..Médio else Total..Aprovação.Médio) %>% 
      select(colnames_excel6[1:9], ens_med_abn, ens_med_repr, ens_med_aprov) 
    #
    sheet02 <- read_excel(x$full_path, na = c("", "--"),
                          sheet = 2, skip=9, 
                          col_names = colnames_excel6, 
                          col_types = "text", 
                          .name_repair = "universal") %>% 
      filter(UF %in% all_of(bla_state_siglas), Rede != "Particular") %>% 
      mutate(ens_med_abn = if("Total.Abandono.no.Ens..Médio" %in% colnames_b[11:63]) 
        Total.Abandono.no.Ens..Médio else Total.Abandono...Médio, 
        ens_med_repr = if("Total.Reprovação.no.Ens..Médio" %in% colnames_b[11:63]) 
          Total.Reprovação.no.Ens..Médio else Total..Reprovação.Médio, 
        ens_med_aprov = if("Total.Aprovação.no.Ens..Médio" %in% colnames_b[11:63]) 
          Total.Aprovação.no.Ens..Médio else Total..Aprovação.Médio) %>% 
      select(colnames_excel6[1:9], ens_med_abn, ens_med_repr, ens_med_aprov) 
    #
    sheet03 <-  read_excel(x$full_path, na = c("", "--"),
                           sheet = 3, skip=9, 
                           col_names = colnames_excel6, 
                           col_types = "text", 
                           .name_repair = "universal") %>% 
      filter(UF %in% all_of(bla_state_siglas), Rede != "Particular") %>% 
      mutate(ens_med_abn = if("Total.Abandono.no.Ens..Médio" %in% colnames_b[11:63]) 
        Total.Abandono.no.Ens..Médio else Total.Abandono...Médio, 
        ens_med_repr = if("Total.Reprovação.no.Ens..Médio" %in% colnames_b[11:63]) 
          Total.Reprovação.no.Ens..Médio else Total..Reprovação.Médio, 
        ens_med_aprov = if("Total.Aprovação.no.Ens..Médio" %in% colnames_b[11:63]) 
          Total.Aprovação.no.Ens..Médio else Total..Aprovação.Médio) %>% 
      select(colnames_excel6[1:9], ens_med_abn, ens_med_repr, ens_med_aprov) 
    #
    sheet04 <-  read_excel(x$full_path, na = c("", "--"),
                           sheet = 4, skip=9, 
                           col_names = colnames_excel6, 
                           col_types = "text", 
                           .name_repair = "universal") %>% 
      filter(UF %in% all_of(bla_state_siglas), Rede != "Particular") %>% 
      mutate(ens_med_abn = if("Total.Abandono.no.Ens..Médio" %in% colnames_b[11:63]) 
        Total.Abandono.no.Ens..Médio else Total.Abandono...Médio, 
        ens_med_repr = if("Total.Reprovação.no.Ens..Médio" %in% colnames_b[11:63]) 
          Total.Reprovação.no.Ens..Médio else Total..Reprovação.Médio, 
        ens_med_aprov = if("Total.Aprovação.no.Ens..Médio" %in% colnames_b[11:63]) 
          Total.Aprovação.no.Ens..Médio else Total..Aprovação.Médio) %>% 
      select(colnames_excel6[1:9], ens_med_abn, ens_med_repr, ens_med_aprov) 
    #
    sheet05 <-  read_excel(x$full_path, na = c("", "--"),
                           sheet = 5, skip=9, 
                           col_names = colnames_excel6, 
                           col_types = "text", 
                           .name_repair = "universal") %>% 
      filter(UF %in% all_of(bla_state_siglas), Rede != "Particular") %>% 
      mutate(ens_med_abn = if("Total.Abandono.no.Ens..Médio" %in% colnames_b[11:63]) 
        Total.Abandono.no.Ens..Médio else Total.Abandono...Médio, 
        ens_med_repr = if("Total.Reprovação.no.Ens..Médio" %in% colnames_b[11:63]) 
          Total.Reprovação.no.Ens..Médio else Total..Reprovação.Médio, 
        ens_med_aprov = if("Total.Aprovação.no.Ens..Médio" %in% colnames_b[11:63]) 
          Total.Aprovação.no.Ens..Médio else Total..Aprovação.Médio) %>% 
      select(colnames_excel6[1:9], ens_med_abn, ens_med_repr, ens_med_aprov)
    #
    sheet06 <-  read_excel(x$full_path, na = c("", "--"),
                           sheet = 6, skip=9, 
                           col_names = colnames_excel6, 
                           col_types = "text", 
                           .name_repair = "universal") %>% 
      filter(UF %in% all_of(bla_state_siglas), Rede != "Particular") %>% 
      mutate(ens_med_abn = if("Total.Abandono.no.Ens..Médio" %in% colnames_b[11:63]) 
        Total.Abandono.no.Ens..Médio else Total.Abandono...Médio, 
        ens_med_repr = if("Total.Reprovação.no.Ens..Médio" %in% colnames_b[11:63]) 
          Total.Reprovação.no.Ens..Médio else Total..Reprovação.Médio, 
        ens_med_aprov = if("Total.Aprovação.no.Ens..Médio" %in% colnames_b[11:63]) 
          Total.Aprovação.no.Ens..Médio else Total..Aprovação.Médio) %>% 
      select(colnames_excel6[1:9], ens_med_abn, ens_med_repr, ens_med_aprov)
    
    school_fail <- rbind(sheet01, sheet02, sheet03, sheet04, sheet05, sheet06)
    
    #summary number of schools and failure rates
    school_fail %>% 
      filter(!is.na(ens_med_aprov), !is.na(ens_med_abn), !is.na(ens_med_repr)) %>% 
      distinct() %>%
      mutate(ens_med_repr = as.numeric(ens_med_repr), 
             ens_med_abn = as.numeric(ens_med_abn)) %>%
      group_by(Ano, UF, Nome.do.Município) %>% 
      summarise(count_schools_medio = length(unique(Código.da.Escola)), 
                fail_rate_percent = median(ens_med_repr, na.rm = TRUE) + 
                  median(ens_med_abn, na.rm = TRUE), 
                tot_abn_reprov = sum(ens_med_repr, na.rm = TRUE) + 
                  sum(ens_med_abn, na.rm= TRUE)) %>% 
      mutate(fail_rate_percent2 = tot_abn_reprov/count_schools_medio) %>% 
      ungroup() %>% data.frame() -> muni_fail
    
    #format to return
    muni_out <- data.frame(flag_type = "mid_2007_2011", 
                           ANO = muni_fail$Ano,   
                           UF = NA,
                           SIGLA = muni_fail$UF, 
                           MUNIC = muni_fail$Nome.do.Município, 
                           count_schools_medio = muni_fail$count_schools_medio, 
                           fail_rate_percent = muni_fail$fail_rate_percent, 
                           fail_rate_percent2 = muni_fail$fail_rate_percent2
    )
  }
  
  if(x$ayear %in% c(2015:2020)){ 
    muni_out <- data.frame(flag_type = "recent_2012_2020", 
                           ANO = NA,   
                           UF = NA,
                           SIGLA = NA, 
                           MUNIC = NA, 
                           count_schools_medio = NA, 
                           fail_rate_percent = NA, 
                           fail_rate_percent2 = NA
    )
  }
  
  muni_out 
}
#2011 with double number of schools
df_inep_fail_2000_2020 <- plyr::ddply(dfnames_data[c(7:9),], .(aid), 
                                      .fun = inep_escola_summary)

#Names for columns 2011
dfnames_data %>% filter(ayear ==2011)  %>% pull(full_path) %>% 
  excel_sheets() -> sheet_names_2011
dfnames_data %>% filter(ayear ==2011) %>% pull(full_path) -> path_2011
read_excel(path_2011, 
           sheet = sheet_names_2011[1], skip=6, n_max=10, 
           .name_repair = "universal") %>% names() -> colnames_2011a
colnames_2011a[1:10]
read_excel(path_2011, 
           sheet = sheet_names_2011[1], skip=7, n_max=8, 
           .name_repair = "universal") %>% names() -> colnames_2011b
colnames_2011b[11:63]
colnames_2011 <- c(colnames_2011a[1:10], colnames_2011b[11:63])
read_excel(path_2011, na = c("", "--"),
           sheet = 1, skip=9, n_max=20, 
           col_names = colnames_2011, 
           col_types = "text",
           .name_repair = "universal") %>% 
  mutate(ens_med_abn = if("Total.Abandono.no.Ens..Médio" %in% colnames_2011) 
    Total.Abandono.no.Ens..Médio else Total.Abandono...Médio, 
    ens_med_repr = if("Total.Reprovação.no.Ens..Médio" %in% colnames_2011) 
      Total.Reprovação.no.Ens..Médio else Total..Reprovação.Médio) %>% 
  select(colnames_2011a[c(1,3,5)], Total.Abandono.no.Ens..Médio, ens_med_abn, ens_med_repr) 


data.frame(a2007 = colnames_2007b[11:63], a2011 = colnames_2011b[11:63])
data.frame(a2007 = colnames_2007a, a2011 = colnames_2011a)


dfnames_data %>% arrange(ayear)  



read_excel(path_2007, na = c("", "--"),
           sheet = sheet_names_2007[1], skip=9, 
           col_names = colnames_2007, 
           col_types = "text", 
           .name_repair = "universal") %>% 
  filter(UF %in% all_of(bla_state_siglas), Rede != "Particular") %>%
  filter(!is.na(Total..Reprovação.Médio), 
         !is.na(Total.Abandono...Médio)) -> test2007

read_excel(path_2007, na = c("", "--"),
           sheet = sheet_names_2007[4], skip=9, 
           col_names = colnames_2007, 
           col_types = "text", 
           .name_repair = "universal") %>% 
  filter(UF %in% all_of(bla_state_siglas), Rede != "Particular") %>%
  filter(!is.na(Total..Reprovação.Médio), 
         !is.na(Total.Abandono...Médio)) -> test2007empty


#filter(mynames == "TX RENDIMENTO ESCOLAS 2007.xls"
#
school_fail <- read_delim(unz(x$zip_name, data_file), delim = "|", 
                          col_select = all_of(indicesc_cols),
                          col_types = cols(.default = "c"), 
                          escape_double = FALSE, 
                          trim_ws = TRUE)



file_name <- "E:/edu_inep/microdados_educação_básica_2001/Dados/INDICESC_2001.CSV"
INDICESC_2001 <- read_delim(file_name, delim = "|", 
                            col_select = all_of(indicesc_cols),
                            col_types = cols(.default = "c"), 
                            escape_double = FALSE, 
                            trim_ws = TRUE) %>% 
  filter(SIGLA %in% all_of(bla_state_siglas), 
         !is.na(IEM00042), !is.na(IEM00047), DEP != "Particular")
#only 2001 to 2003
df_zipnames <- data.frame(aid = paste("microdados_educacao_basica_", 
                                      c(2001:2003), ".zip", sep=""), 
                          zip_name = paste("E:\\edu_inep\\microdados_educacao_basica_", 
                                           c(2001:2003), ".zip", sep=""))

load_inep_failure <- function(x){
  zip_files <- unzip(x$zip_name, list=TRUE)
  zip_files %>% 
    mutate(flag_name = str_detect(Name, "INDICESC")) %>% 
    filter(flag_name) %>% pull(Name) -> data_file
  
  school_fail <- read_delim(unz(x$zip_name, data_file), delim = "|", 
                            col_select = all_of(indicesc_cols),
                            col_types = cols(.default = "c"), 
                            escape_double = FALSE, 
                            trim_ws = TRUE) %>% 
    filter(SIGLA %in% all_of(bla_state_siglas), 
           !is.na(IEM00042), !is.na(IEM00047), DEP != "Particular")
  school_fail 
}

df_inep_fail_2001_2003 <- plyr::ddply(df_zipnames, .(aid), 
                                      .fun = load_inep_failure)
unique(df_inep_fail_2001_2003$ANO)

df_inep_names %>% filter(Length >0) %>% 
  group_by(inep_year, mynames) %>% 
  summarise(file_count = n()) %>%
  mutate(flag_process = case_when(file_count > 5 ~ "INDICESC", 
                                  file_count == 1 ~"old_excel", 
                                  file_count %in% c(3,4) ~"new_excel"))

load_old_excel 

zip_files <- unzip(x$zip_name, list=TRUE)
zip_files %>% 
  mutate(flag_name = str_detect(Name, "INDICESC")) %>% 
  filter(flag_name) %>% pull(Name) -> data_file

school_fail <- read_delim(unz(x$zip_name, data_file), delim = "|", 
                          col_select = all_of(indicesc_cols),
                          col_types = cols(.default = "c"), 
                          escape_double = FALSE, 
                          trim_ws = TRUE) %>% 
  filter(SIGLA %in% all_of(bla_state_siglas), 
         !is.na(IEM00042), !is.na(IEM00047), DEP != "Particular")
school_fail
