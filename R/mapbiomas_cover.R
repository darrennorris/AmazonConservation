#Mapbiomas coverage

#Get mapbiomas data
library(plyr)
library(tidyverse)
library(terra)
library(sf)
library(readxl)
memory.limit()
memory.limit(30000)

#State Poligonos
#sf_states <- sf::st_read("vector//ninestate_poly.shp") 
#Municipality polygons
longname <- "vector//brazil_ninestate_municipalities//ninestate_muni.shp"
sf_munis <- sf::st_read(longname)

#annual coverage
#"1600303"
# zeros are NA http://forum.mapbiomas.ecostage.com.br/t/pixels-com-valor-zero/170/5
tif_files <- list.files("G:/mapbiomas", pattern = ".tif", full.names = TRUE)
data.frame(sf_munis) %>% select(CD_MUN, NM_MUN, SIGLA_UF, AREA_KM2) %>% 
  crossing(tif_files) %>% 
  mutate(ayear = str_sub(tif_files,-8,-5)) %>% 
  mutate(aid = paste(CD_MUN, ayear, sep = "_")) %>% 
  data.frame() -> df_muni_tif

#Missing cover data
missing_munis <- c("2100105", "2100154", "2100303", "2100808", "2100907", 
                   "2101509", "2101707", "2101731", "2102101", "2102200",
                   "2103000", "2103208", "2103406", "2103901", "2105005",
                   "2105922", "2106300", "2106409", "2106607", "2106672",
                   "2107803", "2108058", "2109403", "2110104", "2110237", 
                   "2110278", "2110401", "2110609", "2110906", "2111102", 
                   "2111953", "2112209", "2112506", "2112605", "1708254"
                   )

df_muni_tif %>% 
  filter(CD_MUN %in% all_of(missing_munis)) %>% 
  arrange(desc(AREA_KM2), ayear) %>%
  data.frame() -> df_muni_missing 
df_muni_missing %>% pull(AREA_KM2) %>% unique() %>% sort()
#df_muni_tif %>% 
#  filter(SIGLA_UF =="MA") %>% data.frame() -> df_muni_tif_MA 
#df_muni_tif_MA %>% 
#  filter(NM_MUN %in% all_of(MA_priority)) -> df_muni_tif_MA_priority
#df_muni_tif %>% 
#  filter(SIGLA_UF =="AP") %>% 
#  data.frame() -> df_muni_tif_AP
df_muni_tif %>% 
  filter(SIGLA_UF =="TO", NM_MUN == "Tabocão") %>% 
  data.frame() -> df_muni_tif_TO
df_muni_tif %>% 
  filter(SIGLA_UF =="MT", NM_MUN == "Alta Floresta") %>% 
  data.frame() -> df_muni_tif_AF

dfmulti <- rbind(df_muni_tif_TO, df_muni_tif_AF)
#parallel example
library(doParallel)
## number of cores
cores <- detectCores()
cores
## register
registerDoParallel(cores=cores)
source("R/mapbiomas_summary_calc.R")
#run 
plyr::a_ply(df_muni_missing, .margins = 1,
             .fun = mapbiomas_summary_calc, large_polygon = sf_munis, 
             .parallel = TRUE)            

read.csv("mapbiomas_cover_log.csv") %>% 
  group_by(CD_MUN, AREA_KM2) %>% 
  summarise(time_minutes = sum(time_taken_min), 
            year_count = n()) 
#1.82 i.e 24 hours for 800
#3.52 slower computer 48 hours for 800
#Make total same across years, compare 1991 and 2019
read.csv("mapbiomas_cover.csv") %>% 
  group_by(CD_MUN, year) %>% summarise(tot_ha = sum(area_ha)) %>% 
  arrange(tot_ha)

#Update and run again
df_muni_missing %>% 
  left_join(read.csv("mapbiomas_cover_01.csv") %>% 
              group_by(CD_MUN, year) %>% 
              summarise(count_class = n()) %>%
              mutate(CD_MUN = as.character(CD_MUN), 
                     ayear = as.character(year)) 
  ) %>% filter(is.na(count_class)) %>% 
  select(CD_MUN, ayear) %>% left_join(df_muni_tif) -> df_muni_missing_02

#run 
plyr::a_ply(df_muni_missing_02, .margins = 1,
            .fun = mapbiomas_summary_calc, large_polygon = sf_munis) 

rbind(read.csv("mapbiomas_cover_log_01.csv"), 
      read.csv("mapbiomas_cover_log_02.csv")
               ) %>% 
  group_by(CD_MUN, AREA_KM2) %>% 
  summarise(time_minutes = sum(time_taken_min), 
            year_count = n()) %>% 
  #pull(year_count) %>% summary() # should be 36
  arrange(desc(year_count)) #2103208 has duplicates

#upto 3000 km2 ok. After linear increase in time.
rbind(read.csv("mapbiomas_cover_log_01.csv"), 
      read.csv("mapbiomas_cover_log_02.csv")
) %>% 
  group_by(CD_MUN, AREA_KM2) %>% 
  summarise(time_minutes = sum(time_taken_min), 
            year_count = n()) %>% 
  ggplot(aes(y=time_minutes, x = AREA_KM2)) + 
  geom_point() + stat_smooth(method = "gam")

#update muni list
df_muni_tif %>% 
  left_join(rbind(read.csv("mapbiomas_cover_01.csv"), 
                  read.csv("mapbiomas_cover_02.csv") 
                  )%>% 
              group_by(CD_MUN, year) %>% 
              summarise(count_class = n()) %>%
              mutate(CD_MUN = as.character(CD_MUN), 
                     ayear = as.character(year)) 
  ) %>% filter(is.na(count_class)) %>% 
  select(CD_MUN, ayear) %>% left_join(df_muni_tif) -> df_muni_todo
saveRDS(df_muni_todo, "df_muni_todo.RDS")
df_muni_todo <- readRDS("df_muni_todo.RDS")
df_muni_todo %>% pull(AREA_KM2) %>% unique() %>% sort

df_muni_todo %>% filter(AREA_KM2 <3000) %>% data.frame() -> df_muni_small

#run 
plyr::a_ply(df_muni_small, .margins = 1,
            .fun = mapbiomas_summary_calc, large_polygon = sf_munis) 


#to 
my_list <- split(df_muni_missing_02, f = df_muni_missing_02$aid)
lapply(my_list$`2100808_2014` , mapbiomas_summary_calc(large_polygon = sf_munis))


# missing municipality list -----------------------------------------------


muni_toget <- c("Afonso Cunha", "Água Doce do Maranhão", "Aldeias Altas", 
                "Anapurus", "Araioses", "Barão de Grajaú", "Barreirinhas",
                "Belágua", "Brejo", "Buriti", "Buriti Bravo", 
                "Caxias", "Chapadinha","Cachoeira Grande", "Codó", "Coroatá", 
                "Coelho Neto", "Duque Bacelar","Fortuna", 
                "Humberto de Campos","Gonçalves Dias", "Governador Eugênio Barros", 
                "Governador Luiz Rocha", 
                "Icatu", "Lagoa do Mato","Morros", "Magalhães de Almeida", "Mata Roma", 
                "Matões", "Milagres do Maranhão", "Parnarama", "Paulino Neves",
                "Paraibano", "Pastos Bons", "Pirapemas", "Primeira Cruz",
                "Presidente Juscelino", "Presidente Vargas", "São João do Sóter", 
                "Santa Quitéria do Maranhão", "Santana do Maranhão", 
                "Santo Amaro do Maranhão", "São Benedito do Rio Preto", 
                "São Bernardo", "São Francisco do Maranhão", 
                "São João dos Patos", "Sucupira do Riachão", 
                "Senador Alexandre Costa", "Timbiras", "Timon", "Tutóia", 
                "Urbano Santos", "Vargem Grande")

MA_priority <- c("Aldeias Altas",
                 "Barreirinhas",
                 "Buriti",
                 "Caxias",
                 "Chapadinha",
                 "Parnarama",
                 "Santa Quitéria do Maranhão",
                 "São Benedito do Rio Preto",
                 "Urbano Santos"
)

# dev function using extract ----------------------------------------------


#7:08
mapbiomas_summary <- function(x){

muni_code <- x$CD_MUN
rin <- x$tif_files
rtest <- rast(rin)
cover_name <- names(rtest)
ayear <- substr(cover_name, 17, 20)


sf_munis %>% filter(CD_MUN == muni_code) %>% 
  st_transform(crs = crs(rtest)) -> sf_muni
e2 <- ext(vect(sf_muni))
rtest_crop <- crop(rtest, e2) # 
NAflag(rtest_crop) <- 0
mysizes <- cellSize(rtest_crop)

#these steps take longest
mysize_cells <- extract(mysizes, vect(sf_muni), cells = TRUE)
myvalue_cells <- extract(rtest_crop, vect(sf_muni), cells = TRUE) 
#join
myvalue_cells %>% select(cover_name, cell) %>% 
  left_join(mysize_cells %>% 
              select(area, cell)) -> df_cells
colnames(df_cells)[1] <- "cover_class"
#calculate area 
df_cells %>% 
  group_by(cover_class) %>% summarise(area_m2 = sum(area)) %>% 
  mutate(area_ha = round(area_m2/10000, 3)) %>% 
  filter(!cover_class ==0) %>%
  select(cover_class, area_ha)-> df_cover

dfout <- data.frame(muni_code, ayear, df_cover)
#Export 
if(file.exists("mapbiomas_cover.txt")){
write.table(dfout, "mapbiomas_cover.txt", col.names = FALSE, 
            row.names = FALSE, append = TRUE)}else{ 
            write.table(dfout, "mapbiomas_cover.txt", row.names = FALSE)
            }
}
plyr::a_ply(df_muni_tif_AP, .margins = 1, .fun = mapbiomas_summary)

#OK
plot(crop(rast("E:\\mapbiomas\\brasil_coverage_1985.tif"), e1))

rast("G:\\mapbiomas\\brasil_coverage_1986.tif") %>% 
  crop(e2) %>% plot()


# notes -------------------------------------------------------------------


# raster value legend and colour map for mapbioas v6
mapvals <- read_excel("mapbiomas_6_legend.xlsx")
length(unique(mapvals$aid)) # 34
length(unique(mapvals$class_description)) #34
# "terra" will base colour table on this - table needs full number of levels 
# need NA (start, first level, 0) then integer sequence to max of id values .
cat_levels <- unique(rtest_ap) 

# color table
cat_levels %>% 
  left_join(mapvals, by=c("brasil_coverage_1991" = "aid")) %>% 
  mutate(hexdec_r = paste("#",hexadecimal_code, sep="")
  ) -> cat_labels
df_cols_rgb <- data.frame(t(col2rgb(cat_labels$hexdec_r, alpha = FALSE)))
#Example with NA, NA first so data.frame starts at 0
data.frame(aid = (0:max(cat_levels$brasil_coverage_1991))) %>% 
  left_join(mapvals) %>% left_join(cat_labels) %>% as.data.frame() -> cat_colours
#set categories and corresponding colour table
levels(rtest_ap) <- cat_labels
coltab(rtest_ap) <- cat_colours$hexdec_r
plot(rtest_ap)
