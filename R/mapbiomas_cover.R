#Mapbiomas coverage

#Get mapbiomas data
library(plyr)
library(tidyverse)
library(terra)
library(sf)
library(readxl)
memory.limit(30000)
#Basic reference vectors
bla_state_names <- c("Acre", "Amapá", "Amazonas", "Maranhão", 
                     "Mato Grosso", "Pará", "Tocantins", "Rondônia", "Roraima")
bla_state_siglas <- c("AC", "AP", "AM", "MA", 
                      "MT", "PA", "TO", "RO", "RR")
dfstates <- data.frame(bla_state_names, bla_state_siglas)

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
#State Poligonos
sf_states <- sf::st_read("vector//ninestate_poly.shp") 
#Municipality polygons
longname <- "vector//brazil_ninestate_municipalities//ninestate_muni.shp"
sf_munis <- sf::st_read(longname)

#annual coverage
"1600303"
# zeros are NA http://forum.mapbiomas.ecostage.com.br/t/pixels-com-valor-zero/170/5
tif_files <- list.files("G:/mapbiomas", pattern = ".tif", full.names = TRUE)
data.frame(sf_munis) %>% select(CD_MUN, NM_MUN, SIGLA_UF) %>% 
  crossing(tif_files) %>% data.frame() -> df_muni_tif
df_muni_tif %>% 
  filter(SIGLA_UF =="MA") %>% data.frame() -> df_muni_tif_MA 
df_muni_tif_MA %>% 
  filter(NM_MUN %in% all_of(MA_priority)) -> df_muni_tif_MA_priority
df_muni_tif %>% 
  filter(SIGLA_UF =="TO", NM_MUN == "Tabocão") %>% 
  data.frame() -> df_muni_tif_TO

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
plyr::a_ply(df_muni_tif_MA[1, ], .margins = 1, .fun = mapbiomas_summary)

#OK
plot(crop(rast("E:\\mapbiomas\\brasil_coverage_1985.tif"), e1))

rast("G:\\mapbiomas\\brasil_coverage_1986.tif") %>% 
  crop(e2) %>% plot()

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
