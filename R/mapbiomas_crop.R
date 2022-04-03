#
#crop mapbiomas data
library(plyr)
library(tidyverse)
library(terra)
library(sf)
library(readxl)
memory.limit(30000)

#Municipality polygons
longname <- "vector/brazil_ninestate_municipalities/ninestate_muni.shp"
sf_munis <- sf::st_read(longname) 
data.frame(sf_munis) %>% group_by(SIGLA_UF) %>% 
  summarise(area = sum(AREA_KM2)) %>%
  arrange(desc(area))

sf_instate <- st_read("vector//ninestate_poly.shp")
#Most large in AM, MT, PA
data.frame(sf_munis) %>% pull(AREA_KM2) %>% unique() %>% sort()
data.frame(sf_munis) %>% 
  filter(AREA_KM2 > 10000, AREA_KM2 < 70000) %>% 
  group_by(SIGLA_UF) %>% summarise(large_count = n())

rm("df_muni_tif")
mystates <- c("AM", "MT", "PA")
get_files <- function(folder_name = NA, state_codes = NA) {
  library(tidyverse)
folder_location <- paste(folder_name, 
                         state_codes, sep="")
tif_files <- list.files(folder_location, 
                        pattern = ".tif", full.names = TRUE)
data.frame(folder_id = folder_location, file_id = tif_files) %>% 
  mutate(state_code = str_sub(folder_id,-2,-1), 
         ayear = str_sub(tif_files,-8,-5)) %>% 
  group_by(folder_id, file_id, state_code, ayear) %>% 
  summarise(file_count = n()) %>% 
  ungroup() -> df_muni_tif
return(df_muni_tif)
}
df_muni_tif <- get_files(folder_name = "mapbiomas_ge/state_cover/", 
          state_codes = mystates)

rin <- df_muni_tif$file_id[1] 
rbig <- terra::rast(rin)
res(rbig) *100000
crs(rbig)

state_cover <- function(x, state_id = "AM", 
                        sf_state = NA) {
  library(plyr)
  library(tidyverse)
  library(terra)
  library(sf)
  library(stringi)
  
  state_sigla = state_id 
  rin <- x$file_id 
  rbig <- terra::rast(rin)
  layer_name <- names(rbig)
  layer_year <- stri_sub(layer_name,-4,-1)
  #States
  #sf_state %>% 
  #  filter(SIGLA_UF == state_sigla)  %>% 
  #  st_transform(crs = crs(rbig)) -> sf_state
  #e2 <- ext(vect(sf_state)) 
  
  #Crop
  #rtest_mask <- crop(rbig, e2, snap="out")
  rtest_mask <- rbig
  rm("rbig") 
  
  #Project
  new_crs <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"
  rtest_mask <- project(rtest_mask, new_crs, method = "near") 
  
  #Export
  folder <- paste(state_sigla,"_", "equalarea", sep ="")
  folder_path <- paste("mapbiomas_ge/state_cover/", folder, sep = "")
  outfile <- paste("state_coverage_", state_sigla,"_", 
                   layer_year, ".tif", sep = "")
  f <- file.path(folder_path, outfile)
  writeRaster(rtest_mask, f, datatype = "INT2U", overwrite = TRUE)
  #writeRaster(rtest_mask, outfile, datatype = "INT2U", overwrite = TRUE)
  
  #clear temporary files
  tmpFiles(current =TRUE, remove = TRUE) 
  
  endtime <- Sys.time() 
  #return(print(endtime))
}
#run
plyr::a_ply(df_muni_tif[1, ], .margins = 1, 
            .fun = state_cover)

#parallel example
library(doParallel)
## number of cores
cores <- detectCores()
cores
usecores <- cores -1
## register
cl <- makeCluster(usecores)
registerDoParallel(cl)
#9:18 - 10:12 #Amazonas = 1 hour
#10:24 - 11:23 1 hour for 3, 12 hours for 36
seldone <- which(df_muni_tif$ayear %in% c(2013, 2014, 2015))
plyr::a_ply(df_muni_tif[-seldone, ], .margins = 1, 
            .fun = state_cover, sf_state = sf_instate , .parallel = TRUE)
stopCluster(usecores)

#This way take a very long time for 36 years
# See options for parallel processing
# https://www.gis-blog.com/increasing-the-speed-of-raster-processing-with-r-part-23-parallelisation/
rin <- "F:\\mapbiomas\\brasil_coverage_1985.tif"
rbig <- terra::rast(rin)
layer_name <- names(rbig)
layer_year <- substr(layer_name, 17, 20)

state_id <- "TO"
#States
sf_instate <- st_read("vector\\ninestate_poly.shp") %>% 
  filter(SIGLA_UF == state_id)  %>% 
  st_transform(crs = crs(rbig))
e2 <- ext(vect(sf_instate))

#parallel example
library(doParallel)
## number of cores
cores <- detectCores()
cores
usecores <- cores -1
## register
cl <- makeCluster(usecores)
registerDoParallel(cl)

#Crop
rtest_mask <- crop(rbig, e2, snap="out")
rm("rbig") 

#Project
#EPSG:102033
new_crs <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"
rtest_mask <- project(rtest_mask, new_crs, method = "near")

#Export
outfile <- paste("state_coverage_", state_id,"_", layer_year, ".tif", sep = "")
#f <- file.path("E:/mapbiomas/nine_states", outfile)
writeRaster(rtest_mask, outfile, datatype = "INT2U", overwrite = TRUE)

#clear temporary files
tmpFiles(current =TRUE, remove = TRUE)

stopCluster(usecores)
