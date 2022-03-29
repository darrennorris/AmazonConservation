#
#crop mapbiomas data
library(plyr)
library(tidyverse)
library(terra)
library(sf)
library(readxl)
memory.limit(30000)

#Municipality polygons
longname <- "vector//brazil_ninestate_municipalities//ninestate_muni.shp"
sf_munis <- sf::st_read(longname)
#Most large in AM, MT, PA
data.frame(sf_munis) %>% pull(AREA_KM2) %>% unique() %>% sort()
data.frame(sf_munis) %>% 
  filter(AREA_KM2 > 10000, AREA_KM2 < 70000) %>% 
  group_by(SIGLA_UF) %>% summarise(large_count = n())

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
new_crs <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"
rtest_mask <- project(rtest_mask, new_crs, method = "near")

#Export
outfile <- paste("state_coverage_", state_id,"_", layer_year, ".tif", sep = "")
#f <- file.path("E:/mapbiomas/nine_states", outfile)
writeRaster(rtest_mask, outfile, datatype = "INT2U", overwrite = TRUE)

#clear temporary files
tmpFiles(current =TRUE, remove = TRUE)

stopCluster(usecores)
