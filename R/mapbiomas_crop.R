#
#crop mapbiomas data
library(plyr)
library(tidyverse)
library(terra)
library(sf)
library(readxl)
memory.limit(30000)
#This way take a very long time for 36 years
rin <- "E:\\mapbiomas\\brasil_coverage_1985.tif"
rbig <- terra::rast(rin)
layer_name <- names(rbig)
layer_year <- substr(layer_name, 17, 20)
sf_states <- sf::st_read("vector//ninestate_poly.shp") %>% 
  st_union() %>% st_transform(crs = crs(rbig))
e2 <- ext(vect(sf_states))

#Crop
rtest_mask <- crop(rbig, e2, snap="out")
rm("rbig")

#Export
outfile <- paste("state_coverage_",layer_year, ".grd", sep = "")
f <- file.path("E:/mapbiomas/nine_states", outfile)
writeRaster(rtest_mask, outfile, datatype = "INT2U", overwrite = TRUE)
