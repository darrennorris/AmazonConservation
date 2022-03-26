
#' Title
#' 
#' @title Land cover summary using MapBiomas.
#' @param x Dataframe including locations for raster files.
#' @param large_polygon Sf object holding spatial polygons.
#' 
#' @description Provides a summary of land cover using annual Mapbiomas coverage. 
#' This function serves as a code template and 
#' could be adapted for polygons of farms and river basins
#'
#' @return A text file with annual totals for cover classes. 
#' Also a text log file with process timing. Text files are tab separated.
#'  
#' @export
#'
#' @examples
#' \dontrun{
#' #1) fairly efficient for areas < 6000 km2
#' #run function
#' plyr::ddply(dfmulti, .(SIGLA_UF, NM_MUN),
#'             .fun = mapbiomas_summary_calc, large_polygon = sf_munis)
#' #a_dply version. time is same ddply in a small test 
#' #plyr::a_ply(dfmulti, .margins = 1, 
#' .fun = mapbiomas_summary_calc, large_polygon = sf_munis)
#'             
#' }
mapbiomas_summary_calc <- function(x, large_polygon = NA){
  time_start <- Sys.time()
  polygon_id <- x$CD_MUN[1]
  rin <- x$tif_files
  rtest <- rast(rin)
  #cover_name <- names(rtest)
  #ayear <- substr(cover_name, 17, 20)
  
  large_polygon %>% filter(CD_MUN == polygon_id) %>% 
    st_transform(crs = crs(rtest)) -> sf_muni
  e2 <- ext(vect(sf_muni))
  rtest_mask <- mask(x = crop(rtest, e2), mask = vect(sf_muni))
  NAflag(rtest_mask) <- 0
  
  #calculate area of different cover classes
  #Unique cover class values from all years
  cvals <- unique(na.omit(c(as.matrix(unique(rtest_mask)))))
  dfcvals <- data.frame(class_values = cvals, class_ref = cvals)
  #Fuction to seperatte classes and sum area
  cover_sum <- function(x, rast_stack = rtest_mask){
    myclass <- x$class_ref
    myclass_low <- myclass -1
    myclass_high <- myclass +1
    m <- c(-Inf, myclass_low, NA,
           myclass-0.1, myclass+0.1, 1,
           myclass_high, Inf, NA)
    rclmat <- matrix(m, ncol=3, byrow=TRUE)
    rclass <- classify(rast_stack, rclmat)
    astack <- rclass*cellSize(rast_stack[[1]])
    
    #sum
    class_area <- global(astack, fun="sum",  na.rm=TRUE)
    class_area$tif_ref <- row.names(class_area)
    row.names(class_area) <- NULL
    class_area %>% mutate(year = substr(tif_ref,17,20), 
                          area_ha = sum/10000) %>% data.frame() -> class_area
    class_area
  }
  dfcover <- ddply(dfcvals, .(class_values), .fun = cover_sum)
  rm("rtest_mask")
  rm("large_polygon")
  rm("sf_muni")
  #organise for export
  dfout <- data.frame(CD_MUN = polygon_id, 
                      dfcover[, c('class_values', 'year', 'area_ha')])
  dfout$year <- as.numeric(dfout$year)
  dfout %>% 
    arrange(class_values) %>% data.frame() -> dfout
  
  #Export 
  if(file.exists("mapbiomas_cover.txt")){
    write.table(dfout, "mapbiomas_cover.txt", col.names = FALSE, 
                row.names = FALSE, append = TRUE)}else{ 
                  write.table(dfout, "mapbiomas_cover.txt", row.names = FALSE)
                }
  
  #log file
  time_end <- Sys.time()
  tdiff_sec <- difftime(time_end, time_start, units = "secs")[[1]]
  tdiff_min <- tdiff_sec / 60
  dfout_log <- data.frame(CD_MUN = x$CD_MUN[1], AREA_KM2 = x$AREA_KM2[1],
                          time_taken_min = round(tdiff_min,3),
                          adt_start = time_start)
  
  #Export log
  if(file.exists("mapbiomas_cover_log.txt")){
    write.table(dfout_log, "mapbiomas_cover_log.txt", col.names = FALSE, 
                row.names = FALSE, append = TRUE)}else{ 
                  write.table(dfout_log, "mapbiomas_cover_log.txt", 
                              row.names = FALSE)
                }
}