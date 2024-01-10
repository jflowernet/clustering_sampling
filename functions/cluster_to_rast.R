#convert cluster partition results to raster

cluster_to_rast <- function(raster_temp, cluster_results){
    #create template raster
    raster_template <- terra::rast(raster_temp, nlyrs=1, vals = NA, names = "enviro_region")
    
    #set cluster ids in raster - subset for only raster values that are non-NA
    raster_template[as.numeric(names(cluster_results))] <- cluster_results
    
    raster_template
}