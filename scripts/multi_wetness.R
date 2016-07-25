# multi_wetness.R
# Bring in multiple data sources to build multi-faceted picture of wetness 
# in sprinkler controlled DCAs.

load_all()
load_all("~/code/owensData")
load_all("~/code/owensMaps")
library(dplyr)
library(ggplot2)
library(sp)
library(rgdal)
library(parallel)

output_path <- path.expand("~/dropbox/output/spRinklers/")
data_path <- path.expand("~/dropbox/data/spRinklers/")
sprinkler_polygons <- 
  filter(owens_areas$bacm$polygons, 
         area %in% c("T28S", "T28N", "T1A-4", "T1A-2", "T37-2"))
sprinkler_labels <- 
  filter(owens_areas$bacm$labels, 
         area %in% c("T28S", "T28N", "T1A-4", "T1A-2", "T37-2"))

# surface observations
query1 <- paste0("SELECT * FROM field_data.sprinkler_surface_transects ",
                 "WHERE datetime::date='2016-07-24'")
df0 <- query_owens_aws(query1)
df0 <- select(df0, -added_on, -device_uid)
df1 <- df0
for (i in c("tap_test", "crust_thickness", "crust_hardness", "crust_texture",
            "subsurface_wetness", "efforescence", "free_sand", "stability", 
            "wetness_surface", "position")){
  df1 <- recode_variable_AWS(df1, i)
}
for (j in 1:nrow(df1)){
  tmp <- jsonlite::fromJSON(df1$lat_long[j])
  df1$lat[j] <- as.numeric(tmp$lat)
  df1$long[j] <- as.numeric(tmp$long)
}
df1$lat <- as.numeric(df1$lat)
df1$long <- as.numeric(df1$long)
coordinates(df1) <- c("long", "lat")
proj4string(df1) <- paste0("+proj=longlat")
df2 <- spTransform(df1, CRS(proj_string))
dimnames(df2@coords)[[2]] <- c("x", "y")
dimnames(df2@bbox)[[1]] <- c("x", "y")

df2$area <- sapply(mapply(c, coordinates(df2)[ , "x"], coordinates(df2)[ , "y"],
                          SIMPLIFY=F), 
                   assign_points)
writeOGR(df2, dsn=paste0(output_path, "surface_obs/"), 
         driver="ESRI Shapefile", layer="surface_obs", overwrite_layer=TRUE)

# SpecTIR image
spectir_ras <- 
  raster::raster(paste0(data_path, "swir/06242016_26911.tif"))
spectir_df <- as.data.frame(raster::rasterToPoints(spectir_ras))
names(spectir_df)[3] <- "swir"
