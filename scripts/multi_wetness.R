#' multi_wetness.R
#' Bring in multiple data sources to build multi-faceted picture of wetness 
#' in sprinkler controlled DCAs.

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
df0 <- query_AGOL("OwensLake_Phase7a", "OwensLake_Sprinkler_Transects", 0)
df1 <- filter(df0, !is.na(name))
df2 <- select(df1, -last.edited.user, -last.edited.date, -globalid,
              -created.user)
df2$created.date <- convert_ESRI_date(df2$created.date)
df3 <- recode_variables(df2, "OwensLake_Phase7a", 
                        "OwensLake_Sprinkler_Transects", 0)
df3$area <- sapply(mapply(c, df3$x, df3$y, SIMPLIFY=F), assign_points)
points_df <- SpatialPointsDataFrame(select(df3, x, y), 
                                    select(df3, -x, -y), 
                                    proj4string=CRS(proj_string))
writeOGR(points_df, dsn=paste0(output_path, "surface_obs/"), 
         driver="ESRI Shapefile", layer="surface_obs", overwrite_layer=FALSE)

# SpecTIR image
spectir_ras <- 
  raster::raster(paste0(data_path, "swir/06242016_26911.tif"))
spectir_df <- as.data.frame(raster::rasterToPoints(spectir_ras))
names(spectir_df)[3] <- "swir"
