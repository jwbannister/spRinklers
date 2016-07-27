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
library(RS3)

output_path <- path.expand("~/dropbox/output/spRinklers/")
data_path <- path.expand("~/dropbox/data/spRinklers/")
area_shp <- readOGR(path.expand("~/dropbox/owens/sprinklers/Owens SWIR July24/SHP"), 
                    "SF_Sprinkler_Extent_ver3")
sprinkler_polygons <- 
  filter(owens_areas$bacm$polygons, 
         area %in% c("T28S", "T28N", "T1A-4", "T1A-2", "T37-2"))
sprinkler_polygons$area <- ifelse(sprinkler_polygons$area %in% c("T28S", "T28N"), 
                                  "T28", sprinkler_polygons$area)
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
df2 <- assign_areas_df(df2)

df3 <- data.frame(id=df2$sprinkler_surface_transect_id, x=coordinates(df2)[, 1], 
                  y=coordinates(df2)[, 2])
dist_matrix <- create_dist_matrix(df3, df3, c(2, 3), c(2, 3), 1, 1)
dist_melt <- reshape2::melt(dist_matrix, value.name="dist.delta")
match_df <- dist_melt %>% group_by(Var1) %>%
  filter(dist.delta==sort(dist.delta)[2]) %>% ungroup()
dups <- match_df[duplicated(match_df$Var2), ]$Var2
a <- filter(match_df, Var2 %in% dups) %>% group_by(Var2) %>%
  filter(dist.delta==max(dist.delta))
match_df <- filter(match_df, !(Var1 %in% a$Var1))
match_df <- match_df[!duplicated(match_df$dist.delta), ]
match_df$group.id <- seq(1, nrow(match_df))
pairs_df <- data.frame(id=c(match_df$Var1, match_df$Var2), 
                  pair.id=c(match_df$group.id, match_df$group.id))
df2@data <- df2@data %>% filter(!(sprinkler_surface_transect_id %in% a$Var1)) %>%
  inner_join(pairs_df, by=c("sprinkler_surface_transect_id"="id"))


writeOGR(df2, dsn=paste0(output_path, "surface_obs/"), 
         driver="ESRI Shapefile", layer="surface_obs", overwrite_layer=TRUE)
area_list <- vector(mode="list", 
                    length=length(unique(sprinkler_polygons$area)))
names(area_list) <- unique(sprinkler_polygons$area)

aws_access <- read.table("~/config/credentials/AWS_cred.txt")[2, 1]
aws_secret <- read.table("~/config/credentials/AWS_cred.txt")[4, 1]
S3_connect(aws_access, aws_secret, hostname="s3-us-west-2.amazonaws.com")
S3_get_object("owenslake", "field_data/swift5.jpg", 
              filename=path.expand("~/Desktop/test.jpg"))

# SpecTIR image
spectir_ras <- raster::raster(paste0(data_path, "swir/06242016_26911.tif"))
spectir_df <- raster::extract(spectir_ras, area_shp)

