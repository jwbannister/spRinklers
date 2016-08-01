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
library(gridExtra)
library(grid)

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
df2 <- assign_areas_spdf(df2)
df2@data <- select(df2@data, -lat_long)

a <- data.frame(id=df2@data$sprinkler_surface_transect_id, 
                  x=coordinates(df2)[, 1], y=coordinates(df2)[, 2])
dist_matrix <- create_dist_matrix(a, a, c(2, 3), c(2, 3), 1, 1)
dist_melt <- reshape2::melt(dist_matrix, value.name="dist.delta")
match_df <- dist_melt %>% group_by(Var1) %>%
  filter(dist.delta==sort(dist.delta)[2]) %>% ungroup()
singles <- filter(match_df, dist.delta>20)
singles$Var2 <- NA
singles$dist.delta <- NA
singles$group.id <- seq(1, nrow(singles))
paired <- filter(match_df, !(Var1 %in% singles$Var1))
paired <- paired[!duplicated(paired$dist.delta), ]
paired$group.id <- seq(max(singles$group.id)+1, 
                       max(singles$group.id)+nrow(paired))
b <- rbind(select(singles, id=Var1, group.id), 
           select(paired, id=Var1, group.id), 
           select(paired, id=Var2, group.id))
df2@data <- inner_join(df2@data, b, 
                       by=c("sprinkler_surface_transect_id"="id"))

#writeOGR(df2, dsn=paste0(output_path, "surface_obs/"), 
#         driver="ESRI Shapefile", layer="surface_obs", overwrite_layer=TRUE)

df3 <- cbind(df2@data, df2@coords) %>%
  rename(site.id=sprinkler_surface_transect_id)


#images
spectir_reflect_ras <- raster::raster(paste0(data_path, "20160724_Hyperspec",
                                             "_SWIR_reflectance_mosaic.tif"))
spectir_reflect_df <- as.data.frame(raster::rasterToPoints(spectir_reflect_ras))
names(spectir_reflect_df)[3] <- "swir"

spectir_wetness_ras <- raster::raster(paste0(data_path, "20160724_Hyperspec",
                                             "_SWIR_wetness_mosaic.tif"))
spectir_wetness_df <- as.data.frame(raster::rasterToPoints(spectir_wetness_ras))
names(spectir_wetness_df)[3] <- "wet"
spectir_wetness_df$wet <- factor(spectir_wetness_df$wet)

landsat_wetness_ras <- raster::raster(paste0(data_path, 
                                             "20160724_gbwetness.img"))
landsat_wetness_df <- as.data.frame(raster::rasterToPoints(landsat_wetness_ras))
names(landsat_wetness_df)[3] <- "wet"
landsat_wetness_df$wet <- factor(landsat_wetness_df$wet)

grp <- 39 
group_info <- df3 %>% filter(group.id==grp) %>% select(site.id, position_id)
form_ids <- group_info$site.id
query2 <- paste0("SELECT s3_storage_id FROM field_data.images ",
                 "WHERE form_id ", 
                 "IN (", paste0(form_ids, collapse=", "), ") ", 
                 "AND form_type='sprinkler_surface_transects'")
g <- query_owens_aws(query2)[ , 1]
aws_access <- read.table("~/config/credentials/AWS_cred.txt")[2, 1]
aws_secret <- read.table("~/config/credentials/AWS_cred.txt")[4, 1]
S3_connect(aws_access, aws_secret, hostname="s3-us-west-2.amazonaws.com")
img <- vector(mode="list", length=length(g))
for (i in 1:length(g)){
  query3 <- paste0("SELECT key FROM handlers.s3_storage ",
                   "WHERE s3_storage_id=", g[i])
  k <- query_owens_aws(query3)[ , 1]
  fl <- tempfile()
  S3_get_object("owenslake", k, filename=fl)
  img[[i]] <- rasterGrob(jpeg::readJPEG(fl), interpolate=TRUE)
}

rng <- 30
plot_extent <- df3 %>% filter(group.id==grp) %>%
  summarize(min.x=mean(x)-rng, max.x=mean(x)+rng, 
            min.y=mean(y)-rng, max.y=mean(y)+rng)

system(paste0("gdalwarp -te ", plot_extent$min.x, " ", plot_extent$min.y, 
              " ", plot_extent$max.x, " ", plot_extent$max.y, 
              " -overwrite ", data_path, "heli_merge.tif ", tempdir(), 
              "/heli.tif"))
heli_ras <- raster::stack(paste0(tempdir(), "/heli.tif"))
heli_df <- as.data.frame(raster::rasterToPoints(heli_ras))
heli_df <- data.frame(x=heli_df$x, y=heli_df$y, r=heli_df[ , 3],
                     g=heli_df[ , 4], b=heli_df[ , 5])
p_heli <- df3 %>% filter(group.id==grp) %>%
  ggplot(aes(x=x, y=y)) +
  geom_tile(data=heli_df, aes(fill=rgb(r, g, b, maxColorValue=255)), alpha=0.75) +
  scale_fill_identity() +
  geom_point(color="red") + 
  geom_text(aes(label=site.id), color="red", nudge_x=1, nudge_y=1) +
  coord_equal() +
  theme(panel.grid=element_blank(),
        panel.background=element_rect(fill="white"),
        plot.margin=unit(c(0, 0, 0, 0), "cm"))

clip_df <- filter(landsat_wetness_df, 
                  between(x, plot_extent$min.x, plot_extent$max.x) &
                  between(y, plot_extent$min.y, plot_extent$max.y))
p_ls_w <- df3 %>% filter(group.id==grp) %>%
  ggplot(aes(x=x, y=y)) +
  geom_tile(data=clip_df, aes(fill=wet), color="black") + 
  geom_point(color="red") + 
  geom_text(aes(label=site.id), color="red", nudge_x=1, nudge_y=1) +
  coord_equal() +
  theme(panel.grid=element_blank(),
        panel.background=element_rect(fill="white"),
        plot.margin=unit(c(1, 0, -1, 0), "cm"))


clip_df <- filter(spectir_wetness_df, 
                  between(x, plot_extent$min.x, plot_extent$max.x) &
                  between(y, plot_extent$min.y, plot_extent$max.y))
p_st_w <- df3 %>% filter(group.id==grp) %>%
  ggplot(aes(x=x, y=y)) +
  geom_tile(data=clip_df, aes(fill=wet), color="black") + 
  geom_point(color="red") + 
  geom_text(aes(label=site.id), color="red", nudge_x=1, nudge_y=1) +
  coord_equal() +
  theme(panel.grid=element_blank(),
        panel.background=element_rect(fill="white"),
        plot.margin=unit(c(1, 0, -1, 0), "cm"))



clip_df <- filter(spectir_reflect_df, 
                  between(x, plot_extent$min.x, plot_extent$max.x) &
                  between(y, plot_extent$min.y, plot_extent$max.y))
p_st_r <- df3 %>% filter(group.id==grp) %>%
  ggplot(aes(x=x, y=y)) +
  geom_tile(data=clip_df, aes(fill=swir), color="black") + 
  geom_point(color="red") + 
  geom_text(aes(label=site.id), color="red", nudge_x=1, nudge_y=1) +
  coord_equal() +
  theme(panel.grid=element_blank(),
        panel.background=element_rect(fill="white"),
        plot.margin=unit(c(1, 0, -1, 0), "cm"))

dat <- df3 %>% filter(group.id==grp) %>%
  select("Site ID"=site.id, 
         "DCA"=area,
         "Radius Location"=position_id, 
         "Surface Wetness"=wetness_surface_id, 
         "Tap Test Wetness"=tap_test_id,
         "Subsurface Wetness"=subsurface_wetness_id,
         "Crust Thickness"=crust_thickness_id,
         "Crust Hardness"=crust_hardness_id,
         "Crust Textture"=crust_texture_id,
         "Efflorescent Salt"=efforescence_id,
         "Free Sand"=free_sand_id,
         "Stablity"=stability_id)
ttheme_custom <- ttheme_default(base_size=8)
tbl <- tableGrob(dat, rows=NULL, theme=ttheme_custom)

gs <- list(tbl, p_heli, img[[1]], img[[2]], p_ls_w, p_st_w, p_st_r)
lay <- rbind(c(NA, NA, NA), 
             c(2, 3, 4), 
             c(5, 6, 7),
             c(1, 1, 1))
grid.arrange(grobs=gs, layout_matrix=lay, heights=unit(c(.05, 3, 3, 2), 
                                                       rep("in", 4)))
