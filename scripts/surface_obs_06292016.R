
load_all()
load_all("~/analysis/Rowens")
library(dplyr)
library(jsonlite)
library(ggplot2)
library(foreach)

df0 <- query_AGOL("OwensLake_Phase7a", "OwensLake_Sprinkler_Transects", 0)
df1 <- filter(df0, !is.na(name))

meta <- query_AGOL_json("OwensLake_Phase7a", "OwensLake_Sprinkler_Transects", 0)
meta_codes <- meta$fields$domain$codedValues

for (i in 2:10){
  tmp <- meta_codes[[i]]
  df1[ , i] <- sapply(df1[ , i], 
                      function(x) ifelse(!is.na(x), filter(tmp, code==x)$name,
                                         x))
}
df1[ , 2] <- factor(df1[ , 2], levels=c("Between circles", 
                                        "Within circle\nWithin circle"),
                    labels=c("between_circles", "within_circles"))
df1[ , 3] <- factor(df1[ , 3], levels=c("Dry", "Moist", "Free water (shiny)",  
                                        "Standing water"), 
                    exclude=c(NA, "Too crusted for tap test"),
                    labels=c("dry", "moist", "shiny", "standing_water"))
df1[ , 4] <- factor(df1[ , 4], levels=c("None", "Thin (<0.5 cm)", 
                                        "Thick (>=0.5 cm)"), 
                    labels=c("none", "thin", "thick"))
df1[ , 5] <- factor(df1[ , 5], levels=c("Soft",
                                        "Firm but breaks when tapped",
                                        "Firm but can break",
                                        "Hard, difficult to break"), 
                    labels=c("soft", "semi_firm", "firm", "hard"))
df1[ , 6] <- factor(df1[ , 6], levels=c("Smooth",
                                        'Fine (<4\") heaved\nFine (<4\") heaved',
                                        'Coarse (>=4\") heaved'),
                    labels=c("smooth", "fine_heaved", "coarse_heaved"))
df1[ , 7] <- factor(df1[ , 7], levels=c("Dry", "Moist", "Free water (shiny)"),  
                    labels=c("dry", "moist", "wet"))
df1[ , 8] <- factor(df1[ , 8], levels=c("Absent", "Present"), 
                    labels=c("absent", "present"))
df1[ , 9] <- factor(df1[ , 9], levels=c("Absent", "Present"), 
                    labels=c("absent", "present"))
df1[ , 10] <- factor(df1[ , 10], levels=c("Non-emissive", "Fragile", 
                                          "Emissive"), 
                    labels=c("non_emissive", "fragile", "emissive"))

path <- path.expand("~/dropbox/data/sfwctR/data-raw/non-test_sprinkler_areas/")
lyr <- "sprinkler_DCA"
sprinklers <- rgdal::readOGR(path, lyr)
sprinklers_df <- sprinklers@data %>%
  select(OBJECTID, area=StdPolygon, bacm=BACM_Type, acres=Acres, 
         bacm2=BACM_xtra)
sprinklers_polys <- data.frame(x=c(), y=c())
for (i in 1:nrow(sprinklers_df)){
  sprinklers_df$labpt.x[i] <- sprinklers@polygons[[i]]@Polygons[[1]]@labpt[1]
  sprinklers_df$labpt.y[i] <- sprinklers@polygons[[i]]@Polygons[[1]]@labpt[2]
  temp_polys <- data.frame(sprinklers@polygons[[i]]@Polygons[[1]]@coords)
  names(temp_polys) <- c("x", "y")
  temp_polys$objectid <- sprinklers_df$OBJECTID[i]
  sprinklers_polys <- rbind(sprinklers_polys, temp_polys)
}

for (i in 1:nrow(df1)){
  df1$objectid[i] <- assign_points(c(df1$x[i], df1$y[i]), sprinklers_polys)
}
df2 <- inner_join(df1, select(sprinklers_df, OBJECTID, area, bacm, bacm2), 
                by=c("objectid"="OBJECTID"))
df2 <- select(df2, -last.edited.date, -last.edited.user, -created.date,
              -created.user, -globalid, -other)

swir_ras <- raster::raster("~/dropbox/data/sfwctR/data-raw/20160630_L7_SWIR.tif")
for (i in 1:nrow(df2)){
  df2$swir[i] <- raster::extract(swir_ras, cbind(df2$x[i], df2$y[i]))
  df2$compliant[i] <- ifelse(df2$swir[i]<0.195, T, F)
}

location_plots <- vector(mode="list", length=length(unique(df2$area))) 
names(location_plots) <- unique(df2$area)
map_file <- "~/dropbox/data/sfwctR/data-raw/pleiades_20160401.tif"

for (i in names(location_plots)){
  polys <- inner_join(sprinklers_polys, select(sprinklers_df, OBJECTID, area),
                      by=c("objectid"="OBJECTID")) %>%
    filter(area==i)
  plot.range <- get_plot_range(polys)
  temp_file <- tempfile()
  system(paste0("gdalwarp -te ", plot.range$x[1], " ", plot.range$y[1], " ",
                plot.range$x[2], " ", plot.range$y[2], " ", map_file, " ", 
                temp_file, ".tif"))
  map <- raster::stack(paste0(temp_file, ".tif"))
#  ext <- sp::SpatialPointsDataFrame(coords=cbind(x=plot.range$x, y=plot.range$y), 
#                                data=data.frame(id=1:2), 
#                                proj4string=raster::crs(map))
#  map_sub <- raster::crop(map, raster::extent(ext))
  map_sub <- raster::aggregate(map, 4)
  map_df <- raster::as.data.frame(map_sub, xy=T)
  names(map_df) <- c("x", "y", "r", "g", "b")
  location_plots[[i]] <- df2 %>% filter(area==i) %>%
    ggplot(aes(x=x, y=y)) +
    geom_tile(data=map_df, 
              mapping=aes(x=x, y=y, fill=rgb(r,g,b, maxColorValue = 255)), 
              alpha=0.75) + 
    geom_point(aes(color=position)) +
    geom_path(data=polys, aes(x=x, y=y, group=objectid)) +
    scale_fill_identity() +
    coord_equal() +
    theme_bw() +
    ggtitle(i) +
    theme(axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          legend.position="bottom",
          panel.grid=element_blank())
}

a <- df2 %>% group_by(area, position, compliant) %>%
  do(taptest.dry=tally(filter(., taptest=="dry" | is.na(taptest)))/tally(.),
     taptest.moist=tally(filter(., taptest=="moist"))/tally(.), 
     taptest.shiny=tally(filter(., taptest=="shiny"))/tally(.),
     taptest.standing_water=tally(filter(., taptest=="standing_water"))/tally(.),
     crustthick.none=round(tally(filter(., crustthick=="none"))/tally(.), 2),
     crustthick.thin=round(tally(filter(., crustthick=="thin"))/tally(.), 2),
     crustthick.thick=round(tally(filter(., crustthick=="thick"))/tally(.), 2),
     crusthard.soft=round(tally(filter(., crusthard=="soft"))/tally(.), 2),
     crusthard.semi_firm=round(tally(filter(., crusthard=="semi_firm"))/tally(.), 2),
     crusthard.firm=round(tally(filter(., crusthard=="firm"))/tally(.), 2),
     crusthard.hard=round(tally(filter(., crusthard=="hard"))/tally(.), 2),
     crusttext.smooth=round(tally(filter(., crusttext=="smooth"))/tally(.), 2),
     crusttext.fine_heaved=round(tally(filter(., crusttext=="fine_heaved"))/tally(.), 2),
     crusttext.coarse_heaved=round(tally(filter(., crusttext=="coarse_heaved"))/tally(.), 2),
     subsurfwet.dry=round(tally(filter(., subsurfwet=="dry"))/tally(.), 2),
     subsurfwet.moist=round(tally(filter(., subsurfwet=="moist"))/tally(.), 2),
     subsurfwet.wet=round(tally(filter(., subsurfwet=="wet"))/tally(.), 2),
     efflorescence.absent=round(tally(filter(., efflorescence=="absent"))/tally(.), 2),
     efflorescence.present=round(tally(filter(., efflorescence=="present"))/tally(.), 2),
     freesandsurf.absent=round(tally(filter(., freesandsurf=="absent"))/tally(.), 2),
     freesandsurf.present=round(tally(filter(., freesandsurf=="present"))/tally(.), 2),
     stability.non_emissive=round(tally(filter(., stability=="non_emissive"))/tally(.), 2),
     stability.fragile=round(tally(filter(., stability=="fragile"))/tally(.), 2),
     stability.emissive=round(tally(filter(., stability=="emissive"))/tally(.), 2)) %>%
  ungroup()
for (i in 3:ncol(a)){
  a[ , i] <- unlist(a[ , i])
}
b <- reshape2::melt(a, id.vars=c("area", "position", "compliant"), 
                    variable.name="obs", value.name="pct") %>% arrange(area)
b$obs <- as.character(b$obs)
b$test <- sapply(b$obs, function(x) strsplit(x, ".", fixed=T)[[1]][1])
b$obs <- sapply(b$obs, function(x) strsplit(x, ".", fixed=T)[[1]][2])
df3 <- inner_join(b, select(df2, area, bacm2), by="area") %>%
  select(area, bacm2, position, compliant, test, obs, pct) %>% 
  distinct(area, position, test, obs)
df3$bacm2 <- factor(df3$bacm2, levels=c("2015-2016 Sprinkler Test Area", 
                                        "Sprinkler DCA"), 
                    labels=c("sfwct", "sprinkler_dca"))
df3$obs <- factor(df3$obs, levels=c("dry", "moist", "wet", "shiny", "standing_water", 
                                    "none", "thin", "thick", 
                                    "hard", "firm", "semi_firm", "soft",
                                    "smooth", "fine_heaved", "coarse_heaved",
                                    "absent", "present", 
                                    "non_emissive", "fragile", "emissive"),
                  ordered=T)
levels(df3$obs)[c(4, 5)] <- "wet"
df3 <- arrange(df3, bacm2)
df3[df3$area=='T4-3 Addition', ]$area <- "T4-3 Add"

df_sum <- df3 %>% filter(!compliant) 
bar_plots <- vector(mode="list", length=length(unique(df_sum$test)))
bar_plots[[1]] <- plot_stacked_bars(df_sum, "taptest", "dry", descend=T) +
  facet_grid(position ~ .)
bar_plots[[2]] <- plot_stacked_bars(df_sum, "crustthick", "none", descend=T) +
  facet_grid(position ~ .)
bar_plots[[3]] <- plot_stacked_bars(df_sum, "crusthard", descend=T) +
  facet_grid(position ~ .)
bar_plots[[4]] <- plot_stacked_bars(df_sum, "crusttext", descend=T) +
  facet_grid(position ~ .)
bar_plots[[5]] <- plot_stacked_bars(df_sum, "subsurfwet", "dry", descend=T) +
  facet_grid(position ~ .)
bar_plots[[6]] <- plot_stacked_bars(df_sum, "efflorescence", "absent", descend=T) +
  facet_grid(position ~ .)
bar_plots[[7]] <- plot_stacked_bars(df_sum, "freesandsurf", "absent", descend=T) +
  facet_grid(position ~ .)
bar_plots[[8]] <- plot_stacked_bars(df_sum, "stability", descend=T) +
  facet_grid(position ~ .)

#for (i in 1:length(unique(df_sum$test))){
#  nm <- unique(df_sum$test)[i]
#  png(filename=paste0("~/Desktop/", nm, ".png"))
#  print(bar_plots[[i]])
#  dev.off()
#}
#
#for (i in 1:length(names(location_plots))){
#  png(filename=paste0("~/Desktop/", names(location_plots)[i], ".png"))
#  print(location_plots[[i]])
#  dev.off()
#}

johnd_plots <- vector(mode="list", length=2)
names(johnd_plots) <- c("taptest", "subsurfwet")
df_sum_2 <- df3 %>% filter(position=="within_circles")
df_sum_2$label <- ifelse(df_sum_2$compliant, "compliant", "non-compliant")
johnd_plots[[1]] <- plot_stacked_bars(df_sum_2, "taptest", descend=T) + 
  ggtitle(paste0("taptest, position = within_circles")) +
  scale_fill_manual(values=c("dry"="firebrick", "moist"="gold1", 
                             "wet"="dodgerblue"))
johnd_plots[[2]] <- plot_stacked_bars(df_sum_2, "subsurfwet", descend=T) +
  ggtitle(paste0("subsurfwet, position = within_circles")) +
  scale_fill_manual(values=c("dry"="firebrick", "moist"="gold1", 
                             "wet"="dodgerblue"))

for (i in 1:length(names(johnd_plots))){
  png(filename=paste0("~/Desktop/", names(johnd_plots)[i], ".png"), 
      width=6, height=6, units="in", res=600)
  print(johnd_plots[[i]])
  dev.off()
}
#  
