load_all("~/analysis/windroseR")
load_all("~/analysis/Rowens")
load_all()
library(dplyr)
library(ggplot2)
library(lubridate)

start.date <- mdy("07-01-2015")
end.date <- start.date %m+% years(1)
query1 <- paste0("SELECT m.datetime, m.ws_10m, m.wd_10m, i.deployment, ",
                 "i.northing_utm, i.easting_utm, i.description ",
                 "FROM mets.met_1hour_periods m ",
                 "INNER JOIN instruments.deployments i ",
                 "ON m.deployment_id=i.deployment_id ",
                 "WHERE m.datetime::date ",
                 "BETWEEN '", start.date, "'::date AND '", end.date, "';") 
met_df <- query_owenslake(query1) %>% rename(dir=wd_10m, vel=ws_10m)
met_df <- filter(met_df, vel<200)
query2 <- paste0("SELECT a.datetime, a.dir, a.aspd, i.deployment, ",
                 "i.northing_utm, i.easting_utm, i.description ",
                 "FROM archive.mfile_data a ",
                 "INNER JOIN instruments.deployments i ",
                 "ON a.deployment_id=i.deployment_id ",
                 "WHERE a.datetime::date ",
                 "BETWEEN '", start.date, "'::date AND '", end.date, "';") 
mfile_df <- query_owenslake(query2) %>% rename(vel=aspd)
wind_df <- rbind(met_df, mfile_df)
wind_df <- wind_df[!is.na(wind_df$vel) & !is.na(wind_df$dir), ]
stations <- wind_df %>% 
  select(deployment, x=easting_utm, y=northing_utm, description) %>%
  distinct(deployment)
wind_df <- select(wind_df, -northing_utm, -easting_utm, -description)

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
  temp_polys$OBJECTID <- sprinklers_df$OBJECTID[i]
  sprinklers_polys <- rbind(sprinklers_polys, temp_polys)
}

dist_melt <- create_dist_matrix(stations, sprinklers_df, 
                                  df1_xy_cols=c("x", "y"), 
                                  df2_xy_cols=c("labpt.x", "labpt.y"),
                                  df1_labels="deployment",
                                  df2_labels="area") %>%
  reshape2::melt(value.name="dist") %>%
  rename(station=Var1, area=Var2) %>%
  group_by(area) %>% filter(dist==min(dist)) %>% ungroup()

sprinklers_df <- inner_join(sprinklers_df, dist_melt, by="area")

p1 <- sprinklers_polys %>%
  ggplot(aes(x=x, y=y)) +
  geom_path(aes(group=OBJECTID)) +
#  geom_label(data=sprinklers_df, 
#             mapping=aes(x=labpt.x, y=labpt.y, label=area)) +
  geom_point(data=sprinklers_df, color='red', 
             mapping=aes(x=labpt.x, y=labpt.y)) +
  coord_equal() +
  geom_point(data=stations, color='blue',
             mapping=aes(x=x, y=y))

rose_plots <- vector(mode="list", length=length(unique(sprinklers_df$station)))
names(rose_plots) <- unique(sprinklers_df$station)
for (i in names(rose_plots)){
  rose_plots[[i]] <- filter(wind_df, deployment==i) %>% 
    plot_rose(., value='vel', dir='dir', 
              valueseq=c(4, 8, 12, 16), 
              legend.title='Wind Speed (m/s)',
              plot.title=paste0("Station ", i, " (", 
                                format(start.date, "%m/%d/%y"), " - ", 
                                format(end.date, "%m/%d/%y"), ")"),
              ylabel.loc="W") +
    theme(panel.background=element_blank(), 
          panel.grid.major=element_line(color='grey'),
          legend.position="bottom", 
          legend.text=element_text(size=16),
          legend.title=element_text(size=18), 
          plot.title=element_text(size=24))
}

context_plots <- vector(mode="list", length=length(unique(sprinklers_df$area)))
names(context_plots) <- unique(sprinklers_df$area)
for (j in names(context_plots)){
  ids <- filter(sprinklers_df, area==j)$OBJECTID
  sta <- unique(filter(sprinklers_df, area==j)$station)
  context_plots[[j]] <- sprinklers_polys %>%
    filter(OBJECTID %in% ids) %>%
    ggplot(aes(x=x, y=y)) +
    geom_path(aes(group=OBJECTID)) +
    geom_point(data=filter(stations, deployment==sta), color='blue', 
               shape=18, size=8) +
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.background=element_blank(),
          panel.grid=element_blank()) +
    ggtitle("Wind Station Location") +
    coord_equal()
}

pth <- path.expand("~/dropbox/sprinkler_wetness_frequency/")
for (m in names(rose_plots)){
  dcm <- filter(sprinklers_df, station==m)$area[1]
  png(filename=paste0(pth, "wind_roses/", m, " (", dcm, ").png"))
  print(rose_plots[[m]])
  dev.off()
}
for (p in names(context_plots)){
  png(filename=paste0(pth, "station_locations/", p, ".png"))
  print(context_plots[[p]])
  dev.off()
}

  
