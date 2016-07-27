# bacm_study.R
# misc analyssi and sumamry related to setting up sprinkler BACM study

load_all("~/code/Roses")
load_all("~/code/owensData")
load_all("~/code/owensMaps")
load_all()
library(dplyr)
library(ggplot2)
library(lubridate)

# get wind data
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

# BACM test areas
path <- path.expand("~/dropbox/owens/sprinkler_bacm/Sprinkler_BACM/SHP/")
lyr <- "New sprinkler BACM boundaries"
sprinklers <- rgdal::readOGR(path, lyr)
sprinklers_df <- sprinklers@data %>%
  mutate(group.index=paste0(DCM, "_", TrgtWet), dcm1=substr(DCM, 1, 3))
sprinklers_df$dcm1 <- ifelse(sprinklers_df$dcm1=="T4-", "T4-3", 
                             sprinklers_df$dcm1)
sprinklers_df$group.index1 <- paste0(sprinklers_df$dcm1, "_", 
                                     sprinklers_df$TrgtWet)
sprinklers_polys <- data.frame(x=c(), y=c())
for (i in 1:nrow(sprinklers_df)){
  sprinklers_df$labpt.x[i] <- sprinklers@polygons[[i]]@Polygons[[1]]@labpt[1]
  sprinklers_df$labpt.y[i] <- sprinklers@polygons[[i]]@Polygons[[1]]@labpt[2]
  temp_polys <- data.frame(sprinklers@polygons[[i]]@Polygons[[1]]@coords)
  names(temp_polys) <- c("x", "y")
  temp_polys$OBJECTID <- sprinklers_df$group.index1[i]
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
  geom_point(data=filter(stations, deployment %in% unique(sprinklers_df$station)),
             color='blue',
             mapping=aes(x=x, y=y))

rose_plots <- vector(mode="list", length=length(unique(sprinklers_df$station)))
names(rose_plots) <- unique(sprinklers_df$station)
for (i in names(rose_plots)){
  rose_plots[[i]] <- filter(wind_df, deployment==i) %>% 
    plot_rose_image_only(., value='vel', dir='dir', 
              valueseq=c(4, 8, 12, 16))  
}

out_path <- path.expand("~/dropbox/output/spRinklers/")
for (m in names(rose_plots)){
  svg(filename=paste0(out_path, "roses/", m, ".svg"), bg='transparent')
  print(rose_plots[[m]])
  dev.off()
}

# instruments
lyr <- "sensit_csc2"
instruments <- rgdal::readOGR(path, lyr)
inst_sum <- data.frame(table(instruments$loc_index))
names(inst_sum) <- c("group.index1", "inst.count")

summation <- vector(mode="list", length=0)
summation$new.old <- sprinklers_df %>% group_by(DCM) %>%
  summarize(acres=sum(acres))
summation$target <- sprinklers_df %>% group_by(dcm1, TrgtWet) %>%
  summarize(acres=sum(acres)) %>% 
  mutate(group.index1=paste0(dcm1, "_", TrgtWet)) %>%
  inner_join(inst_sum, by="group.index1")

xlsx::write.xlsx(select(data.frame(summation$target), -group.index1),  
                 file=paste0(out_path, "area_breakdown.xlsx"), row.names=FALSE)
xlsx::write.xlsx(data.frame(summation$new.old), sheetName="Sheet2",  
                 file=paste0(out_path, "area_breakdown.xlsx"), row.names=FALSE, 
                 append=TRUE)
