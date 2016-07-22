#' multi_wetness.R
#' Bring in multiple data sources to build multi-faceted picture of wetness 
#' in sprinkler controlled DCAs.

load_all()
load_all("~/analysis/Rowens")
library(dplyr)
library(ggplot2)

df0 <- query_AGOL("OwensLake_Phase7a", "OwensLake_Sprinkler_Transects", 0)
df1 <- filter(df0, !is.na(name))
df2 <- select(df1, -last.edited.user, -last.edited.date, -globalid,
              -created.user)
df2$created.date <- convert_ESRI_date(df2$created.date)
df3 <- recode_variables(df2, "OwensLake_Phase7a", 
                        "OwensLake_Sprinkler_Transects", 0 
