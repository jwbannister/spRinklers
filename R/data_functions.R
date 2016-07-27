#' Assign text descriptions to factor values
#' 
#' \{recode_variables} pulls factor value descriptions from the AGOL REST 
#' server and changes factor variables pulled with \code{query_AGOL} into 
#' string variables.
#' 
#' @param group Text string. The relevant ArcGIS Online group.
#' @param server Text string. The desired server in the group.
#' @param layer Integer. The number of the desired layer in the server. (Layer 
#' numbers begin with 0)
recode_variables_AGOL <- function(df_in, group, server, layer){
  meta <- query_AGOL_json(group, server, layer)
  meta_codes <- meta$fields$domain$codedValues
  recode <- function(x, codes){
    ifelse(!is.na(x), filter(codes, code==x)$name, x)
  }
  for (i in 1:ncol(df_in)){
    if (!is.null(meta_codes[[i]])){
      df_in[ , i] <- sapply(df_in[ , i], recode, codes=meta_codes[[i]])
    }
  }
  df_in
}

#' Assign text descriptions to factor values from AWS tables.
recode_variable_AWS <- function(df_in, variable){
  plural <- ifelse(substring(variable, nchar(variable)-1)=="ss", 
                   paste0(variable, "es"), paste0(variable, "s"))
  plural <- ifelse(variable %in% c("efforescence", "free_sand"), 
                   "presences", plural)
  plural <- ifelse(variable=="stability", "stabilities", plural)
  query1 <- paste0("SELECT * FROM field_data.", plural, ";")
  xwalk <- query_owens_aws(query1)
  recode <- function(x){
    ifelse(!is.na(x), xwalk[xwalk[ , 1]==x, 2], x)
  }
  df_in[ , paste0(variable, "_id")] <- 
    sapply(df_in[ , paste0(variable, "_id")], recode)
  df_in
}



#' Assign points to a sprinkler area
assign_points <- function(points_vec, poly_df=sprinkler_polygons){
  objs <- data.frame(objectid=unique(poly_df$objectid))
  xwalk <- dplyr::left_join(objs, select(poly_df, area, objectid), 
                             by="objectid")
  for (j in objs$objectid){
    polycheck <- sp::point.in.polygon(points_vec[1], points_vec[2], 
                                      dplyr::filter(poly_df, objectid==j)$x, 
                                      dplyr::filter(poly_df, objectid==j)$y)
    if (polycheck==1) return(filter(xwalk, objectid==j)$area[1]) 
  }
  return(NA)
}

assign_areas_df <- function(df_in){
  df_in$area <- sapply(mapply(c, coordinates(df_in)[ , "x"], 
                                 coordinates(df_in)[ , "y"], SIMPLIFY=F), 
                          assign_points)
  df_in
}

create_dist_matrix <- function(df1, df2, df1_xy_cols, df2_xy_cols, df1_labels,
                               df2_labels){
  mat <- matrix(data=NA, nrow=nrow(df1), ncol=nrow(df2), 
                dimnames=list(df1[ , df1_labels], df2[ , df2_labels]))
  df1_x <- df1[ , df1_xy_cols[1]]
  df1_y <- df1[ , df1_xy_cols[2]]
  df2_x <- df2[ , df2_xy_cols[1]]
  df2_y <- df2[ , df2_xy_cols[2]]
  for (i in 1:ncol(mat)){
    dist_vect <- c()
    for (j in 1:nrow(mat)){
      dist_vect[j] <- sqrt((df1_x[j] - df2_x[i])^2 + (df1_y[j] - df2_y[i])^2) 
    }
    mat[ , i] <- dist_vect
  }
  mat
}

create_dist_matrix <- function(df1, df2, df1_xy_cols, df2_xy_cols, df1_labels,
                               df2_labels){
  mat <- matrix(data=NA, nrow=nrow(df1), ncol=nrow(df2), 
                dimnames=list(df1[ , df1_labels], df2[ , df2_labels]))
  df1_x <- df1[ , df1_xy_cols[1]]
  df1_y <- df1[ , df1_xy_cols[2]]
  df2_x <- df2[ , df2_xy_cols[1]]
  df2_y <- df2[ , df2_xy_cols[2]]
  for (i in 1:ncol(mat)){
    dist_vect <- c()
    for (j in 1:nrow(mat)){
      dist_vect[j] <- sqrt((df1_x[j] - df2_x[i])^2 + (df1_y[j] - df2_y[i])^2) 
    }
    mat[ , i] <- dist_vect
  }
  mat
}

