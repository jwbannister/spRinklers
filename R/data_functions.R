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
recode_variables <- function(df_in, group, server, layer){
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