plot_stacked_bars <- function(df_in, variable, excluded="NA", descend=F){
  ifelse(descend, 
         df1 <- df_in %>% filter(test==variable & obs!=excluded) %>%
           arrange(desc(obs)), 
         df1 <- df_in %>% filter(test==variable & obs!=excluded) %>%
           arrange(obs)) 
  ggplot(df1, aes(x=area, y=pct)) +
    geom_bar(aes(fill=obs), stat='identity', alpha=0.75) +
    ggtitle(paste0(variable, ", compliant = ", unique(df_in$compliant)))
}

