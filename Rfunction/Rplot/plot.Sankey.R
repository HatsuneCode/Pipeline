plot_sankey <- function(df, group, top_n = 5) {
  suppressMessages(library(tidyr))
  suppressMessages(library(ggalluvial))
  suppressMessages(library(ggplot2))
  df <- df[order(df$p.adjust),]
  df_group_all <- df[df$Group == group,]
  
  df <- df %>% 
    separate_rows(geneID, sep = '/')
  df <- df[df$Group == group,]
  df <- df[df$Description %in% head(df_group_all$Description, top_n),]

  p <- ggplot(df,
              aes(axis1 = geneID, axis2 = Description)) +
    geom_alluvium(aes(fill = geneID)) +
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    theme_minimal() +
    labs(title = paste("Sankey Diagram for", group), x = "", y = "") +
    theme(legend.position = "none")
  
  return(p)
}
