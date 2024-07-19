Seurat.plotProp = function(obj, ident.1, ident.2, color = NULL) {
  ##
  suppressMessages(library(Seurat))
  suppressMessages(library(dplyr))
  suppressMessages(library(ggplot2))
  suppressMessages(library(ggalluvial))
  ## stat meta
  meta  = setNames(obj@meta.data[c(ident.1, ident.2)], c('G1', 'G2'))
  Ratio = meta %>% group_by(G1, G2) %>% summarise(n = n()) %>% mutate(Prop = n/sum(n))
  ## plot
  ggplot(Ratio, aes(G1, Prop, fill = G2, stratum = G2, alluvium = G2)) +
    geom_col(width = .5, color = 'black') +
    geom_flow(width = .5,alpha = .4, knot.pos = .5) +
    theme_classic() +
    labs(x = ident.1, y = 'Proportion', fill = ident.2) +
    if (length(color)) scale_fill_manual(values = color)
}
