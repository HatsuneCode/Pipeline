Seurat.plotMeta = function(obj, meta, group.by = 'slides', cols = NULL, ...) {
  suppressMessages(library(Seurat))
  suppressMessages(library(ggplot2))
  VlnPlot(obj, m, pt.size = 0, cols = cols, group.by = group.by) + labs(x = NULL) +
    geom_boxplot(width = .2, outlier.shape = NA) + 
    theme(legend.position = 'none', text = element_text(family = 'serif'))
}
