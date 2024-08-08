Seurat.plotMeta = function(obj, meta, group.by = 'slides', cols = NULL, box.width = .2, ...) {
  suppressMessages(library(Seurat))
  suppressMessages(library(ggplot2))
  VlnPlot(obj, meta, pt.size = 0, cols = cols, group.by = group.by, ...) + labs(x = NULL) +
    geom_boxplot(width = box.width, outlier.shape = NA) + 
    theme(legend.position = 'none', text = element_text(family = 'serif'))
}
