## plot object meta.data ##
Seurat.plotMeta = function(obj, meta, cols = NULL, nrow = 1) {
  wrap_plots(lapply(meta, function(m)
    VlnPlot(obj, m, pt.size = 0, cols = cols, group.by = 'slides') + labs(x = NULL) +
      geom_boxplot(width = .2, outlier.shape = NA) + 
      theme(legend.position = 'none', text = element_text(family = 'serif')) ), nrow = nrow)
}
