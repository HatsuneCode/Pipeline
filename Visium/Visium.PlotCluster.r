## plot each clusters in ST ##
Visium.PlotCluster = function(obj, name = 'clusters', dir = name, ncol = 4) {
  dir.create(dir, F)
  for (i in sort(unique(Idents(obj)))) {
    message('show cluster: ', i)
    p = SpatialPlot(obj, cells.highlight = Cells(obj)[Idents(obj) == i], image.alpha = .7,
                    images = unique(obj$orig.ident), ncol = ncol, cols.highlight = c('red', 'grey80'))
    ggsave(paste0(dir, '/', name, '.c', i, '.png'), p, w = 4*ncol, h = 3*ceiling(length(unique(obj$orig.ident))/ncol))
  }
}
