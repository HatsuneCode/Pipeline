## Show each clusters ##
showCluster = function(obj, name = 'clusters', dir = name) {
  dir.create(dir, F)
  for (i in sort(unique(Idents(obj)))) {
    message('show cluster: ', i)
    p = SpatialPlot(obj, cells.highlight = Cells(obj)[Idents(obj) == i], image.alpha = 0,
                    images = unique(obj$orig.ident), ncol = 5, alpha = .5, cols.highlight = c('red', 'white'))
    ggsave(paste0(dir, '/', name, '.c', i, '.png'), p, w = 4*5, h = 3*ceiling(length(unique(obj$orig.ident))/5))
  }
}
