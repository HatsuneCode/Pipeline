## df: Description, genes: C1qa/C1qb/C1qc
plot.Enrich.circ = function(df) {
  suppressMessages(library(circlize))
  terms = df$Description
  genes = strsplit(df$geneID, '/')
  gene  = unique(unlist(genes))
  matr  = matrix(0, nrow = length(terms), ncol = length(gene), dimnames = list(terms, gene))
  for(i in seq(terms)) {
    te = terms[i]
    ge = genes[[i]]
    matr[te, ge] = 1
  }
  name     = c(terms, gene)
  grid.col = colorRampPalette(color20)(length(name))
  mar      = min(strwidth(name[which.max(nchar(name))], cex = .5, units = 'inches'), 1)
  # plot
  circos.par('canvas.xlim' = c(-1 - mar, 1 + mar), 'canvas.ylim' = c(-1 - mar, 1 + mar), points.overflow.warning = F)
  chordDiagram(t(matr), annotationTrack = 'grid', transparency = .7, grid.col = grid.col, link.sort = T)
  circos.track(track.index = 1, bg.border = NA, panel.fun = function(x, y)
    circos.text(CELL_META$xcenter, CELL_META$ylim[2] + .2, CELL_META$sector.index,
                facing = 'clockwise', cex = .5, adj = c(0, .5), niceFacing = T) )
  circos.clear()
}
