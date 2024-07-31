## df: gene, type, avg_log2FC, p_val; if adj = T: p_val_adj
plot.DEG.heat = function(df, log2FC = 1, pval = .05, adj = T, label = T, grid = T, ...) {
  suppressMessages(library(reshape2))
  suppressMessages(library(ComplexHeatmap))
  suppressMessages(library(circlize))
  ff = gpar(fontfamily = 'serif')
  fc = acast(df, gene ~ type, value.var = 'avg_log2FC', fill = 0)
  pv = acast(df, gene ~ type, value.var = if (adj) 'p_val_adj' else 'p_val', fill = 1)
  p  = Heatmap(fc, name = 'Log2FC',
               row_dend_side = 'right', row_names_side = 'left',
               col = colorRamp2(c(-log2FC, 0, log2FC), c('blue', 'white', 'red')),
               row_names_gp = ff, column_names_gp = ff, 
               cell_fun = function(j, i, x, y, w, h, col) {
                 f = fc[i, j]; p = pv[i, j]
                 if (label) 
                   grid.text(round(f, 2), x, y, gp = ff)
                 if (grid & p < pval)
                   grid.rect(x, y, w, h, 
                             gp = gpar(fill = 'transparent', lwd = 1.5, col = ifelse(f > 0, 'red', 'blue')))
               }, ...)
  p@matrix_legend_param = c(p@matrix_legend_param, gpar(title_gp = ff, labels_gp = ff))
  p
}
