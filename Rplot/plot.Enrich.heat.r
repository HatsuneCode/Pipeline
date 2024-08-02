plot.Enrich.heat = function(df, nes = 2, pval = .05, adj = T, label = T, grid = T, ...) {
  suppressMessages(library(reshape2))
  suppressMessages(library(ComplexHeatmap))
  suppressMessages(library(circlize))
  nes = acast(df, pathway ~ group, value.var = 'NES',  fill = 0)
  pvl = acast(df, pathway ~ group, value.var = if (adj) 'padj' else 'pval', fill = 1)
  ff  = gpar(fontfamily = 'serif')
  p   = Heatmap(nes, name = 'NES',
                col = colorRamp2(c(-nes, 0, nes), c('blue', 'white', 'red')), 
                rect_gp = gpar(col = 'grey80'),
                row_names_side = 'left', show_row_dend = F, row_names_gp = ff, row_names_max_width = max_text_width(rownames(nes)), 
                cluster_columns = F, column_names_side = 'top', column_names_gp = ff,
                cell_fun = function(j, i, x, y, w, h, col) {
                  f = nes[i, j]; p = pvl[i, j]
                  if (label) grid.text(round(f, 2), x, y, gp = ff)
                  if (grid & p < pval) 
                    grid.rect(x, y, w, h,
                              gp = gpar(fill = 'transparent', lwd = 1.5, col = ifelse(f > 0, 'red', 'blue')))
                }, ...)
  p@matrix_legend_param = c(p@matrix_legend_param, gpar(title_gp = ff, labels_gp = ff))
  p
}
