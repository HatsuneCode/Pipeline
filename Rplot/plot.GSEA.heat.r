plot.GSEA.heat = function(df, nes = 2, pval = .05, adj = T, label = T, grid = T, t = F, ...) {
  suppressMessages(library(reshape2))
  suppressMessages(library(ComplexHeatmap))
  suppressMessages(library(circlize))
  Nes = acast(df, pathway ~ group, value.var = 'NES',  fill = 0)
  Pvl = acast(df, pathway ~ group, value.var = if (adj) 'padj' else 'pval', fill = 1)
  if (t) { Nes = t(Nes); Pvl = t(Pvl) }
  ff  = gpar(fontfamily = 'serif')
  p   = Heatmap(Nes, name = 'NES',
                col = colorRamp2(c(-nes, 0, nes), c('blue', 'white', 'red')), 
                row_names_side = 'left', row_names_gp = ff, row_title_gp = ff,
                row_names_max_width = max_text_width(rownames(Nes)), column_names_max_height = max_text_width(colnames(Nes)),
                show_column_dend = F, show_row_dend = F,
                column_names_side = 'top', column_names_gp = ff, column_title_gp = ff, 
                cell_fun = function(j, i, x, y, w, h, col) {
                  f = Nes[i, j]; p = Pvl[i, j]
                  if (label) grid.text(round(f, 2), x, y, gp = ff)
                  if (grid & p < pval) 
                    grid.rect(x, y, w, h,
                              gp = gpar(fill = 'transparent', lwd = 1.5, col = ifelse(f > 0, 'red', 'blue')))
                }, ...)
  p@matrix_legend_param = c(p@matrix_legend_param, gpar(title_gp = ff, labels_gp = ff))
  list(plot = p, pvalue = Pvl)
}
