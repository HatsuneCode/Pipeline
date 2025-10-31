## df: gene, group, avg_log2FC, p_val; if adj = T: p_val_adj
plot.DEG.heat = function(df, tgs = NULL, sample.order = NULL, 
                         log2FC.col = 2, log2FC.grid = log2(1.5),
                         pval = .05, adj = T, label = F, grid = T, 
                         upCol = 'red', dnCol = 'blue', 
                         fill.fc = 0, fill.pv = 1, font.family = 'Arial', ...) {
  suppressMessages(library(reshape2))
  suppressMessages(library(ComplexHeatmap))
  suppressMessages(library(circlize))
  if (length(tgs)) {
    df = df[df$gene %in% tgs,]
    df$gene = factor(df$gene, intersect(tgs, df$gene))
  }
  if (length(sample.order))
    df$group = factor(df$group, intersect(sample.order, df$group))
  if (anyNA(df$group)) stop('NA in samples levels !!!')
  ff = gpar(fontfamily = font.family)
  fc = acast(df, gene ~ group, value.var = 'avg_log2FC', fill = fill.fc, drop = F)
  pv = acast(df, gene ~ group, value.var = if (adj) 'p_val_adj' else 'p_val', fill = fill.pv, drop = F)
  p  = Heatmap(fc, name = 'Log2FC', row_order = if (length(tgs)) levels(df$gene),
               col = colorRamp2(c(-log2FC, 0, log2FC), c('blue', 'white', 'red')),
               row_names_side = 'left', row_dend_side = 'right', row_title_gp = ff, 
               row_names_max_width = max_text_width(rownames(fc)), column_names_max_height = max_text_width(colnames(fc)),
               cluster_columns = F, column_names_side = 'top', column_names_gp = ff, column_title_gp = ff,
               cell_fun = function(j, i, x, y, w, h, col) {
                 f = fc[i, j]; p = pv[i, j]
                 if (grid & p < pval & abs(f) > log2FC.grid )
                   grid.rect(x, y, w, h, 
                             gp = gpar(fill = 'transparent', lwd = 1.5, col = ifelse(f > 0, upCol, dnCol)))
                 if (label) 
                   grid.text(paste0(round(f, 2), '\n(', round(p, 2), ')'), x, y)
                 
               }, ...)
  p@matrix_legend_param = c(p@matrix_legend_param, gpar(title_gp = ff, labels_gp = ff))
  list(plot = p, pvalue = pv)
}

    