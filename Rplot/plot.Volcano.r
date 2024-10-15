## df need: avg_log2FC, p_val_adj, gene
plot_FP = function(df, logFC = 1, padj = .01, label.logFC = 1.5, exprAvg = .1, title = 'DEG FC-Padj', adj = T, pmax = 1e-300) {
  suppressMessages(library(ggplot2))
  suppressMessages(library(ggrepel))
  if (!adj) df$p_val_adj = df$p_val
  ## annot
  df$annot = ifelse(df$avg_log2FC > logFC, 'Up', ifelse(df$avg_log2FC < -logFC, 'Down', 'NS'))
  df$annot[df$p_val_adj > padj | is.na(df$p_val_adj)] = 'NS'
  df$annot[df$average < exprAvg] = 'NS'
  df$annot[df$annot == 'Up']   = paste0('Up (', table(df$annot)['Up'], ')')
  df$annot[df$annot == 'Down'] = paste0('Down (', table(df$annot)['Down'], ')')
  df$annot[df$annot == 'NS']   = paste0('NS (', table(df$annot)['NS'], ')')
  df$annot = factor(df$annot, sort(unique(df$annot), T))
  df$lable = abs(df$avg_log2FC) > label.logFC & !grepl('^NS', df$annot)
  ## color
  annot = levels(df$annot)
  idx   = sapply(c('Up', 'NS', 'Down'), function(i) sum(grepl(i, annot)) )
  color = setNames(c(if (idx[1]) 'red', if (idx[2]) 'black', if (idx[3]) 'blue') , annot)
  ## limit
  lim = max(abs(df$avg_log2FC))
  df$p_val_adj[ -log10(df$p_val_adj) > -log10(pmax) ] = pmax
  ## plot
  p = ggplot(df, aes(avg_log2FC, -log10(p_val_adj))) + 
    geom_point(aes(color = annot)) + xlim(c(-lim, lim)) + theme_bw() +
    geom_hline(yintercept = -log10(padj), linetype = 2) +
    geom_vline(xintercept = c(-logFC, logFC), linetype = 2) +
    geom_text_repel(label = ifelse(df$lable, df$gene, NA), family = 'serif') +
    scale_color_manual(values = color) + 
    labs(x = 'Log2 Fold Change', y = if (adj) expression(-log[10](Padj)) else expression(-log[10](Pvalue)), color = NULL, title = title) +
    theme(text = element_text(family = 'serif', size = 14), plot.title = element_text(hjust = .5))
  return(plot = p, df = df)
}
##########
plot_EF = function(df, logFC = 1, padj = .01, label.logFC = 1.5, exprAvg = .1, title = 'DEG Expr-FC', adj = T) {
  suppressMessages(library(ggplot2))
  suppressMessages(library(ggrepel))
  if (!adj) df$p_val_adj = df$p_val
  ## annot
  df$annot = ifelse(df$avg_log2FC > logFC, 'Up', ifelse(df$avg_log2FC < -logFC, 'Down', 'NS'))
  df$annot[df$p_val_adj > padj | is.na(df$p_val_adj)]  = 'NS'
  df$annot[df$average < exprAvg] = 'NS'
  df$annot[df$annot == 'Up']   = paste0('Up (', table(df$annot)['Up'], ')')
  df$annot[df$annot == 'Down'] = paste0('Down (', table(df$annot)['Down'], ')')
  df$annot[df$annot == 'NS']   = paste0('NS (', table(df$annot)['NS'], ')')
  df$annot = factor(df$annot, sort(unique(df$annot), T))
  df$lable = abs(df$avg_log2FC) > label.logFC & !grepl('^NS', df$annot)
  ## color
  annot = levels(df$annot)
  idx   = sapply(c('Up', 'NS', 'Down'), function(i) sum(grepl(i, annot)) )
  color = setNames(c(if (idx[1]) 'red', if (idx[2]) 'black', if (idx[3]) 'blue') , annot)
  ## limit
  lim = max(abs(df$avg_log2FC))
  ## plot
  p = ggplot(df, aes(average, avg_log2FC)) + 
    geom_point(aes(color = annot)) + ylim(c(-lim, lim)) + theme_bw() +
    geom_hline(yintercept = c(-logFC, logFC), linetype = 2) +
    geom_vline(xintercept = .1, linetype = 2) +
    geom_text_repel(label = ifelse(df$lable, df$gene, NA), family = 'serif') +
    scale_color_manual(values = color) + 
    labs(x = 'Average Expression', y = 'Log2 Fold Change', color = NULL, title = title) + 
    theme(text = element_text(family = 'serif', size = 14), plot.title = element_text(hjust = .5))
  return(plot = p, df = df)
}
#########
plot_FPcur = function(df, logFC = 1, padj = .01, exprAvg = .1, title = 'DEG FC-Padj', adj = T, pmax = 1e-300, dot_num = 500) {
  suppressMessages(library(ggplot2))
  suppressMessages(library(ggrepel))
  ##
  if (!adj) df$p_val_adj = df$p_val
  lim = max(abs(df$avg_log2FC))
  df$p_val_adj[ -log10(df$p_val_adj) > -log10(pmax) ] = pmax
  x_vals = seq(-lim, lim, length.out = dot_num)
  curve_1_vals =   1 / (x_vals - logFC)  + (-log10(padj))
  curve_2_vals = -(1 / (x_vals + logFC)) + (-log10(padj))
  curve_1_vals[curve_1_vals < -log10(padj)] = NA
  curve_2_vals[curve_2_vals < -log10(padj)] = NA
  curve_data = data.frame(x_vals, curve_1_vals, curve_2_vals)
  ##
  curve_1 = function(x) {
    y = 1 / (x - logFC) + (-log10(padj))
    ifelse(y > -log10(padj), y, NA) }
  curve_2 = function(x) {
    y = -(1 / (x + logFC)) + (-log10(padj))
    ifelse(y > -log10(padj), y, NA) }
  ##
  df$lable = (-log10(df$p_val_adj) > curve_1(df$avg_log2FC) | -log10(df$p_val_adj) > curve_2(df$avg_log2FC)) & df$p_val_adj < padj
  df$annot = ifelse(df$avg_log2FC > logFC, 'Up', ifelse(df$avg_log2FC < -logFC, 'Down', 'NS'))
  df$annot[df$p_val_adj > padj | is.na(df$p_val_adj)] = 'NS'
  df$annot[df$average < exprAvg] = 'NS'
  df$annot  = factor(df$annot, levels = c('Up', 'Down', 'NS'))
  df$annot2 = ifelse(df$annot == 'Up' & df$lable, 'Up', ifelse(df$annot == 'Down' & df$lable, 'Down', 'NS'))
  df$annot2[is.na(df$annot2)] = 'NS'
  df$annot2 = factor(df$annot2, levels = c('Up', 'Down', 'NS'))
  color = setNames(c('red', 'blue', 'black'), c('Up', 'Down', 'NS'))
  p = ggplot(df, aes(avg_log2FC, -log10(p_val_adj))) + 
    geom_point(aes(color = annot2)) + 
    geom_hline(yintercept = -log10(padj), linetype = 2) +
    geom_vline(xintercept = c(-logFC, logFC), linetype = 2) +
    geom_text_repel(aes(label = ifelse(df$lable, df$gene, NA)), family = 'serif') +
    geom_line(data = curve_data, aes(x = x_vals, y = curve_1_vals), color = 'purple', linetype = 1, size = 1) +
    geom_line(data = curve_data, aes(x = x_vals, y = curve_2_vals), color = 'purple', linetype = 1, size = 1) +
    scale_color_manual(values = color) +
    xlim(c(-lim, lim)) + 
    labs(x = 'Log2 Fold Change', y = if (adj) expression(-log[10](Padj)) else expression(-log[10](Pvalue)), color = NULL, title = title) +
    theme_bw() + theme(text = element_text(family = 'serif', size = 14), plot.title = element_text(hjust = .5))  
  return(list(plot = p, df = df))
}
