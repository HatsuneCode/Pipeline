plot_FP = function(df, logFC = 1, padj = .01, label.logFC = 1.5, exprAvg = .1, title = 'DEG FC-Padj', adj = T, pmax = 1e-300,dot_num=500) {
  suppressMessages(library(ggplot2))
  suppressMessages(library(ggrepel))
  
  if (!adj) df$p_val_adj = df$p_val
  
  ## 设置 x 轴的最大和最小范围
  lim = max(abs(df$avg_log2FC))
  
  df$p_val_adj[ -log10(df$p_val_adj) > -log10(pmax) ] = pmax
  
  ## 手动生成曲线的点
  x_vals <- seq(-lim, lim, length.out = dot_num)  # x 轴的范围
  curve_1_vals <- 1 / (x_vals - logFC) + (-log10(padj))
  curve_2_vals <- -(1 / (x_vals + logFC)) + (-log10(padj))
  
  ## 只保留 y > -log10(padj) 的部分
  curve_1_vals[curve_1_vals < -log10(padj)] <- NA
  curve_2_vals[curve_2_vals < -log10(padj)] <- NA
  
  curve_data <- data.frame(x_vals, curve_1_vals, curve_2_vals)
  
  ## 定义原始的曲线函数
  curve_1 <- function(x) {
    y = 1 / (x - logFC) + (-log10(padj))
    ifelse(y > -log10(padj), y, NA)
  }
  
  curve_2 <- function(x) {
    y = -(1 / (x + logFC)) + (-log10(padj))
    ifelse(y > -log10(padj), y, NA)
  }
  
  ## 标记需要标注的点
  df$lable = ifelse((-log10(df$p_val_adj) > curve_1(df$avg_log2FC) | 
                       -log10(df$p_val_adj) > curve_2(df$avg_log2FC)) & 
                      (df$avg_log2FC > label.logFC | df$avg_log2FC < -label.logFC) & (df$p_val_adj < padj), 
                    TRUE, FALSE)
  
  ## 设置 Up/Down/NS 标记
  df$annot = ifelse(df$avg_log2FC > logFC, 'Up', ifelse(df$avg_log2FC < -logFC, 'Down', 'NS'))
  df$annot[df$p_val_adj > padj | is.na(df$p_val_adj)] = 'NS'
  df$annot[df$average < exprAvg] = 'NS'
  df$annot = factor(df$annot, levels = c("Up", "Down", "NS"))
  
  ## 设置颜色
  color = setNames(c('red', 'blue', 'black'), c('Up', 'Down', 'NS'))
  
  ## 绘制图像
  p <- ggplot(df, aes(avg_log2FC, -log10(p_val_adj))) + 
    geom_point(aes(color = annot)) + 
    xlim(c(-lim, lim)) + 
    theme_bw() +
    geom_hline(yintercept = -log10(padj), linetype = 2) +
    geom_vline(xintercept = c(-logFC, logFC), linetype = 2) +
    geom_text_repel(aes(label = ifelse(df$lable, df$gene, NA)), family = 'serif') +
    scale_color_manual(values = color) + 
    labs(x = 'Log2 Fold Change', 
         y = if (adj) expression(-log[10](Padj)) else expression(-log[10](Pvalue)), 
         color = NULL, 
         title = title) +
    theme(text = element_text(family = 'serif', size = 14), plot.title = element_text(hjust = .5))
  
  ## 绘制双曲线曲线
  p <- p + 
    geom_line(data = curve_data, aes(x = x_vals, y = curve_1_vals), color = "purple", linetype = 1, size = 1) +
    geom_line(data = curve_data, aes(x = x_vals, y = curve_2_vals), color = "purple", linetype = 1, size = 1)
  
  return(p)
}
