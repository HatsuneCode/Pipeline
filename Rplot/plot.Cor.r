## df: a data.frame need Sample1, Sample2, Gene
## fc_cut: cut abs(log2(FC))
plot.Cor = function(df, sample1, sample2, exp_cut = 1, fc_cut = 1, overlaps = NULL,
                    up.markers = NULL, down.markers = NULL, title = 'Correlation', label = T, fit = NULL) {
  ##
  suppressMessages(library(ggpointdensity))
  suppressMessages(library(viridis))
  suppressMessages(library(ggpubr))
  suppressMessages(library(ggplot2))
  suppressMessages(library(ggrepel))
  ##
  df = data.frame(
    Gene    = as.character(df$Gene),
    Sample1 = as.numeric(df[[sample1]]),
    Sample2 = as.numeric(df[[sample2]])
    )
  df        = df[abs(df$Sample1) > exp_cut & abs(df$Sample2) > exp_cut,]
  df$Log2FC = log2( df$Sample1/df$Sample2 )
  ##
  up.markers   = intersect(up.markers,   df$Gene)
  down.markers = intersect(down.markers, df$Gene)
  markers      = c(up.markers, down.markers)
  df$Label     = if (length(markers)) df$Gene %in% markers else abs(df$Log2FC) > fc_cut
  ##
  p = ggplot(df, aes(Sample1, Sample2)) +
    geom_pointdensity() +
    geom_abline(color = 'black', linetype = 2) +
    geom_hline(yintercept = exp_cut, linetype = 2) +
    geom_vline(xintercept = exp_cut, linetype = 2) +
    geom_abline(slope = c(1/(2^fc_cut), 2^fc_cut), linetype = 2) +
    scale_color_viridis(alpha = .3) +
    geom_smooth(color = 'cyan', linetype = 2, method = fit) +
    labs(x = sample1, y = sample2, title = title) +
    theme_bw() +
    theme(text = element_text(family = 'serif', size = 14), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = .5), 
          legend.position = 'none')
  overlaps = if (!length(overlaps)) sum(df$Label) else overlaps
  if (length(markers) & label) {
    if (length(up.markers)) 
      p = p + geom_point(data = df[df$Gene %in% up.markers,], color = 'red') +
        geom_text_repel(label = ifelse(df$Gene %in% up.markers, df$Gene, NA), 
                        family = 'serif', size = 5, color = 'red', max.time = 2, max.overlaps = overlaps)
    if (length(down.markers)) 
      p = p + geom_point(data = df[df$Gene %in% down.markers,], color = 'blue') +
        geom_text_repel(label = ifelse(df$Gene %in% down.markers, df$Gene, NA), 
                        family = 'serif', size = 5, color = 'blue', max.time = 2, max.overlaps = overlaps)
  } else if (label)
    p = p + geom_text_repel(label = ifelse(df$Label, df$Gene, NA),
                            family = 'serif', size = 5, color = 'red', max.time = 2, max.overlaps = overlaps)
  p + stat_cor(family = 'serif', size = 5, method = 'pearson') +
    stat_cor(family = 'serif', size = 5, method = 'spearman', cor.coef.name = 'rho', label.y.npc = .9)
}
