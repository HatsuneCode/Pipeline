plot.Boxplot = function(expr, target, group = NULL, compare = NULL, cols = NULL, 
                        ylab = 'Normalized Expression', title = 'Boxplot',
                        box.width = .15, pt.size = 1, label = 'p.signif', method = 't.test', 
                        show.legend = T, vln = T, font.family = 'Arial') {
  ## group: findInList(colnames(expr), pair)
  ## compare: list( c('pos1', 'neg1'), c('pos2', 'neg2') )
  ## label: p.signif, p.format
  suppressMessages(library(ggplot2))
  suppressMessages(library(ggpubr))
  if (!length(group)) group = colnames(expr)
  exp = data.frame(Expr = as.numeric(expr[target,]), Group = group)
  p   = ggplot(exp, aes(Group, Expr)) 
  if (vln) p = p + geom_violin(aes(fill = Group))
  p   = p + geom_boxplot(aes(fill = Group), width = box.width, outlier.shape = NA, show.legend = show.legend) +
    geom_point(show.legend = F, size = pt.size) +
    labs(x = NULL, y = ylab, title = title) +
    theme_classic() +
    theme(text = element_text(family = font.family, size = 18), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = .5))
  ## as for font.family: extrafont::font_import(); loadfonts(device = 'win')
  if (length(compare)) {
    p = p + stat_compare_means(comparisons = compare, label = label, method = method)
    p$layers[[3]]$aes_params$family = font.family
  }
  if (length(cols))
    p = p + scale_fill_manual(values = cols)
  p
}
