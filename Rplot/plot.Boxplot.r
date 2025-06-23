plot.Boxplot = function(expr, gene, group = NULL, compare = NULL, cols = NULL, 
                        pt.size = 1, label = 'p.signif', method = 't.test', show.legend = T) {
  ## group: findInList(colnames(expr), pair)
  ## compare: list( c('pos1', 'neg1'), c('pos2', 'neg2') )
  ## label: p.signif, p.format
  suppressMessages(library(ggplot2))
  suppressMessages(library(ggpubr))
  if (!length(group)) group = colnames(expr)
  exp = data.frame(Expr = as.numeric(expr[gene,]), Group = group)
  p   = ggplot(exp, aes(Group, Expr)) + 
    geom_boxplot(aes(fill = Group), outlier.shape = NA, show.legend = show.legend) +
    geom_point(show.legend = F, size = pt.size) +
    labs(x = NULL, y = 'Normalized Expression', title = gene) +
    theme_classic() +
    theme(text = element_text(family = 'serif', size = 14), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = .5))
  if (length(compare)) {
    p = p + stat_compare_means(comparisons = compare, label = label, method = method)
    p$layers[[3]]$aes_params$family = 'serif'
  }
  if (length(cols))
    p = p + scale_fill_manual(values = cols)
  p
}
