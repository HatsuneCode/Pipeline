## group: findInList(colnames(expr), pair)
## compare: list( c('pos1', 'neg1'), c('pos2', 'neg2') )
plot.boxplot = function(expr, gene, group = NULL, compare = NULL, cols = NULL) {
  if (!length(group)) group = colnames(expr)
  exp = data.frame(Expr = expr[gene,], Group = group)
  p   = ggplot(exp, aes(Group, Expr)) + 
    geom_boxplot(aes(fill = Group), outlier.shape = NA, show.legend = F) +
    geom_point(aes(color = Group), show.legend = F) +
    stat_compare_means(comparisons = compare, label = 'p.signif') +
    labs(x = NULL, y = 'Normalized Expression', title = gene) +
    theme_classic() +
    theme(text = element_text(family = 'serif', size = 14), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = .5)) +
    if (length(cols)) 
      scale_fill_manual(values = cols) + scale_color_manual(values = cols)
  p$layers[[3]]$aes_params$family = 'serif'
  p
}
