RNAseq.plotDist = function(expr, name = 'Dist', exp_cut = 10, ylim = c(0, 1e+3), color = NULL) {
  expr = as.matrix(expr)[T]
  expr = expr[expr >= exp_cut]
  if (length(expr)) {
    expr = data.frame(Counts = expr, Name = name)
    ggplot(expr, aes(Name, Counts)) +
      geom_violin(aes(fill = Name), show.legend = F) +
      geom_boxplot(aes(fill = Name), width = .1, outlier.shape = NA, show.legend = F) + 
      ylim(ylim) + labs(x = NULL, y = NULL) +
      theme_classic() +
      theme(text = element_text(family = 'serif', size = 14)) +
      if (length(color)) scale_fill_manual(values = color)
  }
}
