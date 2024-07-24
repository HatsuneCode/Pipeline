## PCA
PCA = function(expr, n.varGene = 3e+3, ...) {
  if (length(n.varGene))
    expr = expr[RNAseq.varGene(expr, n.varGene = n.varGene, ...),]
  pca = data.frame(prcomp(t(expr), ...)$x, check.rows = F)
  pca$sample = rownames(pca)
  pca$sample = factor(pca$sample, pca$sample)
  pca
}
