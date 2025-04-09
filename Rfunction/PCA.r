## PCA
PCA = function(expr, ...) {
  pca = data.frame(prcomp(t(expr), ...)$x, check.rows = F)
  pca$sample = rownames(pca)
  pca$sample = factor(pca$sample, pca$sample)
  pca
}
