## PCA
PCA = function(expr, n.varGene = 3e+3) {
  suppressMessages(library(ggplot2))
  suppressMessages(library(ggrepel))
  #### var Genes
  genes = grep('_', rownames(expr), value = T)
  if (length(genes)) {
    genes = data.frame(Raw = genes, Pro = gsub('_', '-', genes))
  }
  if (length(n.varGene)) {
    suppressMessages(library(Seurat))
    obj = CreateSeuratObject(expr)
    obj = FindVariableFeatures(obj, nfeatures = n.varGene)
    p = VariableFeaturePlot(obj)
    ggsave('PCA.nVar.png', p, w = 6, h = 5)
    var = VariableFeatures(obj)
    rm(obj)
    if (length(genes)) {
      comm = intersect(var, genes$Pro)
      if (length(comm)) var[match(comm, var)] = genes$Raw[match(comm, genes$Pro)]
    }
    expr = expr[var,]
  }
  #### PCA
  pca = data.frame(prcomp(t(expr))$x, check.rows = F)
  pca$sample = rownames(pca)
  pca$sample = factor(pca$sample, pca$sample)
  pca
}
