RNAseq.varGene = function(expr, n.varGene = 3000, outFig = 'RNAseq.nVar.png') {
  suppressMessages(library(Seurat))
  genes = grep('_', rownames(expr), value = T)
  if (length(genes))
    genes = data.frame(Raw = genes, Pro = gsub('_', '-', genes))
  obj = suppressWarnings(CreateSeuratObject(expr))
  obj = FindVariableFeatures(obj, nfeatures = n.varGene, verbose = F)
  p   = VariableFeaturePlot(obj)
  if (length(outFig)) ggsave(outFig, p, w = 6, h = 5)
  var = suppressWarnings(VariableFeatures(obj))
  rm(obj)
  if (length(genes)) {
    comm = intersect(var, genes$Pro)
    if (length(comm)) var[match(comm, var)] = genes$Raw[match(comm, genes$Pro)]
  }
  var
}
