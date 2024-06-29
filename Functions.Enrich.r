## enrich: GO ##
EnrichGO = function(genes, ont = 'BP', pval = .05, og = 'org.Mm.eg.db') {
  suppressMessages(library(clusterProfiler))
  enrichGO(suppressMessages(bitr(genes, 'SYMBOL', 'ENTREZID', og))$ENTREZID, 
           og, ont = ont, pvalueCutoff = pval, readable = T)
}
## fgsea ##
kegg  = msigdbr::msigdbr('Mus musculus', 'C2', 'KEGG')
keggn = setNames(lapply(unique(kegg$gs_name), function(i)
  unique(as.character(kegg$gene_symbol)[kegg$gs_name == i])), unique(kegg$gs_name))
##
fGSEA = function(gene, sig, minSize = 4, maxSize = 500) {
  set.seed(1)
  gsea = fgsea(sig, gene, minSize = minSize, maxSize = maxSize)
  gsea$gene = unlist(lapply(gsea$leadingEdge, function(i) paste(i, collapse = ', ') ))
  data.frame(gsea[, c('pathway', 'NES', 'ES', 'pval', 'padj', 'gene')])
}
