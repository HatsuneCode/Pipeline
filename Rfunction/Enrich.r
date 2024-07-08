## Enrich: GO ##
EnrichGO = function(genes, ont = 'BP', pval = .05, og = 'org.Mm.eg.db') {
  suppressMessages(library(clusterProfiler))
  enrichGO(suppressMessages(bitr(genes, 'SYMBOL', 'ENTREZID', og))$ENTREZID, 
           og, ont = ont, pvalueCutoff = pval, readable = T)
}

## KEGG ##
kegg  = msigdbr::msigdbr('Mus musculus', 'C2', 'KEGG')
keggn = setNames(lapply(unique(kegg$gs_name), function(i)
  unique(as.character(kegg$gene_symbol)[kegg$gs_name == i])), unique(kegg$gs_name))

#### GO:BP ####
bp   = msigdbr::msigdbr('Mus musculus', 'C5', 'BP')
bpn  = setNames(lapply(unique(bp$gs_name), function(i)
  unique(as.character(bp$gene_symbol)[bp$gs_name == i] )), unique(bp$gs_name))
                        
#### GSEA ####
fGSEA = function(gene, sig, minSize = 4, maxSize = 500) {
  suppressMessages(library(fgsea))
  set.seed(1)
  gsea = fgsea(sig, gene, minSize = minSize, maxSize = maxSize)
  if (nrow(gsea)) {
    gsea$gene = unlist(lapply(gsea$leadingEdge, function(i) paste(i, collapse = ', ') ))
    data.frame(gsea[, c('pathway', 'NES', 'ES', 'pval', 'padj', 'gene')])
  }
}
