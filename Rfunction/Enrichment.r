## Enrich: GO ##
Enrich = function(genes, ont = NULL, pval = .05, og = 'org.Mm.eg.db') {
  ## og: org.Hs.eg.db / org.Mm.eg.db
  suppressMessages(library(clusterProfiler))
  map = suppressMessages(bitr(genes, 'SYMBOL', 'ENTREZID', og))
  ids = map$ENTREZID
  if ('GOBP' %in% ont) {
    enrichGO(ids, og, ont = 'BP', pvalueCutoff = pval, readable = T)@result
  }
  if ('KEGG' %in% ont){
    ke = enrichKEGG(ids, pvalueCutoff = pval, organism = ifelse(og == 'org.Mm.eg.db', 'mmu', 'hsa'))@result
    ke$geneID = unlist(lapply(ke$geneID, function(i) paste(map$SYMBOL[match(unlist(strsplit(i, '/')), map$ENTREZID)], collapse = ',') ))
    ke
  }
}
