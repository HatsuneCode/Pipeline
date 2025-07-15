## Enrich: GO ##
Enrich = function(genes, ont = NULL, pval = .05, qval = .2, og = 'org.Mm.eg.db') {
  ## og: org.Hs.eg.db / org.Mm.eg.db
  suppressMessages(library(clusterProfiler))
  map = suppressMessages(bitr(genes, 'SYMBOL', 'ENTREZID', og))
  ids = map$ENTREZID
  if (sum(c('BP', 'MF', 'CC', 'ALL') %in% ont)) {
    go = enrichGO(ids, og, ont = ont, pvalueCutoff = pval, readable = T, qvalueCutoff = qval, pool = T)
    return(go)
  }
  if ('KEGG' %in% ont){
    ke = enrichKEGG(ids, pvalueCutoff = pval, organism = ifelse(og == 'org.Mm.eg.db', 'mmu', 'hsa'))
    ke@result$geneID = unlist(lapply(ke$geneID, function(i) paste(map$SYMBOL[match(unlist(strsplit(i, '/')), map$ENTREZID)], collapse = ',') ))
    ke
  }
}
