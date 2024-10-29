## Enrich: GO ##
Enrich = function(genes, ont = NULL, pval = .05, og = 'org.Mm.eg.db') {
  ## og: org.Hs.eg.db / org.Mm.eg.db
  suppressMessages(library(clusterProfiler))
  ids = suppressMessages(bitr(genes, 'SYMBOL', 'ENTREZID', og))$ENTREZID
  if ('GOBP' %in% ont) {
    enrichGO(ids, og, ont = 'BP', pvalueCutoff = pval, readable = T)@result
  }
  if ('KEGG' %in% ont){
    enrichKEGG(ids, pvalueCutoff = pval, organism = ifelse(og == 'org.Mm.eg.db', 'mmu', 'hsa'))@result
  }
}
