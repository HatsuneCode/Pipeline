## Enrich: GO ##
Enrich = function(genes, ont = NULL, pval = .05, og = 'org.Mm.eg.db') {
  ## og: org.Hs.eg.db / org.Mm.eg.db
  suppressMessages(library(clusterProfiler))
  entrez_ids = suppressMessages(bitr(genes, 'SYMBOL', 'ENTREZID', og))$ENTREZID
  if ('GOBP' %in% ont) {
    enrichGO(entrez_ids, og, ont = 'BP', pvalueCutoff = pval, readable = T)@result
  }
  if ('KEGG' %in% ont){
    enrichKEGG(entrez_ids, organism = ifelse(og == 'org.Mm.eg.db', 'mmu', 'hsa'))@result
  }
}
