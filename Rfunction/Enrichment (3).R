## Enrich: GO ##
Enrich = function(genes, ont = NULL, pval = .05, og = 'org.Mm.eg.db') {
  ## og: org.Hs.eg.db / org.Mm.eg.db
  
  suppressMessages(library(clusterProfiler))
  entrez_ids <- suppressMessages(bitr(genes, 'SYMBOL', 'ENTREZID', og))$ENTREZID
  if ('GOBP' %in% ont) {
    enrich_df <- enrichGO(entrez_ids, og, ont = 'BP', pvalueCutoff = pval, readable = T)@result
  }
  if ('KEGG' %in% ont){
    enrich_df <- enrichKEGG(entrez_ids, organism = ifelse(og == 'org.Mm.eg.db', 'mmu', 'hsa'))@result
    enrich_df <- enrich_df[,c(3:11)]
  }
  return(enrich_df)
}
