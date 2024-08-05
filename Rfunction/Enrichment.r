## Enrich: GO ##
EnrichGO = function(genes, ont = 'BP', pval = .05, og = 'org.Mm.eg.db') {
  ## og: org.Hs.eg.db / org.Mm.eg.db
  suppressMessages(library(clusterProfiler))
  enrichGO(suppressMessages(bitr(genes, 'SYMBOL', 'ENTREZID', og))$ENTREZID, 
           og, ont = ont, pvalueCutoff = pval, readable = T)
}
