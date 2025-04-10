#### GSEA ####
fGSEA = function(gene, sig = NULL, scoreType = 'std', minSize = 2, maxSize = 500, type = NULL, species = 'Homo sapiens') {
  # species: Homo sapiens / Mus musculus
  suppressMessages(library(fgsea))
  suppressMessages(library(msigdbr))
  if ('KEGG' %in% type) {
    kegg  = msigdbr(species, 'C2', 'KEGG')
    keggn = setNames(lapply(unique(kegg$gs_name), function(i)
      unique(as.character(kegg$gene_symbol)[kegg$gs_name == i])), unique(kegg$gs_name))
    rm(kegg)
    sig = keggn
  }
  if ('WIKI' %in% type) {
    wp  = msigdbr(species, 'C2', 'WIKIPATHWAYS')
    wpn = setNames(lapply(unique(wp$gs_name), function(i)
      unique(as.character(wp$gene_symbol)[wp$gs_name == i])), unique(wp$gs_name))
    rm(wp)
    sig = wpn
  }
  if ('REACT' %in% type) {
    reactome  = msigdbr(species, 'C2', 'REACTOME')
    reactomen = setNames(lapply(unique(reactome$gs_name), function(i)
      unique(as.character(reactome$gene_symbol)[reactome$gs_name == i])), unique(reactome$gs_name))
    rm(reactome)
    sig = reactomen
  }
  if ('GOBP' %in% type) {
    bp   = msigdbr(species, 'C5', 'BP')
    bpn  = setNames(lapply(unique(bp$gs_name), function(i)
      unique(as.character(bp$gene_symbol)[bp$gs_name == i] )), unique(bp$gs_name))
    rm(bp)
    sig = bpn
  }
  if ('GOCC' %in% type) {
    cc   = msigdbr(species, 'C5', 'CC')
    ccn  = setNames(lapply(unique(cc$gs_name), function(i)
      unique(as.character(cc$gene_symbol)[cc$gs_name == i] )), unique(cc$gs_name))
    rm(cc)
    sig = ccn
  }
  if ('GOMF' %in% type) {
    mf   = msigdbr(species, 'C5', 'MF')
    mfn  = setNames(lapply(unique(mf$gs_name), function(i)
      unique(as.character(mf$gene_symbol)[mf$gs_name == i] )), unique(mf$gs_name))
    rm(mf)
    sig = mfn
  }
  set.seed(1)
  gsea = fgsea(sig, gene, minSize = minSize, maxSize = maxSize, scoreType = scoreType)
  if (nrow(gsea)) {
    gsea$gene = unlist(lapply(gsea$leadingEdge, function(i) paste(i, collapse = ', ') ))
    data.frame(gsea[, c('pathway', 'NES', 'ES', 'pval', 'padj', 'gene')])
  }
}
