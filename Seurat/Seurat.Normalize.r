Seurat.Normalize = function(obj, group.by = 'samples', assay = 'RNA', var.ctrl = NULL, ...) {
  suppressMessages(library(Seurat))
  ## SCT
  idx = obj@meta.data[[group.by]]
  obj = lapply(unique(idx), function(m) {
    m = as.character(m)
    message('SCT: ', m)
    st = obj[, idx == m]
    st = SCTransform(st, assay, vst.flavor = 'v2', vars.to.regress = c('mt.pct', 'cc.diff', paste0('nCount_', assay)), ...)
    list(obj = st, feature = VariableFeatures(st))
  })
  #### merge
  feature = lapply(obj, function(i) i[[2]] )
  obj     = lapply(obj, function(i) i[[1]] )
  if (length(obj)-1) {
    message('--> merge... <--'); obj = merge(obj[[1]], obj[-1])
  } else obj = obj[[1]]
  #### restore
  VariableFeatures(obj) = unique(if (length(var.ctrl)) 
    cleanGene( unlist(feature[grepl(var.ctrl, unique(idx))]) ) else 
      cleanGene( unlist(feature)) )
  message('--> nVarGene: ', length(VariableFeatures(obj)), ' <--')
  obj = PrepSCTFindMarkers(obj)
  obj
}
