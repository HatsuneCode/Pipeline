## DEG for seurat ##
Seurat.DEG = function(obj, pos = NULL, neg = NULL, group.by = 'seurat_clusters', min.pct = .01, logfc = 0, min.cell = 3, max.cells.per.ident = Inf,
               assay = 'SCT', slot = 'data', method = 'wilcox_limma', only.pos = F, pre = F) {
  name = paste(if (length(pos)) paste(pos, collapse = ',') else 'Others', 'vs',
               if (length(neg)) paste(neg, collapse = ',') else 'Others')
  message('DEG: ', name)
  ##
  DefaultAssay(obj) = assay
  if (pre) obj = if (assay == 'SCT') PrepSCTFindMarkers(obj) else JoinLayers(obj)
  mk  = FindMarkers(obj, group.by = group.by, ident.1 = pos, ident.2 = neg, 
                    min.pct = min.pct, logfc.threshold = logfc, test.use = method, 
                    slot = slot, fc.slot = slot, only.pos = only.pos,
                    min.cells.group = min.cell, max.cells.per.ident = max.cells.per.ident)
  gc()
  ##
  mk$gene = rownames(mk)
  expr  = GetAssayData(obj, assay = assay, layer = slot)[mk$gene,]
  exprP = if (length(pos)) expr[, obj@meta.data[[group.by]] %in% pos] else expr[, !obj@meta.data[[group.by]] %in% neg]
  exprN = if (length(neg)) expr[, obj@meta.data[[group.by]] %in% neg] else expr[, !obj@meta.data[[group.by]] %in% pos]
  ##
  mk$average = rowMeans(expr)
  mk$median  = apply(expr,  1, median)
  mk$posAvg  = rowMeans(exprP)
  mk$posMed  = apply(exprP, 1, median)
  mk$negAvg  = rowMeans(exprN)
  mk$negMed  = apply(exprN, 1, median)
  mk$type    = name
  mk$upDown  = factor(ifelse(mk$avg_log2FC > 0, 'Up', 'Down'), c('Up', 'Down'))
  mk
}
