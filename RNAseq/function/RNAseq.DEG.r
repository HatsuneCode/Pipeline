#### Differential expression analysis ####
#### DESeq2
RNAseq.DESeq2 = function(expr, pos = NULL, neg = NULL, name = NULL, exp_cut = 10, 
                         cut.method = 'inter', shrink.type = 'apeglm', ...) {
  ## cut.method: inter, union
  ## shrink.type: apeglm, ashr, normal
  suppressMessages(library(DESeq2))
  ## expr split
  exprP = if (length(pos)) expr[, colnames(expr) %in% pos, drop = F] else 
    expr[, !colnames(expr) %in% neg, drop = F]
  exprN = if (length(neg)) expr[, colnames(expr) %in% neg, drop = F] else 
    expr[, !colnames(expr) %in% pos, drop = F]
  ## expr type
  type = paste(paste(colnames(exprP), collapse = ','), 'vs', paste(colnames(exprN), collapse = ',') )
  if (!length(name)) name = type
  message('DEG: ', type)
  ## condition control ~ treatment
  condition = factor( c(rep('Neg', ncol(exprN)), rep('Pos', ncol(exprP))), c('Neg', 'Pos') )
  ## counts
  expr  = cbind(exprN, exprP)
  expr  = expr[rowSums(expr) > 0, , drop = F]
  ## cut-off
  if (length(exp_cut)) {
    gene = lapply(c('Pos', 'Neg'), function(g) {
      expr.f = expr[, condition == g, drop = F]
      rownames(expr.f)[apply(expr.f, 1, function(i) !any(i < exp_cut) )]
    } )
    if ('inter' %in% cut.method) gene = Reduce(intersect, gene)
    if ('union' %in% cut.method) gene = Reduce(union, gene)
    message('--> valid gene number: ',  length(gene), ' <--')
    expr = expr[gene, , drop = F]
  }
  ## split pos/neg
  exprP = expr[, condition == 'Pos', drop = F]
  exprN = expr[, condition == 'Neg', drop = F]
  ## meta
  meta = data.frame(row.names = colnames(expr), condition)
  ## DESeq2 normalize
  dds   = DESeqDataSetFromMatrix(countData = expr, colData = meta, design = ~ condition)
  dds.n = estimateSizeFactors(dds)
  ## DESeq2 compare
  dds.r = DESeq(dds, ...)
  dds.s = data.frame(lfcShrink(dds.r, coef = 'condition_Pos_vs_Neg', type = shrink.type))
  dds   = data.frame(results(dds.r), check.names = F)
  dds$lfcS   = dds.s$log2FoldChange
  dds$lfcSES = dds.s$lfcSE
  rm(dds.s, dds.r)
  ## split normalized pos/neg
  expr.n  = counts(dds.n, normalize = T)
  expr.nP = expr.n[, condition == 'Pos', drop = F]
  expr.nN = expr.n[, condition == 'Neg', drop = F]
  ## output
  data.frame(p_val        = dds$pvalue, 
             avg_log2FC   = dds$log2FoldChange,
             avg_log2FC.S = dds$lfcS,
             pct.1        = apply(exprP, 1, function(i) sum(i > 0)/ncol(exprP) ),
             pct.2        = apply(exprN, 1, function(i) sum(i > 0)/ncol(exprN) ),
             p_val_adj    = dds$padj, 
             gene         = rownames(dds),
             ## global
             average.raw  = rowMeans(expr), 
             average.norm = dds$baseMean,
             median.raw   = apply(expr,   1, median), 
             median.norm  = apply(expr.n, 1, median), 
             ## standard error
             lfcSE        = dds$lfcSE,
             lfcSE.S      = dds$lfcSES,
             ## average
             posAvg.raw   = rowMeans(exprP), 
             negAvg.raw   = rowMeans(exprN), 
             posAvg.norm  = rowMeans(expr.nP), 
             negAvg.norm  = rowMeans(expr.nN), 
             ## median
             posMed.raw   = apply(exprP,   1, median),
             negMed.raw   = apply(exprN,   1, median),
             posMed.norm  = apply(expr.nP, 1, median),
             negMed.norm  = apply(expr.nN, 1, median),
             ## meta
             type     = name, 
             upDown   = factor(ifelse(dds$log2FoldChange > 0, 'Up', 'Down'), c('Up', 'Down')),
             validNum = nrow(expr.n),
             row.names = NULL )
}

#### Limma
RNAseq.Limma = function(expr, pos = NULL, neg = NULL, name = NULL, voom = T) {
  suppressMessages(library(limma))
  if (!length(name)) 
    name = paste(paste(pos, collapse = ','), 'vs', paste(neg, collapse = ',') )
  message('DEG: ', name)
  ## expr split
  exprP = if (length(pos)) expr[, colnames(expr) %in% pos, drop = F] else 
    expr[, !colnames(expr) %in% neg, drop = F]
  exprN = if (length(neg)) expr[, colnames(expr) %in% neg, drop = F] else 
    expr[, !colnames(expr) %in% pos, drop = F]
  ## counts
  exp = cbind(exprN, exprP)
  condition = c(rep('Neg', ncol(exprN)), rep('Pos', ncol(exprP)))
  exprP = exp[, condition == 'Pos', drop = F]
  exprN = exp[, condition == 'Neg', drop = F]
  ## make coldata
  coldata = data.frame(group = factor(condition, c('Neg', 'Pos')) )
  ## design
  design = model.matrix(~ 0 + group, data = coldata)
  colnames(design) = levels(coldata$group)
  rownames(design) = colnames(exp)
  ## contrast
  contrast = makeContrasts('Pos-Neg', levels = design)
  ## fit
  fit  = if (voom) lmFit(voom(exp, design), design) else lmFit(exp, design)
  fit2 = contrasts.fit(fit, contrast)
  fit2 = eBayes(fit2)
  ## output
  dds = topTable(fit2, n = Inf)
  dds = dds[row.names(exp),]
  data.frame(p_val = dds$P.Value, avg_log2FC = dds$logFC, 
             pct.1 = apply(exprP, 1, function(i) sum(i > 0)/ncol(exprP) ),
             pct.2 = apply(exprN, 1, function(i) sum(i > 0)/ncol(exprN) ),
             p_val_adj = dds$adj.P.Val, gene = rownames(dds), average = rowMeans(exp), 
             median = apply(exp, 1, median), 
             posAvg = rowMeans(exprP), posMed = apply(exprP, 1, median),
             negAvg = rowMeans(exprN), negMed = apply(exprN, 1, median),
             type = name, 
             upDown = factor(ifelse(dds$logFC > 0, 'Up', 'Down'), c('Up', 'Down')), 
             row.names = NULL )
}
