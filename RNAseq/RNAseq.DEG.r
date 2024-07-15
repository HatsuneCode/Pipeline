#### Differential expression analysis ####
#### DESeq2
DESeq2 = function(expr, pos = NULL, neg = NULL, name = NULL) {
  suppressMessages(library(DESeq2))
  if (!length(name)) 
    name = paste(paste(pos, collapse = ','), 'vs', paste(neg, collapse = ',') )
  message('DEG: ', name)
  ## expr split
  exprP = if (length(pos)) expr[, colnames(expr) %in% pos, drop = F] else 
    expr[, !colnames(expr) %in% neg, drop = F]
  exprN = if (length(neg)) expr[, colnames(expr) %in% neg, drop = F] else 
    expr[, !colnames(expr) %in% pos, drop = F]
  ## condition control ~ treatment
  condition = factor( c(rep('Neg', ncol(exprN)), rep('Pos', ncol(exprP))), c('Neg', 'Pos') )
  ## counts
  expr  = cbind(exprN, exprP)
  expr  = expr[rowSums(expr) > 0,,drop = F]
  exprP = expr[, condition == 'Pos', drop = F]
  exprN = expr[, condition == 'Neg', drop = F]
  ## meta
  meta = data.frame(row.names = colnames(expr), condition)
  ## DESeq2
  dds = DESeqDataSetFromMatrix(countData = expr, colData = meta, design = ~ condition)
  dds = DESeq(dds)
  dds = data.frame(results(dds), check.names = F)
  ## output
  data.frame(p_val = dds$pvalue, avg_log2FC = dds$log2FoldChange, 
             pct.1 = apply(exprP, 1, function(i) sum(i > 0)/ncol(exprP) ),
             pct.2 = apply(exprN, 1, function(i) sum(i > 0)/ncol(exprN) ),
             p_val_adj = dds$padj, gene = rownames(dds), average = rowMeans(expr), 
             median = apply(expr, 1, median), 
             posAvg = rowMeans(exprP), posMed = apply(exprP, 1, median),
             negAvg = rowMeans(exprN), negMed = apply(exprN, 1, median),
             type = name, row.names = NULL)
}
#### Limma
Limma = function(expr, pos = NULL, neg = NULL, name = NULL) {
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
  exp   = cbind(exprN, exprP)
  exp   = exp[rowSums(exp) > 0,,drop = F]
  condition = c(rep('Neg', ncol(exprN)), rep('Pos', ncol(exprP)))
  exprP = exp[, condition == pos, drop = F]
  exprN = exp[, condition == neg, drop = F]
  ## make coldata
  coldata = data.frame(group = factor( condition ), c('Neg', 'Pos') ))
  ## design
  design = model.matrix(~ 0 + group, data = coldata)
  colnames(design) = levels(coldata$group)
  rownames(design) = colnames(exp)
  ## contrast
  contrast = makeContrasts('Neg-Pos', levels = design)
  v = voom(exp, design)
  ## fit
  fit  = lmFit(v, design)
  fit2 = contrasts.fit(fit, contrast)
  fit2 = eBayes(fit2)
  ## output
  dds = topTable(fit2, n = Inf)
  data.frame(p_val = dds$P.Value, avg_log2FC = dds$logFC, 
             pct.1 = apply(exprP, 1, function(i) sum(i > 0)/ncol(exprP) ),
             pct.2 = apply(exprN, 1, function(i) sum(i > 0)/ncol(exprN) ),
             p_val_adj = dds$adj.P.Val, gene = rownames(dds), average = rowMeans(expr), 
             median = apply(expr, 1, median), 
             posAvg = rowMeans(exprP), posMed = apply(exprP, 1, median),
             negAvg = rowMeans(exprN), negMed = apply(exprN, 1, median),
             type = name, row.names = NULL)
}
