# RNAseq: normalize data by DESeq2
RNAseq.Normalize = function(expr, log2 = T, method = 'DESeq2') {
  if (method == 'DESeq2') {
    suppressMessages(library(DESeq2))
    dds  = DESeqDataSetFromMatrix(
      countData = expr,
      colData = data.frame(row.names = colnames(expr), samples = factor(colnames(expr))),
      design = ~ samples)
    dds  = estimateSizeFactors(dds)
    expr = counts(dds, normalize = T)
  }
  if (log2) expr = log2(expr + 1)
  expr
}