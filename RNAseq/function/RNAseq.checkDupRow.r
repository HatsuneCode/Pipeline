# RNAseq: check duplicated rownames
RNAseq.checkDupRow = function(expr, pattern = '^ENS.*?_', method = 'mean', round = T) {
  ## process symbols
  gene  = sub(pattern, '', rownames(expr))
  dgene = unique(gene[duplicated(gene)])
  ## check duplicated symbols
  if (length(dgene)) {
    expr1 = expr[!gene %in% dgene, , drop = F]
    expr2 = do.call(rbind, lapply(dgene, function(g) {
      e = expr[gene == g, , drop = F]
      if (method == 'mean')
        t(setNames(data.frame(colMeans(e)), g))
    }))
    expr = rbind(expr1, expr2)
    rm(expr1, expr2)
  }
  rm(gene, dgene)
  ## restore symbol
  rownames(expr) = sub(pattern, '', rownames(expr))
  if (round) round(expr) else expr
}