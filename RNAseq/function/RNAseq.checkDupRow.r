RNAseq.checkDupRow = function(expr, method = 'mean') {
  ## process symbols
  gene  = sub('^ENS.*?_', '', rownames(expr))
  dgene = unique(gene[duplicated(gene)])
  ## check duplicated symbols
  if (length(dgene)) {
    expr1 = expr[!gene %in% dgene,]
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
  rownames(expr) = sub('^ENS.*?_', '', rownames(expr))
  expr
}
