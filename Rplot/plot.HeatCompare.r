## pList: ComplexHeatmap plots list; pvList: pvalue matrixs list
plot.HeatCompare = function(pList, pvList = NULL, fc = 1, pval = .01, upCol = 'red', downCol = 'blue') {
  mtrs = lapply(pList, function(p) p@matrix )
  rows = lapply(seq(mtrs), function(i) rownames(mtrs[[i]])[row_order(draw(pList[[i]]))] )
  all  = unique(unlist(rows))
  ## process
  for (i in seq(mtrs)) {
    dif = setdiff(all, rows[[i]])
    if (length(dif)) {
      pList[[i]]@matrix = rbind(mtrs[[i]], matrix(0, nrow = length(dif), ncol = ncol(mtrs[[i]]), dimnames = list(dif, colnames(mtrs[[i]]))) )[all,]
      if (length(pvList))
        pvList[[i]] = rbind(pvList[[i]], matrix(1, nrow = length(dif), ncol = ncol(pvList[[i]]), dimnames = list(dif, colnames(pvList[[i]]))) )[all,]
    }
  }
  ## plots
  lapply(seq(pList), function(n) {
    p = pList[[n]]
    p@row_order = seq(all)
    mtr = p@matrix
    if (length(pvList)) pvl = pvList[[n]]
    p@matrix_param$cell_fun = function(j, i, x, y, w, h, col) {
      f = mtr[i, j]; p = pvl[i, j]
      if (length(pvList)) if (abs(f) > fc & p < pval)
        grid.rect(x, y, w, h, gp = gpar(fill = 'transparent', lwd = 1.5, col = ifelse(f > 0, upCol, downCol)))
    }
    p@row_names_param$anno@var_env$value = rownames(mtr)
    p@row_dend_param$cluster = F
    p@row_dend_param$show = F
    p
  })
}
