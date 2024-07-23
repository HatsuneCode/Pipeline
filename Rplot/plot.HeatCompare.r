## p1: ComplexHeatmap plot; pvl1: pvalue matrix
plot.HeatCompare = function(p1, p2, pvl1, pvl2, pval = .05) {
  mtr1 = p1@matrix
  row1 = rownames(mtr1)[row_order(draw(p1))]
  mtr2 = p2@matrix
  row2 = rownames(mtr2)[row_order(draw(p2))]
  all  = unique(c(row1, row2))
  ##
  dif1 = setdiff(all, row1)
  if (length(dif1)) {
    mtr1 = rbind(mtr1, matrix(0, nrow = length(dif1), ncol = ncol(mtr1), dimnames = list(dif1, colnames(mtr1))) )[all,]
    pvl1 = rbind(pvl1, matrix(1, nrow = length(dif1), ncol = ncol(mtr1), dimnames = list(dif1, colnames(mtr1))) )[all,]
  }
  ##
  dif2 = setdiff(all, row2)
  if (length(dif2)) {
    mtr2 = rbind(mtr2, matrix(0, nrow = length(dif2), ncol = ncol(mtr2), dimnames = list(dif2, colnames(mtr2))) )[all,]
    pvl2 = rbind(pvl2, matrix(1, nrow = length(dif2), ncol = ncol(mtr2), dimnames = list(dif2, colnames(mtr2))) )[all,]
  }
  ##
  p1@matrix = mtr1
  p1@row_order = seq(all)
  p1@matrix_param$cell_fun = function(j, i, x, y, w, h, col) {
    f = mtr1[i, j]; p = pvl1[i, j]
    if (p < pval) 
      grid.rect(x, y, w, h, gp = gpar(fill = 'transparent', lwd = 1.5, col = ifelse(f > 0, 'red', 'blue')))
  }
  p1@row_names_param$anno@var_env$value = rownames(mtr1)
  p1@row_dend_param$cluster = F
  p1@row_dend_param$show = F
  ##
  p2@matrix = mtr2
  p2@row_order = seq(all)
  p2@matrix_param$cell_fun = function(j, i, x, y, w, h, col) {
    f = mtr2[i, j]; p = pvl2[i, j]
    if (p < pval) 
      grid.rect(x, y, w, h, gp = gpar(fill = 'transparent', lwd = 1.5, col = ifelse(f > 0, 'red', 'blue')))
  }
  p2@row_names_param$anno@var_env$value = rownames(mtr2)
  p2@row_dend_param$cluster = F
  p2@row_dend_param$show = F
  list(p1, p2)
}
