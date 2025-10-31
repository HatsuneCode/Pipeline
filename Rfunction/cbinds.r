# cbind data.frame
cbinds = function(F1, F2, fill = 0) {
  # check dim
  if (any(dim(F1) == 0)) return(F2)
  if (any(dim(F2) == 0)) return(F1)
  # rownames
  rowall = union(rownames(F1), rownames(F2))
  dF1    = setdiff(rowall, rownames(F1))
  dF2    = setdiff(rowall, rownames(F2))
  # fill F1
  if (length(dF1)) {
    SF1r           = matrix(fill, nrow = length(setdiff(rowall, rownames(F1))), ncol = ncol(F1))
    rownames(SF1r) = dF1
    colnames(SF1r) = colnames(F1)
    F1             = rbind(F1, SF1r)
    rm(SF1r)
  }
  # fill F2
  if (length(dF2)) {
    SF2r           = matrix(fill, nrow = length(setdiff(rowall, rownames(F2))), ncol = ncol(F2))
    rownames(SF2r) = dF2
    colnames(SF2r) = colnames(F2)
    F2             = rbind(F2, SF2r)
    rm(SF2r)
  }
  # match
  F2 = F2[rownames(F1), , drop = FALSE]
  # return
  cbind(F1, F2)
}