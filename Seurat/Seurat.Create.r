Seurat.Create = function(x, samples = NULL, cell.cycle = T) {
  suppressMessages(library(Seurat))
  suppressMessages(library(stringr))
  obj = CreateSeuratObject(x)
  ## check samples
  obj$samples = obj$orig.ident
  if (length(samples)) obj$samples = samples
  ## check mt.pct
  obj$mt.pct  = PercentageFeatureSet(obj, '^mt-|^MT-')
  obj$cc.diff = 0
  ## check cell cycle
  if (cell.cycle) {
    message('--> Normalize data <--')
    obj = NormalizeData(obj, verbose = F)
    message('--> Calculate cell cycle <--')
    cc.s   = intersect(c(str_to_title(cc.genes.updated.2019$s.genes),   cc.genes.updated.2019$s.genes),   rownames(obj))
    cc.g2m = intersect(c(str_to_title(cc.genes.updated.2019$g2m.genes), cc.genes.updated.2019$g2m.genes), rownames(obj))
    obj = CellCycleScoring(obj, s.features = cc.s, g2m.features = cc.g2m)
    obj$cc.diff = obj$S.Score - obj$G2M.Score
  }
  ## return
  obj
}
