## Seurat: Cluster
Seurat.Cluster = function(obj, group.by = 'samples', harmony = T, dims = 1:50, reso = .8) {
  suppressMessages(library(Seurat))
  suppressMessages(library(harmony))
  idx = obj@meta.data[[group.by]]
  obj = RunPCA(obj, verbose = F)
  if (harmony & length(unique(idx))-1) {
    obj = RunHarmony(obj, group.by)
    obj = RunUMAP(obj, dims = dims, reduction = 'harmony')
    obj = FindNeighbors(obj, dims = dims, reduction = 'harmony')
  } else {
    obj = RunUMAP(obj, dims = dims)
    obj = FindNeighbors(obj, dims = dims)
  }
  obj = FindClusters(obj, resolution = reso)
  obj
}