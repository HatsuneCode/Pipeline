## reCluster
Visium.ReCluster = function(obj, harmony = T, group.by = 'slides', dims = 1:50) {
  suppressMessages(library(Seurat))
  suppressMessages(library(harmony))
  #### SCT
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
  obj = FindClusters(obj)
  obj
}
