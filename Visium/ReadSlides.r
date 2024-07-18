## Read slides ##
ReadSlides = function(names, dirs = names, name = 'ST', clean = F, force = T) {
  suppressMessages(library(Seurat))
  suppressMessages(library(stringr))
  ## check
  f = paste0(name, '.ReadSlides.rds')
  if (file.exists(f) & !force) {
    message('!!! There is already a file exists. Reading it... !!!')
    obj = readRDS(f)
    return(obj) }
  ## each
  names = gsub('_', '.', names)
  objs = setNames(suppressWarnings(lapply(seq(names), function(i) {
    n = names[i]
    d = dirs [i]
    message('--> Read ', n, ' in ', d, ' <--')
    # read h5
    h5 = paste0(d, '/filtered_feature_bc_matrix.h5')
    if (!file.exists(h5)) { message('--> No ', h5, ' detected <--'); return() }
    data = Read10X_h5(h5)
    # remove out image
    out = paste0(d, '/Outs.csv')
    if (file.exists(out) & clean) {
      message('--> Remove SPOT out of image <--')
      out  = read.csv(out, header = T)$Barcode
      data = data[, !colnames(data) %in% out]
    } else message('--> No SPOT out <--')
    rm(out)
    # create obj
    message('--> Create seurat object <--')
    colnames(data) = paste0(n, '_', colnames(data))
    obj            = CreateSeuratObject(data, assay = 'ST', project = n)
    obj$mt.pct     = PercentageFeatureSet(obj, '^mt-|^MT-')
    rm(data)
    # normalize
    message('--> Normalize data <--')
    obj = NormalizeData(obj, verbose = F)
    # calculate cell cycle
    message('--> Calculate cell cycle <--')
    cc.s   = intersect(c(str_to_title(cc.genes.updated.2019$s.genes), 
                         cc.genes.updated.2019$s.genes), rownames(obj))
    cc.g2m = intersect(c(str_to_title(cc.genes.updated.2019$g2m.genes), 
                         cc.genes.updated.2019$s.genes), rownames(obj))
    obj = CellCycleScoring(obj, s.features = cc.s, g2m.features = cc.g2m)
    obj$cc.diff = obj$S.Score - obj$G2M.Score
    # set image
    message('--> Set image <--')
    img = Read10X_Image(paste0(d, '/spatial'))[sub('.*_', '', Cells(obj))]
    img = RenameCells(img, paste0(n, '_', Cells(img)))
    DefaultAssay(img) = 'ST'
    obj[['img']] = img
    names(obj@images) = n
    rm(img)
    # SCTransform v2
    message('--> SCT <--')
    obj = SCTransform(obj, assay = 'ST', verbose = F, vars.to.regress = c('mt.pct', 'cc.diff', 'nCount_ST'), vst.flavor = 'v2')
    # variable features
    list(obj = obj, feature = VariableFeatures(obj))
  })), names)
  objs = objs[!!sapply(objs, length)]
  ## merge
  features = lapply(objs, function(i) i[[2]])
  objs     = lapply(objs, function(i) i[[1]])
  if (length(objs) > 1) {
    message('--> merging ... <--')
    obj = merge(objs[[1]], objs[-1])
  } else obj = objs[[1]]
  names(obj@images) = names
  obj@misc = features
  rm(features, objs)
  # slides: names
  obj$slides = factor(obj$orig.ident, intersect(names, obj$orig.ident))
  ## save
  message('--> saving ... <--')
  saveRDS(obj, f)
  message('--> Done <--')
  obj
}
