# Visium: read 10x visium slides
Visium.ReadSlides = function(names, dirs = names, name = 'ST', output = 'Visium.ReadSlides.rds',
                             clean = T, cell.cycle = T, force = F) {
  suppressMessages(library(Seurat))
  suppressMessages(library(stringr))
  ## check output
  output = normalizePath(output, winslash = '/', mustWork = F)
  dir.create(dirname(output), F)
  if (file.exists(output) & !force) {
    message('!!! There is already a file exists. Reading it... !!!')
    return(readRDS(output)) }
  ## process each slides
  names = gsub('_', '.', names)
  objs = setNames(suppressWarnings(lapply(seq(names), function(i) {
    n = names[i]
    d = dirs [i]
    message('--> Read ', n, ' in ', d, ' <--')
    # read h5
    data = Read10X_h5(paste0(d, '/filtered_feature_bc_matrix.h5'))
    # remove out image
    out = paste0(d, '/Outs.csv')
    if (file.exists(out) & clean) {
      message('--> Remove SPOT out of image <--')
      out  = read.csv(out, header = T)$Barcode
      data = data[, !colnames(data) %in% out]
    } else message('--> No SPOT out <--')
    rm(out)
    # create seurat object
    message('--> Create seurat object <--')
    colnames(data) = paste0(n, '_', colnames(data))
    obj            = CreateSeuratObject(data, assay = 'ST', project = n)
    obj$mt.pct     = PercentageFeatureSet(obj, '^mt-|^MT-')
    obj$cc.diff    = 0
    rm(data)
    # normalize counts
    message('--> Normalize data <--')
    obj = NormalizeData(obj, verbose = F)
    # calculate cell cycle
    if (cell.cycle) {
      message('--> Calculate cell cycle <--')
      cc.s   = intersect(c(str_to_title(cc.genes.updated.2019$s.genes), 
                           cc.genes.updated.2019$s.genes), rownames(obj))
      cc.g2m = intersect(c(str_to_title(cc.genes.updated.2019$g2m.genes), 
                           cc.genes.updated.2019$g2m.genes), rownames(obj))
      obj = CellCycleScoring(obj, s.features = cc.s, g2m.features = cc.g2m)
      obj$cc.diff = obj$S.Score - obj$G2M.Score
    }
    # set image
    message('--> Set image <--')
    img = Read10X_Image(paste0(d, '/spatial'))[sub('.*_', '', Cells(obj))]
    img = RenameCells(img, paste0(n, '_', Cells(img)))
    DefaultAssay(img) = 'ST'
    obj[['img']] = img
    names(obj@images) = n
    rm(img)
    obj
  })), names)
  message('--> saving ... <--')
  saveRDS(objs, output)
  message('--> Done <--')
  objs
}
