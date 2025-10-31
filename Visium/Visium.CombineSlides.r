# Visium: combine slides objects
Visium.CombineSlides = function(objs, output = 'Visium.CombineSlides.rds', force = F) {
  ## check output
  output = normalizePath(output, winslash = '/', mustWork = F)
  dir.create(dirname(output), F)
  if (file.exists(output) & !force) {
    message('!!! There is already a file exists. Reading it... !!!')
    return(readRDS(output)) }
  ## merge
  message('--> merging ... <--')
  obj   = merge(objs[[1]], objs[-1])
  obj   = JoinLayers(obj)
  names = names(objs)
  names(obj@images) = names
  obj@misc = setNames(lapply(objs, function(obj.i) VariableFeatures(obj.i) ), names)
  # slides order: names
  obj$slides = factor(obj$orig.ident, names)
  message('--> saving ... <--')
  saveRDS(obj, output)
  message('--> Done <--')
  obj
}