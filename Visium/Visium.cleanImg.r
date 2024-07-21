Visium.cleanImg = function(obj, images) {
  ## remove otiose
  obj@images = obj@images[intersect(names(obj@images), images)]
  ## match cells
  for (m in names(obj@images))
    obj@images[[m]] = obj@images[[m]][intersect(Cells(obj), Cells(obj@images[[m]])),]
  obj
}
