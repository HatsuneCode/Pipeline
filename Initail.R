path = 'C:/Hatsune/Pipeline'
load = scan()
if (1 %in% load) {
  ## r scripts ##
  lhsong = new.env()
  rs = list.files(path, '.r$', full.names = T, recursive = T)
  for (f in rs) { message('source: ', f); source(f, local = lhsong) }
  rm(f, rs)
  attach(lhsong)
  rm(lhsong)
}
