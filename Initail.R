## r scripts ##
lhsong = new.env()
rs = list.files('C:/Rcodes/Pipeline/', '.r$', full.names = T, recursive = T)
for (f in rs) { message('source: ', f); source(f, local = lhsong) }
rm(f, rs)
attach(lhsong)
rm(lhsong)
