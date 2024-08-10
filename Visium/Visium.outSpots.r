## output spots ##
Visium.outSpots = function(obj, split = 'slides', group = NULL, name = 'SPOTout', outdir = name) {
  idx = if (!length(group)) Idents(obj) else obj@meta.data[[group]]
  dir.create(outdir, F)
  lapply(unique(obj@meta.data[[split]]), function(s) {
    s = as.character(s)
    m = obj@meta.data[[split]] == s
    message('--> outSpots: ', s, ' to ', outdir, ' <--')
    cells = sub(paste0('^', s, '_'), '', Cells(obj)[m])
    write.csv(setNames(data.frame(cells, as.character(idx)[m]), c('Barcode', name)), 
              paste0(outdir, '/', name, '_', gsub('/', '', s), '.cells.csv'), row.names = F, quote = F)
  })
  message('--> done <--')
}
