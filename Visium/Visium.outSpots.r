## output spots ##
Visium.outSpots = function(obj, name = 'SPOTout', outdir = name) {
  dir.create(outdir, F)
  lapply(unique(obj$slides), function(s) {
    message('--> out: ', s, ' to ', outdir, ' <--')
    cells = sub(paste0('^', s, '_'), '', Cells(obj)[obj$slides == s])
    write.csv(data.frame(Barcode = cells, Spot = s), 
              paste0(outdir, '/', name, '-', gsub('/', '', s), '.cells.csv'),
              row.names = F, quote = F)
  })
  message('--> done <--')
}
