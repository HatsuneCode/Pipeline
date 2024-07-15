path = '/home/songlianhao/Project/ADPD/b2.bulkMut/Fastqs'

fs = list.files(path, recursive = T, full.names = T, pattern = 'genes.results')
ns = sub('.genes.results$', '', sub('.*/', '', fs))

ms = c('expected_count', 'TPM', 'FPKM')

data = lapply(seq(ns), function(i) {
  f = fs[i]
  n = ns[i]
  message(n)
  df = read.table(f, sep = '\t', header = T)
  setNames(lapply(ms, function(m) {
    setNames(data.frame(df[[m]], row.names = df$gene_id, check.rows = F), n)
  }), ms)
})

message('---> saving... <---')

data = lapply(ms, function(m) {
  message(m)
  df = do.call(cbind, lapply(data, function(i) i[[m]] ))
  df = cbind(Gene = rownames(df), df)
  write.table(df, paste0(m, '.xls'), sep = '\t', quote = F, row.names = F)
})
