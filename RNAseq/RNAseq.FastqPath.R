## detect fastqs and make samples for yml ##
suppressMessages(library(yaml))
path  = 'fastq.dir'
path  = normalizePath(path, '/', T)
fastq = grep('fq.gz|fastq.gz', list.files(path, full.names = T, recursive = T), value = T)
names = sub('_R[1-2].fq.gz', '', sub('.*/', '', fastq), names)
##
samples = setNames(lapply(unique(names), function(n) {
  fq = fastq[names == n]
  r1 = fq[grep('R1', sub(paste0('.*/', n, '_'), '', fq))]
  r2 = fq[grep('R2', sub(paste0('.*/', n, '_'), '', fq))]
  c(r1, r2)
}), unique(names))
write_yaml(list(samples = samples), 'samples.yml')
