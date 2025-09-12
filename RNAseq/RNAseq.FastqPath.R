suppressWarnings(library(crayon, warn.conflicts = F))
Pa    = crayon::cyan
Er    = crayon::red$bold
Sa    = crayon::blue
No    = crayon::magenta$bold
Wa    = crayon::yellow
timer = function() crayon::yellow( sub('\\..*', '', as.character(Sys.time() )) )
size  = function(x) round(object.size(x) / 1024^3, 1)
checkPath = function(x) normalizePath(x, '/', T)
`%|||%` = function(x, y) if (length(x)) if (all(is.na(x))) y else x else y 

## args ##
args = commandArgs()
me   = normalizePath(sub('--file=', '', grep('--file=', args, value = T)), '/')
args = args[-seq(grep('--args', args))]
message(Wa('-->', timer(), 'Run: ', me, '<--'))
main = Er(me, '<fastq_dir> <pattern> (pattern like _R[1-2].fq.gz) <RNAseq.parameter.yml>')
file = args[1]
part = args[2] %|||% '_R[1-2].fq.gz'
para = args[3]
if (is.na(file)) { message(main); q('no') }
file = normalizePath(file, '/', T)
message(Sa('--> Fq dir:', Pa(file), '<--'))
message(Sa('--> Pattern:', Pa(part), '<--'))

## detect fastqs and make samples for yml ##
fastq = grep(part, list.files(file, full.names = T, recursive = T), value = T)
names = sub(part, '', sub('.*/', '', fastq), names)
##
samples = setNames(lapply(unique(names), function(n) {
  fq = fastq[names == n]
  r1 = fq[grep('1', sub(paste0('.*/', n), '', fq))]
  r2 = fq[grep('2', sub(paste0('.*/', n), '', fq))]
  c(r1, r2)
}), unique(names))
yaml::write_yaml(list(samples = samples), 'samples.yml')

## combine with para.yml
system(paste0('cat ', para, ' samples.yml > RNAseq.parameter.yaml; rm samples.yml'))
