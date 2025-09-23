suppressWarnings(library(crayon, warn.conflicts = F))
Pa    = crayon::cyan
Er    = crayon::red$bold
Sa    = crayon::blue
No    = crayon::magenta$bold
Wa    = crayon::yellow
timer = function() crayon::yellow( sub('\\..*', '', as.character(Sys.time() )) )
size  = function(x) round(object.size(x) / 1024^3, 1)
checkPath = function(x) normalizePath(x, '/', T)

## yml ##
suppressMessages(library(yaml))
suppressMessages(library(rlang))
handlers = list('bool#no'  = function(x) if ( x %in% c('false', 'False', 'FALSE', 'no')  ) F else x, 
                'bool#yes' = function(x) if ( x %in% c('true',  'True', 'TRUE',  'yes') ) T else x )

## args ##
args  = commandArgs()
me    = normalizePath(sub('--file=', '', grep('--file=', args, value = T)), '/')
args  = args[-seq(grep('--args', args))]
message(Wa('-->', timer(), 'Run: ', me, '<--'))

main = Er(me, '<groups.yml> <RNAseq.parameter.yml>')
file1 = args[1]
file2 = args[2]
if (is.na(file1) | is.na(file2)) { message(main); q('no') }
file1 = checkPath(file1)
file2 = checkPath(file2)

## 0. check parameters ##
message(Sa('-->', timer(), '1. check parameters:', Pa(file), '<--'))
groups    = yaml.load_file(file1, handlers = handlers)$groups
parameter = yaml.load_file(file2, handlers = handlers)

# check software
softwares  = lapply(parameter$softwares.splicing, checkPath)

# check reference
references = parameter$references.splicing

# check paired
paired = parameter$pairEnd

# check outdir
outdir = as.character(parameter$outdir %||% '.')
dir.create(outdir, F, T)
setwd(outdir)
d = 'rMATS.stat'
dir.create(d, F); setwd(d)

# process groups
sapply(seq(groups), function(i) {
  name = names(groups)[i]
  ## BAM ##
  p.n  = lapply(groups[[i]], function(ii) 
    paste(paste0(ii, '.port/portcullis.filtered.bam'), collapse = ',') )
  lapply(seq(p.n), function(ii) 
    writeLines(p.n[[ii]], paste0(name, '.', names(p.n)[ii], '.txt')) )
  ## CMD ##
  cmd = paste0(
    softwares$rmats.py, ' ', softwares$rmats.main, ' --nthread 6',
    ' --task post -t ',if (paired) 'paired' else 'single',
    ' --b1 ', name, '.pos.txt --b2 ', name, '.neg.txt --gtf ', references$Gtf,
    ' --readLength ', references$ReadLength, ' --od ', name, 
    '.rMATS --tmp ../rMATS.TMP; echo ', name, '.rMATS.stat done')
  writeLines(cmd, paste0(name, '.stat.sh'))
})

# run rMATS
thread = 6
cmd = paste('for i in *.stat.sh; do echo sh $i; done | parallel -j', thread, '> rMATS.stat.log 2>&1')
system(cmd)

# DEGs
rMATS = do.call(rbind, lapply(c('RI', 'SE', 'MXE', 'A3SS', 'A5SS'), function(ty) {
  message('--> ', ty, ' <--')
  do.call(rbind, lapply(regions, function(r) do.call(rbind, lapply(models.p, function(m) {
    g = paste0(m, '.', r)
    RI = read.table(paste0(g, '.rMATS/', ty, '.MATS.JC.txt'), sep = '\t', header = T)
    message('--> ', g, ': ', length(unique(RI$geneSymbol)), ' <--')
    ## cut FDR,Inc and IJC+SJC
    RI.sig = RI[RI$FDR <= .1 & abs(RI$IncLevelDifference) >= .1 &
                  sapply(strsplit(RI$IJC_SAMPLE_1, ','), function(i) mean(as.numeric(i)) ) +
                  sapply(strsplit(RI$SJC_SAMPLE_1, ','), function(i) mean(as.numeric(i)) ) >= 10 &
                  sapply(strsplit(RI$IJC_SAMPLE_2, ','), function(i) mean(as.numeric(i)) ) +
                  sapply(strsplit(RI$SJC_SAMPLE_2, ','), function(i) mean(as.numeric(i)) ) >= 10, 
                c('geneSymbol', 'FDR', 'IncLevelDifference') ]
    RI.sig = RI.sig[order(RI.sig$FDR),]
    RI.sig = RI.sig[!duplicated(RI.sig$geneSymbol),]
    RI.sig$Group = g
    RI.sig$Type  = ty
    RI.sig
})))) }))
write.table(rMATS, 'rMATS.stat.txt', sep = '\t', row.names = F)
table(rMATS$Group, rMATS$Type)
