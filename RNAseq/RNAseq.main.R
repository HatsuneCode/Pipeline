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
handlers = list('bool#no'  = function(x) if ( x %in% c('false', 'False', 'FALSE', 'no')  ) FALSE else x, 
                'bool#yes' = function(x) if ( x %in% c('true',  'True', 'TRUE',  'yes') ) TRUE  else x )
## args ##
args  = commandArgs()
me    = normalizePath(sub('--file=', '', grep('--file=', args, value = T)), '/')
args  = args[-seq(grep('--args', args))]
message(Wa('-->', timer(), 'Run: ', me, '<--'))

main = Er(me, '<RNAseq.parameter.yml>')
file = args[1]
if (is.na(file)) { message(main); q('no') }
file = normalizePath(file, '/', T)

## 0. check parameters ##
message(Sa('-->', timer(), '1. check parameters:', Pa(file), '<--'))
parameter = yaml.load_file(file, handlers = handlers)
# check project name
project  = as.character(parameter$project %||% 'RNAseq')
thread   = as.numeric(parameter$thread %||% 1)
pairEnd  = parameter$pairEnd  %||% T
cleanFq  = parameter$cleanFq  %||% T
cleanBam = parameter$cleanBam %||% T
# check softwre paths
softwares  = lapply(parameter$softwares, checkPath)
# check steps
enforce    = parameter$enforce %||% F
steps      = parameter$steps
fastp      = steps$fastp %||% T
# check para
fastpPara  = parameter$fastp
fastp_LR   = fastpPara$length_required %||% 15
# check reference
references = parameter$references
# check sample names and paths
samples    = lapply(parameter$samples, checkPath)
# check outdir
outdir     = as.character(parameter$outdir %||% '.')

## 1. set work dir ##
dir.create(outdir, F, T)
setwd(outdir)
wdir = getwd()
message(Sa('-->', timer(), 'working in dir:', Pa(wdir), '<--'))
dir.create('log', F)

## 2. run each samples
message(Sa('-->', timer(), '2. RNAseq pipeline running... <--'))
suppressMessages(library(future.apply))
if (thread > 1) plan(multicore(workers = function() thread ))
message(Sa('-->', timer(), 'use Cores:', Pa(thread), 'availableCores:', 
	   Pa(as.numeric(suppressWarnings(availableCores()))), '<--'))
run = function(i) {
  n = names(samples)[i]
  message(Sa('-->', timer(), paste0('No.', i, ':'), 'working on', Pa(n), '<--'))
  dir.create(n, F)
  log = paste0('log/', n, '.log')
  log = if (file.exists(log)) readLines(log)
  ## check each steps
  rn.fp = !sum(grepl('Fastp work is done', log))   | enforce
  rn.bt = !sum(grepl('Bowtie2 work is done', log)) | enforce 
  rn.st = !sum(grepl('STAR work is done', log))    | enforce
  rn.rs = !sum(grepl('RSEM work is done', log))    | enforce 
  message(Sa('--> run fastp:', Pa(ifelse(rn.fp & fastp, 'need', 'skip')), '<--'))
  message(Sa('--> run bowtie2:', Pa(ifelse(rn.bt, 'need', 'skip')), '<--'))
  message(Sa('--> run STAR:', Pa(ifelse(rn.st, 'need', 'skip')), '<--'))
  message(Sa('--> run RSEM:', Pa(ifelse(rn.st, 'need', 'skip')), '<--'))
  ## make RNAseq.sh
  sh = c(
   err = 'set -e',
   # s0.cd
   cd = paste0('cd ', wdir, '/', n),
   rn = paste0('echo "--> ', n, ' <--"; echo This work is running... > ../log/', n, '.log'),
   '',
   # s1.fastp
   if (rn.fp)
   s1 = if (fastp) 
     paste0(softwares$fastp, ' -q 20 -u 10 -l ', fastp_LR, ' -w 8 -i ', samples[[i]][1], if (pairEnd) paste0(' -I ', samples[[i]][2]), ' -o ', n, '.r1.fq.gz', if (pairEnd) paste0(' -O ', n, '.r2.fq.gz --detect_adapter_for_pe'), ' -j ', n, '.fastp.json -h ', n, '.fastp.html >> ../log/', n, '.log 2>&1') else
     paste0('cat ', samples[[i]][1], ' > ', n, '.r1.fq.gz', if (pairEnd) paste0('; cat ', samples[[i]][2], ' > ', n, '.r2.fq.gz')),
   dn.fastp = paste0('msg="Fastp work is done."; echo $msg; echo $msg >> ../log/', n, '.log'),
   '',
   # s2.bowtie2
   if (rn.bt)
   s2 = paste0(softwares$bowtie2, ' -p 8 -x ', references$rRNAref, ' --local ', ifelse(pairEnd, '-1', '-U'), ' ', n, '.r1.fq.gz', if (pairEnd) paste0(' -2 ', n, '.r2.fq.gz'), ' --un', if (pairEnd) '-conc ', n, '.filter.fq -S /dev/null > ', n, '.rRNA.log 2>&1', if (cleanFq) '; rm *r*.fq.gz'),
   dn.bt2 = paste0('msg="Bowtie2 work is done."; echo $msg; echo $msg >> ../log/', n, '.log'),
   '',
   # s3.STAR
   if (rn.st)
   s3 = paste0(softwares$STAR, ' --outReadsUnmapped Fastx --runThreadN 6 --genomeDir ', references$STARref, ' --readFilesIn ', n, '.filter', if (pairEnd) '.1', '.fq ', if (pairEnd) paste0(n, '.filter.2.fq '), '--outBAMsortingThreadN 6 --outSAMattributes All --outSAMtype BAM SortedByCoordinate --quantMode TranscriptomeSAM GeneCounts --outFileNamePrefix ', n, '. >> ../log/', n, '.log 2>&1; gzip -1 ', n, '.Unmapped.out.mate*', if (cleanFq) '; rm *filter*fq'),
   dn.star = paste0('msg="STAR work is done."; echo $msg; echo $msg >> ../log/', n, '.log'),
   '',
   # s4.RSEM
   if (rn.rs)
   s4 = paste0(softwares$RSEM, ' --alignments', if (pairEnd) ' --paired-end', ' -p 8 --append-names --no-bam-output ', n, '.Aligned.toTranscriptome.out.bam ', references$RSEM, ' ', n, ' >> ../log/', n, '.log 2>&1', if (cleanBam) '; rm *.bam'),
   dn.rsem = paste0('msg="RSEM work is done."; echo $msg; echo $msg >> ../log/', n, '.log'),
   '',
   # index
   dn = paste0('msg="This work is done."; echo $msg; echo $msg >> ../log/', n, '.log')
  )
  writeLines(sh, paste0(n, '/', n, '.RNAseq.sh'))
  ## qsub
  system(paste0('sh ', n, '/', n, '.RNAseq.sh'))
}
## run
if (thread > 1) future_lapply(seq(samples), run) else lapply(seq(samples), run)
message(Sa('-->', timer(), 'all samples done <--'))

## done ##
message(Wa('-->', timer(), 'Done:', me, '<--'))

