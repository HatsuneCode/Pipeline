#! /home/songlianhao/conda/envs/lhsong/bin/Rscript
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
handlers = list('bool#no'  = function(x) if ( x %in% c('false', 'FALSE', 'no')  ) FALSE else x, 
                'bool#yes' = function(x) if ( x %in% c('true',  'TRUE',  'yes') ) TRUE  else x )
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
message(Sa('-->', timer(), '1. check parameters <--'))
parameter = yaml.load_file(file, handlers = handlers)
# check project name
project = as.character(parameter$project %||% 'RNAseq')
thread  = as.numeric(parameter$thread %||% 1)
pairEnd = parameter$pairEnd %||% T
# check detect time interval
timeIt  = as.numeric(parameter$detect %||% 30)
if (timeIt < 1) stop(Er('!!! Detect time interval could not less than 1 minutes !!!'))
# check softwre paths
softwares  = lapply(parameter$softwares, checkPath)
# check steps
steps      = parameter$steps
fastp      = steps$fastp %||% T
rseqc      = steps$rseqc %||% F
# check para
fastpPara  = parameter$fastp
fastp_LR   = fastpPara$length_required %||% 15
# check reference
references = parameter$references
# check sample names and paths
samples    = lapply(parameter$samples, checkPath)
# check mem
mem        = parameter$mem %||% 10
# check outdir
outdir     = parameter$outdir %||% '.'
outdir     = as.character(outdir)

## 1. set work dir ##
dir.create(outdir, F, T)
setwd(outdir)
wdir = getwd()
dir.create('log', F)

## choose cu ##
pe    = system('pestat', intern = T)
pe    = grep('fat', grep('free', pe, value = T), invert = T, value = T)
gpu   = paste0('cu', 26:35)
load  = unlist(lapply(strsplit(pe, '\\s+'), function(i) setNames(i[4], i[2]) ))
load  = load[!names(load) %in% gpu]
#### less than 70
nodes = names(load[which(as.numeric(sub('\\*$', '', load)) < 70)])

## 2. run each samples
message(Sa('-->', timer(), '2. RNAseq pipeline running... <--'))
suppressMessages(library(future.apply))
if (thread > 1) plan(multicore(workers = function() thread ))
run = future_lapply(seq(samples), function(i) {
  n = names(samples)[i]
  message(Sa('-->', timer(), paste0('No.', i, ':'), 'working on', Pa(n), '<--'))
  dir.create(n, F)
  ## make RNAseq.sh
  node = nodes[i %% length(nodes) +1]
  sh = c(
   # s0.pbs
   pbs = paste0('#PBS -N ', n, '\n#PBS -o ', wdir, '/', n, '.out\n#PBS -e ', wdir, '/', n, '.err\n#PBS -l nodes=', node, ':ppn=1\n#PBS -l mem=', mem, 'GB'),
   err = 'set -e',
   # s0.cd
   cd = paste0('cd ', wdir, '/', n),
   rn = paste0('echo This work is running... > ../log/', n, '.log'),
   # s1.fastp
   s1 = if (fastp) 
     paste0(softwares$fastp, ' -q 20 -u 10 -l ', fastp_LR, ' -w 8 -i ', samples[[i]][1], if (pairEnd) paste0(' -I ', samples[[i]][2]), ' -o ', n, '.r1.fq.gz', if (pairEnd) paste0(' -O ', n, '.r2.fq.gz '), ' -j ', n, '.fastp.json -h ', n, '.fastp.html >> ../log/', n, '.log 2>&1') else
     paste0('cat ', samples[[i]][1], ' > ', n, '.r1.fq.gz', if (pairEnd) paste0('; cat ', samples[[i]][2], ' > ', n, '.r2.fq.gz')),
   # s2.bowtie2
   s2 = paste0(softwares$bowtie2, ' -p 8 -x ', references$rRNAref, ' --local ', ifelse(pairEnd, '-1', '-U'), ' ', n, '.r1.fq.gz', if (pairEnd) paste0(' -2 ', n, '.r2.fq.gz'), ' --un-conc-gz ', n, '.filter.fq.gz -S rRNA.sam > ', n, '.rRNA.log 2>&1; rm rRNA.sam'),
   # s3.STAR
   s3 = paste0(softwares$STAR, ' --runThreadN 6 --genomeDir ', references$STARref, ' --readFilesIn ', n, '.filter.fq.1.gz ', if (pairEnd) paste0(n, '.filter.fq.2.gz '), '--readFilesCommand zcat --outBAMsortingThreadN 6 --outSAMattributes All --outSAMtype BAM SortedByCoordinate --quantMode TranscriptomeSAM GeneCounts --outFileNamePrefix ', n, '. >> ../log/', n, '.log 2>&1'),
   # s4.RSEM
   s4 = paste0(softwares$RSEM, ' --alignments', if (pairEnd) ' --paired-end', ' -p 8 --append-names --no-bam-output ', n, '.Aligned.toTranscriptome.out.bam ', references$RSEM, ' ', n, ' >> ../log/', n, '.log 2>&1'),
   # s5.Seq satu
   s5 = paste0(softwares$Rscript, ' /home/songlianhao/pipeline/RNAseq.SeqSatu.r ', n, ' > ', n, '.seqSatu.log 2>&1'),
   # clean
   cl = paste0('rm ', n, '.*.fq*gz'),
   # index
   dn = paste0('echo This work is done. >> ../log/', n, '.log')
  )
  writeLines(sh, paste0(n, '/', n, '.RNAseq.sh'))
  ## qsub
  system(paste0('qsub ', n, '/', n, '.RNAseq.sh'))
})

## 3. check logs
message(Sa('-->', timer(), '3. Run log checking... <--'))
idx  = 0
while(idx < length(samples)) {
  Sys.sleep(60)
  idx = sapply(names(samples), function(n) {
    log = paste0('log/', n, '.log')
    message(Sa('-->', timer(), 'Detect:', Pa(n), Pa(log), '<--'))
    logs = list.files('log', '.log$', full.names = T)
    log  = intersect(log, logs)
    if (!length(log)) {
      message(Er('!!!', timer(), 'Detect:', Pa(n), 'no log file detected !!!'))
      return(0)
    }
    log = system(paste('tail -n 1', log), intern = T)
    if (log == 'This work is done.') {
      message(Sa('-->', timer(), 'Detect:', Pa(n), 'this work is done <--'))
      return(1)
    } else {
      message(Sa('-->', timer(), 'Detect:', Pa(n), 'this work is running... <--'))
      return(0)     
    }
  })
  idx = sum(idx)
  message(Sa('-->', timer(), 'waiting...'))
  Sys.sleep(timeIt*60)
}
message(Sa('-->', timer(), 'all samples done <--'))

## done ##
message(Wa('-->', timer(), 'Done:', me, '<--'))
