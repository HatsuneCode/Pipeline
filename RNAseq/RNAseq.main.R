## RNAseq Pipeline -- lhsong, hatsunecode@gmail.com
## https://github.com/HatsuneCode/Pipeline/blob/main/RNAseq/RNAseq.main.R
## version v1.0
version = 1

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

main = Er(me, '<RNAseq.parameter.yml>')
file = args[1]
if (is.na(file)) { message(main); q('no') }
file = normalizePath(file, '/', T)

## 0. check parameters ##
message(Sa('-->', timer(), '1. check parameters:', Pa(file), '<--'))
parameter = yaml.load_file(file, handlers = handlers)

# check project
project  = as.character(parameter$project %||% 'RNAseq')
thread   = as.numeric(parameter$thread %||% 1)
pairEnd  = parameter$pairEnd  %||% T
cleanFq  = parameter$cleanFq  %||% T
cleanBam = parameter$cleanBam %||% T
versionP = parameter$version  %||% 0
if (versionP != version) {message(Er('Parameter version', versionP, 'not equal to pipeline version', version, '!')); q('no')}

# check steps
enforce    = parameter$enforce %||% F
steps      = parameter$steps
fastp      = steps$fastp       %||% T
splice     = steps$splicing    %||% T

# check softwre paths
softwares  = lapply(c(parameter$softwares, if (splice) parameter$softwares.splicing), checkPath)

# check para
fastpPara  = parameter$fastp
fastp_LR   = fastpPara$length_required   %||% 15
fastp_Q    = fastpPara$quality_value     %||% 20
fastp_U    = fastpPara$unqualified_limit %||% 10
fastp_W    = fastpPara$worker_thread     %||% 8
bowtiePara = parameter$bowtie2
bowtie_W   = bowtiePara$worker_thread    %||% 8
starPara   = parameter$STAR
star_W     = starPara$worker_thread      %||% 8
rsemPara   = parameter$RSEM
rsem_W     = rsemPara$worker_thread      %||% 8
portPara   = parameter$portcullis
port_W     = portPara$worker_thread      %||% 8
rmatsPara  = parameter$rmats
rmats_W    = rmatsPara$worker_thread     %||% 8

# check reference
references = c(parameter$references, if (splice) parameter$references.splicing)
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

## 2. set constants
fpd = 'Fastp work is done'
btd = 'Bowtie2 work is done'
std = 'STAR work is done'
rsd = 'RSEM work is done'
ptd = 'Portcullis work is done'
rtd = 'RMATS work is done'

## 3. run each samples
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
  rn.fp = !sum(grepl(fpd, log)) | enforce
  rn.bt = !sum(grepl(btd, log)) | enforce 
  rn.st = !sum(grepl(std, log)) | enforce
  rn.rs = !sum(grepl(rsd, log)) | enforce 
  rn.pt = !sum(grepl(ptd, log)) | enforce
  rn.rt = !sum(grepl(rtd, log)) | enforce
  message(Sa('--> run fastp:',    Pa(ifelse(rn.fp & fastp, 'need', 'skip')), '<--'))
  message(Sa('--> run bowtie2:',  Pa(ifelse(rn.bt,  'need', 'skip')), '<--'))
  message(Sa('--> run STAR:',     Pa(ifelse(rn.st,  'need', 'skip')), '<--'))
  message(Sa('--> run RSEM:',     Pa(ifelse(rn.rs,  'need', 'skip')), '<--'))
  message(Sa('--> run Splicing:', Pa(ifelse(splice, 'need', 'skip')), '<--'))
  if (splice) {
    message(Sa('--> run Splicing - portcullis:', Pa(ifelse(rn.pt, 'need', 'skip')), '<--'))
    message(Sa('--> run Splicing - rMATS:',      Pa(ifelse(rn.rt, 'need', 'skip')), '<--'))
  }

  ## make RNAseq.sh
  sh = c(
   err = 'set -e',
   # s0.cd
   cd = paste0('cd ', wdir, '/', n),
   rn = paste0('echo "--> ', n, ' <--"; echo This work is running... > ../log/', n, '.log'),
   '',
   # s1.fastp
   s1 = paste0(if (!rn.fp) '## ', if (fastp) 
     paste0(softwares$fastp, ' -q ', fastp_Q, ' -u ', fastp_U, ' -l ', fastp_LR, ' -w ', fastp_W, ' -i ', samples[[i]][1], 
            if (pairEnd) paste0(' -I ', samples[[i]][2]), ' -o ', n, '.r1.fq.gz', 
            if (pairEnd) paste0(' -O ', n, '.r2.fq.gz --detect_adapter_for_pe'),
            ' -j ', n, '.fastp.json -h ', n, '.fastp.html >> ../log/', n, '.log 2>&1') else 
       paste0('cat ', samples[[i]][1], ' > ', n, '.r1.fq.gz', 
              if (pairEnd) paste0('; cat ', samples[[i]][2], ' > ', n, '.r2.fq.gz'))),
   dn.fastp = paste0('msg="', fpd, '"; echo $msg; echo $msg >> ../log/', n, '.log'),
   '',
   # s2.bowtie2
   s2 = paste0(if (!rn.bt) '## ', softwares$bowtie2, ' -p ', bowtie_W, ' -x ', references$rRNAref, 
               ' --local ', ifelse(pairEnd, '-1', '-U'), ' ', n, '.r1.fq.gz', 
               if (pairEnd) paste0(' -2 ', n, '.r2.fq.gz'), ' --un', 
               if (pairEnd) '-conc', ' ', n, '.filter.fq -S /dev/null > ', n, '.rRNA.log 2>&1', 
               if (cleanFq) '; rm *r*.fq.gz'),
   dn.bt2 = paste0('msg="', btd, '"; echo $msg; echo $msg >> ../log/', n, '.log'),
   '',
   # s3.STAR
   s3 = paste0(if (!rn.st) '## ', softwares$STAR, 
               ' --outReadsUnmapped Fastx --runThreadN ', star_W, ' --genomeDir ', references$STARref, 
               ' --readFilesIn ', n, '.filter', if (pairEnd) '.1', '.fq ', 
               if (pairEnd) paste0(n, '.filter.2.fq '), 
               '--outBAMsortingThreadN ', star_W, ' --outSAMattributes All --outSAMtype BAM SortedByCoordinate --quantMode TranscriptomeSAM GeneCounts --outFileNamePrefix ', 
               n, '. >> ../log/', n, '.log 2>&1; gzip -1 ', n, '.Unmapped.out.mate*', 
               if (cleanFq) '; rm *filter*fq'),
   dn.star = paste0('msg="', std, '"; echo $msg; echo $msg >> ../log/', n, '.log'),
   '',
   # s4.RSEM
   s4 = paste0(if (!rn.rs) '## ', softwares$RSEM, ' -q --alignments', 
               if (pairEnd) ' --paired-end', ' -p ', rsem_W, ' --append-names --no-bam-output ', 
               n, '.Aligned.toTranscriptome.out.bam ', references$RSEM, ' ', n, 
               ' >> ../log/', n, '.log 2>&1'),
   dn.rsem = paste0('msg="', rsd, '"; echo $msg; echo $msg >> ../log/', n, '.log'),
   '',
   # s5.Portcullis
   s5 = paste0(if (!(splice & rn.pt)) '## ', softwares$portcullis, ' full -t ', port_W, ' -b ', 
               references$Genome, ' ', n, '.Aligned.sortedByCoord.out.bam -o ', n, 
               '.port >> ../log/', n, '.log 2>&1'),
   dn.port = paste0('msg="', ptd, '"; echo $msg; echo $msg >> ../log/', n, '.log'),
   '',
   # s6.rMATS-turbo
   s6 = paste0(if (!(splice & rn.rt)) '## ', 'echo ', n, 
               '.port/portcullis.filtered.bam > portBAM.txt; ', 
               softwares$rmats.py, ' ', softwares$rmats.main, ' -t ', 
               if (pairEnd) 'paired' else 'single', ' --gtf ', references$Gtf, 
               ' --b1 portBAM.txt --readLength ', references$ReadLength, 
               ' --nthread ', rmats_W, ' --statoff --od ', n, '.rmats --tmp ', n, 
               '.rmatsTMP >> ../log/', n, '.log 2>&1; mv ', 
               n, '.rmatsTMP/*.rmats ', n, '.rmatsTMP/', n, '.rmats.tmp'),
   dn.rmats = paste0('msg="', rtd, '"; echo $msg; echo $msg >> ../log/', n, '.log'),
   '',
   # s7.cleanBAM
   s7 = paste0(if (!cleanBam) '## ', 'rm *.bam'),
   '',
   dn = paste0('msg="This work is done."; echo $msg; echo $msg >> ../log/', n, '.log')
  )
  writeLines(sh, paste0(n, '/', n, '.RNAseq.sh'))
  ## qsub
  system(paste0('sh ', n, '/', n, '.RNAseq.sh'))
}
## run each
r = if (thread > 1) future_lapply(seq(samples), run) else lapply(seq(samples), run)
message(Sa('-->', timer(), 'all samples done <--'))

## 4. combine files
if (splice) {
  system('mkdir rMATS.TMP; cp */*.rmatsTMP/*.rmats.tmp rMATS.TMP/; cat */portBAM.txt > rMATS.BAM.txt')
  message(Sa('-->', timer(), 'combine files done <--'))
}

## done ##
message(Wa('-->', timer(), 'Done:', me, '<--'))

