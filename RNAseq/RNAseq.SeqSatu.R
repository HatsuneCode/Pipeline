#! /home/songlianhao/conda/envs/lhsong/bin/Rscript
suppressWarnings(library(crayon, warn.conflicts = F))
Pa    = crayon::cyan
Er    = crayon::red$bold
Sa    = crayon::blue
No    = crayon::magenta$bold
Wa    = crayon::yellow
timer = function() crayon::yellow( sub('\\..*', '', as.character(Sys.time() )) )
size  = function(x) round(object.size(x) / 1024^3, 1)

plot.Saturation = function(expr, reads, sn = 100) {
  suppressMessages(library(ggplot2))
  ## prob
  prob  = c(expr, reads - sum(expr))
  steps = floor(reads/sn)
  sizes = rates = rep(0, sn +1)
  M     = 1e+6
  ## stat
  for (i in 1:sn) {
    size = i*steps
    rmu  = rmultinom(10, size = size, prob = prob)
    mm   = apply(rmu, 1, mean)
    sizes[i+1] = size / M
    rates[i+1] = length(mm[mm > .5])*100 / length(prob)
  }
  res = data.frame(sizes = sizes, rates = rates)
  ## plot 
  ggplot(res, aes(sizes, rates)) + 
    geom_line(color = 'red', linewidth = 1) + 
    geom_point(size = .1) + 
    ylim(c(0, 100)) +
    labs(x = 'Reads (M)', y = 'Gene Number', title = 'Sequence Saturation') +
    theme_bw() + 
    theme(text = element_text(family = 'serif', size = 14), 
          plot.title = element_text(hjust = .5))
}

## args ##
args  = commandArgs()
me    = normalizePath(sub('--file=', '', grep('--file=', args, value = T)), '/')
args  = args[-seq(grep('--args', args))]
message(Wa('-->', timer(), 'Run: ', me, '<--'))

main = Er(me, '<RNAseq_name>')
name = args[1]
if (is.na(name)) { message(main); q('no') }

#### 1. total reads
f = paste0(name, '.Log.final.out')
message(Sa('-->', timer(), '1. read total reads from', Pa(f), '<--'))
reads = unlist(strsplit(grep('Number of input reads', readLines(f), value = T), '\t'))[2]
reads = as.numeric(reads)

#### 2. read counts
f = paste0(name, '.genes.results')
message(Sa('-->', timer(), '2. read counts from', Pa(f), '<--'))
expr = read.table(f, sep = '\t', header = T)[, 'expected_count']
expr = round(expr)
p    = plot.Saturation(expr, reads)
ggsave(paste0(name, '.saturation.png'), p, w = 6, h = 5)

## done ##
message(Wa('-->', timer(), 'Done:', me, '<--'))
