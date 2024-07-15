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

main = Er(me, '<RNAseq_path>')
path = args[1]
if (is.na(path)) { message(main); q('no') }
path = normalizePath(path, '/', T)

suppressMessages(library(rjson))
suppressMessages(library(reshape2))
suppressMessages(library(ggplot2))

#### 1. stat Fastp
message(Sa('-->', timer(), '1. stat Fastp <--'))
fs   = list.files(path, '.fastp.json$', recursive = T, full.names = T)
ns   = sub('.fastp.json$', '', sub('.*/', '', fs))
stat = do.call(rbind, lapply(seq(fs), function(i) {
  f = fs[i]
  n = ns[i]
  message(f)
  json = fromJSON(file = f)
  q30  = paste0(round(json$summary$before_filtering$q30_rate, 4)*100, '%')
  q30f = paste0(round(json$summary$after_filtering$q30_rate,  4)*100, '%')
  Gb   = json$summary$before_filtering$total_bases/1000^3
  Gbf  = json$summary$after_filtering$total_bases /1000^3
  data.frame(Name = n, RawQ30 = q30, FilterQ30 = q30f, RawG = Gb, FilterG = Gbf)
}))
write.table(stat, '1.stat.G.xls', sep = '\t', row.names = F, quote = F)
## plot G
df = melt(stat, id.vars = c('Name', 'RawQ30', 'FilterQ30'))
df = df[order(df$value, decreasing = T),]
df$Name = factor(df$Name, unique(df$Name))
p = ggplot(df, aes(Name, value, group = variable)) +
  geom_col(aes(fill = variable), position = position_identity() ) +
  geom_hline(yintercept = min(df$value), linetype = 2, color = 'white') +
  labs(x = NULL, y = 'Total bases (G)', fill = NULL) +
  scale_y_continuous(expand = c(.01, .1)) +
  theme_bw(base_family = 'serif') +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('1.stat.G.png', p, w = 1 + nrow(stat)*.5, h = 4)
## plot Q30
df = melt(stat, id.vars = c('Name', 'RawG', 'FilterG'))
df$value = as.numeric(sub('%', '', df$value))
df = df[order(df$value, decreasing = T),]
df$Name = factor(df$Name, unique(df$Name))
p = ggplot(df, aes(Name, value, group = variable)) +
  geom_col(aes(fill = variable), position = position_identity() ) +
  geom_hline(yintercept = min(df$value), linetype = 2, color = 'white') +
  labs(x = NULL, y = 'Q30 (%)', fill = NULL) +
  scale_y_continuous(expand = c(.01, .1)) +
  coord_cartesian(ylim = c(min(70, min(df$value)), max(df$value))) +
  theme_bw(base_family = 'serif') +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('1.stat.Q30.png', p, w = 1 + nrow(stat)*.5, h = 4)

#### 2. stat rRNA
message(Sa('-->', timer(), '2. stat rRNA <--'))
fs   = list.files(path, '.rRNA.log$', full.names = T, recursive = T)
ns   = sub('.rRNA.log$', '', sub('.*/', '', fs))
stat = do.call(rbind, lapply(seq(fs), function(i) {
  f = fs[i]
  n = ns[i]
  message(f)
  data.frame( Name = n, rRNA = sub(' .*', '', readLines(f)[15]) )
}))
write.table(stat, '2.stat.rRNA.xls', sep = '\t', row.names = F, quote = F)
## plot rRNA
stat$Map = as.numeric(sub('%', '', stat$rRNA))
stat = stat[order(stat$Map, decreasing = T),]
stat$Name = factor(stat$Name, unique(stat$Name))
p = ggplot(stat, aes(Name, Map)) +
  geom_col(aes(fill = Map)) +
  geom_hline(yintercept = min(stat$Map), linetype = 2, color = 'white') +
  scale_fill_gradient2(high = '#FF0000', mid = '#FF6900', low = '#FFBF00', midpoint = mean(stat$Map)) +
  scale_y_continuous(expand = c(.01, .1)) +
  labs(x = NULL, y = 'Mapped rRNA (%)', fill = 'Map (%)') +
  theme_bw(base_family = 'serif') +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('2.stat.rRNA.png', p, w = 1 + nrow(stat)*.5, h = 4)

#### 3. stat STAR
message(Sa('-->', timer(), '3. stat STAR <--'))
fs   = list.files(path, '.Log.final.out$', full.names = T, recursive = T)
ns   = sub('.Log.final.out$', '', sub('.*/', '', fs))
meta = c('Uniquely mapped reads %', '% of reads mapped to multiple loci', 
         '% of reads unmapped: too many mismatches', '% of reads unmapped: too short')
stat = do.call(rbind, lapply(seq(fs), function(i) {
  f  = fs[i]
  n  = ns[i]
  message('--> ', n, ' ', f, ' <--')
  a  = readLines(f)
  df = data.frame(t(setNames(sapply(meta, function(m) sub('.*\t', '', grep(m, a, value = T)) ), 
                             c('Mapped Uniquely %', 'Mapped Mulitple %', 'Unmapped Too mismatches %', 'Unmapped Too short %'))), check.names = F)
  df$Name = n
  df
}))
write.table(stat, '3.stat.STAR.xls', sep = '\t', row.names = F, quote = F)
## plot STAR
colnames(stat)[1] = 'Mapped'
stat$Mapped = as.numeric(sub('%', '', stat$Mapped))
stat = stat[order(stat$Mapped, decreasing = T), ]
stat$Name = factor(stat$Name, unique(stat$Name))
p = ggplot(stat, aes(Name, Mapped)) +
  geom_col(aes(fill = Mapped)) +
  scale_fill_gradient2(high = '#FF0000', mid = '#FF6900', low = '#FFBF00', midpoint = mean(stat$Mapped)) +
  geom_hline(yintercept = min(stat$Mapped), linetype = 2, color = 'white') +
  labs(x = NULL, y = 'Mapped Uniquely (%)', fill = 'Map (%)') + 
  scale_y_continuous(expand = c(.01, .1)) +
  coord_cartesian(ylim = c(min(70, min(stat$Mapped)), max(stat$Mapped))) +
  theme_bw(base_family = 'serif') +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('3.stat.STAR.png', p, w = 1 + nrow(stat)*.5, h = 4)

#### 4. stat RSEM
message(Sa('-->', timer(), '4. stat RSEM <--'))
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
data = lapply(ms, function(m) {
  df = do.call(cbind, lapply(data, function(i) i[[m]] ))
  df = cbind(Gene = rownames(df), df)
  write.table(df, paste0('4.', m, '.xls'), sep = '\t', quote = F, row.names = F)
})

## done ##
message(Wa('-->', timer(), 'Done:', me, '<--'))
