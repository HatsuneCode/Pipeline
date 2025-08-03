suppressWarnings(library(crayon, warn.conflicts = F))
Pa    = crayon::cyan
Er    = crayon::red$bold
Sa    = crayon::blue
No    = crayon::magenta$bold
Wa    = crayon::yellow
timer = function() crayon::yellow( sub('\\..*', '', as.character(Sys.time() )) )
size  = function(x) round(object.size(x) / 1024^3, 1)
checkPath = function(x) normalizePath(x, '/', T)
checkDupRow = function(expr, method = 'mean', round = T) {
  ## process symbols
  gene  = sub('^ENS.*?_', '', rownames(expr))
  dgene = unique(gene[duplicated(gene)])
  ## check duplicated symbols
  if (length(dgene)) {
    expr1 = expr[!gene %in% dgene,]
    expr2 = do.call(rbind, lapply(dgene, function(g) {
      e = expr[gene == g, , drop = F]
      if (method == 'mean')
        t(setNames(data.frame(colMeans(e)), g))
    }))
    expr = rbind(expr1, expr2)
    rm(expr1, expr2)
  }
  rm(gene, dgene)
  ## restore symbol
  rownames(expr) = sub('^ENS.*?_', '', rownames(expr))
  if (round) round(expr) else expr
}

## yml ##
suppressMessages(library(yaml))
suppressMessages(library(rlang))
handlers = list('bool#no'  = function(x) if ( x %in% c('false', 'False', 'FALSE', 'no')  ) FALSE else x,
                'bool#yes' = function(x) if ( x %in% c('true',  'True',  'TRUE',  'yes') ) TRUE  else x )

## args ##
args  = commandArgs()
me    = normalizePath(sub('--file=', '', grep('--file=', args, value = T)), '/')
args  = args[-seq(grep('--args', args))]
message(Wa('-->', timer(), 'Run: ', me, '<--'))
main  = Er(me, '<RNAseq.parameter.yml> (<order>)')
path  = args[1]
if (is.na(path)) { message(main); q('no') }
path  = normalizePath(path, '/', T)
order = args[2]
order = ifelse(is.na(order), 'no', order)
order = order == 'order'

suppressMessages(library(rlang))
suppressMessages(library(rjson))
suppressMessages(library(reshape2))
suppressMessages(library(ggplot2))
suppressMessages(library(patchwork))

color20 = c('#00468B', '#5377A7', '#6C6DA4', '#925E9F', '#759EDD', '#0099B4', '#42C1BB', 
            '#76D1B1', '#0A7C2E', '#B8D24D', '#EDE447', '#FAB158', '#FDAF91', '#FF7777', 
            '#FD0000', '#AD002A', '#AE8691', '#DEB8A1', '#4C4E4E', '#5B4232')

#### 0. read yml samples
message(Sa('-->', timer(), 'check parameters:', Pa(path), '<--'))
parameter = yaml.load_file(path, handlers = handlers)
samples   = names(parameter$samples)
outdir    = as.character(parameter$outdir %||% '.')
pref      = paste0(outdir, '/', samples, '/', samples)
wdir      = paste0(outdir, '.stat')
dir.create(wdir, F); setwd(wdir)
message(Sa('-->', timer(), 'working in dir:', Pa(wdir), '<--'))

#### 1. stat Fastp
message(Sa('-->', timer(), '1. stat Fastp <--'))
fs   = paste0(pref, '.fastp.json')
stat = do.call(rbind, lapply(seq(fs), function(i) {
  f = fs[i]
  n = samples[i]
  message(f)
  json = fromJSON(file = f)
  q30  = paste0(round(json$summary$before_filtering$q30_rate, 4)*100, '%')
  q30f = paste0(round(json$summary$after_filtering$q30_rate,  4)*100, '%')
  Gb   = json$summary$before_filtering$total_bases/1000^3
  Gbf  = json$summary$after_filtering$total_bases /1000^3
  data.frame(Name = n, RawQ30 = q30, FilterQ30 = q30f, RawG = Gb, FilterG = Gbf)
}))
write.table(stat, '1.stat.G.xls', sep = '\t', row.names = F, quote = F)

## set color ##
sampleCol = setNames( colorRampPalette(color20)(nrow(stat)), stat$Name )

## plot G
df = melt(stat, id.vars = c('Name', 'RawQ30', 'FilterQ30'))
if (order) df = df[order(df$value, decreasing = T),]
df$Name = factor(df$Name, unique(df$Name))
p = ggplot(df, aes(Name, value, group = variable)) +
  geom_col(aes(fill = variable), position = position_identity() ) +
  geom_hline(yintercept = min(df$value), linetype = 2, color = 'white') +
  labs(x = NULL, y = 'Total bases (G)', fill = NULL) +
  scale_y_continuous(expand = c(.01, .1)) +
  coord_cartesian(ylim = c(min(df$value), max(df$value))) +
  theme_bw(base_family = 'serif') +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('1.stat.G.png', p, w = 2 + nrow(stat)*.2, h = 4, limitsize = F)
## plot Q30
df = melt(stat, id.vars = c('Name', 'RawG', 'FilterG'))
df = rbind(df[df$variable == 'FilterQ30',], df[df$variable == 'RawQ30',])
df$value = as.numeric(sub('%', '', df$value))
if (order) df = df[order(df$value, decreasing = T),]
df$Name = factor(df$Name, unique(df$Name))
p = ggplot(df, aes(Name, value, group = variable)) +
  geom_col(aes(fill = variable), position = position_identity() ) +
  geom_hline(yintercept = min(df$value), linetype = 2, color = 'white') +
  labs(x = NULL, y = 'Q30 (%)', fill = NULL) +
  scale_y_continuous(expand = c(.01, .1)) +
  coord_cartesian(ylim = c(min(df$value), max(df$value))) +
  theme_bw(base_family = 'serif') +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('1.stat.Q30.png', p, w = 2 + nrow(stat)*.2, h = 4, limitsize = F)

#### 2. stat rRNA
message(Sa('-->', timer(), '2. stat rRNA <--'))
fs = paste0(pref, '.rRNA.log')
stat = do.call(rbind, lapply(seq(fs), function(i) {
  f = fs[i]
  n = samples[i]
  message(f)
  data.frame( Name = n, rRNA = sub(' .*', '', readLines(f)[15]) )
}))
write.table(stat, '2.stat.rRNA.xls', sep = '\t', row.names = F, quote = F)
## plot rRNA
stat$Map = as.numeric(sub('%', '', stat$rRNA))
if (order) stat = stat[order(stat$Map, decreasing = T),]
stat$Name = factor(stat$Name, unique(stat$Name))
p = ggplot(stat, aes(Name, Map)) +
  geom_col(aes(fill = Name), show.legend = F) +
  geom_hline(yintercept = min(stat$Map), linetype = 2, color = 'white') +
  scale_y_continuous(expand = c(.01, .1)) +
  scale_fill_manual(values = sampleCol) +
  labs(x = NULL, y = 'Mapped rRNA (%)', fill = 'Map (%)') +
  theme_bw(base_family = 'serif') +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('2.stat.rRNA.png', p, w = 1.5 + nrow(stat)*.2, h = 4, limitsize = F)

#### 3. stat STAR
message(Sa('-->', timer(), '3. stat STAR <--'))
fs   = paste0(pref, '.Log.final.out')
meta = c('Uniquely mapped reads %', '% of reads mapped to multiple loci', 
         '% of reads unmapped: too many mismatches', '% of reads unmapped: too short')
stat = do.call(rbind, lapply(seq(fs), function(i) {
  f  = fs[i]
  n  = samples[i]
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
if (order) stat = stat[order(stat$Mapped, decreasing = T), ]
stat$Name = factor(stat$Name, unique(stat$Name))
p = ggplot(stat, aes(Name, Mapped)) +
  geom_col(aes(fill = Name), show.legend = F) +
  geom_hline(yintercept = min(stat$Mapped), linetype = 2, color = 'white') +
  labs(x = NULL, y = 'Mapped Uniquely (%)', fill = 'Map (%)') + 
  scale_y_continuous(expand = c(.01, .1)) +
  scale_fill_manual(values = sampleCol) +
  coord_cartesian(ylim = c(min(stat$Mapped), max(stat$Mapped))) +
  theme_bw(base_family = 'serif') +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('3.stat.STAR.png', p, w = 1.5 + nrow(stat)*.2, h = 4, limitsize = F)

#### 4. stat RSEM
message(Sa('-->', timer(), '4. stat RSEM <--'))
## genes
fs = paste0(pref, '.genes.results')
ms = c('expected_count', 'TPM', 'FPKM')
data = lapply(seq(fs), function(i) {
  f = fs[i]
  n = samples[i]
  message('Genes:', n)
  df = read.table(f, sep = '\t', header = T)
  setNames(lapply(ms, function(m) {
    setNames(data.frame(df[[m]], row.names = df$gene_id, check.rows = F), n)
  }), ms)
})
cnts = setNames(lapply(ms, function(m) {
  df = do.call(cbind, lapply(data, function(i) i[[m]] ))
  df = cbind(Gene = rownames(df), df)
  write.table(df, paste0('4.', m, '.xls'), sep = '\t', quote = F, row.names = F)
  df
}), ms)
## isoforms
fs = paste0(pref, '.isoforms.results')
ms = c(ms, 'IsoPct')
data = lapply(seq(fs), function(i) {
  f = fs[i]
  n = samples[i]
  message('Isoforms:', n)
  df = read.table(f, sep = '\t', header = T)
  setNames(lapply(ms, function(m) {
    setNames(data.frame(df[[m]], row.names = df$transcript_id, check.rows = F), n)
  }), ms)
})
cnts.iso = setNames(lapply(ms, function(m) {
  df = do.call(cbind, lapply(data, function(i) i[[m]] ))
  df = cbind(Gene = rownames(df), df)
  write.table(df, paste0('4.', m, '.isoforms.xls'), sep = '\t', quote = F, row.names = F)
  df
}), ms)

#### 5. plot features
message(Sa('-->', timer(), '5. plot feauters <--'))
## plot top50 genes
data = cnts$TPM
rownames(data) = data[,1]
data = checkDupRow(data[, -1, drop = F], round = F)
ncol = floor(sqrt(ncol(data)))
patn = '^[M,m][T,t]-|^R[P,p][L,l,S,s][0-9,P,p]|^H[B,b][B,b,A,a]'
p = wrap_plots(lapply(seq(ncol(data)), function(i) {
  s = sort(setNames(data[, i, drop = T], rownames(data)), T)
  s = setNames(data.frame(log2(s[1:50]+1), check.names = F, check.rows = F), 'TPM')
  s$Gene = factor(rownames(s), rev(rownames(s)))
  labels = setNames(ifelse(grepl(patn, levels(s$Gene)), 'red', 'black'), s$Gene)
  ggplot(s, aes(TPM, Gene)) + 
    geom_col(aes(fill = TPM)) +
    geom_vline(xintercept = min(s$TPM), linetype = 2, color = 'white') +
    scale_fill_gradient2(high = '#FF0000', mid = '#FF6900', low = '#FFBF00', midpoint = mean(s$TPM)) +
    labs(x = 'Log2 TPM', y = NULL, title = paste('Top50:', colnames(data)[i]), fill = expression(log[2](TPM))) +
    coord_cartesian(xlim = c(min(3, min(s$TPM)), max(s$TPM))) +
    scale_x_continuous(expand = c(.01, .1)) +
    theme_bw(base_family = 'serif') +
    theme(text = element_text(size = 12), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(color = labels),
          plot.title = element_text(hjust = .5) )
}), ncol = ncol)
ggsave('5.Top50_Genes.png', p, w = 4*ncol, h = 6*ceiling(ncol(data)/ncol), limitsize = F)
## plot confidence gene numbers
data = cnts$expected_count
rownames(data) = data[,1]
data = checkDupRow(data[, -1, drop = F])
ncol = floor(sqrt(ncol(data)))
p = wrap_plots(lapply(seq(ncol(data)), function(i) {
  s = sort(setNames(data[, i, drop = T], rownames(data)), T)
  s = s[s > 10]
  s = setNames(data.frame(s[s < quantile(s, .99)], check.names = F, check.rows = F), 'Counts')
  p = ggplot(s, aes(Counts)) + 
    geom_histogram(binwidth = 200, boundary = 0, aes(fill = log2(after_stat(count)+1)), show.legend = F) +
    scale_fill_gradientn(colors = c('#FFBF00', '#FF6900', '#FF0000') ) +
    geom_text(aes(label = after_stat(count)), vjust = -.3, stat = 'bin', binwidth = 200, boundary = 0, size = 2, family = 'serif') +
    labs(x = 'Counts', y = 'nGene', title = paste('Gene Counts:', colnames(data)[i]) ) +
    theme_bw(base_family = 'serif') +
    theme(text = element_text(size = 12), plot.title = element_text(hjust = .5) )
}), ncol = ncol)
ggsave('5.GeneCounts.png', p, w = 3.8*ncol, h = 3.5*ceiling(ncol(data)/ncol), limitsize = F)

## done ##
message(Wa('-->', timer(), 'Done:', me, '<--'))

