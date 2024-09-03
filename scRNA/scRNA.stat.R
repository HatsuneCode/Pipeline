#! /home/songlianhao/conda/envs/lhsong/bin/Rscript
suppressWarnings(library(crayon, warn.conflicts = F))
Pa    = crayon::cyan
Er    = crayon::red$bold
Sa    = crayon::blue
No    = crayon::magenta$bold
Wa    = crayon::yellow
timer = function() crayon::yellow( sub('\\..*', '', as.character(Sys.time() )) )
size  = function(x) round(object.size(x) / 1024^3, 1)

## args ##
args  = commandArgs()
me    = normalizePath(sub('--file=', '', grep('--file=', args, value = T)), '/')
args  = args[-seq(grep('--args', args))]
message(Wa('-->', timer(), 'Run: ', me, '<--'))

main  = Er(me, '<scRNA_path> <order>')
path  = args[1]
order = args[2]
if (is.na(path)) { message(main); q('no') }
path  = normalizePath(path, '/', T)
order = ifelse(is.na(order), 'no', order)
order = order == 'order'

suppressMessages(library(rlang))
suppressMessages(library(ggplot2))
suppressMessages(library(ComplexHeatmap))

color20 = c('#00468B', '#5377A7', '#6C6DA4', '#925E9F', '#759EDD', '#0099B4', '#42C1BB', 
            '#76D1B1', '#0A7C2E', '#B8D24D', '#EDE447', '#FAB158', '#FDAF91', '#FF7777', 
            '#FD0000', '#AD002A', '#AE8691', '#DEB8A1', '#4C4E4E', '#5B4232')

remComma = function(x) as.numeric(gsub(',', '', x))
remPct   = function(x) as.numeric(gsub('%', '', x))

## meta ##
dir = list.dirs(path, recursive = F)
ids = sub('.*/', '', dir)
fs  = sapply(dir, function(d) 
  list.files(paste0(d, '/outs/'), '^metrics_summary.csv$', full.names = T) )
meta = do.call(rbind, lapply(seq(fs), function(i) {
  f  = fs[i]
  id = ids[i]
  message(f)
  metr = read.csv(f, check.names = F)
  for (i in c('Estimated Number of Cells', 'Number of Reads', 'Median Genes per Cell', 'Median UMI Counts per Cell', 'Total Genes Detected')) metr[[i]] = remComma(metr[[i]])
  metr$`Sample ID` = id
  metr$`Number of Reads (GB)`      = paste0(metr$`Number of Reads`, ' (', round(metr$`Number of Reads` / 1e+7 *3, 2), ')')
  metr$`Sequencing Saturation (%)` = remPct(metr$`Sequencing Saturation`)
  metr$`Q30 Bases in RNA Read (%)` = remPct(metr$`Q30 Bases in RNA Read`)
  metr$`Q30 Bases in Barcode (%)`  = remPct(metr$`Q30 Bases in Barcode`)
  metr$`Reads Mapped Confidently to Genome (%)` = remPct(metr$`Reads Mapped Confidently to Genome`)
  metr[,c('Sample ID', 'Estimated Number of Cells', 'Number of Reads (GB)', 
          'Median Genes per Cell', 'Median UMI Counts per Cell',
          'Sequencing Saturation (%)',
          'Q30 Bases in RNA Read (%)', 'Q30 Bases in Barcode (%)',
          'Reads Mapped Confidently to Genome (%)',
          'Total Genes Detected')]
}))
write.table(meta, '10x.metrics.xls', sep = '\t', row.names = F, quote = F)

## set color ##
sampleCol = setNames( colorRampPalette(color20)(nrow(meta)), meta$`Sample ID` )

## 1. plot Cells Number
message(Sa('-->', timer(), '1. Number of Cells <--'))
tmp = meta
if (order) tmp = tmp[order(tmp$`Estimated Number of Cells`, decreasing = T),]
tmp$`Sample ID` = factor(tmp$`Sample ID`, tmp$`Sample ID`)
p = ggplot(tmp, aes(`Sample ID`, `Estimated Number of Cells`)) +
  geom_col(aes(fill = `Sample ID`), show.legend = F) +
  geom_text(aes(label = `Estimated Number of Cells`), 
            vjust = 1.5, family = 'serif', size = 3 ) +
  labs(x = NULL, title = 'Cells Number') +
  scale_y_continuous(expand = c(.01, .1)) +
  scale_fill_manual(values = sampleCol) +
  theme_bw() + 
  theme(text = element_text(family = 'serif', size = 14),
        plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 45, hjust = 1) )
ggsave('1.nCells.png', p, w = 1 + nrow(tmp)*.5, h = 4 + as.numeric(max_text_width(tmp$`Sample ID`)/25.4), limitsize = F)

## 2. plot Reads (GB)
message(Sa('-->', timer(), '2. Reads GB <--'))
tmp = meta
tmp$GB = as.numeric(sub(')$', '', sub('.*\\(', '', tmp$`Number of Reads (GB)`)))
if (order) tmp = tmp[order(tmp$GB, decreasing = T),]
tmp$`Sample ID` = factor(tmp$`Sample ID`, tmp$`Sample ID`)
p = ggplot(tmp, aes(`Sample ID`, GB)) +
  geom_col(aes(fill = `Sample ID`), show.legend = F) +
  geom_text(aes(label = paste(GB, 'G')), 
            vjust = 1.5, family = 'serif', size = 3 ) +
  labs(x = NULL, y = 'Number of Reads (GB)', title = 'Sequencing Depth') +
  scale_y_continuous(expand = c(.01, .1)) +
  scale_fill_manual(values = sampleCol) +
  theme_bw() + 
  theme(text = element_text(family = 'serif', size = 14),
        plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 45, hjust = 1) )
ggsave('2.G.png', p, w = 1 + nrow(tmp)*.5, h = 4 + as.numeric(max_text_width(tmp$`Sample ID`)/25.4), limitsize = F)

## 3. Median Genes ##
message(Sa('-->', timer(), '3. Meidan Genes <--'))
tmp = meta
if (order) tmp = tmp[order(tmp$`Median Genes per Cell`, decreasing = T),]
tmp$`Sample ID` = factor(tmp$`Sample ID`, tmp$`Sample ID`)
p = ggplot(tmp, aes(`Sample ID`, `Median Genes per Cell`)) +
  geom_col(aes(fill = `Sample ID`), show.legend = F) +
  geom_text(aes(label = `Median Genes per Cell`), 
            vjust = 1.5, family = 'serif', size = 3 ) +
  labs(x = NULL, title = 'Median Genes') +
  scale_y_continuous(expand = c(.01, .1)) +
  scale_fill_manual(values = sampleCol) +
  theme_bw() + 
  theme(text = element_text(family = 'serif', size = 14),
        plot.title = element_text(hjust = .5), 
        axis.text.x = element_text(angle = 45, hjust = 1) )
ggsave('3.nGene.png', p, w = 1 + nrow(tmp)*.5, h = 4 + as.numeric(max_text_width(tmp$`Sample ID`)/25.4), limitsize = F)

## 4. Median UMIs ##
message(Sa('-->', timer(), '4. Meidan UMIs <--'))
tmp = meta
if (order) tmp = tmp[order(tmp$`Median UMI Counts per Cell`, decreasing = T),]
tmp$`Sample ID` = factor(tmp$`Sample ID`, tmp$`Sample ID`)
p = ggplot(tmp, aes(`Sample ID`, `Median UMI Counts per Cell`)) +
  geom_col(aes(fill = `Sample ID`), show.legend = F) +
  geom_text(aes(label = `Median UMI Counts per Cell`),
            vjust = 1.5, family = 'serif', size = 3 ) +
  labs(x = NULL, title = 'Median UMIs') +
  scale_y_continuous(expand = c(.01, .1)) +
  scale_fill_manual(values = sampleCol) +
  theme_bw() + 
  theme(text = element_text(family = 'serif', size = 14),
        plot.title = element_text(hjust = .5), 
        axis.text.x = element_text(angle = 45, hjust = 1) )
ggsave('4.nUMI.png', p, w = 1 + nrow(tmp)*.5, h = 4 + as.numeric(max_text_width(tmp$`Sample ID`)/25.4), limitsize = F)

## 5. Sequencing Saturation ##
message(Sa('-->', timer(), '5. Sequencing Saturation <--'))
tmp = meta
if (order) tmp = tmp[order(tmp$`Sequencing Saturation (%)`, decreasing = T),]
tmp$`Sample ID` = factor(tmp$`Sample ID`, tmp$`Sample ID`)
p = ggplot(tmp, aes(`Sample ID`, `Sequencing Saturation (%)`)) +
  geom_col(aes(fill = `Sample ID`), show.legend = F) +
  geom_text(aes(label = `Sequencing Saturation (%)`), 
            vjust = 1.5, family = 'serif', size = 3 ) +
  labs(x = NULL, title = 'Sequencing Saturation') +
  scale_y_continuous(expand = c(.01, .1)) +
  scale_fill_manual(values = sampleCol) +
  theme_bw() + 
  theme(text = element_text(family = 'serif', size = 14),
        plot.title = element_text(hjust = .5), 
        axis.text.x = element_text(angle = 45, hjust = 1) )
 ggsave('5.Saturation.png', p, w = 1 + nrow(tmp)*.5, h = 4 + as.numeric(max_text_width(tmp$`Sample ID`)/25.4), limitsize = F)

## 6. Q30 RNA Reads ##
message(Sa('-->', timer(), '6. Q30 Bases in RNA Read <--'))
tmp = meta
if (order) tmp = tmp[order(tmp$`Q30 Bases in RNA Read (%)`, decreasing = T),]
p = ggplot(tmp, aes(`Sample ID`, `Q30 Bases in RNA Read (%)`)) +
  geom_col(aes(fill = `Sample ID`), show.legend = F) +
  geom_text(aes(label = `Q30 Bases in RNA Read (%)`), 
            vjust = 1.5, family = 'serif', size = 3 ) +
  labs(x = NULL, title = 'Q30 in RNA Reads') +
  scale_y_continuous(expand = c(.01, .1)) +
  scale_fill_manual(values = sampleCol) +
  theme_bw() + 
  theme(text = element_text(family = 'serif', size = 14),
        plot.title = element_text(hjust = .5), 
        axis.text.x = element_text(angle = 45, hjust = 1) )
ggsave('6.Q30_RNA.png', p, w = 1 + nrow(tmp)*.5, h = 4 + as.numeric(max_text_width(tmp$`Sample ID`)/25.4), limitsize = F)

## 7. Q30 Barcode ##
message(Sa('-->', timer(), '7. Q30 Bases in Barcode <--'))
tmp = meta
if (order) tmp = tmp[order(tmp$`Q30 Bases in Barcode (%)`, decreasing = T),]
p = ggplot(tmp, aes(`Sample ID`, `Q30 Bases in Barcode (%)`)) +
  geom_col(aes(fill = `Sample ID`), show.legend = F) +
  geom_text(aes(label = `Q30 Bases in Barcode (%)`), 
            vjust = 1.5, family = 'serif', size = 3 ) +
  labs(x = NULL, title = 'Q30 in Barcode') +
  scale_y_continuous(expand = c(.01, .1)) +
  scale_fill_manual(values = sampleCol) +
  theme_bw() + 
  theme(text = element_text(family = 'serif', size = 14),
        plot.title = element_text(hjust = .5), 
        axis.text.x = element_text(angle = 45, hjust = 1) )
ggsave('7.Q30_Barcode.png', p, w = 1 + nrow(tmp)*.5, h = 4 + as.numeric(max_text_width(tmp$`Sample ID`)/25.4), limitsize = F)

## 8. STAR Map ##
message(Sa('-->', timer(), '8. Genome mapping <--'))
tmp = meta
if (order) tmp = tmp[order(tmp$`Reads Mapped Confidently to Genome (%)`, decreasing = T),]
p = ggplot(tmp, aes(`Sample ID`, `Reads Mapped Confidently to Genome (%)`)) +
  geom_col(aes(fill = `Sample ID`), show.legend = F) +
  geom_text(aes(label = `Reads Mapped Confidently to Genome (%)`),
            vjust = 1.5, family = 'serif', size = 3 ) +
  labs(x = NULL, title = 'Genome Mapping') +
  scale_y_continuous(expand = c(.01, .1)) +
  scale_fill_manual(values = sampleCol) +
  theme_bw() +
  theme(text = element_text(family = 'serif', size = 14),
        plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 45, hjust = 1) )
ggsave('8.GenomeMap.png', p, w = 1 + nrow(tmp)*.5, h = 4 + as.numeric(max_text_width(tmp$`Sample ID`)/25.4), limitsize = F)

## 9. Gene detect ##
message(Sa('-->', timer(), '9. Gene detect <--'))
tmp = meta
if (order) tmp = tmp[order(tmp$`Total Genes Detected`, decreasing = T),]
tmp$`Sample ID` = factor(tmp$`Sample ID`, tmp$`Sample ID`)
p = ggplot(tmp, aes(`Sample ID`, `Total Genes Detected`)) +
  geom_col(aes(fill = `Sample ID`), show.legend = F) +
  geom_text(aes(label = paste0(round(`Total Genes Detected`/1e+3,2), 'k') ),
            vjust = 1.5, family = 'serif', size = 3 ) +
  labs(x = NULL, title = 'Detected Gene Number') +
  scale_y_continuous(expand = c(.01, .1)) +
  scale_fill_manual(values = sampleCol) +
  theme_bw() +
  theme(text = element_text(family = 'serif', size = 14),
        plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 45, hjust = 1) )
ggsave('9.GeneDetect.png', p, w = 1 + nrow(tmp)*.5, h = 4 + as.numeric(max_text_width(tmp$`Sample ID`)/25.4), limitsize = F)

## done ##
message(Wa('-->', timer(), 'Done:', me, '<--'))
