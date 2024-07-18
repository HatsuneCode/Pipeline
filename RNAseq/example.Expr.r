## 1. read expr
expr = read.table('HiMAP.counts.txt', sep = '\t', header = T, row.names = 1, check.names = F)
expr = expr[rowSums(expr) > 0,]

## 2. stat G
stat = setNames(data.frame(colSums(expr)/(1e+6*3), check.rows = F), 'G')
stat$GB = round(stat$G, 2)
stat = stat[order(stat$G, decreasing = T),]
stat$Sample = factor(rownames(stat), rownames(stat))
p = ggplot(stat, aes(Sample, G)) +
  geom_col(aes(fill = G)) +
  geom_hline(yintercept = min(stat$G), linetype = 2, color = 'white') +
  labs(x = NULL, y = 'Total bases (G)', fill = NULL) +
  scale_y_continuous(expand = c(.01, .1)) +
  scale_fill_gradient2(high = '#FF0000', mid = '#FF6900', low = '#FFBF00', midpoint = mean(stat$G)) +
  theme_bw(base_family = 'serif') +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1));p
ggsave('1.stat.G.png', p, w = 1.5 + nrow(stat)*.2, h = 4)
## Top100
data = expr
ncol = 6
p = wrap_plots(lapply(seq(ncol(data)), function(i) {
  s = sort(setNames(data[, i, drop = T], rownames(data)), T)
  s = setNames(data.frame(log2(s[1:100]+1), check.names = F, check.rows = F), 'TPM')
  s$Gene = factor(rownames(s), rev(rownames(s)))
  labels = setNames(ifelse(grepl('^MT-|^RP[L,S][0-9]', levels(s$Gene)), 'red', 'black'), s$Gene)
  ggplot(s, aes(TPM, Gene)) +
    geom_col(aes(fill = TPM)) +
    geom_vline(xintercept = min(s$TPM), linetype = 2, color = 'white') +
    scale_fill_gradient2(high = '#FF0000', mid = '#FF6900', low = '#FFBF00', midpoint = mean(s$TPM)) +
    labs(x = 'Log2 TPM', y = NULL, title = paste('Top100:', colnames(data)[i]), fill = expression(log[2](TPM))) +
    coord_cartesian(xlim = c(min(3, min(s$TPM)), max(s$TPM))) +
    scale_x_continuous(expand = c(.01, .1)) +
    theme_bw(base_family = 'serif') +
    theme(text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(color = labels),
          plot.title = element_text(hjust = .5) )
}), ncol = ncol)
ggsave('2.Top100_Genes.png', p, w = 4*ncol, h = 12*ceiling(ncol(data)/ncol), limitsize = F)
## gene number
data = expr
ncol = 6
binwidth = 50
p = wrap_plots(lapply(seq(ncol(data)), function(i) {
  s = sort(setNames(data[, i, drop = T], rownames(data)), T)
  s = setNames(data.frame(s[s > 10 & s < quantile(s, .99)], check.names = F, check.rows = F), 'Counts')
  p = ggplot(s, aes(Counts)) +
    geom_histogram(binwidth = binwidth, boundary = 0, aes(fill = log2(after_stat(count)+1)), show.legend = F) +
    scale_fill_gradientn(colors = c('#FFBF00', '#FF6900', '#FF0000') ) +
    geom_text(aes(label = after_stat(count)), vjust = -.3, stat = 'bin', binwidth = binwidth, boundary = 0, size = 2, family = 'serif') +
    labs(x = 'Counts', y = 'nGene', title = paste('Gene Counts:', colnames(data)[i]) ) +
    theme_bw(base_family = 'serif') +
    theme(text = element_text(size = 12), plot.title = element_text(hjust = .5) )
}), ncol = ncol)
ggsave('2.GeneCounts.png', p, w = 3.8*ncol, h = 3.5*ceiling(ncol(data)/ncol), limitsize = F)

## 3. PCA
expr = expr[,colnames(expr) != 'yk2_A_1']
dds  = DESeqDataSetFromMatrix(
  countData = expr,
  colData = data.frame(row.names = colnames(expr), samples = factor(colnames(expr))),
  design = ~ samples)
dds  = estimateSizeFactors(dds)
dds  = counts(dds, normalize = T)
pca  = PCA(log2(dds+1))
pca$group = ''
for (i in seq(pair)) pca$group[pca$sample %in% pair[[i]] ] = names(pair)[i]
pca$type  = ifelse(grepl('Ctrl', pca$group), 'Ctrl', 'Treat')
p = ggplot(pca, aes(PC1, PC2, group = group)) +
  geom_point(aes(color = group, shape = type), size = 4) + geom_text_repel(label = pca$group) +
  labs(title = 'Samples PCA') +
  theme_classic() + 
  theme(plot.title = element_text(hjust = .5), text = element_text(size = 14, family = 'serif'))
p$layers[[2]]$aes_params$family = 'serif';p
ggsave('3.PCA.png', p, w = 8, h = 6)

## 4. DEG
ctrl = pair$`Ctrl DMSO`
DEG = do.call(rbind, lapply(seq(pair[-17]), function(i) {
  n = names(pair)[i]
  message('Pair: ', n)
  DESeq2(expr, pos = pair[[i]], neg = ctrl, name = n)
}))
### filter counts > 1
DEG = DEG[DEG$posMed > 1 & DEG$negMed > 1,]
write.table(DEG, '1.HiMAP.DEG.xls', sep = '\t', row.names = F)

## 5. Volcano
ncol = 4
p = wrap_plots(lapply(names(pair[-17]), function(n) {
  plot_FP(DEG[DEG$type == n,], title = n)
}), ncol = ncol)
ggsave('1.HiMAP.DEG.volcanoFP.png', p, w = 6*ncol, h = 4.5*ceiling(length(p)/ncol) )
p = wrap_plots(lapply(names(pair[-17]), function(n) {
  plot_EF(DEG[DEG$type == n,], title = n) + xlim(c(0, 1e+3))
}), ncol = ncol)
ggsave('1.HiMAP.DEG.volcanoEF.png', p, w = 6*ncol, h = 4.5*ceiling(length(p)/ncol) )

## 6. Enrich ##
## Enrich: GSEA
DEG  = DEG[DEG$p_val < .05,]
DEG  = DEG[abs(DEG$avg_log2FC) > log2(1.5),]
KEGG = do.call(rbind, lapply(setdiff(unique(DEG$type), 'JQ1 5μm'), function(n) {
  deg  = DEG[DEG$type == n, ]
  deg  = sort(setNames(deg$avg_log2FC, deg$gene))
  type = sum(abs(deg) - deg)
  if (length(deg) > 5) {
    message('KEGG: ', n)
    gsea = fGSEA(deg, type = 'KEGG', scoreType = if (type > 0) 'std' else 'pos')
    if (length(gsea)) {
      gsea$type = n
      gsea
    }}
}))
write.table(KEGG, '2.Enrich.KEGG.xls', sep = '\t', row.names = F, quote = F)
#### heatmap
df = KEGG
df$pathway = Title(df$pathway)
p = plot.Enrich.heat(df, adj = F, column_split = sort(sub(' .*', '', unique(df$type))) );p
png('2.Enrich.KEGG.png', 
    w = as.numeric(max_text_width(df$pathway)/25.4) + .3 +length(unique(df$type))*.5, 
    h = as.numeric(max_text_width(df$type)/25.4) + length(unique(df$pathway))*.2, 
    units = 'in', res = 300)
draw(p); dev.off()
## Enrich: GOBP
DEG  = DEG[which(DEG$p_val_adj < .01),]
DEG  = DEG[abs(DEG$avg_log2FC) > 1,]
GOBP = do.call(rbind, lapply(unique(DEG$type), function(n) {
  message(n)
  deg = DEG[DEG$type == n,]
  do.call(rbind, lapply(c('up', 'down'), function(i) {
    df = if (i == 'up') deg[deg$avg_log2FC > 0,] else deg[deg$avg_log2FC < 0,]
    if (nrow(df)) {
      go = EnrichGO(df$gene, og = 'org.Hs.eg.db')
      go = data.frame(go@result)
      if (nrow(go)) {
        go = go[go$pvalue < .05,]
        go = go[order(go$qvalue),]
        f  = gsub('/', '.', paste0(n, '.', i, '.goCirc.png'))
        png(f, w = 6, h = 6, units = 'in', res = 300)
        plot.Enrich.circ(data.frame(go[1:min(20, nrow(go)),])); dev.off()
        go$type  = i
        go$Group = n
      }} }))
}))
write.table(GOBP, '2.Enrich.GOBP.xls', sep = '\t', row.names = F, quote = F)
