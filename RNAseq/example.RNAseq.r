## 1. read bulk ##
expr = read.table('4.expected_count.xls', sep = '\t', header = T, row.names = 1)
expr = expr[rowSums(expr) > 0,]
expr = checkDupRow(expr)
expr = round(expr)

## 2. DEG (counts) ##
ctrl = c('Ctrl.1', 'Ctrl.2')
pair = list(
  'GHK vs Ctrl'    = c('GHK.1', 'GHK.2'),
  'KVK vs Ctrl'    = c('KVK.1', 'KVK.2'),
  'NAD vs Ctrl'    = c('NAD.1', 'NAD.2'),
  'RA vs Ctrl'     = c('RA.1',  'RA.2'),
  'VGVAPG vs Ctrl' = c('VGVAPG.1', 'VGVAPG.2')
)
DEG = do.call(rbind, lapply(seq(pair), function(i) {
  n = names(pair)[i]
  message('DEG: ', n)
  DESeq2(expr, pos = pair[[i]], neg = ctrl, name = n)
}))
### filter counts > 10
DEG = DEG[DEG$posMed > 10 & DEG$negMed > 10,]
write.table(DEG, '1.Bulk.DEG.xls', sep = '\t', row.names = F)

## 3. Volcano ##
ncol = 3
p = wrap_plots(lapply(names(pair), function(n) {
  plot_FP(DEG[DEG$type == n,], title = n)
}), ncol = ncol)
ggsave('1.Bulk.DEG.volcanoFP.png', p, w = 6*ncol, h = 4.5*ceiling(length(p)/ncol), limitsize = F)
p = wrap_plots(lapply(names(pair), function(n) {
  plot_EF(DEG[DEG$type == n,], title = n) + xlim(c(0, 1e+3))
}), ncol = ncol)
ggsave('1.Bulk.DEG.volcanoEF.png', p, w = 6*ncol, h = 4.5*ceiling(length(p)/ncol), limitsize = F)

## 4. Enrich ##
## Enrich: GSEA
DEG  = DEG[DEG$p_val < .05,]
DEG  = DEG[abs(DEG$avg_log2FC) > log2(1.5),]
KEGG = do.call(rbind, lapply(unique(DEG$type), function(n) {
  deg  = DEG[DEG$type == n, ]
  deg  = sort(setNames(deg$avg_log2FC, deg$gene))
  type = sum(abs(deg) - deg)
  if (length(deg) > 5) {
    message('KEGG: ', n)
    gsea = fGSEA(deg, type = 'KEGG', scoreType = if (type > 0) 'std' else 'pos')
    if (length(gsea)) {
      gsea$type = n
      gsea
    }
  }
}))
write.table(KEGG, '2.Enrich.KEGG.xls', sep = '\t', row.names = F, quote = F)
#### heatmap
df = KEGG
df$pathway = Title(df$pathway)
p = plot.Enrich.heat(df, adj = F);p
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
        go
      }} }))
}))
write.table(GOBP, '2.Enrich.GOBP.xls', sep = '\t', row.names = F, quote = F)

## 5. PCA ##
dds  = DESeqDataSetFromMatrix(
  countData = expr,
  colData = data.frame(row.names = colnames(expr), samples = factor(colnames(expr))),
  design = ~ samples)
dds  = estimateSizeFactors(dds)
dds  = counts(dds, normalize = T)
pca  = PCA(log2(dds+1))
pca$group = sub('\\..*', '', pca$sample)
pca$type  = ifelse(grepl('Ctrl', pca$sample), 'Ctrl', 'Treat')
p = ggplot(pca, aes(PC1, PC2, group = group)) +
  geom_point(aes(color = group, shape = type), size = 4) + geom_text_repel(label = pca$sample) +
  labs(title = 'Samples PCA') +
  theme_classic() + 
  theme(plot.title = element_text(hjust = .5), text = element_text(size = 14, family = 'serif'))
p$layers[[2]]$aes_params$family = 'serif'
ggsave('3.PCA.png', p, w = 7, h = 6)
