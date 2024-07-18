## 1. read expr
expr = read.table('HiMAP/HiMAP.counts.txt', sep = '\t', header = T, row.names = 1, check.names = F)
expr = expr[rowSums(expr) > 0,]
himap = expr
## 2. read bulk ##
expr = read.table('bulk/4.expected_count.xls', sep = '\t', header = T, row.names = 1)
expr = expr[rowSums(expr) > 0,]
expr = checkDupRow(expr)
expr = round(expr)
bulk = expr
## 3. combine ##
genes = intersect(rownames(himap), rownames(bulk))
cnts  = cbind(himap[genes,], bulk[genes,])
## 4. extract samples ##
targ = c(
  ## NAD
  'yk1_A_1', 'yk1_B_1',
  ## RA
  'yk4_A_1', 'yk4_B_1',
  ## GHK
  'yk7_A_1', 'yk7_B_1',
  ## Ctrl
  'NC_1',    'NC_2',
  ## Bulk
  'NAD.1', 'RA.1', 'GHK.1', 'Ctrl.1',  'Ctrl.2')
expr = cnts[, targ]
colnames(expr) = c(
  'NAD A1', 'NAD B1', 'RA A1', 'RA B1', 'GHK A1', 'GHK B1', 'Ctrl H1', 'Ctrl H2',
  'NAD.1', 'RA.1', 'GHK.1', 'Ctrl.1', 'Ctrl.2')
## 5. read DEG ##
himap.deg = read.table('HiMAP/1.HiMAP.DEG.xls', sep = '\t', header = T)
himap.deg = himap.deg[which(himap.deg$p_val_adj < .01),]
himap.deg = himap.deg[abs(himap.deg$avg_log2FC) > 1,]
bulk.deg  = read.table('bulk/1.Bulk.DEG.xls', sep = '\t', header = T)
bulk.deg  = bulk.deg[which(bulk.deg$p_val_adj < .01),]
bulk.deg  = bulk.deg[abs(bulk.deg$avg_log2FC) > 1,]
# NAD 2mM ; RA 2uM
degH = himap.deg[himap.deg$type == 'NAD 2mM',]
degB = bulk.deg[bulk.deg$type == 'NAD vs Ctrl',]
deg.up  = list(cleanGene(degH$gene[degH$avg_log2FC > 0], cleanMT = F), 
               cleanGene(degB$gene[degB$avg_log2FC > 0], cleanMT = F) )
deg.dn  = list(cleanGene(degH$gene[degH$avg_log2FC < 0], cleanMT = F), 
               cleanGene(degB$gene[degB$avg_log2FC < 0], cleanMT = F) )
venn = deg.up
venn.diagram(venn, 'DEGdn.venn.png', disable.logging = T, col = colorRampPalette(color20)(length(venn)), cex = 2,
             category.names = c('HiMAP', 'Bulk') )
## 6. Cor ##
pos = 'Ctrl H2'
neg = 'Ctrl.2'
p   = expr[, pos]
n   = expr[, neg]
com = p != 0 & n != 0
p   = scale(p[com])
n   = scale(n[com])
min = min(p, n)
df  = setNames(data.frame(p - min, n - min, rownames(expr)[com]), c(pos, neg, 'Gene'))
p   = plot.Cor(df, sample1 = pos, sample2 = neg, exp_cut = -Inf, label = F, title = paste(pos, 'vs', neg),
               up.markers = Reduce(union, deg.up)[seq(15)], down.markers = Reduce(union, deg.dn)[seq(15)] ) + 
  xlim(c(0, 2)) + ylim(c(0, 2));p
ggsave(paste0(pos, 'vs', neg, '.png'), p, w = 6, h = 5)
