Seurat.plot.heatmap = function(obj,min.pct = 0.25,logfc.threshold = 0.25,top.n =20,label.n = 5,col = NULL,cleangene = T){
  DEG <- Seurat::FindAllMarkers(obj,
                                      only.pos = TRUE,
                                      min.pct = min.pct,
                                      logfc.threshold = logfc.threshold)
  
  if (cleangene) {
    DEG = DEG[cleanGene(DEG$gene),]
  }
  
  DEG_top <- DEG %>%
    dplyr::group_by(cluster) %>%
    dplyr::top_n(n = top.n, wt = avg_log2FC)
  
  DEG_label = DEG %>%
    dplyr::group_by(cluster) %>%
    dplyr::top_n(n = label.n, wt = avg_log2FC)
  
  markGenes = DEG_label$gene
  
  ncluster = length(unique(Idents(obj)))
  
  
  st.data <- prepareDataFromscRNA(object = obj,
                                  diffData = DEG_top,
                                  showAverage = TRUE,)
  
  visCluster(object = st.data,
             plot.type = "heatmap",
             column_names_rot = 45,
             show_row_dend = F,
             markGenes = markGenes,
             markGenes.side = "right",
             line.side = "left",
             cluster.order = c(1:ncluster),
             sample.col = col,
             add.bar = F)

}

##example
## Seurat.plot.heatmap(obj = Oligo,cleangene = T,col = c("#712820","#BD956A","#8C549C","#585658"))
