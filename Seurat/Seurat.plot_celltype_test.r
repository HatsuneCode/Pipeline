## obj = seurat obj
plot_celltype_test = function(obj,idents = "cell",sample_column = "orig.ident"
                              ,sample = c("Young1","Young2","Young3","Old1","Old2","Old3")
                              ,group = c(rep("Young",3),rep("Old",3)),my_comparisons =list(c("Young", "Old"))
                              ,method = "t.test", label = "p.signif",nrow = 4,ncol = 3,col = c("#5488ef","#d6503a")){
  metadata = obj@meta.data
  
  Cellratio <- prop.table(table(metadata[,idents], metadata[,sample_column]), margin = 2) %>% data.frame()
  cellper <- dcast(Cellratio,Var2~Var1, value.var = "Freq")
  rownames(cellper) <- cellper[,1]
  cellper <- cellper[,-1]
  samples <- data.frame(Sample=sample, Group=group)
  
  rownames(samples)=samples[,"Sample"]
  cellper$sample <- samples[rownames(cellper),'Sample']
  cellper$group <- samples[rownames(cellper),'Group']
  

  pplist = list()
  sce_groups = unique(metadata[,idents])
  sce_groups = sce_groups[order(sce_groups)]
  
  for(group_ in sce_groups){
    cellper_  = cellper %>% dplyr::select(one_of(c('sample','group',group_)))
    colnames(cellper_) = c('sample','group','percent')
    cellper_$percent = as.numeric(cellper_$percent)
    cellper_ <- cellper_ %>% group_by(group) %>% mutate(upper =  quantile(percent, 0.75), 
                                                        lower = quantile(percent, 0.25),
                                                        mean = mean(percent),
                                                        median = median(percent))
    print(group_)
    print(cellper_$median)
    
    pp1 = ggplot(cellper_,aes(x=group,y=percent)) + 
      geom_jitter(shape = 21,aes(fill=group),width = 0.25) + 
      scale_fill_manual(values = col)+
      stat_summary(fun=mean, geom="point", color="grey60") +
      theme_cowplot() + 
      theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10),legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),plot.title = element_text(size = 10,face = 'plain'),legend.position = 'none') + 
      labs(title = group_,y='Percentage') +
      geom_errorbar(aes(ymin = lower, ymax = upper),col = "grey60",width =  1)
    
   
    labely = max(cellper_$percent)
    compare_means(percent ~ group,  data = cellper_)
    my_comparisons <- my_comparisons
    pp1 = pp1 + stat_compare_means(comparisons = my_comparisons,size =3,method = method, label = label)
    pplist[[group_]] = pp1
  }
  p = wrap_plots(pplist,nrow = nrow,ncol = ncol)
  p
}
