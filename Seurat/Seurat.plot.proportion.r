##obj=seurat object;cell_column=colname for cell_tye;group_column=colname for group;group_levels=ordering of groups;col=color
plot.proportion = function(obj,cell_column,group_column,group_levels,col){
  meta.data = obj@meta.data[,c(cell_column,group_column)]
  colnames(meta.data) = c("cell","group")
  
  Ratio <- meta.data %>%
    group_by(group, cell) %>% 
    summarise(n=n()) %>%
    mutate(relative_freq = n/sum(n))
  
  Ratio$group =factor(Ratio$group, levels = group_levels)
  
  ggplot(Ratio, aes(x = group, y= relative_freq, fill = cell,
                    stratum=cell, alluvium=cell)) +
    geom_col(width = 0.5, color='black')+
    geom_flow(width=0.5,alpha=0.4, knot.pos=0.5)+
    theme_classic() +
    labs(x='Group',y = 'Proportion')+
    scale_fill_manual(values = col)
  
}

#Example
plot.proportion(obj = OB,cell_column = "cell",group_column = "age",group_levels = c("Young","Old"),col=color20)
