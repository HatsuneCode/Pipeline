## Need cmd.out equal to getwd() ##
GSEA.prepare = function(expr.p, expr.n, name.p = 'Pos', name.n = 'Neg', 
                        set.max = 1e3, set.min = 15, nperm = 1e3,
                        cmd.gsea = '/mnt/d/Linux/software/GSEA_Linux_4.3.3/gsea-cli.sh',
                        cmd.out  = '/mnt/e/Project/NDD-Database',
                        cmd.gmt  = '/mnt/d/Resources/GMT/hsa/GOandKEGG.v2024.1.Hs.symbols.gmt') {
  wdir = getwd()
  message('--> Prepare GSEA: ', wdir, ' <--')
  ## p vs n
  expr.pn = cbind(expr.p, expr.n)
  expr.pn = expr.pn[rowSums(expr.pn) > 0,]
  expr.pn = cbind('NAME' = rownames(expr.pn), 'DESCRIPTION' = NA, expr.pn)
  ## pref
  dir.create('GSEA/', F)
  s.p     = name.p
  s.n     = name.n
  vs.pn   = paste0(s.p, '_versus_', s.n)
  pref.pn = paste0('GSEA/', vs.pn)
  expFile = paste0(pref.pn, '.exp.txt')
  clsFile = paste0(pref.pn, '.cls.txt')
  ## expr
  message('--> save: ', expFile, ' <--')
  write.table(expr.pn, expFile, sep = '\t', row.names = F, quote = F)
  ## cls
  cls.pn = c(paste(ncol(expr.pn)-2, 2, 1, collapse = ' '),
             paste('#', s.p, s.n, collapse = ' '),
             paste(paste(rep(0, ncol(expr.p)), collapse = ' '),
                   paste(rep(1, ncol(expr.n)), collapse = ' ')) )
  message('--> save: ', clsFile, ' <--')
  writeLines(cls.pn, clsFile)
  ## cmd: GSEA
  cmd1  = paste0(cmd.gsea, ' GSEA -out ', cmd.out, '/GSEA -gmx ', gmt,
                 ' -collapse No_Collapse -mode Max_probe -norm meandiv -nperm ', nperm, ' -permute gene_set',
                 ' -rnd_seed 149 -rnd_type no_balance -scoring_scheme weighted -metric Signal2Noise -sort real',
                 ' -order descending -create_gcts false -create_svgs false -include_only_symbols true -make_sets true',
                 ' -median false -num 100 -plot_top_x 0 -save_rnd_lists false -set_max ', set.max, ' -set_min ', set.min, ' -zip_report true',
                 ' -res ', cmd.out, '/', expFile, ' -cls ', cmd.out, '/', clsFile, '#', vs.pn, ' -rpt_label ', vs.pn)
  cmd2  = paste0(cmd.gsea, ' LeadingEdgeTool -enrichment_zip false -out LE.', vs.pn, ' -dir ', vs.pn, '.Gsea*')
  writeLines(c(cmd1, cmd2), paste0('GSEA/', vs.pn, '.GSEA.sh'))
}
