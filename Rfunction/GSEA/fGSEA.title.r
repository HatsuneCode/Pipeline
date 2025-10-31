## process enrichment pathway title
fGSEA.title = function(x) {
  suppressMessages(library(stringr))
  subT = list(
    'Abc '   = 'ABC ',
    'Atp '   = 'ATP ',
    'Dna '   = 'DNA ',
    'Rna '   = 'RNA ',
    'Mrna '  = 'mRNA ',
    ' Tca '  = ' TCA ',
    'Ecm '   = 'ECM ',
    ' Hcm'   = ' HCM',
    'Nad '   = 'NAD ',
    'Gnrh '  = 'GNRH ',
    'Jak '   = 'JAK ',
    'Ppar '  = 'PPAR ', 
    'Vegf '  = 'VEGF ',
    'Tgf '   = 'TGF ',
    'Wnt '   = 'WNT ',
    'Erbb '  = 'ERBB ',
    'Mapk '  = 'MAPK ',
    ' Ii '   = ' II ', 
    ' And '  = ' and ',
    ' Into ' = ' into ',
    ' From ' = ' from ',
    ' To '   = ' to ',
    ' Of '   = ' of ',
    ' Nadh ' = ' NADH ',
    ' Atp '  = ' ATP '
  )
  x = gsub('_', ' ', sub('^REACTOME_', '', sub('^WP_', '', sub('^KEGG_', '', sub('^GOBP_', '', sub('^GOCC_', '', sub('^GOMF_', '', x)))))))
  x = str_to_title(x)
  for (i in seq(subT)) x = gsub(names(subT)[i], subT[[i]], x)
  x
}
