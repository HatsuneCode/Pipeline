## project ##
project = 'ADPD'
wdir    = '/home/songlianhao/Project/ADPD'

## models ##
mods    = c('5XFAD', 'Tau', 'Gba', 'Syn', 'TDP43', 'WT28M', 'WT9M')
mods.WC = c('WT28MWC', 'WT2MWC')

## slides ##
slides    = unlist(lapply(mods,    function(m) paste0(m, c('.S55', '.S70', '.S83', '.S94')) ))
slides.WC = unlist(lapply(mods.WC, function(m) paste0(m, c('.S61', '.S81', '.S89', '.S94')) ))

#### color ####
modColor = c('5XFAD'    = '#B92166', 
             'Tau'      = '#FAB158', 
             'Gba'      = '#0099B4', 
             'Syn'      = '#0A7C2E',
             'TDP43'    = '#DEB8A1',
             'WT28M'    = '#A92929',
             'WT9M'     = '#00468B',
             'WT2MWC'   = '#759EDD',
             'WT28MWC'  = '#FF7777' )
slidesColor = stats::setNames(modColor[sub('\\..*', '', c(slides, slides.WC))], c(slides, slides.WC))

#### fontfamily ####
ff = gpar(fontfamily = 'serif')
