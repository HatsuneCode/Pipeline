## Mitochondrial Complex Genes ##
## from mitocarta3.0
mitoCPX = list(
  Complex1 = unlist(strsplit(c(
    'Acad9, Aifm1, Dmac1, Dmac2, Ecsit, Foxred1, Lyrm2', 
    'mt-Nd1, mt-Nd2, mt-Nd3, mt-Nd4, mt-Nd4l, mt-Nd5, mt-Nd6', 
    'Ndufa1, Ndufa10, Ndufa11, Ndufa12, Ndufa13, Ndufa2, Ndufa3, Ndufa4, Ndufa5, Ndufa6, Ndufa7, Ndufa8, Ndufa9', 
    'Ndufab1, Ndufaf1, Ndufaf2, Ndufaf3, Ndufaf4, Ndufaf5, Ndufaf6, Ndufaf7, Ndufaf8', 
    'Ndufb1, Ndufb2, Ndufb3, Ndufb4, Ndufb5, Ndufb6, Ndufb7, Ndufb8, Ndufb9, Ndufb10, Ndufb11', 
    'Ndufc1, Ndufc2, Ndufs1, Ndufs2, Ndufs3, Ndufs4, Ndufs5, Ndufs6, Ndufs7, Ndufs8, Ndufv1, Ndufv2, Ndufv3', 
    'Nubpl, Timmdc1, Tmem126a, Tmem126b, Tmem186, Tmem70'
  ), ', ')),
  Complex2 = unlist(strsplit(c(
    'Sdha, Sdhaf1, Sdhaf2, Sdhaf3, Sdhaf4, Sdhb, Sdhc, Sdhd'
  ), ', ')),
  Complex3 = unlist(strsplit(c(
    'Bcs1l, Cyc1, Lyrm7, mt-Cytb, Ttc19, Uqcc1, Uqcc2, Uqcc3, Uqcr10, Uqcr11, Uqcrb, Uqcrc1, Uqcrc2, Uqcrfs1, Uqcrh, Uqcrq'
  ), ', ')),
  Complex4 = unlist(strsplit(c(
    'Cep89, Cmc1, Cmc2, Coa3, Coa4, Coa5, Coa6, Coa7, Coa8', 
    'Cox10, Cox11, Cox14, Cox15, Cox16, Cox17, Cox18, Cox19, Cox20, Cox4i1, Cox4i2, Cox5a, Cox5b', 
    'Cox6a1, Cox6a2, Cox6b1, Cox6b2, Cox6c, Cox7a1, Cox7a2, Cox7a2l, Cox7b, Cox7c, Cox8a, Cox8b, Cox8c', 
    'Higd1a, mt-Co1, mt-Co2, mt-Co3, Pet100, Pet117, Pnkd, Sco1, Sco2, Smim20, Surf1, Taco1, Timm21, Tmem177'
  ), ', ')),
  Complex5 = unlist(strsplit(c(
    'Atp5a1, Atp5b, Atp5c1, Atp5d, Atp5e, Atp5g1, Atp5g2, Atp5g3, Atp5h, Atp5j, Atp5j2, Atp5k, Atp5l, Atp5md, Atp5mpl, Atp5o, Atp5pb', 
    'Atpaf1, Atpaf2, Atpif1, Atpsckmt, Dmac2l, Fmc1, mt-Atp6, mt-Atp8, Tmem70'
  ), ', '))
)

## load msigdbr ##
kegg  = msigdbr::msigdbr('Mus musculus', 'C2', 'KEGG')
keggn = stats::setNames(lapply(unique(kegg$gs_name), function(i)
  unique(as.character(kegg$gene_symbol)[kegg$gs_name == i])), unique(kegg$gs_name))
bp   = msigdbr::msigdbr('Mus musculus', 'C5', 'BP')
bpn  = stats::setNames(lapply(unique(bp$gs_name), function(i)
  unique(as.character(bp$gene_symbol)[bp$gs_name == i] )), unique(bp$gs_name))
cc   = msigdbr::msigdbr('Mus musculus', 'C5', 'CC')
ccn  = stats::setNames(lapply(unique(cc$gs_name), function(i)
  unique(as.character(cc$gene_symbol)[cc$gs_name == i] )), unique(cc$gs_name))
mf   = msigdbr::msigdbr('Mus musculus', 'C5', 'MF')
mfn  = stats::setNames(lapply(unique(mf$gs_name), function(i)
  unique(as.character(mf$gene_symbol)[mf$gs_name == i] )), unique(mf$gs_name))
rm(kegg, bp, cc, mf)
                       
## Complement Related Genes ##
ComplementRG = sort(unique(c(keggn$KEGG_COMPLEMENT_AND_COAGULATION_CASCADES, as.character(unlist(bpn[grep('complement', names(bpn), ignore.case = T)])))))
## Lysosome Related Genes ##
LysosomeRG = sort(unique(c(keggn$KEGG_LYSOSOME, as.character(unlist(bpn[grep('lysosome', names(bpn), ignore.case = T)])))))

## Disease associate microglia (DAM) genes ##
DAM = c('Ank', 'Apoe', 'Axl', 'B2m', 'Cadm1', 'Ccl2', 'Ccl6', 'Cd52', 'Cd63', 'Cd68', 'Cd9', 'Clec7a', 'Csf1', 'Cst7', 'Cstb', 'Ctsa', 'Ctsb', 'Ctsd', 'Ctsl', 'Ctsz', 'Cx3cr1', 'Gusb', 'H2-D1', 'Hif1a', 'Itgax',
        'Lilrb4', 'Lpl', 'Lyz2', 'P2ry12', 'Serinc3', 'Serpine2', 'Spp1', 'Tgfbr1', 'Timp2', 'Tmem119', 'Trem2', 'Txnip', 'Tyrobp')

                       
