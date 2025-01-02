cleanGene = function(x, cleanMT = F, cleanSex = T, value = F) {
  idx = !(
    ## clean ribosomal gene
    grepl('^R[P,p][L,l,S,s][0-9,P,p]', x) |
      ## clean unclassified gene
      grepl('^ENS[M,G]', x) |
      grepl('^A[A,C,W][0-9]', x) | 
      grepl('^AI[0-9][0-9][0-9]', x) | 
      grepl('^B[B,C][0-9][0-9][0-9]', x) |
      grepl('^Gm[0-9]', x) |
      ## clean non-coding RNA
      grepl('^R[N,n][0-9]', x) | 
      grepl('Rik', x) | 
      grepl('^LINC[0-9]', x) |
      ## clean pseudogene
      grepl('-ps', x) |
      ## clean Hemoglobin
      grepl('^H[B,b].-', x) |
      ## clean mt-TRNA
      grepl('^mt-T|^MT-T', x) )
  if (cleanMT)  idx = idx & !grepl('^mt-|^MT-', x)
  if (cleanSex) idx = idx & !x %in% 
      c('Xist', 'XIST', 'Ddx3y', 'DDX3Y', 'Eif2s3y', 'EIF2S3Y', 'Uty', 'UTY', 'Kdm5d', 'KDM5D')
  if (value) x[idx] else idx
}
