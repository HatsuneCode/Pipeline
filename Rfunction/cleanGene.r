cleanGene = function(x, cleanMT = T, cleanSex = T, value = T) {
  idx = !(
    ## clean ribosomal gene
    grepl('^R[P,p][L,l,S,s][0-9,P,p]', x) |
      ## clean unclassified gene
      grepl('^ENS[M,G]', x) |
      grepl('^A[A-Z][0-9][0-9]', x) | 
      grepl('^B[B,C][0-9][0-9][0-9]', x) |
      grepl('^Gm[0-9]', x) |
      ## clean DNA segment
      grepl('^D[0-9][0-9,A-Z]', x) |
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
  ## clean mitochondrial gene
  if (cleanMT)  idx = idx & !grepl('^mt-|^MT-', x)
  ## clean sex related gene
  if (cleanSex) idx = idx & !x %in% 
      c('Xist', 'XIST', 'Ddx3y', 'DDX3Y', 'Eif2s3y', 'EIF2S3Y', 'Uty', 'UTY', 'Kdm5d', 'KDM5D')
  if (value) x[idx] else idx
}
