## Remove unused genes ##
cleanGene = function(x, cleanMT = T, value = T) {
  idx = !(grepl('^R[P,p][L,l,S,s][0-9,P,p]', x) | grepl('^ENS[M,G]', x) | 
            grepl('^Gm[0-9]', x) | grepl('^R[N,n][0-9]', x) | grepl('Rik', x) | grepl('^H[B,b].-', x) |
            grepl('^A[A,C,W][0-9]', x) | grepl('^mt-T|^MT-T', x) | grepl('^LINC[0-9]', x) )
  if (cleanMT) idx = idx & !grepl('^mt-|^MT-', x)
  if (value) x[idx] else idx
}
