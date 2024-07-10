## Remove unused genes ##
cleanGene = function(x, cleanMT = T, value = T) {
  idx = !(grepl('^Rp[l,s][0-9,p]', x) | grepl('^ENSM', x) | 
            grepl('^Gm[0-9]', x) | grepl('^Rn[0-9]', x) | grepl('Rik', x) | grepl('^Hb.-', x) |
            grepl('^A[A,C,W][0-9]', x))
  if (cleanMT) idx = idx & !grepl('^mt-', x)
  if (value) x[idx] else idx
}
