# Chi-squared test
Chisq = function(Total, A, B, AB) {
  nAB  = B - AB
  AnB  = A - AB
  nAnB = Total - A - B + AB 
  dm = matrix(c(AB, nAB, AnB, nAnB), nrow = 2, byrow = T)
  chisq.test(dm)
}