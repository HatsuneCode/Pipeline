## extract samples
extract = function(x, num = 2000) {
  if (num >= 1) {
    if (length(x) > num) {
      set.seed(1)
      sample(x, num)
    } else x
  } else {
    set.seed(1)
    sample(x, length(x)*num)
  }
}