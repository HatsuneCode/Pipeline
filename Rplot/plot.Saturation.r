plot.Saturation = function(expr, reads, sn = 100) {
  suppressMessages(library(ggplot2))
  ## prob
  prob  = c(expr, reads - sum(expr))
  steps = floor(reads/sn)
  sizes = rates = rep(0, sn +1)
  M     = 1e+6
  ## stat
  for (i in 1:sn) {
    size = i*steps
    rmu  = rmultinom(10, size = size, prob = prob)
    mm   = apply(rmu, 1, mean)
    sizes[i+1] = size / M
    rates[i+1] = length(mm[mm > .5])*100 / length(prob)
  }
  res = data.frame(sizes = sizes, rates = rates)
  ## plot 
  ggplot(res, aes(sizes, rates)) +
    geom_line(color = 'red', linewidth = 1) +
    geom_point(size = .1) +
    ylim(c(0, 100)) +
    labs(x = 'Reads (M)', y = 'Gene Number', title = 'Sequence Saturation') +
    theme_bw() +
    theme(text = element_text(family = 'serif', size = 14),
          plot.title = element_text(hjust = .5))
}
