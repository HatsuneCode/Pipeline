findInList = function(x, list) {
  df = do.call(rbind, lapply(seq(list), function(i)
    data.frame(Name = names(list[i]), Value = list[[i]]) ))
  df$Name[match(x, df$Value)]
}
