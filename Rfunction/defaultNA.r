## Default for NULL and NA value
`%|||%` = function(x, y) 
  if (!length(x)) y else if (all(is.na(x))) y else x