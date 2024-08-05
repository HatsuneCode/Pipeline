GSEA.wh = function(df, row = 'pathway', col = 'group') {
  w = as.numeric(max_text_width(df[[row]])/25.4) + length(unique(df[[col]]))*.2 + .3
  h = as.numeric(max_text_width(df[[col]])/25.4) + length(unique(df[[row]]))*.15
  list(w = w, h = h)
}
