wh = function(df, row, col) {
  w = as.numeric(max_text_width(df[[row]])/25.4) + length(unique(df[[col]]))*.2 + .5
  h = as.numeric(max_text_width(df[[col]])/25.4) + length(unique(df[[row]]))*.15 + .3
  list(w = w, h = h)
}
