plot.Density.3d = function(df , fc = 1 , adj = T){
  suppressMessages(library(KernSmooth))
  selected_col <- if (adj) "p_val_adj" else "p_val"
  dat <- df[, c("avg_log2FC", selected_col)]
  
  dat$log_p <- -log10(dat[,selected_col])
  dat2 <- dat[dat$avg_log2FC > fc,]
  dat2 <- dat2[dat2$p_val_adj != 0,]
  dat2 <- na.omit(dat2)
  
  fc_sd <- sd(dat2$avg_log2FC)
  log_p_sd <- sd(dat2$log_p)
  n_points <- nrow(dat2)
  grid_size <- max(100, sqrt(n_points))
  density_est <- bkde2D(cbind(dat2$avg_log2FC, dat2$log_p), 
                        bandwidth = c(fc_sd * 0.1, log_p_sd * 0.1), 
                        gridsize = c(grid_size, grid_size))
  x_vals <- density_est$x1
  y_vals <- density_est$x2
  z_vals <- matrix(density_est$fhat, nrow = length(x_vals), byrow = TRUE)
  
  p <- persp(x_vals, y_vals, z_vals, 
        xlab = "avg_log2FC", 
        ylab =  paste0("-log10 ",selected_col), 
        zlab = "Density", 
        col = "green", 
        theta = 30, 
        phi = 30, 
        shade = 0.5, 
        ticktype = "detailed")
  invisible(p)
  }
