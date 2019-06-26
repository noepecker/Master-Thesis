wind_time2 <- function(data, divisions = 10, time_intervals = 5, weight = c(0.2, 0.8)){
  
  source("wind_est.R")
  source("wind_pred2.R")
  
  df <- data
  div <- divisions + 2
  
  # SPACE DIVISION
  
  #Include an extra because the function does not predict on the edges
  #With this extra we do not waste observations
  
  x_range <- max(df$Long) - min(df$Long)
  x_extra <- x_range/div
  x_min <- floor(min(df$Long)) - x_extra 
  x_max <- ceiling(max(df$Long)) + x_extra
  y_range <- max(df$Lat) - min(df$Lat)
  y_extra <- y_range/div
  y_min <- floor(min(df$Lat)) - y_extra
  y_max <- ceiling(max(df$Lat)) + y_extra
  z_range <- max(df$Alt) - min(df$Alt)
  z_extra <- z_range/div
  z_min <- floor(min(df$Alt)) - z_extra
  z_max <- ceiling(max(df$Alt)) + z_extra
  t_min <- floor(min(df$dtm))
  t_max <- ceiling(max(df$dtm))
  
  #Dimensions
  
  x_dim <- c(x_min, x_max)
  y_dim <- c(y_min, y_max)
  z_dim <- c(z_min, z_max)
  t_dim <- c(t_min, t_max)
  dim_mat <- data.frame(x_dim, y_dim, z_dim, t_dim)
  
  nrow <- div 
  ncol <- div 
  nflo <- div 
  
  #Cells
  
  ni <- seq(x_min, x_max, length.out = nrow )
  nj <- seq(y_min, y_max, length.out = ncol)
  nk <- seq(z_min, z_max, length.out = nflo)
  
  # Time Division
  
  t_min <- floor(min(df$dtm))
  t_max <- ceiling(max(df$dtm))
  
  t_div <- time_intervals + 1
  tt <- seq(t_min, t_max, length.out = t_div)
  
  # Init 
  old_wd <- array(0, dim = c(nrow-1, ncol-1, nflo-1))
  old_wd_var <- array(0, dim = c(nrow-1, ncol-1, nflo-1))
  old_ws <- array(0, dim = c(nrow-1, ncol-1, nflo-1))
  old_ws_var <- array(0, dim = c(nrow-1, ncol-1, nflo-1))
  
  for(t in 1:(t_div-1)){
    df_t <- df %>% filter(tt[t] < dtm &  dtm < tt[t+1]) #Select data for time t
    w_est <- wind_est(df_t,div) #Obtain rasults
    w_pred <- wind_pred2(w_est) #Predict for all the space with no data
    
    #Old values
    
    #Weighted mean with the prevoius time 
    
    for(k in 1:(nflo-1)){
      for(i in 1:(nrow-1)){
        for(j in 1:(ncol-1)){
          
          if(old_wd[i,j,k] != 0){
            old_wd[i,j,k] <- weighted.mean.circular(c(old_wd[i,j,k], (w_pred$wind_dir)[i,j,k]), weight)
          }else{old_wd[i,j,k] <- (w_pred$wind_dir)[i,j,k]}
          
          m <- length(old_wd_var[i,j,k][old_wd_var[i,j,k] != 0])
          if(m > 0){
            old_wd_var[i,j,k] <- weighted.mean.circular(c(old_wd_var[i,j,k], (w_pred$wind_dir_var)[i,j,k]), weight)
          }else{old_wd_var[i,j,k] <- (w_pred$wind_dir_var)[i,j,k]}
          
          m <- length(old_ws[i,j,k][old_ws[i,j,k] != 0])
          if(m > 0){
            old_ws[i,j,k] <- 0.1*old_ws[i,j,k] + 0.9*(w_pred$wind_spd)[i,j,k]
          }else{old_ws[i,j,k] <- (w_pred$wind_spd)[i,j,k]}
          
          m <- length(old_ws_var[i,j,k][old_ws_var[i,j,k] != 0])
          if(m > 0){
            old_ws_var[i,j,k] <- 0.1*old_ws[i,j,k] + 0.9*(w_pred$wind_spd_var)[i,j,k]
          }else{old_ws_var[i,j,k] <- (w_pred$wind_spd_var)[i,j,k]}
          
        }
      }
    }
  }
  
  return(list(wind_spd = old_ws, wind_spd_var = old_ws_var, wind_dir = old_wd, wind_dir_var = old_wd_var, divisions = div, dimensions = dim_mat))
  
  
}

