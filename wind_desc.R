wind_desc <- function(data, divisions = 10){
  
  #Data must have columns with Latitude (Lat), Longitude (Long)
  #Altitude (Alt), time (dtm), Wind Speed (WindSpeed) and Wind Direction (WindDir)
  
  #Load Data

  df <- data
  
  x_range <- max(df$Long) - min(df$Long)
  x_extra <- x_range/divisions
  x_min <- floor(min(df$Long)) - x_extra 
  x_max <- ceiling(max(df$Long)) + x_extra
  y_range <- max(df$Lat) - min(df$Lat)
  y_extra <- y_range/divisions
  y_min <- floor(min(df$Lat)) - y_extra
  y_max <- ceiling(max(df$Lat)) + y_extra
  z_range <- max(df$Alt) - min(df$Alt)
  z_extra <- z_range/divisions
  z_min <- floor(min(df$Alt)) - z_extra
  z_max <- ceiling(max(df$Alt)) + z_extra
  t_min <- floor(min(df$dtm))
  t_max <- ceiling(max(df$dtm))
  
  x_dim <- c(x_min, x_max)
  y_dim <- c(y_min, y_max)
  z_dim <- c(z_min, z_max)
  t_dim <- c(t_min, t_max)
  dim_mat <- data.frame(x_dim, y_dim, z_dim, t_dim)
  
  df <- df %>% filter(WindSpeed <= 100)
  
  #DIVISION
  
  div <- divisions 
  nrow <- div
  ncol <- div
  nflo <- div
  
  #Cells
  
  ni <- seq(x_min, x_max, length.out = nrow )
  nj <- seq(y_min, y_max, length.out = ncol)
  nk <- seq(z_min, z_max, length.out = nflo)
  t_f <- length(df[,1])
  
  
  #Average Speed taking into account sin and cos
  
  # init
  cnt_s <- array(0, dim = c(nrow-1, ncol-1, nflo-1))
  w_s_x <- array(0, dim = c(nrow-1, ncol-1, nflo-1))
  w_s_y <- array(0, dim = c(nrow-1, ncol-1, nflo-1))
  
  for(t in 1:t_f){
    r <- 0 # init para dejar de buscar una posición para este dato
    for(k in (nflo-1):1){#La mayoría de los datos están para grandes alturas, por eso es mejor empezar por arriba
      for(i in 1:(nrow-1)){
        for(j in 1:(ncol-1)){
          if(r == 0){
            if((nk[k] <= df$Alt[t]) & (df$Alt[t] <= nk[k+1])){
              if((ni[i] <= df$Long[t]) &  (df$Long[t] <= ni[i+1])){
                if((nj[j] <= df$Lat[t]) &  (df$Lat[t] <= nj[j+1])){
                  w_s_x[i,j,k] <- w_s_x[i,j,k] + sin(df$WindDir[t])*df$WindSpeed[t]
                  w_s_y[i,j,k] <- w_s_y[i,j,k] + cos(df$WindDir[t])*df$WindSpeed[t]
                  cnt_s[i,j,k] <- cnt_s[i,j,k] + 1
                  r <- r + 1
                }
              }
            }
          }
        }
      }
    }
  }
  
  
  #Mean of cos and sin and then reconstruct the average wind speed
  
  w_s <- array(0, dim = c(nrow-1, ncol-1, nflo-1))
  w_d <- array(0, dim = c(nrow-1, ncol-1, nflo-1))
  
  for(k in 1:(nflo-1)){
    for(i in 1:(nrow-1)){
      for(j in 1:(ncol-1)){
        if(cnt_s[i,j,k] > 0){
          
          w_s[i,j,k] <- sqrt((w_s_x[i,j,k]/cnt_s[i,j,k])^2 + (w_s_y[i,j,k]/cnt_s[i,j,k])^2)
          
          if((w_s_x[i,j,k] > 0) & (w_s_y[i,j,k] > 0)){
            w_d[i,j,k] <- atan((w_s_x[i,j,k]/cnt_s[i,j,k])/(w_s_y[i,j,k]/cnt_s[i,j,k]))
          }
          if((w_s_x[i,j,k] > 0) & (w_s_y[i,j,k] < 0)){
            w_d[i,j,k] <- atan((w_s_x[i,j,k]/cnt_s[i,j,k])/(w_s_y[i,j,k]/cnt_s[i,j,k])) + pi
          }
          if((w_s_x[i,j,k] < 0) & (w_s_y[i,j,k] < 0)){
            w_d[i,j,k] <- atan((w_s_x[i,j,k]/cnt_s[i,j,k])/(w_s_y[i,j,k]/cnt_s[i,j,k])) - pi
          }
          if((w_s_x[i,j,k] < 0) & (w_s_y[i,j,k] > 0)){
            w_d[i,j,k] <- atan((w_s_x[i,j,k]/cnt_s[i,j,k])/(w_s_y[i,j,k]/cnt_s[i,j,k]))
          }
        }
      }
    }
  }
  
  #Standard Deviation with Bootstrap
  
  for(k in 1:(nflo-1)){
    for(i in 1:(nrow-1)){
      for(j in 1:(ncol-1)){
        if(cnt_s[i,j,k] > 0){
          
          
          
          
          
        }
      }
    }
  }
  
  return(list(wind_spd = w_s, wind_dir = w_d, divisions = div, dimensions = dim_mat))
  
}



