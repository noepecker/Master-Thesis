wind_est <- function(data, divisions = 10){
  
  #Data must have columns with Latitude (Lat), Longitude (Long)
  #Altitude (Alt), time (dtm), Wind Speed (WindSpeed) and Wind Direction (WindDir)
  
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
  
  # SPACE DIVISION
  
  div <- divisions 
  nrow <- div
  ncol <- div
  nflo <- div
  
  #Cells
  
  ni <- seq(x_min, x_max, length.out = nrow )
  nj <- seq(y_min, y_max, length.out = ncol)
  nk <- seq(z_min, z_max, length.out = nflo)
  t_f <- length(df[,1])
  
  #Mean and standard deviation of Speed and Direction without relationship
  
  # init
  cnt_d <- array(0, dim = c(nrow-1, ncol-1, nflo-1))
  w_d4 <- array(0, dim = c(nrow-1, ncol-1, nflo-1,2000))
  w_s4 <- array(0, dim = c(nrow-1, ncol-1, nflo-1,2000))
  h <- 1
  
  for(k in 1:(nflo-1)){
    for(i in 1:(nrow-1)){
      for(j in 1:(ncol-1)){
        h <- 1
        for(t in 1:t_f){
          if((nk[k] <= df$Alt[t]) & (df$Alt[t] <= nk[k+1])){
            if((ni[i] <= df$Long[t]) &  (df$Long[t] <= ni[i+1])){
              if((nj[j] <= df$Lat[t]) &  (df$Lat[t] <= nj[j+1])){
                w_d4[i,j,k,h] <- df$WindDir[t] #Save a vector of directions in h
                w_s4[i,j,k,h] <- df$WindSpeed[t] #Save vector of speeds in h
                h=h+1
                cnt_d[i,j,k] <- cnt_d[i,j,k]+1
              }
            }
          }
        }
      }
    }
  }
  
  #Now we compute de mean and standard deviation of each vector
  
  w_d3 <- array(0, dim = c(nrow-1, ncol-1, nflo-1))
  w_d3_var <- array(0, dim = c(nrow-1, ncol-1, nflo-1))
  w_s3 <- array(0, dim = c(nrow-1, ncol-1, nflo-1))
  w_s3_var <- array(0, dim = c(nrow-1, ncol-1, nflo-1))
  
  for(k in 1:(nflo-1)){
    for(i in 1:(nrow-1)){
      for(j in 1:(ncol-1)){
        if(cnt_d[i,j,k] > 1){
          
          w_d3[i,j,k] <- circ.mean(w_d4[i,j,k,][w_d4[i,j,k,] != 0]) #We eliminate the 0s of the vector
          if(abs(circ.disp(w_d4[i,j,k,][w_d4[i,j,k,] != 0])[,4]) < 1e-12){ #SD for cells with repeated values
            w_d3_var[i,j,k] <- sqrt(-2* log(circ.disp(jitter(w_d4[i,j,k,][w_d4[i,j,k,] != 0], factor = 10/cnt_d[i,j,k]))[,3]))
          }else{
            w_d3_var[i,j,k] <- sqrt(-2* log(circ.disp(w_d4[i,j,k,][w_d4[i,j,k,] != 0])[,3])) #Circ.disp puede dar valores negativos muy pequeños en vez de 0,
          } #Factor para el jitter que depende del numero de valores repetidos
          
          w_s3[i,j,k] <- mean(w_s4[i,j,k,][w_s4[i,j,k,] != 0])
          if(sd(w_s4[i,j,k,][w_s4[i,j,k,] != 0]) < 1e-12){ #Variance for cells with repeated values
            w_s3_var[i,j,k] <- sd(jitter(w_s4[i,j,k,][w_s4[i,j,k,] != 0], factor = 10/cnt_d[i,j,k]))
          }else{
            w_s3_var[i,j,k] <- sd(w_s4[i,j,k,][w_s4[i,j,k,] != 0])
          }
        }else{
          w_d3_var[i,j,k] == 0
          w_d3[i,j,k] == 0 
          w_s3_var[i,j,k] == 0
          w_s3[i,j,k] == 0
        }
      }
    }
  }
  
  
  return(list(wind_spd = w_s3, wind_spd_var = w_s3_var, wind_dir = w_d3, wind_dir_var = w_d3_var, divisions = divisions, dimensions = dim_mat))
  
}