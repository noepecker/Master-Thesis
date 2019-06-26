wind_pred2 <- function(estimated_data){
  #Estimated data must be the object created by the function wind_est, which is a list of 4 matrices.
  
  w_s3 <- estimated_data$wind_spd
  w_s3_var <- estimated_data$wind_spd_var
  w_d3 <- estimated_data$wind_dir
  w_d3_var <- estimated_data$wind_dir_var
  
  # SPACE DIVISION
  
  div <- estimated_data$divisions
  nrow <- div
  ncol <- div
  nflo <- div
  dim_mat <- estimated_data$dimensions
  x_min <- dim_mat[1,1]
  x_max <- dim_mat[2,1]
  y_min <- dim_mat[1,2]
  y_max <- dim_mat[2,2]
  z_min <- dim_mat[1,3]
  z_max <- dim_mat[2,3]
  ni <- seq(x_min, x_max, length.out = nrow )
  nj <- seq(y_min, y_max, length.out = ncol)
  nk <- seq(z_min, z_max, length.out = nflo)

  
   #while((length(a2$wind_spd[c(-1,-5),c(-1,-5),c(-1,-5)]== 0)) > 0){
    for(r in 1:15){
    
    # init
    
    #29 porque solo usamos los vecinos más proximos y como máximo hay 26
    w_d4_pred <- array(0, dim = c(nrow-1, ncol-1, nflo-1, 29))
    w_d4_var_pred <- array(0, dim = c(nrow-1, ncol-1, nflo-1, 29))
    w_s4_pred <- array(0, dim = c(nrow-1, ncol-1, nflo-1, 29))
    w_s4_var_pred <- array(0, dim = c(nrow-1, ncol-1, nflo-1, 29))
    
  
    h <- 1
    
    for(k in 2:(nflo-2)){
      for(i in 2:(nrow-2)){
        for(j in 2:(ncol-2)){
          if((w_s3[i,j,k] == 0)){
            h <- 1
            for(ib in (i-1):(i+1)){
              for(jb in (j-1):(j+1)){
                for(kb in (k-1):(k+1)){
                  w_d4_pred[i,j,k,h] <- w_d3[ib,jb,kb] #Multiply by WindSpeed??
                  w_d4_var_pred[i,j,k,h] <- w_d3_var[ib,jb,kb]
                  w_s4_pred[i,j,k,h] <- w_s3[ib,jb,kb] 
                  w_s4_var_pred[i,j,k,h] <- w_s3_var[ib,jb,kb]
                  h <- h + 1
                }
              }
            }
          }
        }
      }
    }
    
    
    w_d3_pred <- array(0, dim = c(nrow-1, ncol-1, nflo-1))
    w_d3_var_pred <- array(0, dim = c(nrow-1, ncol-1, nflo-1))
    w_s3_pred <- array(0, dim = c(nrow-1, ncol-1, nflo-1))
    w_s3_var_pred <- array(0, dim = c(nrow-1, ncol-1, nflo-1))
    
    for(k in 1:(nflo-1)){
      for(i in 1:(nrow-1)){
        for(j in 1:(ncol-1)){
          
          #Direction Mean
          m=length(w_d4_pred[i,j,k,][w_d4_pred[i,j,k,] != 0])
          if(m > 1){
            w_d3_pred[i,j,k] <- circ.mean(w_d4_pred[i,j,k,][w_d4_pred[i,j,k,] != 0])
          }else{w_d3_pred[i,j,k] <- w_d3[i,j,k]}
          
          #Direction Variance
          m=length(w_d4_var_pred[i,j,k,][w_d4_var_pred[i,j,k,] != 0])
          if(m > 1){
            w_d3_var_pred[i,j,k] <- mean(w_d4_var_pred[i,j,k,][w_d4_var_pred[i,j,k,] != 0])
          }else{w_d3_var_pred[i,j,k] <- w_d3_var[i,j,k]}
          
          #Speed Mean
          m=length(w_s4_pred[i,j,k,][w_s4_pred[i,j,k,] != 0])
          if(m > 1){
            w_s3_pred[i,j,k] <- mean(w_s4_pred[i,j,k,][w_s4_pred[i,j,k,] != 0])
          }else{w_s3_pred[i,j,k] <- w_s3[i,j,k]}
          
          #Speed Variance
          m=length(w_s4_var_pred[i,j,k,][w_s4_var_pred[i,j,k,] != 0])
          if(m > 1){
            w_s3_var_pred[i,j,k] <- mean(w_s4_var_pred[i,j,k,][w_s4_var_pred[i,j,k,] != 0])
          }else{w_s3_var_pred[i,j,k] <- w_s3_var[i,j,k]}
        }
      }
    }
    
    w_d3 <- w_d3_pred
    w_d3_var <- w_d3_var_pred
    w_s3 <- w_s3_pred
    w_s3_var <- w_s3_var_pred
    
    
  }
  
  
  return(list(wind_spd = w_s3_pred, wind_spd_var = w_s3_var_pred, wind_dir = w_d3_pred, wind_dir_var = w_d3_var_pred, divisions = div, dimensions = dim_mat))
  
  
}