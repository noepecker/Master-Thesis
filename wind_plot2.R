wind_plot2 <- function(data, layer, direction_var = FALSE){
  
  # div <- data$divisions
  # 
  # if(layer < 1 | div <= layer){
  #   stop(print(c("Layer must be between 1 and number of divisions-1, Maximum layer:", print(div-1))))
  # }
  # DATA
  
  w_s3 <- data$wind_spd
  w_s3_var <- data$wind_spd_var
  w_d3 <- data$wind_dir
  w_d3_var <- data$wind_dir_var
  
  
  # SPACE DIVISION
  
  div <- data$divisions 
  nrow <- div
  ncol <- div
  nflo <- div
  dim_mat <- data$dimensions
  x_min <- dim_mat[1,1]
  x_max <- dim_mat[2,1]
  y_min <- dim_mat[1,2]
  y_max <- dim_mat[2,2]
  z_min <- dim_mat[1,3]
  z_max <- dim_mat[2,3]
  ni <- seq(x_min, x_max, length.out = nrow )
  nj <- seq(y_min, y_max, length.out = ncol)
  nk <- seq(z_min, z_max, length.out = nflo)
  # PLOT
  
  #Define Coordinates
  lon <- ni[-1] 
  lat <- nj[-1]
  
  #Size of arrows
  delta_lon <- matrix( w_s3[,,layer]*sin(w_d3[,,layer]), nrow = length(lon), ncol = length(lat)) #We can use w_s3 or w_s, depending on the method
  delta_lat <- matrix( w_s3[,,layer]*cos(w_d3[,,layer]), nrow = length(lon), ncol = length(lat))
  spd <- matrix( w_s3[,,layer], nrow = length(lon), ncol = length(lat))
  dir_var <- matrix( w_d3_var[,,layer], nrow = length(lon), ncol = length(lat))
  spd_var <- matrix( w_s3_var[,,layer], nrow = length(lon), ncol = length(lat))
  
  #Create data frame to plot it
  delta_lon <- melt(delta_lon, value.name = "delta_lon")
  delta_lat <- melt(delta_lat, value.name = "delta_lat")
  spd <- melt(spd, value.name = "spd")
  dir_var <- melt(dir_var, value.name = "dir_var")
  spd_var <- melt(spd_var, value.name = "spd_var")
  
  #Merge columns in a data frame
  #rm(wind)
  wind <- merge(lon,lat)
  wind <- wind %>% rename(
    Longitude = x,
    Latitude = y
  )
  
  wind$delta_lon <- delta_lon[, 3]
  wind$delta_lat <- delta_lat[, 3]
  wind$spd <- spd[,3]
  wind$Dir_SD <- dir_var[,3]
  wind$Spd_SD <- spd_var[,3]

  if(direction_var == TRUE){
    #Plot in selected area
    p <- qmplot(Longitude, Latitude, data=wind,
                extent="panel", geom = "blank",
                zoom=7, maptype = "toner-lite",
                main = NULL, f=0.2, darken = 0) +
      ggtitle("Wind")  +
      scale_colour_gradient(low="darkgreen",high="lightgreen") +
      geom_quiver(aes(u=delta_lon, v=delta_lat, colour = Dir_SD), center=TRUE, size=1.5, alpha = 1)
    #El color es la desviacion típica, podemos elegir entre varianza de la dirección (Dir_SD) y de la velocidad (Spd_SD)
  }else{
    p <- qmplot(Longitude, Latitude, data=wind,
                extent="panel", geom = "blank",
                zoom=7, maptype = "toner-lite",
                main = NULL, f=0.2, darken = 0) +
      ggtitle("Wind")  +
      #scale_colour_gradient(low="darkblue",high="red") +
      geom_quiver(aes(u=delta_lon, v=delta_lat, colour = Spd_SD), center=TRUE, size=1.5, alpha = 1)
  }
  
  
  
  return(p)
  
  
}