deccma_VC <- function(filepath){
        # function to calculate VC from climate data and extract data as dataframe
        # Read file names with daily mean temperature 
        files <- list.files(path = file.path(filepath, "03236"), pattern="*_dist.nc")
        sites_meanTemp <- vector("list",length(files))
        for (i in 1:length(files)){
                nc <- nc_open(file.path(filepath, "03236",files[i])) # open nc files using ncdf4 package
                sites_meanTemp[[i]] <- ncvar_get(nc, "air_temperature") # extract temperature variable data from a nc file
        }
        #sites_meanTemp[[10]] <- sites_meanTemp[[10]][,1:3287] # to remove data beyond 31 Dec 2099 in some files
        sites_meanTemp <- lapply(sites_meanTemp,KelvinCelsius) # convert temperature unit from Kelvin to celsius
        
        # Read file names with daily max temperature 
        files <- list.files(path = file.path(filepath, "03236.max"), pattern="*_dist.nc")
        sites_maxTemp <- vector("list",length(files))
        for (i in 1:length(files)){
                nc <- nc_open(file.path(filepath, "03236.max",files[i]))
                sites_maxTemp[[i]] <- ncvar_get(nc, "air_temperature")
        }
        #sites_maxTemp[[10]] <- sites_maxTemp[[10]][,1:3287]
        sites_maxTemp <- lapply(sites_maxTemp,KelvinCelsius)
        
        # Read file names with daily min temperature 
        files <- list.files(path = file.path(filepath, "03236.min"), pattern="*_dist.nc")
        sites_minTemp <- vector("list",length(files))
        for (i in 1:length(files)){
                nc <- nc_open(file.path(filepath, "03236.min", files[i]))
                sites_minTemp[[i]] <- ncvar_get(nc, "air_temperature")
        }
        #sites_minTemp[[10]] <- sites_minTemp[[10]][,1:3287]
        sites_minTemp <- lapply(sites_minTemp,KelvinCelsius)
        
        # calculate DTR from max and min temperature
        sites_dtr <- mapply('-', sites_maxTemp, sites_minTemp, SIMPLIFY = FALSE)
        
        # Calculate daily average VC for  
        sitesVC <- vector("list", length(files))
        for (k in 1:length(files)){
                for (i in 1:(dim(sites_meanTemp[[k]])[1]*dim(sites_meanTemp[[k]])[2])) {
                        sinusoidal_temperature <- Temp(0:47, sites_meanTemp[[k]][i], sites_dtr[[k]][i])
                        sitesVC[[k]][i] <- mean(VectorialCapacity(sinusoidal_temperature))
                }
        }
        # match the dimension of sitesVC with that of temperature values
        for (i in 1:length(sitesVC)){
                dim(sitesVC[[i]]) <- dim(sites_meanTemp[[i]]) 
        }
        
        sites_meanTemp.df <- listTodf(sites_meanTemp) %>% rename(meanTemp = value) %>% select(meanTemp)
        sites_maxTemp.df <- listTodf(sites_maxTemp) %>% rename(maxTemp = value) %>% select(maxTemp)
        sites_minTemp.df <- listTodf(sites_minTemp) %>% rename(minTemp = value) %>% select(minTemp)
        sites_dtr.df <- listTodf(sites_dtr) %>% rename(dtr = value) %>% select(dtr)
        sitesVC.df <- listTodf(sitesVC) %>% rename(VC = value) %>% select(VC)
        df <- cbind(sites_maxTemp.df,sites_meanTemp.df,sites_minTemp.df,sites_dtr.df,sitesVC.df)
        return(df)
}
