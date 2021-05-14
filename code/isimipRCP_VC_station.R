#' Calculate VC from ISIMIP data for RCP scenarios (2006-2099)
#' 
#' function to extract temp data from three dimensional ISIMIP 
#' netCDF files and make two dimensional df
#' 
#' @param filepath File path to a ISIMIP model
#'  
#' @return returns df with mean, max, min and DTR variables for a ISIMIP
#' model

isimipRCP_VC_v2 <- function(filepath){
        # Read file names with daily mean temperature 
        files <- list.files(path = file.path(filepath, "tas"), 
                            pattern = "*_station.nc")
        sites_meanTemp <- vector("list", length(files))
        for (i in 1:length(files)){
                # open nc files using ncdf4 package
                nc <- nc_open(file.path(filepath, "tas", files[i]))
                # extract temperature variable data from a nc file
                sites_meanTemp[[i]] <- ncvar_get(nc, "tasAdjust") 
        }
        # Remove data beyond 31 Dec 2099 in some files
        sites_meanTemp[[10]] <- sites_meanTemp[[10]][,1:3287]
        # convert temperature unit using function from functions.R
        sites_meanTemp <- lapply(sites_meanTemp, KelvinCelsius) 
        
        # Read file names with daily max temperature 
        files <- list.files(path = file.path(filepath, "tasmax"), 
                            pattern = "*_station.nc")
        sites_maxTemp <- vector("list", length(files))
        for (i in 1:length(files)){
                # open nc files using ncdf4 package
                nc <- nc_open(file.path(filepath, "tasmax", files[i]))
                # extract temperature variable data from a nc file
                sites_maxTemp[[i]] <- ncvar_get(nc, "tasmaxAdjust")
        }
        # Remove data beyond 31 Dec 2099 in some files
        sites_maxTemp[[10]] <- sites_maxTemp[[10]][,1:3287]
        # convert temperature unit using function from functions.R
        sites_maxTemp <- lapply(sites_maxTemp, KelvinCelsius)
        
        # Read file names with daily min temperature 
        files <- list.files(path = file.path(filepath, "tasmin"), 
                            pattern = "*_station.nc")
        sites_minTemp <- vector("list", length(files))
        for (i in 1:length(files)){
                # open nc files using ncdf4 package
                nc <- nc_open(file.path(filepath, "tasmin", files[i]))
                # extract temperature variable data from a nc file
                sites_minTemp[[i]] <- ncvar_get(nc, "tasminAdjust")
        }
        # Remove data beyond 31 Dec 2099 in some files
        sites_minTemp[[10]] <- sites_minTemp[[10]][,1:3287]
        # convert temperature unit using function from functions.R
        sites_minTemp <- lapply(sites_minTemp, KelvinCelsius)
        
        # calculate DTR from max and min temperature
        sites_dtr <- mapply('-', sites_maxTemp, sites_minTemp, 
                            SIMPLIFY = FALSE)
        
        # Calculate daily average VC for  
        sitesVC <- vector("list", length(files))
        for (k in 1:length(files)){
                for (i in 1:(dim(sites_meanTemp[[k]])[1]*dim(sites_meanTemp[[k]])[2])) {
                        # Use Temp function from functions.R
                        sinusoidal_temperature <- Temp(0:47, sites_meanTemp[[k]][i], 
                                                       sites_dtr[[k]][i])
                        # Use VectorialCapacity function from vc_estimation.R
                        sitesVC[[k]][i] <- mean(VectorialCapacity(sinusoidal_temperature))
                }
        }
        
        # match the dimension of sitesVC with that of temperature values
        for (i in 1:length(sitesVC)){
                dim(sitesVC[[i]]) <- dim(sites_meanTemp[[i]]) 
        }
        
        # use listTodf function from functions.R to convert list containing
        # temp data to a df
        sites_meanTemp.df <- listTodf(sites_meanTemp) %>% 
                rename(meanTemp = value) %>% 
                select(meanTemp)
        sites_maxTemp.df <- listTodf(sites_maxTemp) %>% 
                rename(maxTemp = value) %>% 
                select(maxTemp)
        sites_minTemp.df <- listTodf(sites_minTemp) %>% 
                rename(minTemp = value) %>% 
                select(minTemp)
        sites_dtr.df <- listTodf(sites_dtr) %>% 
                rename(dtr = value) %>% 
                select(dtr)
        sitesVC.df <- listTodf(sitesVC) %>% 
                rename(VC = value) %>% 
                select(VC)
        
        # Column bind all temp and VC variables into one df
        df <- cbind(sites_maxTemp.df, sites_meanTemp.df, sites_minTemp.df, 
                    sites_dtr.df, sitesVC.df)
        
        return(df)
}
