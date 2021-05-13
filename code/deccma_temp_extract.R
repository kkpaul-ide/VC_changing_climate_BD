#' Extract DECCMA temp data
#' 
#' function to extract temp data from three dimensional DECCMA 
#' netCDF files and make two dimensional df
#' 
#' @param filepath File path to a DECCMA model
#'  
#' @return returns df with mean, max, min and DTR variables for a DECCMA
#' model

deccma_temp_extract <- function(filepath){

        # Read file names with daily mean temperature 
        files <- list.files(path = file.path(filepath, "03236"), pattern="*_dist.nc")
        sites_meanTemp <- vector("list",length(files))
        for (i in 1:length(files)){
                # open nc files using ncdf4 package
                nc <- nc_open(file.path(filepath, "03236",files[i]))
                # extract temperature variable data from a nc file
                sites_meanTemp[[i]] <- ncvar_get(nc, "air_temperature") 
        }
        # use KelvinCelsius function from functions.R
        sites_meanTemp <- lapply(sites_meanTemp,KelvinCelsius) 
        
        # Read file names with daily max temperature 
        files <- list.files(path = file.path(filepath, "03236.max"), pattern="*_dist.nc")
        sites_maxTemp <- vector("list",length(files))
        for (i in 1:length(files)){
                # open nc files using ncdf4 package
                nc <- nc_open(file.path(filepath, "03236.max",files[i]))
                # extract temperature variable data from a nc file
                sites_maxTemp[[i]] <- ncvar_get(nc, "air_temperature")
        }
        # use KelvinCelsius function from functions.R
        sites_maxTemp <- lapply(sites_maxTemp,KelvinCelsius)
        
        # Read file names with daily min temperature 
        files <- list.files(path = file.path(filepath, "03236.min"), pattern="*_dist.nc")
        sites_minTemp <- vector("list",length(files))
        for (i in 1:length(files)){
                # open nc files using ncdf4 package
                nc <- nc_open(file.path(filepath, "03236.min", files[i]))
                # extract temperature variable data from a nc file
                sites_minTemp[[i]] <- ncvar_get(nc, "air_temperature")
        }
        # use KelvinCelsius function from functions.R
        sites_minTemp <- lapply(sites_minTemp,KelvinCelsius)
        
        # calculate DTR from max and min temperature
        sites_dtr <- mapply('-', sites_maxTemp, sites_minTemp, SIMPLIFY = FALSE)
        
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
       
        # Column bind all temp variables into one df
        df <- cbind(sites_maxTemp.df,sites_meanTemp.df,sites_minTemp.df,sites_dtr.df)
        return(df)
}
