extractPRData <- function(basepath){
        # function to extract rainfall data as dataframe
        # Read file names with daily mean temperature 
        files <- list.files(path = file.path(basepath, "pr"), pattern="*_dist.nc")
        sites_pr_tot <- vector("list",length(files))
        for (i in 1:length(files)){
                nc <- nc_open(file.path(basepath, "pr",files[i])) # open nc files using ncdf4 package
                sites_pr_tot[[i]] <- ncvar_get(nc, "prAdjust") # extract temperature variable data from a nc file
        }
        sites_pr_tot[[10]] <- sites_pr_tot[[10]][,1:3287] # to remove data beyond 31 Dec 2099 in some files
        sites_pr_tot.df <- listTodf(sites_pr_tot) %>% rename(tot_pr = value) %>% select(tot_pr)
        return(sites_pr_tot.df)
}
