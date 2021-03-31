## deccma_data.R 

# Script to compare DECCMA data with observed temperature data for validity
# checking. 

# Kishor Kumar Paul

# Set-up ------------------------------------------------------------------
# Restart R and source to file directory

#check/install/load packages
mypackages <- c("tidyverse", "reshape2", "lubridate", "ncdf4")

for (p in mypackages){
        if(!require(p, character.only = TRUE)){
                install.packages(p)
                library(p, character.only = TRUE)
        }
}

# Organize directories
basepath <- getwd()

cnrm_cm5_path <- file.path(basepath, "data", "DECCMA data", "Bangladesh", 
                           "HadRM3P_CNRM-CM5")
gfdl_cm3_path <- file.path(basepath, "data", "DECCMA data", "Bangladesh", 
                           "HadRM3P_GFDL-CM3")  
hadgem2_es_path <- file.path(basepath, "data", "DECCMA data", "Bangladesh", 
                             "HadRM3P_HadGEM2-ES")

# Source specific functions
source(file.path(basepath, "code", "functions.R"))
source(file.path(basepath, "code", "vc_estimation.R")) 
source(file.path(basepath, "code", "deccma_temp_extract.R"))
source(file.path(basepath, "code", "date_df_deccma.R")) 

# Read division and districts names 
df <- read.table(file.path(basepath,"data","centroids.txt"),
                 header = TRUE, sep = " ") %>% select(div_name, dist_name)

# List of districts in sequence of used to extract data from netCDF files
dist <- read.table(file.path(basepath,"data","centroids.txt"),
                   header = TRUE, sep = " ") %>% pull(dist_name)

# Make a list of divisions with corresponding districts
division <- list(Barisal = df$dist_name[df$div_name=="Barisal"],
                Khulna = df$dist_name[df$div_name=="Khulna"],
                Chittagong = df$dist_name[df$div_name=="Chittagong"],
                Rajshahi = df$dist_name[df$div_name=="Rajshahi"],
                Dhaka = df$dist_name[df$div_name=="Dhaka"],
                Rangpur = df$dist_name[df$div_name=="Rangpur"],
                Sylhet = df$dist_name[df$div_name=="Sylhet"],
                Mymensingh = df$dist_name[df$div_name=="Mymensingh"])

# Script parameters -------------------------------------------------------
# To be changed by user
reload <- FALSE
saveResults <- FALSE

# Extract data for three DECCMA models-------------------------------------

if (reload) {
        load(file.path(basepath, "data", "DECCMA data", "Bangladesh",
                "deccma.RData"))
} else {
        # Execute function to convert netCDF to two dimensional df
        # containing temp data using function in deccma_temp_extract.R
        cnrm_cm5.sites <- deccma_temp_extract(cnrm_cm5_path)
        gfdl_cm3.sites <- deccma_temp_extract(gfdl_cm3_path)
        hadgem2_es.sites <- deccma_temp_extract(hadgem2_es_path)
        
        # Create a df of districts corresponding to number of days available 
        # for each model
        # cnrm_cm5: temp data for 51,098 days (Jan 1960 to Nov 2099) available,
        # Standard gregorian (with leap days)
        cnrm_cm5.df <- data.frame(district = rep(dist, times = 51098))
        # gfdl_cm3: temp data for 54,350 days (Jan 1951 to Nov 2099) available,
        # 365 days every year (no leap days)
        gfdl_cm3.df <- data.frame(district = rep(dist,times = 54350))
        # hadgem2_es: temp data for 53,270 days (Jan 1951 to Nov 2099) available,
        # 360 days every year (30 days every month)
        hadgem2_es.df <- data.frame(district = rep(dist,times = 53270))
        
        
        # column bind district, date and VC for each model
        cnrm_cm5 <- cbind(cnrm_cm5.df, date.df_cnrm_cm5, cnrm_cm5.sites) %>% 
                mutate(mod = "HadRM3P_CNRM-CM5")
        
        gfdl_cm3 <- cbind(gfdl_cm3.df, date.df_gfdl_cm3, gfdl_cm3.sites) %>% 
                mutate(mod = "HadRM3P_GFDL-CM3")
        
        hadgem2_es <- cbind(hadgem2_es.df, date.df_hadgem2_es, hadgem2_es.sites) %>% 
                mutate(mod = "HadRM3P_HadGEM2-ES")
        
        # Row bind data for each model into one df
        deccma_t <- rbind(cnrm_cm5,gfdl_cm3,hadgem2_es)
        
        ## group districts into division and create division variable
        deccma_t <- deccma_t %>%
                mutate(division = case_when(
                        district %in% division$Barisal ~ "Barisal",
                        district %in% division$Khulna ~ "Khulna",
                        district %in% division$Chittagong ~ "Chittagong",
                        district %in% division$Rajshahi ~ "Rajshahi",
                        district %in% division$Dhaka ~ "Dhaka",
                        district %in% division$Rangpur ~ "Rangpur",
                        district %in% division$Sylhet ~ "Sylhet",
                        district %in% division$Mymensingh ~ "Mymensingh"))
        
        save(cnrm_cm5.sites, gfdl_cm3.sites, hadgem2_es.sites, deccma_t, 
             file=file.path(basepath, "data", "DECCMA data", "Bangladesh",
                            "deccma.RData"))
}

# Validity check with observed data----------------------------------------
# Calculate monthly observed temp 
vecCapacity <- read.csv(file.path(basepath, "data","vec_capacity.csv"), 
        header = TRUE)

obs_mon_avg <- vecCapacity %>%
        mutate(period = case_when(year %in% c(1986:2015) ~ '1986-2015')) %>%
        filter(is.na(period) == F) %>%
        group_by(division,month) %>%
        summarise(Maximum = mean(max_temp, na.rm = TRUE),
                Minimum = mean(min_temp, na.rm = TRUE)) %>%
        gather(temp, value, Maximum, Minimum)

# Calculate monthly temp for DECCMA models 

# First convert month year day variable to numeric
cols.num <- c("year","month","day")
deccma_t[cols.num] <- sapply(deccma_t[cols.num],as.numeric)

deccma_month_Avg <- deccma_t %>%
        mutate(period = case_when(year %in% c(1986:2015) ~ '1986-2015')) %>%
        filter(is.na(period) == F) %>%
        group_by(division, mod, month, period) %>%
        summarise(Maximum = mean(maxTemp, na.rm = TRUE),
                Minimum = mean(minTemp, na.rm = TRUE)) %>%
        gather(temp, value, Maximum, Minimum)


# Plot temp for observed and DECCMA ---------------------------------------

tmin_tmax <- ggplot() +
        geom_line(deccma_month_Avg, 
                  mapping = aes(x=month,y=value, color=mod))+
        facet_grid(division ~ temp, scales = "free" )+
        geom_line(obs_mon_avg, mapping = aes(month, value), 
                  linetype = "dashed")+
        scale_x_continuous(breaks = round(seq(min(deccma_month_Avg$month), 
                                              max(deccma_month_Avg$month), 
                                              by = 1),1),
                           labels = c("J","F","M","A","M","J","J","A","S",
                                      "O","N","D"))+
        labs(x = "Month", y = "Temperature (°C)",
             title = "Monthly averaged temperature for different models,
             1986-2015",
             subtitle = "Dashed line: Observed maximum and minimum 
             temperature monthly averaged for each division")+
        theme_bw() +
        theme(legend.title = element_blank(),
              legend.text = element_text(size=10),
              axis.text.x = element_text(size=8),
              axis.text.y = element_text(size=8))

if (saveResults) {
        ggsave(filename = "tmin_max_validation_deccma.tiff", 
                plot = tmin_tmax, device = "tiff", 
                path = file.path(basepath, "outputs", "2021 03"), 
                width = 8.27, height = 11.69 , units = "in", dpi = 600) 
}
