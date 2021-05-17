## isimip_data.R 

# Script to compare ISIMIP data with observed temperature data for validity
# checking, calculate VC and create plots with ISIMIP data 

# Kishor Kumar Paul

# Set-up ------------------------------------------------------------------
# Restart R and source to file directory

#check/install/load packages
mypackages<-c("tidyverse", "reshape2", "lubridate", "ncdf4", "dplyr")

for (p in mypackages){
        if(!require(p, character.only = TRUE)){
                install.packages(p)
                library(p, character.only = TRUE)
        }
}

# Organize directories
basepath <- getwd()
isimipPath <- file.path(basepath, "data", "ISIMIP_data", "bangladesh")

# Create path for each model and scenario
gfdlRCP45_path <- file.path(isimipPath, "GFDL-ESM2M", "rcp45")
gfdlRCP85_path <- file.path(isimipPath, "GFDL-ESM2M", "rcp85")
hadgemRCP45_path <- file.path(isimipPath, "HadGEM2-ES", "rcp45")
hadgemRCP85_path <- file.path(isimipPath, "HadGEM2-ES", "rcp85")
ipslRCP45_path <- file.path(isimipPath, "IPSL-CM5A-LR", "rcp45")
ipslRCP85_path <- file.path(isimipPath, "IPSL-CM5A-LR", "rcp85")
mirocRCP45_path <- file.path(isimipPath, "MIROC5", "rcp45")
mirocRCP85_path <- file.path(isimipPath, "MIROC5", "rcp85")
gfdlHist_path <- file.path(isimipPath, "GFDL-ESM2M", "historical")
hadgemHist_path <- file.path(isimipPath, "HadGEM2-ES", "historical")
ipslHist_path <- file.path(isimipPath, "IPSL-CM5A-LR", "historical")
mirocHist_path <- file.path(isimipPath, "MIROC5", "historical")

# Source specific functions
source(file.path(basepath, "code", "functions.R"))
source(file.path(basepath, "code", "vc_estimation.R")) 
source(file.path(basepath, "code", "isimipRCP_VC_station.R"))  
source(file.path(basepath, "code", "isimipHist_VC_station.R")) 

# Read division and districts names 
df <- read.csv(file.path(basepath,"data","ListOf35BMDstations.csv"),
               header = TRUE, sep = ",")

# Make a list of divisions with corresponding districts
divs <- list(Mymensingh = "Mymensingh",
             Sylhet     = c("Sylhet","Srimangal"),
             Rajshahi   = c("Ishurdi", "Rajshahi", "Bogra"),
             Rangpur    = c("Syedpur", "Dinajpur", "Rangpur"),
             Dhaka      = c("Dhaka", "Faridpur", "Madaripur", "Tangail"),
             Barisal    = c("Barisal", "Bhola", "Khepupara", "Patuakhali"),
             Khulna     = c("Chuadanga", "Jessore", "Khulna", "Mongla", 
                             "Satkhira"),
             Chittagong = c("Chittagong (AP)", "Chandpur", 
                            "Chittagong (City)", "Comilla", "Cox's Bazar",
                            "Feni", "Hatiya", "Kutubdia", "Maijdee Court", 
                            "Rangamati", "Sandwip", "Sitakunda", "Teknaf"))

# Script parameters -------------------------------------------------------
# To be changed by user
reload <- FALSE
saveResults <- FALSE

# Extract data for ISIMIP models-------------------------------------------
if (reload) {
        load(file.path(basepath, "outputs", "stations_isimip.RData"))
} else {
        # Execute function to calculate VC for each model data using 
        # function in isimipRCP_VC.R & isimipHist_VC.R
        gfdlRCP45_sites <- isimipRCP_VC_v2(gfdlRCP45_path)
        gfdlRCP85_sites <- isimipRCP_VC_v2(gfdlRCP85_path)
        hadgemRCP45_sites <- isimipRCP_VC_v2(hadgemRCP45_path)
        hadgemRCP85_sites <- isimipRCP_VC_v2(hadgemRCP85_path)
        ipslRCP45_sites <- isimipRCP_VC_v2(ipslRCP45_path)
        ipslRCP85_sites <- isimipRCP_VC_v2(ipslRCP85_path)
        mirocRCP45_sites <- isimipRCP_VC_v2(mirocRCP45_path)
        mirocRCP85_sites <- isimipRCP_VC_v2(mirocRCP85_path)
        gfdlHist_sites <- isimipHist_VC_v2(gfdlHist_path)
        hadgemHist_sites <- isimipHist_VC_v2(hadgemHist_path)
        ipslHist_sites <- isimipHist_VC_v2(ipslHist_path)
        mirocHist_sites <- isimipHist_VC_v2(mirocHist_path)
        
        # Create df of districts corresponding to number of days 
        # available for each model and RCP scenario/historical period
        # RCP scenarios: Temp data for 34,333 days (Jan 2006 to Dec 2099) available  
        dist.df <- data.frame(district = rep(df$Station, times = 34333))
        # Historical: Temp data for 20,089 days (Jan 1951 to Dec 2005) available   
        hist.dist.df <- data.frame(district = rep(df$Station, times = 20089))
        
        # create df of dates corresponding to RCP scenarios 
        date.df <- data.frame(date = c(rep(seq(as.Date("2006/1/1"), 
                                               as.Date("2010/12/31"), "days"), each=35),
                                       rep(seq(as.Date("2011/1/1"), 
                                               as.Date("2020/12/31"), "days"), each=35),
                                       rep(seq(as.Date("2021/1/1"), 
                                               as.Date("2030/12/31"), "days"), each=35),
                                       rep(seq(as.Date("2031/1/1"), 
                                               as.Date("2040/12/31"), "days"), each=35),
                                       rep(seq(as.Date("2041/1/1"), 
                                               as.Date("2050/12/31"), "days"), each=35),
                                       rep(seq(as.Date("2051/1/1"), 
                                               as.Date("2060/12/31"), "days"), each=35),
                                       rep(seq(as.Date("2061/1/1"), 
                                               as.Date("2070/12/31"), "days"), each=35),
                                       rep(seq(as.Date("2071/1/1"), 
                                               as.Date("2080/12/31"), "days"), each=35),
                                       rep(seq(as.Date("2081/1/1"), 
                                               as.Date("2090/12/31"), "days"), each=35),
                                       rep(seq(as.Date("2091/1/1"), 
                                               as.Date("2099/12/31"), "days"), each=35)))
        
        # create df of dates corresponding to historical period 
        hist.date.df <- data.frame(date = c(rep(seq(as.Date("1951/1/1"), 
                                                    as.Date("1960/12/31"), "days"), each=35),
                                            rep(seq(as.Date("1961/1/1"), 
                                                    as.Date("1970/12/31"), "days"), each=35),
                                            rep(seq(as.Date("1971/1/1"), 
                                                    as.Date("1980/12/31"), "days"), each=35),
                                            rep(seq(as.Date("1981/1/1"), 
                                                    as.Date("1990/12/31"), "days"), each=35),
                                            rep(seq(as.Date("1991/1/1"), 
                                                    as.Date("2000/12/31"), "days"), each=35),
                                            rep(seq(as.Date("2001/1/1"), 
                                                    as.Date("2005/12/31"), "days"), each=35)))
        
        # column bind district, date and VC for each model
        gfdlRCP45_df <- cbind(dist.df, date.df, gfdlRCP45_sites) %>% 
                mutate(gcm = "GFDL-ESM2M") %>%
                mutate(rcp = "RCP 4.5")
        gfdlRCP85_df <- cbind(dist.df, date.df, gfdlRCP85_sites) %>% 
                mutate(gcm = "GFDL-ESM2M") %>%
                mutate(rcp = "RCP 8.5")
        hadgemRCP45_df <- cbind(dist.df, date.df, hadgemRCP45_sites) %>% 
                mutate(gcm = "HadGEM2-ES") %>%
                mutate(rcp = "RCP 4.5")
        hadgemRCP85_df <- cbind(dist.df, date.df, hadgemRCP85_sites) %>% 
                mutate(gcm = "HadGEM2-ES") %>%
                mutate(rcp = "RCP 8.5")
        ipslRCP45_df <- cbind(dist.df, date.df, ipslRCP45_sites) %>% 
                mutate(gcm = "IPSL-CM5A-LR") %>%
                mutate(rcp = "RCP 4.5")
        ipslRCP85_df <- cbind(dist.df, date.df, ipslRCP85_sites) %>% 
                mutate(gcm = "IPSL-CM5A-LR") %>%
                mutate(rcp = "RCP 8.5")
        mirocRCP45_df <- cbind(dist.df, date.df, mirocRCP45_sites) %>% 
                mutate(gcm = "MIROC5") %>%
                mutate(rcp = "RCP 4.5")
        mirocRCP85_df <- cbind(dist.df, date.df, mirocRCP85_sites) %>% 
                mutate(gcm="MIROC5") %>%
                mutate(rcp = "RCP 8.5")
        
        gfdlHist_df <- cbind(hist.dist.df, hist.date.df, gfdlHist_sites) %>% 
                mutate(gcm = "GFDL-ESM2M") %>%
                mutate(rcp = "Historical")
        hadgemHist_df <- cbind(hist.dist.df, hist.date.df, hadgemHist_sites) %>% 
                mutate(gcm="HadGEM2-ES") %>%
                mutate(rcp = "Historical")
        ipslHist_df <- cbind(hist.dist.df, hist.date.df, ipslHist_sites) %>% 
                mutate(gcm="IPSL-CM5A-LR") %>%
                mutate(rcp = "Historical")
        mirocHist_df <- cbind(hist.dist.df, hist.date.df, mirocHist_sites) %>% 
                mutate(gcm="MIROC5") %>%
                mutate(rcp = "Historical")
        
        # Row bind data for each model into one df
        hist.sites.df <- rbind(gfdlRCP45_df,gfdlRCP85_df,hadgemRCP45_df,hadgemRCP85_df,
                               ipslRCP45_df,ipslRCP85_df,mirocRCP45_df,mirocRCP85_df,
                               gfdlHist_df,hadgemHist_df,ipslHist_df,mirocHist_df)
        
        ## group districts into division and create division variable
        hist.sites.df <- hist.sites.df %>%
                mutate(division = case_when(
                        district %in% divs$Barisal ~ "Barisal",
                        district %in% divs$Khulna ~ "Khulna",
                        district %in% divs$Chittagong ~ "Chittagong",
                        district %in% divs$Rajshahi ~ "Rajshahi",
                        district %in% divs$Dhaka ~ "Dhaka",
                        district %in% divs$Rangpur ~ "Rangpur",
                        district %in% divs$Sylhet ~ "Sylhet",
                        district %in% divs$Mymensingh ~ "Mymensingh"))
        
        # Create year, month, day variable from date variable
        hist.sites.df <- createDate(hist.sites.df)
        hist.sites.df <- within(hist.sites.df, rm(date))
        
        #Convert month year day variable to numeric
        cols.num <- c("year","month","day")
        hist.sites.df[cols.num] <- sapply(hist.sites.df[cols.num],as.numeric)
        
        if (saveResults) {
                save(hist.sites.df, file=file.path(basepath, "outputs",  
                                           "stations_isimip.RData"))
        }
}

# Validity check with observed data----------------------------------------

# Calculate monthly observed temp 
# load observed VC
vecCapacity <- read.csv(file.path(basepath, "outputs", "vec_capacity.csv"), 
                        header = TRUE)

vecCapacity <- vecCapacity %>%
        mutate(station = replace(station, station=="Ambagan(Ctg)", "Chittagong (AP)")) %>%
        mutate(station = replace(station, station=="chuadanga", "Chuadanga")) %>%
        mutate(station = replace(station, station=="Chittagong", "Chittagong (City)")) %>%
        mutate(station = replace(station, station=="M.court", "Maijdee Court")) %>%
        mutate(station = replace(station, station=="sydpur", "Syedpur"))

colnames(hist.sites.df)[colnames(hist.sites.df) == "district"] <- "station"

# Observed
#historical 
obs_mon_avg_hist <- vecCapacity %>%
        mutate(period = case_when(year %in% c(1986:2005) ~ "1986-2005")) %>%
        filter(is.na(period) == F) %>%
        group_by(station,month) %>%
        summarise(tmax = mean(max_temp, na.rm = TRUE),
                  tmin = mean(min_temp, na.rm = TRUE),
                  tmean = mean(avg_temp, na.rm = TRUE))
# rcp
obs_mon_avg_rcp <- vecCapacity %>%
        mutate(period = case_when(year %in% c(2006:2015) ~ "2006-2015")) %>%
        filter(is.na(period) == F) %>%
        group_by(station,month,period) %>%
        summarise(tmax = mean(max_temp, na.rm = TRUE),
                  tmin = mean(min_temp, na.rm = TRUE),
                  tmean = mean(avg_temp, na.rm = TRUE))

# Calculate monthly temp for ISIMIP models 
isimip_month_Avg_hist <- hist.sites.df %>%
        mutate(period = case_when(year %in% c(1986:2005) ~ '1986-2005')) %>%
        filter(is.na(period) == F) %>%
        group_by(gcm, station, month) %>%
        summarise(tmax = mean(maxTemp, na.rm = TRUE),
                  tmin = mean(minTemp, na.rm = TRUE),
                  tmean = mean(meanTemp, na.rm = TRUE))


# Plot temp for observed and ISIMIP ---------------------------------------
tmax <- ggplot() +
        geom_line(isimip_month_Avg_hist, mapping = aes(x=month,y=tmax,color=gcm))+
        facet_wrap( ~ station)+
        geom_line(obs_mon_avg_hist, mapping = aes(month, tmax), linetype = "dashed")+
        scale_x_continuous(breaks = round(seq(min(isimip_month_Avg_hist$month),
                                              max(isimip_month_Avg_hist$month), 
                                              by = 1),1),
                           labels = c("J","F","M","A","M","J","J","A","S",
                                      "O","N","D"))+
        labs(x = "Month", y = "Maximum temperature (°C)")+
        theme_bw() +
        theme(legend.title = element_blank(),
              legend.text = element_text(size=8),
              legend.position="top",
              axis.text.x = element_text(size=8),
              axis.text.y = element_text(size=8))

print(tmax)

if (saveResults) {
        ggsave(filename = "tmax_validation_isimip_v2.tiff", plot = tmax, 
               device = "tiff", path = file.path(basepath, "outputs", "Figures_2021_VC_paper"), 
               width = 11.69, height = 8.27 , units = "in", dpi = 600) 
}

tmin <- ggplot() +
        geom_line(isimip_month_Avg_hist, mapping = aes(x=month,y=tmin,color=gcm))+
        facet_wrap( ~ station)+
        geom_line(obs_mon_avg_hist, mapping = aes(month, tmin), linetype = "dashed")+
        scale_x_continuous(breaks = round(seq(min(isimip_month_Avg_hist$month),
                                              max(isimip_month_Avg_hist$month), 
                                              by = 1),1),
                           labels = c("J","F","M","A","M","J","J","A","S",
                                      "O","N","D"))+
        labs(x = "Month", y = "Minimum temperature (°C)")+
        theme_bw() +
        theme(legend.title = element_blank(),
              legend.text = element_text(size=8),
              legend.position="top",
              axis.text.x = element_text(size=8),
              axis.text.y = element_text(size=8))

print(tmin)

if (saveResults) {
        ggsave(filename = "tmin_validation_isimip_v2.tiff", plot = tmin, device = "tiff", 
               path = file.path(basepath, "outputs", "Figures_2021_VC_paper"), 
               width = 11.69, height = 8.27, units = "in", dpi = 600) 
}

tmean <-  ggplot() +
        geom_line(isimip_month_Avg_hist, mapping = aes(x=month,y=tmean,color=gcm))+
        facet_wrap( ~ station)+
        geom_line(obs_mon_avg_hist, mapping = aes(month, tmean), linetype = "dashed")+
        scale_x_continuous(breaks = round(seq(min(isimip_month_Avg_hist$month),
                                              max(isimip_month_Avg_hist$month), 
                                              by = 1),1),
                           labels = c("J","F","M","A","M","J","J","A","S",
                                      "O","N","D"))+
        labs(x = "Month", y = "Mean temperature (°C)")+
        theme_bw() +
        theme(legend.title = element_blank(),
              legend.text = element_text(size=8),
              legend.position="top",
              axis.text.x = element_text(size=8),
              axis.text.y = element_text(size=8))

print(tmean)

if (saveResults) {
        ggsave(filename = "tmean_validation_isimip_v2.tiff", plot = tmean, device = "tiff", 
               path = file.path(basepath, "outputs", "Figures_2021_VC_paper"), 
               width = 11.69, height = 8.27, units = "in", dpi = 600) 
}

# Plot annual VC for ISIMIP data--------------------------------------------------

# Take yearly average
sites_Avg <- hist.sites.df %>% group_by(division, rcp, gcm, year) %>%
        summarise(VC = mean(VC, na.rm = TRUE)) %>%
        dplyr::select(division, rcp,gcm,year,everything())


# calculate division wise average VC for 1975-2015 to be added as horizontal line
div_avg <-  vecCapacity %>% 
        filter(year %in% c(1986:2005)) %>%
        group_by(division) %>%
        summarise(mean = mean(averageVC,na.rm = TRUE)) %>%
        mutate(rcp = "Historical")

# calculate mean of four gcms over the RCM period to be plotted as mean
av_line <- sites_Avg %>% group_by(division,year,rcp) %>%
        summarise(mean = mean(VC)) %>%
        filter(rcp != "Historical")

# plot         
g0 <- ggplot(sites_Avg ) +
        geom_line(aes(x=year,y=VC, group=gcm, color=gcm))+
        labs(x = "Year",y = "Vectorial Capacity (per day)")+
        geom_line(data = av_line, aes(x=year,y=mean)) +
        facet_grid(division ~ rcp, scales = "free_x", space = "free_x")+
        geom_hline(data = div_avg, aes(yintercept = mean), linetype="dashed")+
        scale_x_continuous(breaks = seq(1950,2100,10)) +
        theme_bw() +
        theme(legend.title = element_blank(),
              legend.text = element_text(size=8),
              legend.position="top",
              axis.text.x = element_text(size=8),
              axis.text.y = element_text(size=8))

print(g0)
if (saveResults) {
        ggsave(filename = "VC_divisions_ISIMIP_v6.tiff", plot = g0, 
               device = "tiff", path = file.path(basepath, "outputs", "Figures_2021_VC_paper"), 
               width = 8.27, height = 11.69, units = "in", dpi = 600) 
}

# Plot annual cycle/seasonality of VC for ISIMIP data----------------------

# Take monthly average
sites_month_Avg <- hist.sites.df %>%
        mutate(period = case_when(year %in% c(1986:2005) ~ '1986-2005',
                                  year %in% c(2030:2049) ~ '2030-2049',
                                  year %in% c(2080:2099) ~ '2080-2099')) %>%
        filter(is.na(period) == F) %>%
        group_by(division, period, rcp, gcm, month) %>%
        summarise(VC = mean(VC, na.rm = TRUE)) %>%
        dplyr::select(division, period,gcm,month,everything())

# Observed
mon_avg <- vecCapacity %>%
        filter(year %in% c(1986:2005)) %>%
        group_by(division,month) %>%
        summarise(VC = mean(averageVC, na.rm = TRUE))


g1 <- ggplot() +
        geom_line(sites_month_Avg, mapping = aes(x=month,y=VC, color=gcm))+
        labs(x = "Month", y = "Vectorial Capacity (per day)")+
        facet_grid(division ~ rcp + period)+
        scale_x_continuous(breaks = round(seq(min(sites_month_Avg$month), 
                                              max(sites_month_Avg$month), 
                                              by = 1),1),
                           labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))+
        theme_bw() +
        theme(legend.title = element_blank(),
              legend.text = element_text(size=8),
              legend.position="top",
              axis.text.x = element_text(size=8),
              axis.text.y = element_text(size=8))+
        geom_line(mon_avg, mapping = aes(month, VC), linetype = "dashed")

print(g1)
if (saveResults) {
        ggsave(filename = "vc_seasonality_division_ISIMIP_twenty year_3.tiff", 
               plot = g1, device = "tiff", path = file.path(basepath, "outputs", "Figures_2021_VC_paper"), 
               width = 11.69, height = 8.27, units = "in", dpi = 600) 
}

## 2020-2039 Supplementary figure
sites_month_Avg <- hist.sites.df %>%
        mutate(period = case_when(year %in% c(1986:2005) ~ '1986-2005',
                                  year %in% c(2020:2039) ~ '2020-2039')) %>%
        filter(is.na(period) == F) %>%
        group_by(division, period, rcp, gcm, month) %>%
        summarise(VC = mean(VC, na.rm = TRUE)) %>%
        dplyr::select(division, period,gcm,month,everything())

sites_month_Avg$rcp[sites_month_Avg$rcp=="historical"] <- "Historical"

# Observed
mon_avg <- vecCapacity %>%
        filter(year %in% c(1986:2005)) %>%
        group_by(division,month) %>%
        summarise(VC = mean(averageVC, na.rm = TRUE))

g2 <- ggplot() +
        geom_line(sites_month_Avg, mapping = aes(x=month,y=VC, color=gcm))+
        labs(x = "Month",
             y = "Vectorial Capacity (per day)",
             title = expression(paste("Monthly averaged VC of ",italic("Aedes aegypti")," at eight divisions of Bangladesh")),
             subtitle = "Dashed line: division-specific monthly average VC calculated with observed data, 1986-2005")+
        facet_grid(division ~ period + rcp)+
        scale_x_continuous(breaks = round(seq(min(sites_month_Avg$month), 
                                              max(sites_month_Avg$month), 
                                              by = 1),1),
                           labels = c("J","F","M","A","M","J","J","A","S",
                                      "O","N","D"))+
        theme_bw() +
        theme(legend.title = element_blank(),
              legend.text = element_text(size=8),
              legend.position="top",
              axis.text.x = element_text(size=8),
              axis.text.y = element_text(size=8))+
        geom_line(mon_avg, mapping = aes(month, VC), linetype = "dashed")

print(g2)
if (saveResults) {
        ggsave(filename = "vc_seasonality_division_ISIMIP_2020-39.tiff", 
               plot = g2, device = "tiff", path = file.path(basepath, "outputs", "Figures_2021_VC_paper"), 
               width = 8.27, height = 11.69 , units = "in", dpi = 600) 
}



