## plot_maps_contour.R 

# Script to plot background map, VC change map and 
# calculate VC for a range of temp and dtr to create contour plot. 

# Kishor Kumar Paul

# Set-up ------------------------------------------------------------------
# Restart R and source to file directory
rm(list = ls())

#check/install/load packages
mypackages<-c( "raster", "RColorBrewer","grid", "stringr", "viridis",
              "tidyverse", "reshape2", "rgdal","cowplot", "readr")

for (p in mypackages){
        if(!require(p, character.only = TRUE)){
                install.packages(p)
                library(p, character.only = TRUE)
        }
}

# Organize directories
basepath <- getwd()
# set path and shape file name
shp_path <- file.path(basepath, "data", "bgd_adm_bbs_20201113_shp")
shp_name <- "bgd_admbnda_adm1_bbs_20201113"

# Source specific functions
source(file.path(basepath, "code", "functions.R"))
source(file.path(basepath, "code", "vc_estimation.R"))

# Script parameters -------------------------------------------------------
# To be changed by user
saveResults <- FALSE

# Background map-----------------------------------------------------------
# read shapefile
bd_shp <- readOGR(dsn = shp_path,
              layer = shp_name)

bd_tidy <- broom::tidy(bd_shp)

# allocate an id variable to the sp data
bd_shp@data$id <- rownames(bd_shp@data)

# join the data
bd <- left_join(bd_tidy,bd_shp@data)

# Read weather station data
weather_stations <- read_tsv(file.path(basepath, "data", "locationStations.txt"), 
                             col_names = TRUE)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for division's names
div.lab.data <- bd %>%
        group_by(ADM1_EN) %>%
        summarise(long = mean(long), lat = mean(lat))

plot <- ggplot(data = bd, aes(x = long, y = lat)) +
        geom_polygon(aes(group = group, fill = ADM1_EN)) +
        geom_text(aes(label = ADM1_EN), data = div.lab.data, size = 4, 
                  vjust = 0.95, hjust = 0.5 )+
        scale_fill_grey()+
        theme_void()+
        theme(legend.position = "none")+
        geom_point(weather_stations, mapping = aes(x=lon,y=lat,group= NA),
                   fill = "blue", alpha = 1, size = 2, shape = 21)+
        ggsn::scalebar(data=bd,location = "bottomleft",dist = 100,transform = TRUE, dist_unit = "km",
                       model='WGS84', height = 0.01,anchor = c(x = 88, y = 21))+
        ggsn::north(data = bd, location = "bottomleft", scale = 0.15, 
                    anchor = c(x = 90.7, y = 20.8))+
        #labs(title = "Divisions of Bangladesh with weather station locations")+
        annotate(geom="text", x=91, y=25.6, label="Weather stations",
                 color="black")+
        annotate("point", x=90.3, y=25.6, fill = "blue", alpha = 1, size = 2, 
                 shape = 21 )

plot + theme(panel.background = element_rect(fill = NA),
        #plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(fill = NA, colour = "black", size = 1),
        )

if (saveResults) {
        ggsave(file.path(basepath, "outputs", "2021 02", "bd_map_3.tiff")) 
}

# Spatial distribution of VC maps------------------------------------------
# load observed VC
vecCapacity <- read.csv(file.path(basepath, "data", "vec_capacity.csv"),
                        header = TRUE)

# take mean of observed VC over selected years
obsVC <- vecCapacity %>%
        filter(year %in% c(1986:2005)) %>%
        group_by(division) %>%
        summarise(obVC = mean(averageVC, na.rm = TRUE)) %>%
        rename(ADM1_EN = division) 

# load isimip data
load(file.path(basepath, "data", "ISIMIP_data", "districts_isimip.RData"))

# take average for three twenty year periods after 
# creating a variable with three different periods
dec_Avg <- hist.sites.df %>% 
        mutate(period = case_when(year %in% c(2020:2039) ~ '2020-2039', 
                                  year %in% c(2050:2069) ~ '2050-2069',
                                  year %in% c(2080:2099) ~ '2080-2099')) %>%
        filter(is.na(period) == F) %>% # leave those outside the selected periods
        group_by(division, rcp, gcm, period) %>%
        summarise(VC = mean(VC, na.rm = TRUE)) %>% # Calculate average 
        rename(ADM1_EN = division) # rename division column to ADM1_EN
        
# Join observed values & take difference
dec_Avg1 <- left_join(dec_Avg,obsVC) %>%
        mutate(diffVC = VC - obVC)

# Join vc data with sp data
bddf <- left_join(bd,dec_Avg1)

# preparing the plot
g <- ggplot(data = bddf, aes(x = long, y = lat, group = group, fill = diffVC)) +
        geom_polygon() +
        geom_path(color = "black", size = 0.05) +
        coord_equal() +
        facet_grid(gcm ~ rcp + period)+
        scale_fill_gradient2(low = "blue", mid = "grey", high = "red", # colors
                             midpoint = 0)+
        theme_void()+
        theme(strip.text.y = element_text(angle = 270))+
        labs(
             # title = "Change in VC of Aedes aegypti over decades at eight divisions of Bangladesh",
             # subtitle = "In reference to division specific observed VC for 1986-2005",
             fill = "Change\nof\nVC")

# add the vertical line separating RCP 4.5 and 8.5 scenarios
grid::grid.draw(linesGrob(x = unit(c(0.459, 0.459), "npc"), 
                          y = unit(c(0.02, 0.92), "npc")))

if (saveResults) {
        ggsave(file.path(basepath, "outputs", "2021 03", "spatial_decades_VC_v3.tiff"),
               width = 8.27, height = 11.69, units = "in", dpi = 600) # vertical line is not being saved
}

# Contour plot ------------------------------------------------------------
# Prepare data for contour
temp <- seq(5,45,by=0.1)
dtr <- seq(0,25,by=1)
vc <- vector("list", 26)

for (k in 1:26){
        for (i in 1:401) {
                sinusoidal_temperature <- Temp(0:47, temp[i], dtr[k])
                vc[[k]][i] <- mean(VectorialCapacity(sinusoidal_temperature))
        }
}

names(vc) <- c("DTR_0","DTR_1","DTR_2","DTR_3","DTR_4","DTR_5","DTR_6","DTR_7","DTR_8",
               "DTR_9","DTR_10","DTR_11","DTR_12","DTR_13","DTR_14","DTR_15",
               "DTR_16","DTR_17","DTR_18","DTR_19","DTR_20","DTR_21","DTR_22","DTR_23","DTR_24","DTR_25")

dat1 <- data.frame(vc)
dat <- data.frame(temp,dat1)
dat <- melt(data = dat, id.vars = "temp")
names(dat) <- c("Temperature","DTR","VC")
dat$DTR <- as.numeric(str_sub(dat$DTR, str_locate(dat$DTR, '_')[1,1] + 1))

# Contour plot using ISIMIP data
# Take monthly average 
sites_month_Avg <- hist.sites.df %>%
        mutate(period = case_when(year %in% c(2020:2039) ~ '2020-39',
                                  year %in% c(2050:2069) ~ '2050-69',
                                  year %in% c(2080:2099) ~ '2080-99')) %>%
        filter(is.na(period)==F) %>%
        group_by(division, rcp, gcm, period, month) %>%
        summarise(Temperature = mean(meanTemp, na.rm = TRUE),
                  DTR = mean(dtr, na.rm=TRUE),
                  VC = mean(VC, na.rm = TRUE)) %>%
        dplyr::select(rcp,gcm,period, month,everything())

# Create season variable
sites_month_Avg <- sites_month_Avg %>%
        mutate(season = case_when(month %in% c(12,1,2) ~ 'Winter/Dry',
                                  month %in% c(3,4,5) ~ 'Pre-monsoon',
                                  month %in% c(6,7,8) ~ 'Monsoon',
                                  month %in% c(9,10,11) ~ 'Post-monsoon'))


g1 <- ggplot(dat, aes(x = Temperature, y = DTR, z = VC)) +
        stat_contour(geom = "polygon", aes(fill = ..level..)) +
        geom_tile(aes(fill = VC)) +
        stat_contour(bins = 15) +
        xlab("Temperarue") +
        ylab("DTR") +
        guides(fill = guide_colorbar(title = "VC"))+
        scale_fill_viridis()+
        geom_point(sites_month_Avg %>% 
                           filter(rcp=="RCP 8.5"),mapping = aes(x=Temperature, y=DTR, group=season, col = season),alpha=0.7)+
        scale_color_manual(values = c("red","pink","orange","black"))+
        facet_grid(gcm~period)+
        # labs(title= "Theoretical contour of mean temperature, DTR, and VC",
        #      subtitle = "Superimposed with division and month specific VC at three time periods",
        #      col=element_blank())+
        theme(panel.background = element_blank())

if (saveResults) {
        ggsave(filename = "Contour plot_2.tiff", plot = g1, device = "tiff", 
               path = file.path(basepath, "outputs", "2021 02"), 
               width = 8.27, height = 7.5, units = "in", dpi = 600) 
}


