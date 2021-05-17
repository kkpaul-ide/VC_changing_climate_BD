## observed_data.R 

# Script to calculate VC with observed temperature data 
# and associated plots 

# Kishor Kumar Paul

# Set-up ------------------------------------------------------------------
# Restart R and source to file directory

#check/install/load packages
mypackages<-c("tidyverse", "cowplot", "Kendall")

for (p in mypackages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

# Organize directories
basepath <- getwd()

# Source specific functions
source(file.path(basepath, "code", "vc_estimation.R"))
source(file.path(basepath, "code", "functions.R"))

# Script parameters -------------------------------------------------------
# To be changed by user
reload <- FALSE
saveResults <- FALSE

# Calculate VC with observed temp data models------------------------------
if (reload) {
  observedVC <- read.csv(file.path(basepath, "outputs", "vec_capacity.csv"))
} else {
  # Read daily maximum, minimum, average and dtr data
  daily_temperature <- read.csv(file.path(basepath, "data", 
                                          "daily_temperature.csv"))
  daily_temperature$vc <- 0
  daily_temperature$averageVC <- 0
  
  for (ii in 1:483491) {
    # Use Temp function from functions.R
    sinusoidal_temperature <- Temp(0:47, daily_temperature$avg_temp[ii], 
                                   daily_temperature$dtr[ii])
    daily_temperature$averageVC[ii] <- mean(VectorialCapacity(sinusoidal_temperature))
    daily_temperature$vc[ii] <- VectorialCapacity(daily_temperature$avg_temp[ii])
  }
  
  # allocate division to weather stations in observed data
  observedVC <- daily_temperature %>%
    mutate(division = case_when(station %in% c("Barisal","Bhola","Khepupara", 
                                               "Patuakhali") ~ "Barisal",
                                station %in% c("Ambagan(Ctg)","Chandpur",
                                               "Chittagong", "Comilla",
                                               "Cox's Bazar", "Feni", "Hatiya",
                                               "Kutubdia", "M.court", "Rangamati",
                                               "Sandwip", "Sitakunda", 
                                               "Teknaf") ~ "Chittagong",
                                station %in% c("Dhaka", "Faridpur",
                                               "Madaripur", "Tangail") ~ "Dhaka",
                                station %in% c("chuadanga", "Jessore", "Khulna", 
                                               "Mongla", "Satkhira") ~ "Khulna",
                                station %in% c("Mymensingh") ~ "Mymensingh",
                                station %in% c("Ishurdi", "Rajshahi",
                                               "Bogra") ~ "Rajshahi",
                                station %in% c("sydpur", "Dinajpur",
                                               "Rangpur") ~ "Rangpur",
                                station %in% c("Sylhet","Srimangal") ~ "Sylhet"))
  
  # save dataframe as a csv file
  if (saveResults) {
    write.csv(observedVC, file = file.path(basepath, "outputs", "vec_capacity.csv"), 
              row.names = FALSE)
  }
}

##Validation of station data with Shamsuddin Shahid paper------------------

#Take monthly average for 17 stations (Ref Shahid et al. 2010)

stations <- c("Sylhet", "Srimangal", "Comilla", "Rangamati", "Chittagong",
              "Cox's Bazar", "M.court", "Faridpur", "Dhaka", "Mymensingh",
              "Khulna", "Barisal", "Satkhira", "Jessore","Bogra",
              "Dinajpur","Rangpur")

# take averages for each month of each year
avgValues <- observedVC %>% 
  filter(year < 2016) %>% 
  group_by(year, month) %>% 
  summarise(mean = mean(avg_temp, na.rm = TRUE),
            min = mean(min_temp, na.rm = TRUE),
            max = mean(max_temp, na.rm = TRUE),
            dtr = mean(dtr, na.rm=TRUE),
            avg_vc = mean(averageVC, na.rm = TRUE)) %>%
  mutate(ym = year + month/12) %>%
  select(year, month, ym, everything())

# convert data from wide to long; for easier plotting of three types of 
# temperature in single plot
avgValues_long <- avgValues %>%
  gather(variable, value, -c(year,month,ym))

# Plot monthly averaged maximum, mean and minimum temperature
var <- c("mean","max","min")

allTemp <- ggplot(avgValues_long %>% 
                    filter(variable %in% var), aes(x=ym,y=value))+
  geom_line(aes(linetype=variable))+
  coord_cartesian(xlim = c(1975, 2015),
                  ylim = c(9, 35))+
  scale_linetype_manual(labels = c("Maximum", "Mean", "Minimum"),
                        values=c("longdash","solid", "dotted"))+
  labs(x = "Year",
       y = "Temperature (°C)")+
  theme(legend.title = element_blank(),
        legend.position="top",
        legend.text = element_text(size=12),
        panel.background=element_blank(),
        plot.background=element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))

print(allTemp)

if (saveResults) {
  ggsave(filename = "time_series_observed_v2.tiff", plot = allTemp, 
         device = "tiff", 
         path = file.path(basepath, "outputs", "2021 03")) 
}
#####

## Prepare annual trend plot ----------------------------------------------
##Take yearly average for all stations
avgYrValues <- observedVC %>% 
  group_by(year) %>% 
  summarise(mean = mean(avg_temp, na.rm = TRUE),
            min = mean(min_temp, na.rm = TRUE),
            max = mean(max_temp, na.rm = TRUE),
            dtr = mean(dtr, na.rm=TRUE),
            avg_vc = mean(averageVC, na.rm = TRUE)) %>%
  select(year, everything())

# the Mann-Kendall trend test
avgVC <- avgYrValues$avg_vc
MannKendall(avgVC)

# Plot yearly averaged mean temperature
meanTemp <- ggplot(avgYrValues, aes(x=year,y=mean)) +
  geom_line()+
  coord_cartesian(xlim = c(1975, 2015),
                  ylim = c(24.5, 26.5))+
  geom_smooth(method = "lm", se = FALSE)+
  labs(x = "Year",
       y = "Temperature (°C)",
       title = "Yearly averaged mean temperature, 1975-2015")+
  theme_bw()+
  annotate("text", x = 1980, y = 24.6, label = "Intercept:")+
  annotate("text", x = 1995, y = 24.6, label = "Slope:")+
  annotate("text", x = 1983, y = 24.6, 
           label = round(coef(lm(mean ~ year, data = avgYrValues))[1], digits = 3))+
  annotate("text", x = 1998, y = 24.6, 
           label = round(coef(lm(mean ~ year, data = avgYrValues))[2], digits = 3))+
  annotate("text", x = 2006, y = 24.6, label = "R^2 = -0.02")
  
# fit <- (lm(mean ~ year, data = avgYrValues))
# summary(fit)

# Plot yearly averaged DTR
dtr <- ggplot(avgYrValues, aes(x=year,y=dtr)) +
  geom_line()+
  coord_cartesian(xlim = c(1975, 2015),
                  ylim = c(8, 10.5))+
  geom_smooth(method = "lm", se = FALSE)+
  labs(x = "Year",
       y = "DTR (°C)",
       title = "Yearly averaged diurnal temperature range, 1975-2015")+
  theme_bw()+
  annotate("text", x = 1980, y = 8.3, label = "Intercept:")+
  annotate("text", x = 1995, y = 8.3, label = "Slope:")+
  annotate("text", x = 1983, y = 8.3, 
           label = round(coef(lm(dtr ~ year, data = avgYrValues))[1],digits = 3))+
  annotate("text", x = 1998, y = 8.3, 
           label = round(coef(lm(dtr ~ year, data = avgYrValues))[2],digits = 3))+
  annotate("text", x = 2006, y = 8.3, label = "R^2 = 0.29")

# fit <- (lm(dtr ~ year, data = avgYrValues))
# summary(fit)

# Plot yearly averaged VC
avgVC <- ggplot(avgYrValues, aes(x=year, y=avg_vc)) +
  geom_line()+
  coord_cartesian(xlim = c(1975, 2015),
                  ylim = c(0.6, 1.2))+
  geom_smooth(method = "lm", se = FALSE)+
  labs(x = "Year",
       y = "VC (per day)",
       title = "Yearly averaged vectorial capacity of Aedes aegypti mosquitoes, 1975-2015")+
  theme_bw()+
  annotate("text", x = 1980, y = 0.7, label = "Intercept:")+
  annotate("text", x = 1995, y = 0.7, label = "Slope:")+
  annotate("text", x = 1983, y = 0.7, 
           label = round(coef(lm(avg_vc ~ year, data = avgYrValues))[1], digits = 3))+
  annotate("text", x = 1998, y = 0.7, 
           label = round(coef(lm(avg_vc ~ year, data = avgYrValues))[2], digits = 3))+
  annotate("text", x = 2006, y = 0.7, label = "R^2 = 0.40")

# fit <- (lm(avg_vc ~ year, data = avgYrValues))
# summary(fit)

plot_grid(meanTemp,dtr,avgVC,nrow = 3, labels = c("A","B","C"))

if (saveResults) {
  ggsave(filename = "Observed_t_dtr_vc.tiff", device = "tiff", 
         path = file.path(basepath, "outputs", "2021 03")) 
}
###

# calculate division-wise annual VC----------------------------------------
div_Avg <- observedVC %>% 
  group_by(division, year) %>%
  summarise(mean = mean(avg_temp, na.rm = TRUE),
            min = mean(min_temp, na.rm = TRUE),
            max = mean(max_temp, na.rm = TRUE),
            dtr = mean(dtr, na.rm=TRUE),
            vc = mean(averageVC, na.rm = TRUE))