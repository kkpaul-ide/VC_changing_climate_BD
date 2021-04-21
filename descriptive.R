## descriptive.R 

# Script to extract descriptive stats from observed and ISIMIP data 

# Kishor Kumar Paul

# Set-up ------------------------------------------------------------------
# Restart R and source to file directory

#check/install/load packages
mypackages<-c("dplyr", "ggplot2", "pastecs")

for (p in mypackages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

# Organize directories
basepath <- getwd()

# show number in ordinary format
options(scipen = 999)

# load observed VC
observedVC <- read.csv(file.path(basepath, "data","vec_capacity.csv"), header = TRUE)

# Create season variable
observedVC <- observedVC %>%
        mutate(season = case_when(month %in% c(12,1,2) ~ 'Winter/Dry',
                                  month %in% c(3,4,5) ~ 'Pre-monsoon',
                                  month %in% c(6,7,8) ~ 'Monsoon',
                                  month %in% c(9,10,11) ~ 'Post-monsoon'))


# take seasonal average over the years
div_Avg <- observedVC %>% 
        group_by(season, year) %>%
        summarise(mean = mean(avg_temp, na.rm = TRUE),
                  min = mean(min_temp, na.rm = TRUE),
                  max = mean(max_temp, na.rm = TRUE),
                  dtr = mean(dtr, na.rm=TRUE),
                  vc = mean(averageVC, na.rm = TRUE))

# plot seasonal VC
ggplot(div_Avg, mapping = aes(x=year,y=dtr)) +
  geom_line()+
  geom_smooth(method = "lm", se = FALSE)+
  facet_grid(season ~ .)

# Explore fit for a division
fit <- (lm(vc ~ year, data = div_Avg %>% filter(division == "Sylhet")))
summary(fit)


# load isimip data
load(file.path(basepath, "data", "ISIMIP_data", "districts_isimip.RData"))

# Create season variable
hist.sites.df$season[hist.sites.df$month %in% c(12,1,2)] <- "Winter"
hist.sites.df$season[hist.sites.df$month %in% c(3,4,5)] <- "Pre-monsoon"
hist.sites.df$season[hist.sites.df$month %in% c(6,7,8)] <- "Monsoon"
hist.sites.df$season[hist.sites.df$month %in% c(9,10,11)] <- "Post-monsoon"

# Take monthly average
sites_month_Avg <- hist.sites.df %>%
        group_by(division, rcp, gcm, month) %>%
        summarise(VC = mean(VC, na.rm = TRUE)) %>%
        dplyr::select(division, rcp,gcm,month,everything())

# explore VC by rcp scenario for each month
by(sites_month_Avg$VC, sites_month_Avg$month, summary)
by(sites_month_Avg$VC, sites_month_Avg$rcp, summary)
by(sites_month_Avg$VC[sites_month_Avg$month==1], sites_month_Avg$rcp[sites_month_Avg$month==1], summary)
by(sites_month_Avg$VC[sites_month_Avg$month==1], sites_month_Avg$rcp[sites_month_Avg$month==1], summary)

# Take yearly average
sites_Avg <- hist.sites.df %>% 
        filter(year %in% c(1996:2015)) %>%
        group_by(year, gcm, rcp, division) %>%
        summarise(VC = mean(VC, na.rm = TRUE)) %>%
        dplyr::select(gcm,year,everything())

by(sites_Avg$VC[sites_Avg$rcp=="historical"], sites_Avg$division[sites_Avg$rcp=="historical"], summary)
by(sites_Avg$VC[sites_Avg$rcp=="RCP 4.5"], sites_Avg$division[sites_Avg$rcp=="RCP 4.5"], summary)
##
