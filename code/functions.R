#' Sinusoidal temperature
#' 
#' Calculates temperature for different time points as sinusoidal distribution
#' 
#' @param t Time from 0 to 47 to signify differences in 30 mins interval.
#' @param avg_temp Daily average temperature in celsius.
#' @param dtr Difference between max and min temperature in celsius (also 
#' called diurnal temperature range)
#' 
#' @return returns a temperature value accounting for average temp and DTR

Temp <- function(t, avg_temp, dtr) {
        temp <- avg_temp + dtr*sin(t*(pi/24))/2
        return(temp)
}

#' list to df
#' 
#' Convert a list to dataframe
#' 
#' @param lis the list to be converted
#' @return ...
#' 
listTodf <- function(lis){
        lis <- lis %>% # convert the list to a dataframe
                unlist(recursive = FALSE) %>% 
                enframe() 
}

#' kelvin to celsius
#' 
#' Convert temperature from kelvin to celsius
#' 
#'  @param temp temperature value in kelvin
#'  @return temperature value in celsius
#'   
KelvinCelsius <- function(temp){
        tempincelsius <- temp - 273.15
        return(tempincelsius)
}

#' createDate
#' 
#' function to create year month day from date variable
#' 
#' @param df Data frame with a date variable
#' @return Date variable within df data frame split into year, month and day
createDate <- function(df){
        df <-  df %>% mutate(year = format(as.Date(df$date), "%Y"),
                             month = format(as.Date(df$date), "%m"),
                             day = format(as.Date(df$date), "%d"))
}

## function to plot values
# df: data frame to plot
graph <- function(df){
        ggplot(df) +
                scale_x_continuous(breaks = round(seq(min(df$month), max(df$month), by = 1),1),
                                   labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
                theme_bw() +
                theme(legend.title = element_blank())
}

