## date_df_deccma.R 

# Script to create date df for three DECCMA models to be merged with
# district and temperature df. 

# Kishor Kumar Paul

#check/install/load packages
mypackages <- c("dplyr")

for (p in mypackages){
        if(!require(p, character.only = TRUE)){
                install.packages(p)
                library(p, character.only = TRUE)
        }
}

## function to create year month day from date variable
# df: data frame with a date variable
# date: date variable within df data frame that needs to be split into year, month and day
createDate <- function(df){
        df <-  df %>% mutate(year = format(as.Date(df$date), "%Y"),
                             month = format(as.Date(df$date), "%m"),
                             day = format(as.Date(df$date), "%d"))
}
# date variable for CNRM-CM5
date_cnrm_cm5 <- data.frame(
        date = c(rep(seq(as.Date("1960/1/1"), as.Date("1960/12/31"), "days"),each=64),
                 rep(seq(as.Date("1961/1/1"), as.Date("1961/12/31"), "days"),each=64),
                 rep(seq(as.Date("1962/1/1"), as.Date("1962/12/31"), "days"),each=64),
                 rep(seq(as.Date("1963/1/1"), as.Date("1963/12/31"), "days"),each=64),
                 rep(seq(as.Date("1964/1/1"), as.Date("1964/12/31"), "days"),each=64),
                 rep(seq(as.Date("1965/1/1"), as.Date("1965/12/31"), "days"),each=64),
                 rep(seq(as.Date("1966/1/1"), as.Date("1966/12/31"), "days"),each=64),
                 rep(seq(as.Date("1967/1/1"), as.Date("1967/12/31"), "days"),each=64),
                 rep(seq(as.Date("1968/1/1"), as.Date("1968/12/31"), "days"),each=64),
                 rep(seq(as.Date("1969/1/1"), as.Date("1969/12/31"), "days"),each=64),
                 rep(seq(as.Date("1970/1/1"), as.Date("1970/12/31"), "days"),each=64),
                 rep(seq(as.Date("1971/1/1"), as.Date("1971/12/31"), "days"),each=64),
                 rep(seq(as.Date("1972/1/1"), as.Date("1972/12/31"), "days"),each=64),
                 rep(seq(as.Date("1973/1/1"), as.Date("1973/12/31"), "days"),each=64),
                 rep(seq(as.Date("1974/1/1"), as.Date("1974/12/31"), "days"),each=64),
                 rep(seq(as.Date("1975/1/1"), as.Date("1975/12/31"), "days"),each=64),
                 rep(seq(as.Date("1976/1/1"), as.Date("1976/12/31"), "days"),each=64),
                 rep(seq(as.Date("1977/1/1"), as.Date("1977/12/31"), "days"),each=64),
                 rep(seq(as.Date("1978/1/1"), as.Date("1978/12/31"), "days"),each=64),
                 rep(seq(as.Date("1979/1/1"), as.Date("1979/12/31"), "days"),each=64),
                 rep(seq(as.Date("1980/1/1"), as.Date("1980/12/31"), "days"),each=64),
                 rep(seq(as.Date("1981/1/1"), as.Date("1981/12/31"), "days"),each=64),
                 rep(seq(as.Date("1982/1/1"), as.Date("1982/12/31"), "days"),each=64),
                 rep(seq(as.Date("1983/1/1"), as.Date("1983/12/31"), "days"),each=64),
                 rep(seq(as.Date("1984/1/1"), as.Date("1984/12/31"), "days"),each=64),
                 rep(seq(as.Date("1985/1/1"), as.Date("1985/12/31"), "days"),each=64),
                 rep(seq(as.Date("1986/1/1"), as.Date("1986/12/31"), "days"),each=64),
                 rep(seq(as.Date("1987/1/1"), as.Date("1987/12/31"), "days"),each=64),
                 rep(seq(as.Date("1988/1/1"), as.Date("1988/12/31"), "days"),each=64),
                 rep(seq(as.Date("1989/1/1"), as.Date("1989/12/31"), "days"),each=64),
                 rep(seq(as.Date("1990/1/1"), as.Date("1990/12/31"), "days"),each=64),
                 rep(seq(as.Date("1991/1/1"), as.Date("1991/12/31"), "days"),each=64),
                 rep(seq(as.Date("1992/1/1"), as.Date("1992/12/31"), "days"),each=64),
                 rep(seq(as.Date("1993/1/1"), as.Date("1993/12/31"), "days"),each=64),
                 rep(seq(as.Date("1994/1/1"), as.Date("1994/12/31"), "days"),each=64),
                 rep(seq(as.Date("1995/1/1"), as.Date("1995/12/31"), "days"),each=64),
                 rep(seq(as.Date("1996/1/1"), as.Date("1996/12/31"), "days"),each=64),
                 rep(seq(as.Date("1997/1/1"), as.Date("1997/12/31"), "days"),each=64),
                 rep(seq(as.Date("1998/1/1"), as.Date("1998/12/31"), "days"),each=64),
                 rep(seq(as.Date("1999/1/1"), as.Date("1999/12/31"), "days"),each=64),
                 rep(seq(as.Date("2000/1/1"), as.Date("2000/12/31"), "days"),each=64),
                 rep(seq(as.Date("2001/1/1"), as.Date("2001/12/31"), "days"),each=64),
                 rep(seq(as.Date("2002/1/1"), as.Date("2002/12/31"), "days"),each=64),
                 rep(seq(as.Date("2003/1/1"), as.Date("2003/12/31"), "days"),each=64),
                 rep(seq(as.Date("2004/1/1"), as.Date("2004/12/31"), "days"),each=64),
                 rep(seq(as.Date("2005/1/1"), as.Date("2005/12/31"), "days"),each=64),
                 rep(seq(as.Date("2006/1/1"), as.Date("2006/12/31"), "days"),each=64),
                 rep(seq(as.Date("2007/1/1"), as.Date("2007/12/31"), "days"),each=64),
                 rep(seq(as.Date("2008/1/1"), as.Date("2008/12/31"), "days"),each=64),
                 rep(seq(as.Date("2009/1/1"), as.Date("2009/12/31"), "days"),each=64),
                 rep(seq(as.Date("2010/1/1"), as.Date("2010/12/31"), "days"),each=64),
                 rep(seq(as.Date("2011/1/1"), as.Date("2011/12/31"), "days"),each=64),
                 rep(seq(as.Date("2012/1/1"), as.Date("2012/12/31"), "days"),each=64),
                 rep(seq(as.Date("2013/1/1"), as.Date("2013/12/31"), "days"),each=64),
                 rep(seq(as.Date("2014/1/1"), as.Date("2014/12/31"), "days"),each=64),
                 rep(seq(as.Date("2015/1/1"), as.Date("2015/12/31"), "days"),each=64),
                 rep(seq(as.Date("2016/1/1"), as.Date("2016/12/31"), "days"),each=64),
                 rep(seq(as.Date("2017/1/1"), as.Date("2017/12/31"), "days"),each=64),
                 rep(seq(as.Date("2018/1/1"), as.Date("2018/12/31"), "days"),each=64),
                 rep(seq(as.Date("2019/1/1"), as.Date("2019/12/31"), "days"),each=64),
                 rep(seq(as.Date("2020/1/1"), as.Date("2020/12/31"), "days"),each=64),
                 rep(seq(as.Date("2021/1/1"), as.Date("2021/12/31"), "days"),each=64),
                 rep(seq(as.Date("2022/1/1"), as.Date("2022/12/31"), "days"),each=64),
                 rep(seq(as.Date("2023/1/1"), as.Date("2023/12/31"), "days"),each=64),
                 rep(seq(as.Date("2024/1/1"), as.Date("2024/12/31"), "days"),each=64),
                 rep(seq(as.Date("2025/1/1"), as.Date("2025/12/31"), "days"),each=64),
                 rep(seq(as.Date("2026/1/1"), as.Date("2026/12/31"), "days"),each=64),
                 rep(seq(as.Date("2027/1/1"), as.Date("2027/12/31"), "days"),each=64),
                 rep(seq(as.Date("2028/1/1"), as.Date("2028/12/31"), "days"),each=64),
                 rep(seq(as.Date("2029/1/1"), as.Date("2029/12/31"), "days"),each=64),
                 rep(seq(as.Date("2030/1/1"), as.Date("2030/12/31"), "days"),each=64),
                 rep(seq(as.Date("2031/1/1"), as.Date("2031/12/31"), "days"),each=64),
                 rep(seq(as.Date("2032/1/1"), as.Date("2032/12/31"), "days"),each=64),
                 rep(seq(as.Date("2033/1/1"), as.Date("2033/12/31"), "days"),each=64),
                 rep(seq(as.Date("2034/1/1"), as.Date("2034/12/31"), "days"),each=64),
                 rep(seq(as.Date("2035/1/1"), as.Date("2035/12/31"), "days"),each=64),
                 rep(seq(as.Date("2036/1/1"), as.Date("2036/12/31"), "days"),each=64),
                 rep(seq(as.Date("2037/1/1"), as.Date("2037/12/31"), "days"),each=64),
                 rep(seq(as.Date("2038/1/1"), as.Date("2038/12/31"), "days"),each=64),
                 rep(seq(as.Date("2039/1/1"), as.Date("2039/12/31"), "days"),each=64),
                 rep(seq(as.Date("2040/1/1"), as.Date("2040/12/31"), "days"),each=64),
                 rep(seq(as.Date("2041/1/1"), as.Date("2041/12/31"), "days"),each=64),
                 rep(seq(as.Date("2042/1/1"), as.Date("2042/12/31"), "days"),each=64),
                 rep(seq(as.Date("2043/1/1"), as.Date("2043/12/31"), "days"),each=64),
                 rep(seq(as.Date("2044/1/1"), as.Date("2044/12/31"), "days"),each=64),
                 rep(seq(as.Date("2045/1/1"), as.Date("2045/12/31"), "days"),each=64),
                 rep(seq(as.Date("2046/1/1"), as.Date("2046/12/31"), "days"),each=64),
                 rep(seq(as.Date("2047/1/1"), as.Date("2047/12/31"), "days"),each=64),
                 rep(seq(as.Date("2048/1/1"), as.Date("2048/12/31"), "days"),each=64),
                 rep(seq(as.Date("2049/1/1"), as.Date("2049/12/31"), "days"),each=64),
                 rep(seq(as.Date("2050/1/1"), as.Date("2050/12/31"), "days"),each=64),
                 rep(seq(as.Date("2051/1/1"), as.Date("2051/12/31"), "days"),each=64),
                 rep(seq(as.Date("2052/1/1"), as.Date("2052/12/31"), "days"),each=64),
                 rep(seq(as.Date("2053/1/1"), as.Date("2053/12/31"), "days"),each=64),
                 rep(seq(as.Date("2054/1/1"), as.Date("2054/12/31"), "days"),each=64),
                 rep(seq(as.Date("2055/1/1"), as.Date("2055/12/31"), "days"),each=64),
                 rep(seq(as.Date("2056/1/1"), as.Date("2056/12/31"), "days"),each=64),
                 rep(seq(as.Date("2057/1/1"), as.Date("2057/12/31"), "days"),each=64),
                 rep(seq(as.Date("2058/1/1"), as.Date("2058/12/31"), "days"),each=64),
                 rep(seq(as.Date("2059/1/1"), as.Date("2059/12/31"), "days"),each=64),
                 rep(seq(as.Date("2060/1/1"), as.Date("2060/12/31"), "days"),each=64),
                 rep(seq(as.Date("2061/1/1"), as.Date("2061/12/31"), "days"),each=64),
                 rep(seq(as.Date("2062/1/1"), as.Date("2062/12/31"), "days"),each=64),
                 rep(seq(as.Date("2063/1/1"), as.Date("2063/12/31"), "days"),each=64),
                 rep(seq(as.Date("2064/1/1"), as.Date("2064/12/31"), "days"),each=64),
                 rep(seq(as.Date("2065/1/1"), as.Date("2065/12/31"), "days"),each=64),
                 rep(seq(as.Date("2066/1/1"), as.Date("2066/12/31"), "days"),each=64),
                 rep(seq(as.Date("2067/1/1"), as.Date("2067/12/31"), "days"),each=64),
                 rep(seq(as.Date("2068/1/1"), as.Date("2068/12/31"), "days"),each=64),
                 rep(seq(as.Date("2069/1/1"), as.Date("2069/12/31"), "days"),each=64),
                 rep(seq(as.Date("2070/1/1"), as.Date("2070/12/31"), "days"),each=64),
                 rep(seq(as.Date("2071/1/1"), as.Date("2071/12/31"), "days"),each=64),
                 rep(seq(as.Date("2072/1/1"), as.Date("2072/12/31"), "days"),each=64),
                 rep(seq(as.Date("2073/1/1"), as.Date("2073/12/31"), "days"),each=64),
                 rep(seq(as.Date("2074/1/1"), as.Date("2074/12/31"), "days"),each=64),
                 rep(seq(as.Date("2075/1/1"), as.Date("2075/12/31"), "days"),each=64),
                 rep(seq(as.Date("2076/1/1"), as.Date("2076/12/31"), "days"),each=64),
                 rep(seq(as.Date("2077/1/1"), as.Date("2077/12/31"), "days"),each=64),
                 rep(seq(as.Date("2078/1/1"), as.Date("2078/12/31"), "days"),each=64),
                 rep(seq(as.Date("2079/1/1"), as.Date("2079/12/31"), "days"),each=64),
                 rep(seq(as.Date("2080/1/1"), as.Date("2080/12/31"), "days"),each=64),
                 rep(seq(as.Date("2081/1/1"), as.Date("2081/12/31"), "days"),each=64),
                 rep(seq(as.Date("2082/1/1"), as.Date("2082/12/31"), "days"),each=64),
                 rep(seq(as.Date("2083/1/1"), as.Date("2083/12/31"), "days"),each=64),
                 rep(seq(as.Date("2084/1/1"), as.Date("2084/12/31"), "days"),each=64),
                 rep(seq(as.Date("2085/1/1"), as.Date("2085/12/31"), "days"),each=64),
                 rep(seq(as.Date("2086/1/1"), as.Date("2086/12/31"), "days"),each=64),
                 rep(seq(as.Date("2087/1/1"), as.Date("2087/12/31"), "days"),each=64),
                 rep(seq(as.Date("2088/1/1"), as.Date("2088/12/31"), "days"),each=64),
                 rep(seq(as.Date("2089/1/1"), as.Date("2089/12/31"), "days"),each=64),
                 rep(seq(as.Date("2090/1/1"), as.Date("2090/12/31"), "days"),each=64),
                 rep(seq(as.Date("2091/1/1"), as.Date("2091/12/31"), "days"),each=64),
                 rep(seq(as.Date("2092/1/1"), as.Date("2092/12/31"), "days"),each=64),
                 rep(seq(as.Date("2093/1/1"), as.Date("2093/12/31"), "days"),each=64),
                 rep(seq(as.Date("2094/1/1"), as.Date("2094/12/31"), "days"),each=64),
                 rep(seq(as.Date("2095/1/1"), as.Date("2095/12/31"), "days"),each=64),
                 rep(seq(as.Date("2096/1/1"), as.Date("2096/12/31"), "days"),each=64),
                 rep(seq(as.Date("2097/1/1"), as.Date("2097/12/31"), "days"),each=64),
                 rep(seq(as.Date("2098/1/1"), as.Date("2098/12/31"), "days"),each=64),
                 # data up to Nov 24, 2099
                 rep(seq(as.Date("2099/1/1"), as.Date("2099/11/24"), "days"),each=64) 
        ))
date.df_cnrm_cm5 <- createDate(date_cnrm_cm5) %>% 
        within(rm(date))
remove(date_cnrm_cm5)

# date variable for GFDL-CM3 
date_gfdl_cm3 <- data.frame(
        date = c(rep(seq(as.Date("1951/1/1"), as.Date("1951/12/31"), "days"),each=64),
                 rep(seq(as.Date("1952/1/1"), as.Date("1952/12/31"), "days"),each=64),
                 rep(seq(as.Date("1953/1/1"), as.Date("1953/12/31"), "days"),each=64),
                 rep(seq(as.Date("1954/1/1"), as.Date("1954/12/31"), "days"),each=64),
                 rep(seq(as.Date("1955/1/1"), as.Date("1955/12/31"), "days"),each=64),
                 rep(seq(as.Date("1956/1/1"), as.Date("1956/12/31"), "days"),each=64),
                 rep(seq(as.Date("1957/1/1"), as.Date("1957/12/31"), "days"),each=64),
                 rep(seq(as.Date("1958/1/1"), as.Date("1958/12/31"), "days"),each=64),
                 rep(seq(as.Date("1959/1/1"), as.Date("1959/12/31"), "days"),each=64),
                 rep(seq(as.Date("1960/1/1"), as.Date("1960/12/31"), "days"),each=64),
                 rep(seq(as.Date("1961/1/1"), as.Date("1961/12/31"), "days"),each=64),
                 rep(seq(as.Date("1962/1/1"), as.Date("1962/12/31"), "days"),each=64),
                 rep(seq(as.Date("1963/1/1"), as.Date("1963/12/31"), "days"),each=64),
                 rep(seq(as.Date("1964/1/1"), as.Date("1964/12/31"), "days"),each=64),
                 rep(seq(as.Date("1965/1/1"), as.Date("1965/12/31"), "days"),each=64),
                 rep(seq(as.Date("1966/1/1"), as.Date("1966/12/31"), "days"),each=64),
                 rep(seq(as.Date("1967/1/1"), as.Date("1967/12/31"), "days"),each=64),
                 rep(seq(as.Date("1968/1/1"), as.Date("1968/12/31"), "days"),each=64),
                 rep(seq(as.Date("1969/1/1"), as.Date("1969/12/31"), "days"),each=64),
                 rep(seq(as.Date("1970/1/1"), as.Date("1970/12/31"), "days"),each=64),
                 rep(seq(as.Date("1971/1/1"), as.Date("1971/12/31"), "days"),each=64),
                 rep(seq(as.Date("1972/1/1"), as.Date("1972/12/31"), "days"),each=64),
                 rep(seq(as.Date("1973/1/1"), as.Date("1973/12/31"), "days"),each=64),
                 rep(seq(as.Date("1974/1/1"), as.Date("1974/12/31"), "days"),each=64),
                 rep(seq(as.Date("1975/1/1"), as.Date("1975/12/31"), "days"),each=64),
                 rep(seq(as.Date("1976/1/1"), as.Date("1976/12/31"), "days"),each=64),
                 rep(seq(as.Date("1977/1/1"), as.Date("1977/12/31"), "days"),each=64),
                 rep(seq(as.Date("1978/1/1"), as.Date("1978/12/31"), "days"),each=64),
                 rep(seq(as.Date("1979/1/1"), as.Date("1979/12/31"), "days"),each=64),
                 rep(seq(as.Date("1980/1/1"), as.Date("1980/12/31"), "days"),each=64),
                 rep(seq(as.Date("1981/1/1"), as.Date("1981/12/31"), "days"),each=64),
                 rep(seq(as.Date("1982/1/1"), as.Date("1982/12/31"), "days"),each=64),
                 rep(seq(as.Date("1983/1/1"), as.Date("1983/12/31"), "days"),each=64),
                 rep(seq(as.Date("1984/1/1"), as.Date("1984/12/31"), "days"),each=64),
                 rep(seq(as.Date("1985/1/1"), as.Date("1985/12/31"), "days"),each=64),
                 rep(seq(as.Date("1986/1/1"), as.Date("1986/12/31"), "days"),each=64),
                 rep(seq(as.Date("1987/1/1"), as.Date("1987/12/31"), "days"),each=64),
                 rep(seq(as.Date("1988/1/1"), as.Date("1988/12/31"), "days"),each=64),
                 rep(seq(as.Date("1989/1/1"), as.Date("1989/12/31"), "days"),each=64),
                 rep(seq(as.Date("1990/1/1"), as.Date("1990/12/31"), "days"),each=64),
                 rep(seq(as.Date("1991/1/1"), as.Date("1991/12/31"), "days"),each=64),
                 rep(seq(as.Date("1992/1/1"), as.Date("1992/12/31"), "days"),each=64),
                 rep(seq(as.Date("1993/1/1"), as.Date("1993/12/31"), "days"),each=64),
                 rep(seq(as.Date("1994/1/1"), as.Date("1994/12/31"), "days"),each=64),
                 rep(seq(as.Date("1995/1/1"), as.Date("1995/12/31"), "days"),each=64),
                 rep(seq(as.Date("1996/1/1"), as.Date("1996/12/31"), "days"),each=64),
                 rep(seq(as.Date("1997/1/1"), as.Date("1997/12/31"), "days"),each=64),
                 rep(seq(as.Date("1998/1/1"), as.Date("1998/12/31"), "days"),each=64),
                 rep(seq(as.Date("1999/1/1"), as.Date("1999/12/31"), "days"),each=64),
                 rep(seq(as.Date("2000/1/1"), as.Date("2000/12/31"), "days"),each=64),
                 rep(seq(as.Date("2001/1/1"), as.Date("2001/12/31"), "days"),each=64),
                 rep(seq(as.Date("2002/1/1"), as.Date("2002/12/31"), "days"),each=64),
                 rep(seq(as.Date("2003/1/1"), as.Date("2003/12/31"), "days"),each=64),
                 rep(seq(as.Date("2004/1/1"), as.Date("2004/12/31"), "days"),each=64),
                 rep(seq(as.Date("2005/1/1"), as.Date("2005/12/31"), "days"),each=64),
                 rep(seq(as.Date("2006/1/1"), as.Date("2006/12/31"), "days"),each=64),
                 rep(seq(as.Date("2007/1/1"), as.Date("2007/12/31"), "days"),each=64),
                 rep(seq(as.Date("2008/1/1"), as.Date("2008/12/31"), "days"),each=64),
                 rep(seq(as.Date("2009/1/1"), as.Date("2009/12/31"), "days"),each=64),
                 rep(seq(as.Date("2010/1/1"), as.Date("2010/12/31"), "days"),each=64),
                 rep(seq(as.Date("2011/1/1"), as.Date("2011/12/31"), "days"),each=64),
                 rep(seq(as.Date("2012/1/1"), as.Date("2012/12/31"), "days"),each=64),
                 rep(seq(as.Date("2013/1/1"), as.Date("2013/12/31"), "days"),each=64),
                 rep(seq(as.Date("2014/1/1"), as.Date("2014/12/31"), "days"),each=64),
                 rep(seq(as.Date("2015/1/1"), as.Date("2015/12/31"), "days"),each=64),
                 rep(seq(as.Date("2016/1/1"), as.Date("2016/12/31"), "days"),each=64),
                 rep(seq(as.Date("2017/1/1"), as.Date("2017/12/31"), "days"),each=64),
                 rep(seq(as.Date("2018/1/1"), as.Date("2018/12/31"), "days"),each=64),
                 rep(seq(as.Date("2019/1/1"), as.Date("2019/12/31"), "days"),each=64),
                 rep(seq(as.Date("2020/1/1"), as.Date("2020/12/31"), "days"),each=64),
                 rep(seq(as.Date("2021/1/1"), as.Date("2021/12/31"), "days"),each=64),
                 rep(seq(as.Date("2022/1/1"), as.Date("2022/12/31"), "days"),each=64),
                 rep(seq(as.Date("2023/1/1"), as.Date("2023/12/31"), "days"),each=64),
                 rep(seq(as.Date("2024/1/1"), as.Date("2024/12/31"), "days"),each=64),
                 rep(seq(as.Date("2025/1/1"), as.Date("2025/12/31"), "days"),each=64),
                 rep(seq(as.Date("2026/1/1"), as.Date("2026/12/31"), "days"),each=64),
                 rep(seq(as.Date("2027/1/1"), as.Date("2027/12/31"), "days"),each=64),
                 rep(seq(as.Date("2028/1/1"), as.Date("2028/12/31"), "days"),each=64),
                 rep(seq(as.Date("2029/1/1"), as.Date("2029/12/31"), "days"),each=64),
                 rep(seq(as.Date("2030/1/1"), as.Date("2030/12/31"), "days"),each=64),
                 rep(seq(as.Date("2031/1/1"), as.Date("2031/12/31"), "days"),each=64),
                 rep(seq(as.Date("2032/1/1"), as.Date("2032/12/31"), "days"),each=64),
                 rep(seq(as.Date("2033/1/1"), as.Date("2033/12/31"), "days"),each=64),
                 rep(seq(as.Date("2034/1/1"), as.Date("2034/12/31"), "days"),each=64),
                 rep(seq(as.Date("2035/1/1"), as.Date("2035/12/31"), "days"),each=64),
                 rep(seq(as.Date("2036/1/1"), as.Date("2036/12/31"), "days"),each=64),
                 rep(seq(as.Date("2037/1/1"), as.Date("2037/12/31"), "days"),each=64),
                 rep(seq(as.Date("2038/1/1"), as.Date("2038/12/31"), "days"),each=64),
                 rep(seq(as.Date("2039/1/1"), as.Date("2039/12/31"), "days"),each=64),
                 rep(seq(as.Date("2040/1/1"), as.Date("2040/12/31"), "days"),each=64),
                 rep(seq(as.Date("2041/1/1"), as.Date("2041/12/31"), "days"),each=64),
                 rep(seq(as.Date("2042/1/1"), as.Date("2042/12/31"), "days"),each=64),
                 rep(seq(as.Date("2043/1/1"), as.Date("2043/12/31"), "days"),each=64),
                 rep(seq(as.Date("2044/1/1"), as.Date("2044/12/31"), "days"),each=64),
                 rep(seq(as.Date("2045/1/1"), as.Date("2045/12/31"), "days"),each=64),
                 rep(seq(as.Date("2046/1/1"), as.Date("2046/12/31"), "days"),each=64),
                 rep(seq(as.Date("2047/1/1"), as.Date("2047/12/31"), "days"),each=64),
                 rep(seq(as.Date("2048/1/1"), as.Date("2048/12/31"), "days"),each=64),
                 rep(seq(as.Date("2049/1/1"), as.Date("2049/12/31"), "days"),each=64),
                 rep(seq(as.Date("2050/1/1"), as.Date("2050/12/31"), "days"),each=64),
                 rep(seq(as.Date("2051/1/1"), as.Date("2051/12/31"), "days"),each=64),
                 rep(seq(as.Date("2052/1/1"), as.Date("2052/12/31"), "days"),each=64),
                 rep(seq(as.Date("2053/1/1"), as.Date("2053/12/31"), "days"),each=64),
                 # four days data missing for 2054
                 rep(seq(as.Date("2054/1/1"), as.Date("2054/12/27"), "days"),each=64), 
                 rep(seq(as.Date("2055/1/1"), as.Date("2055/12/31"), "days"),each=64),
                 rep(seq(as.Date("2056/1/1"), as.Date("2056/12/31"), "days"),each=64),
                 rep(seq(as.Date("2057/1/1"), as.Date("2057/12/31"), "days"),each=64),
                 rep(seq(as.Date("2058/1/1"), as.Date("2058/12/31"), "days"),each=64),
                 rep(seq(as.Date("2059/1/1"), as.Date("2059/12/31"), "days"),each=64),
                 rep(seq(as.Date("2060/1/1"), as.Date("2060/12/31"), "days"),each=64),
                 rep(seq(as.Date("2061/1/1"), as.Date("2061/12/31"), "days"),each=64),
                 rep(seq(as.Date("2062/1/1"), as.Date("2062/12/31"), "days"),each=64),
                 rep(seq(as.Date("2063/1/1"), as.Date("2063/12/31"), "days"),each=64),
                 rep(seq(as.Date("2064/1/1"), as.Date("2064/12/31"), "days"),each=64),
                 rep(seq(as.Date("2065/1/1"), as.Date("2065/12/31"), "days"),each=64),
                 rep(seq(as.Date("2066/1/1"), as.Date("2066/12/31"), "days"),each=64),
                 rep(seq(as.Date("2067/1/1"), as.Date("2067/12/31"), "days"),each=64),
                 rep(seq(as.Date("2068/1/1"), as.Date("2068/12/31"), "days"),each=64),
                 rep(seq(as.Date("2069/1/1"), as.Date("2069/12/31"), "days"),each=64),
                 rep(seq(as.Date("2070/1/1"), as.Date("2070/12/31"), "days"),each=64),
                 rep(seq(as.Date("2071/1/1"), as.Date("2071/12/31"), "days"),each=64),
                 rep(seq(as.Date("2072/1/1"), as.Date("2072/12/31"), "days"),each=64),
                 rep(seq(as.Date("2073/1/1"), as.Date("2073/12/31"), "days"),each=64),
                 rep(seq(as.Date("2074/1/1"), as.Date("2074/12/31"), "days"),each=64),
                 rep(seq(as.Date("2075/1/1"), as.Date("2075/12/31"), "days"),each=64),
                 rep(seq(as.Date("2076/1/1"), as.Date("2076/12/31"), "days"),each=64),
                 rep(seq(as.Date("2077/1/1"), as.Date("2077/12/31"), "days"),each=64),
                 rep(seq(as.Date("2078/1/1"), as.Date("2078/12/31"), "days"),each=64),
                 rep(seq(as.Date("2079/1/1"), as.Date("2079/12/31"), "days"),each=64),
                 rep(seq(as.Date("2080/1/1"), as.Date("2080/12/31"), "days"),each=64),
                 rep(seq(as.Date("2081/1/1"), as.Date("2081/12/31"), "days"),each=64),
                 rep(seq(as.Date("2082/1/1"), as.Date("2082/12/31"), "days"),each=64),
                 rep(seq(as.Date("2083/1/1"), as.Date("2083/12/31"), "days"),each=64),
                 rep(seq(as.Date("2084/1/1"), as.Date("2084/12/31"), "days"),each=64),
                 rep(seq(as.Date("2085/1/1"), as.Date("2085/12/31"), "days"),each=64),
                 rep(seq(as.Date("2086/1/1"), as.Date("2086/12/31"), "days"),each=64),
                 rep(seq(as.Date("2087/1/1"), as.Date("2087/12/31"), "days"),each=64),
                 rep(seq(as.Date("2088/1/1"), as.Date("2088/12/31"), "days"),each=64),
                 rep(seq(as.Date("2089/1/1"), as.Date("2089/12/31"), "days"),each=64),
                 rep(seq(as.Date("2090/1/1"), as.Date("2090/12/31"), "days"),each=64),
                 rep(seq(as.Date("2091/1/1"), as.Date("2091/12/31"), "days"),each=64),
                 rep(seq(as.Date("2092/1/1"), as.Date("2092/12/31"), "days"),each=64),
                 rep(seq(as.Date("2093/1/1"), as.Date("2093/12/31"), "days"),each=64),
                 rep(seq(as.Date("2094/1/1"), as.Date("2094/12/31"), "days"),each=64),
                 rep(seq(as.Date("2095/1/1"), as.Date("2095/12/31"), "days"),each=64),
                 rep(seq(as.Date("2096/1/1"), as.Date("2096/12/31"), "days"),each=64),
                 rep(seq(as.Date("2097/1/1"), as.Date("2097/12/31"), "days"),each=64),
                 rep(seq(as.Date("2098/1/1"), as.Date("2098/12/31"), "days"),each=64),
                 rep(seq(as.Date("2099/1/1"), as.Date("2099/11/30"), "days"),each=64)
        ))

date.df_gfdl_cm3 <- createDate(date_gfdl_cm3) %>%
        filter(!(month == "02" & day == "29")) %>% # no leap days
        within(rm(date))
remove(date_gfdl_cm3)

# date variable for HadGEM2-ES

year <- rep(seq(1951, 2098, by =1), each = 12*30*64)
month <- rep(c(rep(seq(1, 12, by = 1), each = 30*64)),148)
day <- rep(c(rep(c(rep(seq(1, 30, by = 1), each = 64)),12)),148)

date.df_hadgem2_es <- data.frame(cbind(year,month,day)) %>%
        # 10 missing days in 1989
        filter(!(year==1989 & month ==12 & day %in% c(22:31))) %>% 
        filter(!(year==2098 & month == 12 & day ==30))

remove(year, month, day)































        
