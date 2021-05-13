#' Calculate of vectorial capacity

#' Calculate biting rate
#' 
#' The BitingRate function calculates the average daily mosquito 
#' biting rate component of the vectorial capacity calculation
#' 
#' @param temp tempature in celsius
#' @param a1 Default regression coeficient from Scott et al. 2000 
#' @param a2 Default intercept from Scott et al. 2000
#' 
#' @return Returns the biting rate which is a numeric value for the
#' number of people each mosquito bites per day

BitingRate <- function(temp, a1 = 0.0043, a2 = 0.0943) {
  biteRate <- a1 * temp + a2
  biteRate[(temp < 12.4 | temp > 32)] <- 0
  
  # biteRate <- vector(mode = "numeric", length = length(temp))
  # 
  # for (ii in 1:length(temp)) {
  #   if (12.4 < temp[ii] && temp[ii] < 32) {
  #     biteRate[ii] <- a1 * temp[ii] + a2
  #   } else {
  #     # If temperature is less than 12.4 or greater than 32 we assume VC is 0 
  #     # Described in Limitation section of Liu-Helmersson et al. 2014
  #     biteRate[ii] <- 0
  #   }
  # } 
  return(biteRate)
}

#' Probability of transmission from human to vector
#' 
#' The HumanToVector function calculates the probability of human to 
#' mosquito infection per bite component of vectorial capacity calculation
#' 
#' @param temp tempature in celsius
#' @param bm1 default regression coeficient from Lambrechts et al. 2011
#' @param bm2 Default intercept from Lambrechts et al. 2011
#' 
#' @return Returns probability of human to mosquito infection per bite 

HumanToVector <- function(temp, bm1 = 0.0729, bm2 = 0.9037) {
  # if (12.4 <= temp && temp <= 26.1) {
  humanVector <- (bm1 * temp) - bm2
  humanVector[(temp >= 26.1 & temp < 32.5)] <- 1
  humanVector[(temp < 12.4 | temp >= 32.5)] <- 0
  
  # } else if (26.1 < temp && temp <32.5) {
  #   humanVector <- 1
  # } else {
  #   # If tempurature is less than 12.4 or greater than 32 we assume VC is 0 
  #   humanVector <- 0
  # }
  return(humanVector)
}

#' Probability of transmission from vector to human
#' 
#' The VectorToHuman function calculates the probability of human to 
#' mosquito infection per bite component of vectorial capacity calculation
#' 
#' @param temp tempature in celsius
#' @param bh1 default regression coefficient from Lambrechts et al. 1995
#' @param bh2 default regression coefficient from Lambrechts et al. 1995
#' @param bh3 default regression coefficient from Lambrechts et al. 1995
#' 
#' @return Returns probability of mosquito to human infection per bite

VectorToHuman <- function (temp, bh1 = 0.001044, bh2 = 12.286, bh3 = 32.461) {
  
  #  if (12.286 <= temp && temp < 32.461) {
  vectorHuman <- bh1 * temp * (temp-bh2) * sqrt(bh3-temp)
  vectorHuman[(temp < 12.286 | temp >= 32.461)] <- 0
  
  # } else {
  #   # If tempurature is less than 12.4 or greater than 32 we assume VC is 0 
  #   vectorHuman <- 0
  # }
  return(vectorHuman)
}

#' Calculate extrinsic incubation period
#' 
#' The ExtrinsicIncubationPeriod function calculates the extrinsic 
#' incubation period component of vectorial capacity calculation
#' 
#' @param temp tempature in celsius
#' @param n1 default coefficient of an exponential function fit to data from 
#' Watts et al. 1987, McLean et al. 1974
#' @param n2 default coefficient of an exponential function fit to data from 
#' Watts et al. 1987, McLean et al. 1974
#' @param n3 default coefficient of an exponential function fit to data from 
#' Watts et al. 1987, McLean et al. 1974
#' 
#' @return Returns extrinsic incubation period in days

ExtrIncPeriod <- function (temp, n1 = 4, n2 = 5.15, n3 = 0.123) {
  
  # if (12 <= temp && temp <= 36) {
  extrincPeriod <- n1 + exp(1)^(n2-(n3 * temp))
  extrincPeriod[(temp < 12 | temp > 36)] <- 0
  # } else {
  #   # If tempurature is less than 12.4 or greater than 32 we assume VC is 0 
  #   extrincPeriod <- 0
  # }
  return(extrincPeriod)
}

#' Calculate vector mortality rate
#' 
#' VectMortRate function calculates mortality rate of vectors per day
#' 
#' @param temp tempature in celsius
#' @param mu1 default coefficient of the polynomial function fit to data by 
#' Yang et al 2009
#' @param mu2 default coefficient of the polynomial function fit to data by 
#' Yang et al 2009
#' @param mu3 default coefficient of the polynomial function fit to data by 
#' Yang et al 2009
#'  @param mu4 default coefficient of the polynomial function fit to data by 
#' Yang et al 2009
#'  @param mu5 default coefficient of the polynomial function fit to data by 
#' Yang et al 2009
#' 
#' @return Returns mortality rate of vectors per day

VectorMortRate <- function (temp, mu1 = 0.8692, mu2 = 0.1590, mu3 = 0.01116, 
                            mu4 = 0.0003408, mu5 = 0.000003809) {
  
  # if (10.54 <= temp && temp <= 33.4) {
  vectorMort <- mu1 - (mu2 * temp) + (mu3 * temp^2) - (mu4 * temp^3) + (mu5 * temp^4)
  vectorMort[(temp < 10.54 | temp > 33.4)] <- 1e8
  # } else {
  #   vectorMort <- 1e8 # Set to be effectively infinite because mosquitos can't survive outside this tempature range
  # }
  return(vectorMort)
}

#' Calculate female vector to human population ratio
#' 
#' VectorHumanRatio function calculates vector to human population ratio 
#' assumed to depend on temperature the same way as the life expectancy or 
#' inverse of the mortality rate as used by Brady et al 2014
#' 
#' @param temp tempature in celsius
#' 
#' @return Returns vector to human population ratio

VectorHRconstant <- function(mmax) {
  #
  #
  tempatureRange <- seq(10.54, 33.4, by = 0.001)
  return(mmax*min(VectorMortRate(tempatureRange)))
}

c15 <- VectorHRconstant(1.5)

VectorHumanRatio <- function (temp) {

  vectorhumanRatio <- c15/VectorMortRate(temp)
  return(vectorhumanRatio)
}

#' Calculate Vectorial Capacity
#' 
#' VectorialCapacity function calculates vectorial capacity for Aedes vector 
#' in a given temperature
#' 
#' @param temp tempature in celsius
#' 
#' @return Returns Vectorial Capacity


VectorialCapacity <- function(temp) {
  
  vecCapacity <- VectorHumanRatio(temp) * BitingRate(temp)^2 * VectorToHuman(temp) * 
    HumanToVector(temp) * exp(-VectorMortRate(temp)*ExtrIncPeriod(temp)) / VectorMortRate(temp)
  
  return(vecCapacity)
}


