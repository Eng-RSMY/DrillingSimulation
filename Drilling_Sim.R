##################################################
#Setup needed packages and working directory#
##################################################

library(triangle)
library(ks)
library(MASS)
library(rgl)
library(graphics)
library(truncnorm)
library(LaplacesDemon)

#########################################################
#Generate Unifrom Distribution for Number of Wells Drawn#
#########################################################

number_wells <- round(runif(10000, min=10, max=30))

#########################################################
#Calculate Total Number of Wet and Dry Wells#
#########################################################

hydro <- rtruncnorm(n=10000, a=0, b=1, mean=0.99, sd=0.05)
res <- rtruncnorm(n=10000, a=0, b=1, mean = .8, sd=.1)

hist(hydro, breaks=50, main='Hydrocarbons Probability Distribution',
     xlab = 'Probability', ylab = 'Simulation Frequency',col="lightblue")
hist(res, breaks=50, main='Reservoir Probability Distribution',
     xlab = 'Probability', ylab = 'Simulation Frequency',col="lightblue")

PWet <- (hydro * res)
total_wet <- rep(0,10000)
total_dry <- rep(0,10000)


for (i in 1:10000){
  well_ind = number_wells[i]
  bern <- rbern(well_ind, PWet[i])
  total_wet[i] <- sum(bern)
  total_dry[i] <- number_wells[i]-total_wet[i]
}

sum(total_wet)/sum(number_wells)


hist(total_wet, breaks=50, main='Total Simulated Wet Wells',
     xlab = 'Probability', ylab = 'Simulation Frequency')
hist(total_dry, breaks=50, main='Total Simulated Dry Wells',
     xlab = 'Probability', ylab = 'Simulation Frequency')

proportion_wet <- total_wet / number_wells
Var<-quantile(proportion_wet, c(.05))

hist(proportion_wet, breaks=50, main='Proportion Simulated Wet Wells',
     xlab = 'Probability', ylab = 'Simulation Frequency',col="lightblue")

Vcar_prep <- ifelse (proportion_wet >= .5357143, NA, proportion_wet)

cCar<-mean(Vcar_prep,na.rm=TRUE)
