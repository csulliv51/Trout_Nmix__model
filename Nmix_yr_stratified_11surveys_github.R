library(unmarked);library(tidyverse);library(AHMbook);library(jagsUI);library(rjags);
library(dplyr);library(dbplyr);library(unmarked);library(ggplot2);library(gridExtra);
library(scales);library(Hmisc); library(shinystan); library(rstanarm); library(bayesplot)


setwd("C:/Users/cjs18021/OneDrive - University of Connecticut/UConn/Research/Dissertation/Papers/Counting Trout/2022/Data")
UAC_data <-read.csv("UAC_count_data_11surveys.csv")
head(UAC_data)

#dimensions for surveys
nsites = 15
nsurveys = 11
nyears = 11

#pull count data and organize
UAC_long <- as.matrix(UAC_data[,c(4:14)]) 
head(UAC_long)

y_UAC_long = array(NA, dim = c(nsites, nsurveys, nyears)) 
for (i in 1:nyears){
  y_UAC_long[,,i] <- UAC_long[(15*i-14):(15*i),] #
}




##########################################################################################################
###########          covariate importation and standardization                              ##############
##########################################################################################################
#site covariates
covar <- read.csv("site_covs.csv") 
covar <- covar[,-c(1)] #removing objectID
covar$Trib <- as.integer(covar$Trib) ; covar$Habitat <- as.integer(covar$Habitat); covar$TMA <- as.integer(covar$TMA)
str(covar)

#site/year covariates
depth <- read.csv("depth_covs.csv")
dep = depth[,-c(1:3)]
dep_data <- array(NA, dim = c(nsites, nyears))
for (i in 1:nyears){
  dep_data[,i] <- dep[(15*i-14):(15*i)] #number of sites and number of sites - 1
}
dep_data2 <- dep_data
dep_data <- standardize(dep_data) 


velocity <- read.csv("velocity_covs.csv")
vel = velocity[,-c(1:3)]
vel_data <- array(NA, dim = c(nsites, nyears))
for (i in 1:nyears){
  vel_data[,i] <- vel[(15*i-14):(15*i)] #number of sites and number of sites - 1
}
vel_data2 <- vel_data
vel_data <- standardize(vel_data) 



tempdiff <- read.csv("tempdiff_covs.csv")
tdiff = tempdiff[,-c(1:3)]
tdiff_data <- array(NA, dim = c(nsites, nyears))
for (i in 1:nyears){
  tdiff_data[,i] <- tdiff[(15*i-14):(15*i)] #number of sites and number of sites - 1
}
tdiff_data <- standardize(tdiff_data) 


conductivity <- read.csv("conductivity_covs.csv")
conductivity = conductivity[,-c(1:3)]
conductivity_data <- array(NA, dim = c(nsites, nyears))
for (i in 1:nyears){
  conductivity_data[,i] <- conductivity[(15*i-14):(15*i)] #number of sites and number of sites - 1
}
conductivity.NA = rnorm(165, mean(conductivity_data, na.rm = TRUE), sd(conductivity_data, na.rm = TRUE))
conductivity_data[is.na(conductivity_data)] <- conductivity.NA
conductivity_data2 <- conductivity_data
conductivity_data <- standardize(conductivity_data) 



tribdis <- read.csv("trib_dis_covs.csv")
tribdis = tribdis[,-c(1:3)]
tribdis_data <- array(NA, dim = c(nsites, nyears))
for (i in 1:nyears){
  tribdis_data[,i] <- tribdis[(15*i-14):(15*i)] #number of sites and number of sites - 1
}
tribdis.NA = rnorm(165, mean(tribdis_data, na.rm = TRUE), sd(tribdis_data, na.rm = TRUE))
tribdis_data[is.na(tribdis_data)] <- tribdis.NA
tribdis_data2 <- tribdis_data
tribdis_data <- standardize(tribdis_data) 


DO <- read.csv("DO_covs.csv")
DO = DO[,-c(1:3)]
DO_data <- array(NA, dim = c(nsites, nyears))
for (i in 1:nyears){
  DO_data[,i] <- DO[(15*i-14):(15*i)] #number of sites and number of sites - 1
}
DO_data.NA = rnorm(165, mean(DO_data, na.rm = TRUE), sd(DO_data, na.rm = TRUE))
DO_data[is.na(DO_data)] <- DO_data.NA
DO_data2 <- DO_data
DO_data <- standardize(DO_data) 

tvar <- read.csv("tvar_covs.csv")
tvar = tvar[,-c(1:3)]
tvar_data <- array(NA, dim = c(nsites, nyears))
for (i in 1:nyears){
  tvar_data[,i] <- tvar[(15*i-14):(15*i)] #number of sites and number of sites - 1
}
tvar_data.NA = rnorm(165, mean(tvar_data, na.rm = TRUE), sd(tvar_data, na.rm = TRUE))
tvar_data[is.na(tvar_data)] <- tvar_data.NA
tvar_data2 <- tvar_data
tvar_data <- standardize(tvar_data) 

ggd <- read.csv("ggd_covs.csv")
ggd = ggd[,-c(1:3)]
ggd_data <- array(NA, dim = c(nsites, nyears))
for (i in 1:nyears){
  ggd_data[,i] <- ggd[(15*i-14):(15*i)] #number of sites and number of sites - 1
}
ggd_data.NA = rnorm(165, mean(ggd_data, na.rm = TRUE), sd(ggd_data, na.rm = TRUE))
ggd_data[is.na(ggd_data)] <- ggd_data.NA
ggd_data2 <- ggd_data
ggd_data <- standardize(ggd_data) 



##detection covs
lumin = read.csv(file = "luminance_covs_11surveys.csv")
lumin = lumin[,-c(1)]
lumin_long = as.matrix(lumin[,c(3:13)]) 
lumin_data <- array(NA, dim = c(nsites, nsurveys, nyears))
for (i in 1:nyears){
  lumin_data[,,i] <- lumin_long[(15*i-14):(15*i),] #number of sites and number of sites - 1
}
luminance.NA = rnorm(1691, mean(lumin_data, na.rm = TRUE), sd(lumin_data, na.rm = TRUE))
lumin_data[is.na(lumin_data)] <- luminance.NA
luminance <- standardize(lumin_data) 



bright = read.csv(file = "brightness_covs_11surveys.csv")
bright = bright[,-c(1)]
bright_long = as.matrix(bright[,c(3:13)]) 
bright_data <- array(NA, dim = c(nsites, nsurveys, nyears))
for (i in 1:nyears){
  bright_data[,,i] <- bright_long[(15*i-14):(15*i),]
}
brightness.NA = rnorm(1691, mean(bright_data, na.rm = TRUE), sd(bright_data, na.rm = TRUE))
bright_data[is.na(bright_data)] <- brightness.NA
brightness <- standardize(bright_data) 

Watershed_km <- standardize(covar$Watershed_km)
stock.km.<- standardize(covar$stock.km.)
gradient<- standardize(covar$gradient)
forest<- standardize(covar$forest)
till<- standardize(covar$till)
area <- standardize(covar$area)

library(corrplot)
c <- read.csv("corr.csv")
C <- cor(c)
corrplot.mixed(C, order = 'AOE')

#bundle count and covariate data
str(bdata2 <- list(C=y_UAC_long, nsites=nsites, nsurveys=nsurveys, nyears=nyears,
                   trib=covar$Trib, habitat=covar$Habitat, watershed=Watershed_km, area = area,
                   stock=stock.km., TMA=covar$TMA, gradient=gradient, forest=forest, till=till,
                   conduct=conductivity_data, tempdiff=tdiff_data, DO=DO_data, tribdis=tribdis_data,
                   dep=dep_data, vel=vel_data, tvar = tvar_data, ggd = ggd_data,
                   lumin=luminance, bright=brightness, e = 1e-06))


################################################################################################
##        Zero-inflated, poisson Year-stratified N Mixture Model, with random effects         ##
################################################################################################

#specify the model
cat(file="Nmix4.txt","
    model {
    
    #priors
        for (t in 1:nyears){                                                    #Loop over 11 years
            alpha0[t] ~ dunif(0,1)                                              #prior for detection
            omega[t] ~ dgamma(0.25,0.25)                                        #prior for occupancy (ZIF model)
            }
            
        beta0 ~ dnorm(0, 0.1)                                                  #prior for abundance
       
        for (k in 1:10) {                                                        #prior for k abundance covariates
            beta[k] ~ dnorm(0, 0.1)
            }       

        for (k in 1:1) {                                                        #prior for k detection covariates
            alpha[k] ~ dnorm (0, 0.1)
            }

        for (t in 1:nyears){                                                    #randon survey effect (obs model)
            tau.p[t] <- pow(sd.p[t], -2)
            sd.p[t] ~ dunif(0,3)
            }
        
        tau.lam <- pow(sd.lam, -2)                                              #random site effect (eco model)
        sd.lam ~ dunif(0, 2)
   
   
   #likelihood
     #ecological model
        for (i in 1:nsites){
          for(t in 1:nyears){
            z[i,t] ~ dbern(omega[t])
            N[i,t] ~ dpois(lambda[i,t]*z[i,t])                                  #poisson
            eps.lam[i,t] ~ dnorm(0, tau.lam)                                    #random effects
            log(lambda[i,t]) <- beta0 + beta[1]*area[i]
                                + beta[2]*gradient[i] 
                                + beta[3]*tempdiff[i,t] + beta[4]*DO[i,t] 
                                + beta[5]*tribdis[i,t] + beta[6]*dep[i,t]
                                + beta[7]*vel[i,t] + beta[8]*conduct[i,t]
                                + beta[9]*tvar[i,t] + beta[10]*ggd[i,t]
                                + eps.lam[i,t]
      
    #observation model
        for (j in 1:nsurveys){
            C[i,j,t] ~ dbin(p[i,j,t], N[i,t])
            eps.p[i,j,t] ~ dnorm(0, tau.p[t])
            logit(p[i,j,t]) <- alpha0[t] + alpha[1]*bright[i,j,t]
                               + eps.p[i,j,t]
    
    #Posterior Predictive Check
        C.sim[i,j,t] ~ dbin(p[i,j,t], N[i,t])                                   #create new data set under model
        e.count[i,j,t] <- N[i,t] * p[i,j,t]                                     #expected data
   
    #chi-2 discrepancy for the actual data set
        chi2.actual[i,j,t] <- pow((C[i,j,t]-e.count[i,j,t]),2)/(e.count[i,j,t]+e)    #add e to avoid division by 0
    #Chi-2 discrepancy for perfect or simulated data
        chi2.sim[i,j,t] <- pow((C.sim[i,j,t]-e.count[i,j,t]),2)/(e.count[i,j,t]+e)
      }
     }
    }
    
    
    fit.actual <- sum(chi2.actual[,,])                                          #overall fit statistic for actual data
    fit.sim <- sum(chi2.sim[,,])                                                #overall fit statistic for simulated data
    bpv <- step(fit.sim-fit.actual)                                             #Bayesian p-value
    c.hat <- fit.actual/fit.sim                                                 #c-hat estimate
    
    #derived quantities
      for (i in 1:nsites){
         for (t in 1:nyears){
        totalN[i,t] <- sum(N[i,t])
      }
    }
   }
    ")



#jags settings
Nst <- apply(y_UAC_long, c(1,3), max, na.rm = TRUE)
Nst[Nst == "-Inf"] <- 1
inits <- function() list(N=Nst)

params <- c("alpha0", "alpha", "beta0", "beta", "totalN", "fit.actual", 
            "fit.sim", "bpv", "c.hat",  "lambda") #lambda last

na <- 1000; ni <- 100000; nt <- 50; nb <- 80000; nc <- 3

out3 <- jags(bdata2, inits, params, "Nmix4.txt", n.adapt=na, n.chains=nc, n.thin=nt,
             n.iter=ni, n.burnin=nb, parallel=TRUE)

#Look at the outputs for model fit
#traceplot(out3)
#plot(out3)
print(out3$summary,4)
(show <- as.data.frame(out3$summary))