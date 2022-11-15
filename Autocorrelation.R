



library(tidyverse)
library(ggplot2)
library(brms)
library(bayesplot)
library(patchwork)
library(RColorBrewer)
library(tidybayes)
library(ggdist)
library(modelr)
library(viridis)

alpha_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/alpha_div_percent.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
gamma_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/gamma_div_percent.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))




###############################################
###                                         ###
###           Construct and Plot            ###
###              the Variograms             ###
###############################################

#####################
# Observed Richness #
#####################


??variogram
# Make the variogram
TheVariogram = variogram(SLOPE_LogObs ~ 1, data = master.spdf)

# Fit the model to the variogram data
# This method automatically determines the nugget, sill, and range without
# having to trial and error initial values
vario.fit = autofitVariogram( SLOPE_LogObs ~ 1,
                              master.spdf,
                              model = c("Sph", "Exp", "Gau", "Ste"),
                              kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10),
                              fix.values = c(NA, NA, NA),
                              start_vals = c(NA,NA,NA),
                              verbose = T)

plot(TheVariogram, vario.fit$var_model)

# Extract the nugget
vario.fit$var_model$psill[1]

# Extract the sill
vario.fit$var_model$psill[2]

# Extract the range
vario.fit$var_model$range[2]

# Observed Range = 647,058.4 meters
