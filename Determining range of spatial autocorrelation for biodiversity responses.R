#################################################################################################
########                                                                                 ########
########             Determining the range of spatial autocorrelation                    ######## 
########                      for each biodiversity metric                               ########
#################################################################################################

# This code constructs variograms for slopes of observed richness, rarefied richness,
# and abundance (calculated with gls AR1) to determine the range of autocorrelation for each
# response.


########## clean workspace and load required packages ####################
rm(list = ls() )
gc() #releases memory

library(raster)
library(sp)
library(tidyverse)
library(sf)
library(geoR)
library(gstat)
library(automap)


###############################################
###                                         ###
###             Data Loading                ###
###                                         ###
###############################################

# Load the GLS frequentist master for years 1992 - 2018
setwd("I:/MAS/01_projects/iUpdate/00_Data/processed")
# setwd("~/share/groups/MAS/01_projects/iUpdate/00_Data/processed")
master <- read.csv("GLS Master_Trends of Responses and Predictors_1992 to 2018.csv", header = TRUE)


###############################################
###                                         ###
###           Data Manipulation             ###
###                                         ###
###############################################

# Remove routes from the master that are in Canada or Alaska
# Canada's country number is 124, Alaska's statnum is 03 (though it won't have the leading zero)

master <- master[master$countrynum != 124 & master$statenum != 3, ]

# Define the coordinate system
albers = sp :: CRS("+init=epsg:5070")

# Convert master df to sf object
master.sf <- sf :: st_as_sf(master, coords = c("x", "y"), crs = albers)

# then convert master df to spatial points dataframe
master.spdf <- as(master.sf, "Spatial")


###############################################
###                                         ###
###           Construct and Plot            ###
###              the Variograms             ###
###############################################

#####################
# Observed Richness #
#####################

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


#####################
# Rarefied Richness #
#####################

# Subset master spdf to only rarefied and x and y so that I can remove any NA values in
# Rarefied
master.spdf.r <- master.spdf[ , "SLOPE_LogRare"]
which(is.na(master.spdf.r$SLOPE_LogRare))
master.spdf.r <- master.spdf.r[-which(is.na(master.spdf.r$SLOPE_LogRare)), ]

# Make the variogram
TheVariogram2 = variogram(SLOPE_LogRare ~ 1, data = master.spdf.r)

# Fit the model to the variogram data
# This method automatically determines the nugget, sill, and range without
# having to trial and error initial values
vario.fit2 = autofitVariogram( SLOPE_LogRare ~ 1,
                              master.spdf.r,
                              model = c("Sph", "Exp", "Gau", "Ste"),
                              kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10),
                              fix.values = c(NA, NA, NA),
                              start_vals = c(NA,NA,NA),
                              verbose = T)

plot(TheVariogram2, vario.fit2$var_model)

# Extract the nugget
vario.fit2$var_model$psill[1]

# Extract the sill
vario.fit2$var_model$psill[2]

# Extract the range
vario.fit2$var_model$range[2]

# Rarefied Range = 709,766.5 meters


#####################
#     Abundance     # 
#####################


# Make the variogram
TheVariogram3 = variogram(SLOPE_LogAb ~ 1, data = master.spdf)

# Fit the model to the variogram data
# This method automatically determines the nugget, sill, and range without
# having to trial and error initial values
vario.fit3 = autofitVariogram( SLOPE_LogAb ~ 1,
                               master.spdf,
                               model = c("Sph", "Exp", "Gau", "Ste"),
                               kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10),
                               fix.values = c(NA, NA, NA),
                               start_vals = c(NA,NA,NA),
                               verbose = T)

plot(TheVariogram3, vario.fit3$var_model)

# Extract the nugget
vario.fit3$var_model$psill[1]

# Extract the sill
vario.fit3$var_model$psill[2]

# Extract the range
vario.fit3$var_model$range[2]

# Abundance Range = 109739.3 meters
