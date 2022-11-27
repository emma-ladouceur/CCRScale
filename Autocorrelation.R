




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
TheVariogram = variogram(log_alpha_rich_p ~ 1, data = alpha_dat)

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







library(tidyverse)
library(ggplot2)
library(brms)
library(bayesplot)
library(patchwork)


alpha_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/alpha_div.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
gamma_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/gamma_div.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))



load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/alpha_rich_d.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/alpha_pie_d.Rdata") 
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/gamma_rich_d.Rdata") 
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/gamma_pie_d.Rdata") 
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/beta_div_d.Rdata") 
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/beta_pie_d.Rdata") 


load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/alpha_rich_c.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/alpha_pie_c.Rdata") 
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/gamma_rich_c.Rdata") 
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/gamma_pie_c.Rdata") 
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/beta_div_c.Rdata") 
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/beta_pie_c.Rdata") 


p.alpha.rich.fixed.p <- as_draws_df(p.alpha.rich, subset = floor(runif(n = 1000, 1, max = 2000)))

p.alpha.rich.fixed.p
nrow(p.alpha.rich.fixed.p)
head(p.alpha.rich.fixed.p)
colnames(p.alpha.rich.fixed.p)


# b_Intercept
# b_log_YSA
# sd_Field__Intercept
# sd_Field__log_YSA

mcmc_acf(as_draws_df(p.alpha.rich, subset = floor(runif(n = 1000, 1, max = 2000)), variable = c("b_Intercept", "b_log_YSA", "sd_Field__Intercept", "sd_Field__log_YSA"), regex = TRUE))

mcmc_acf(as_draws_df(p.alpha.rich, variable = c("b_Intercept", "b_log_YSA", "sd_Field__Intercept", "sd_Field__log_YSA")))


#mcmc_plot(d.alpha.rich, type = 'acf')
mcmc_plot(p.alpha.rich, type = 'acf')


??as_draws
summary(p.alpha.rich)
# discrete models
mcmc_acf(as_draws_array(d.alpha.rich, variable = c("^b_"), regex = TRUE))
mcmc_acf(as_draws_array(d.alpha.spie, variable = c("^b_"), regex = TRUE))
mcmc_acf(as_draws_array(d.gamma.rich, variable = c("^b_"), regex = TRUE))
mcmc_acf(as_draws_array(d.gamma.spie, variable = c("^b_"), regex = TRUE))
mcmc_acf(as_draws_array(d.beta.div, variable = c("^b_"), regex = TRUE))
mcmc_acf(as_draws_array(d.beta.spie, variable = c("^b_"), regex = TRUE))


mcmc_acf(as_draws_array(p.alpha.rich, variable = c("^b_log_YSA", "^r_Field"), regex = TRUE))
mcmc_acf(as_draws_array(p.alpha.spie, variable = c("^b_"), regex = TRUE))
mcmc_acf(as_draws_array(p.gamma.rich, variable = c("^b_"), regex = TRUE))
mcmc_acf(as_draws_array(p.gamma.spie, variable = c("^b_"), regex = TRUE))
mcmc_acf(as_draws_array(p.beta.div, variable = c("^b_"), regex = TRUE))
mcmc_acf(as_draws_array(p.beta.spie, variable = c("^b_"), regex = TRUE))


mcmc_acf(as_draws_array(d.alpha.rich, variable = c("^r_"), regex = TRUE))
mcmc_acf(as_draws_array(d.alpha.spie, variable = c("^r_"), regex = TRUE))
mcmc_acf(as_draws_array(d.gamma.rich, variable = c("^r_"), regex = TRUE))
mcmc_acf(as_draws_array(d.gamma.spie, variable = c("^r_"), regex = TRUE))
mcmc_acf(as_draws_array(d.beta.div, variable = c("^r_"), regex = TRUE))
mcmc_acf(as_draws_array(d.beta.spie, variable = c("^r_"), regex = TRUE))


mcmc_acf(as_draws_array(p.alpha.rich, variable = c("^r_"), regex = TRUE))
mcmc_acf(as_draws_array(p.alpha.spie, variable = c("^r_"), regex = TRUE))
mcmc_acf(as_draws_array(p.gamma.rich, variable = c("^r_"), regex = TRUE))
mcmc_acf(as_draws_array(p.gamma.spie, variable = c("^r_"), regex = TRUE))
mcmc_acf(as_draws_array(p.beta.div, variable = c("^r_"), regex = TRUE))
mcmc_acf(as_draws_array(p.beta.spie, variable = c("^r_"), regex = TRUE))



mcmc_acf(posterior_samples(d.alpha.rich))
mcmc_acf(posterior_samples(d.alpha.spie))
mcmc_acf(posterior_samples(d.gamma.rich))
mcmc_acf(posterior_samples(d.gamma.spie))
mcmc_acf(posterior_samples(d.beta.div))
mcmc_acf(posterior_samples(d.beta.spie))

# percent continuous models
mcmc_acf(posterior_samples(p.alpha.rich, variable = c("b_Intercept", "b_log_YSA", "sd_Field__Intercept", "sd_Field__log_YSA")))
mcmc_acf(posterior_samples(p.alpha.spie))
mcmc_acf(posterior_samples(p.gamma.rich))
mcmc_acf(posterior_samples(p.gamma.spie))
mcmc_acf(posterior_samples(p.beta.div))
mcmc_acf(posterior_samples(p.beta.spie))



# models residuals
ma<-residuals(p.alpha.rich)
ma<-as.data.frame(ma)
ar.plot<-cbind(alpha_dat,ma$Estimate)

head(ar.plot)
plot(ar.plot)

par(mfrow=c(1,2))
with(ar.plot, plot(Field, ma$Estimate))
with(ar.plot, plot(Year, ma$Estimate))



