

library(brms)
library(tidyverse)


# alpha analysis

alpha_dat <- read.csv("~/Dropbox/Projects/CCRScale/data/alpha_summary.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


View(alpha_dat)
head(alpha_dat)

alpha_dat$Year<-as.factor(as.character(alpha_dat$Year))
is.numeric(alpha_dat$YSA)
is.numeric(alpha_dat$Year)


# # biodiversity ????
# S_alpha <- brms::brm(S_PIE_biomass ~  Year * YSA +
#                        (Year * YSA  | Field/Transect),
#                      data = alpha_dat, family = 'poisson', cores = 4, chains = 4)


# productivity
S_alpha <- brms::brm(plot_biomass ~  Year * YSA +
                       (Year * YSA  | Field/Transect),
                     data = alpha_dat, family = 'poisson', cores = 4, chains = 4)



# species richness
S_alpha <- brms::brm(S_biomass ~  site_status * YSA +
                             (site_status * YSA  | Field/Transect),
                           data = alpha_dat, family = 'poisson', cores = 4, chains = 4)



# evennness
S_alpha <- brms::brm(ENSPIE_biomass ~  Year * YSA +
                       (Year * YSA  | Field/Transect),
                     data = alpha_dat, family = 'poisson', cores = 4, chains = 4)




