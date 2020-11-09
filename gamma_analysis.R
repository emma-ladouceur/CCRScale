


library(brms)
library(tidyverse)


# gamma analysis

gamma_dat <- read.csv("~/Dropbox/Projects/CCRScale/data/gamma_summary.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))



head(gamma_dat)

gamma_dat$Year<-as.factor(as.character(gamma_dat$Year))
is.numeric(gamma_dat$YSA)
is.numeric(gamma_dat$Year)


# # biodiversity ????
# S_gamma <- brms::brm(S_PIE_biomass ~  Year * YSA +
#                        (Year * YSA  | Field/Transect),
#                      data = gamma_dat, family = 'poisson', cores = 4, chains = 4)


# productivity
S_gamma <- brms::brm(plot_biomass ~  Year * YSA +
                       (Year * YSA  | Field/Transect),
                     data = gamma_dat, family = 'poisson', cores = 4, chains = 4)



# species richness
S_gamma <- brms::brm(S_biomass ~  Year * YSA +
                       (Year * YSA  | Field/Transect),
                     data = gamma_dat, family = 'gaussian', cores = 4, chains = 4)



# evennness
S_gamma <- brms::brm(ENSPIE_biomass ~  Year * YSA +
                       (Year * YSA  | Field/Transect),
                     data = gamma_dat, family = 'poisson', cores = 4, chains = 4)




