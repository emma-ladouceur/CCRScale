
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/CCRScale'
gamma_dat <- read.csv(paste0(path, '/gamma_div.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


gamma_dat$Field<-as.factor(as.character(gamma_dat$Field))
gamma_dat$Year<-as.factor(as.character(gamma_dat$Year))
gamma_dat$site_status <- factor(gamma_dat$site_status  , levels=c("never-ploughed","old field"))


d.gamma.spie <-  brm(gamma_ENSPIE ~  site_status + (1 | Field)  + (1 | Year),
                     data = gamma_dat, family = student(), cores = 4, iter=3000, warmup = 1000, chains = 4)



save(d.gamma.spie,
     file=Sys.getenv('OFILE'))



