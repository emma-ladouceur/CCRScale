
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/CCRScale'
alpha_dat <- read.csv(paste0(path, '/alpha_div.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


alpha_dat$Field<-as.factor(as.character(alpha_dat$Field))
alpha_dat$Year<-as.factor(as.character(alpha_dat$Year))
alpha_dat$YSA<-as.factor(as.character(alpha_dat$YSA))
alpha_dat$site_status <- factor(alpha_dat$site_status  , levels=c("never-plowed","old field"))


d.alpha.spie <-  brm(alpha_ENSPIE ~  site_status + (1 | Field)  + (1 | Year) + (1 | YSA),
                     data = alpha_dat, family = student(), cores = 4, iter=3000, warmup = 1000, chains = 4)



save(d.alpha.spie,
     file=Sys.getenv('OFILE'))



