
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/CCRScale'
gamma_dat <- read.csv(paste0(path, '/gamma_div.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


gamma_dat$site_status <- factor(gamma_dat$site_status  , levels=c("never-plowed","old field"))


d.beta.spie <-  brm(beta_ENSPIE ~  site_status + (1 | Field) + (1 | Year),
                 data = gamma_dat, family=student(),cores = 4, iter=4000,warmup=1000, chains = 4)


save(d.beta.spie,
     file=Sys.getenv('OFILE'))



