
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/CCRScale'
alpha_dat <- read.csv(paste0(path, '/alpha_div_percent.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


p.alpha.spie <-  brm(log_alpha_ENSPIE_p ~  log_YSA + ( 1 + log_YSA  | Field/Transect/Plot) + (1 | Year),
                     data = alpha_dat, family=student(), cores = 4, iter=7000,warmup = 1000, chains = 4)


save(p.alpha.spie,
     file=Sys.getenv('OFILE'))



