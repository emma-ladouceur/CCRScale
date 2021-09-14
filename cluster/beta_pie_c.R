
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/CCRScale'
gamma_dat <- read.csv(paste0(path, '/gamma_div_percent.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



p.beta.spie <-  brm(log_beta_ENSPIE_p ~  log_YSA + (1 + log_YSA | Field) + (1 | Year),
                   data = gamma_dat, family=student(),cores = 4, iter=10000,warmup=1000, chains = 4,
                   control = list(adapt_delta = 0.99))


save(p.beta.spie,
     file=Sys.getenv('OFILE'))



