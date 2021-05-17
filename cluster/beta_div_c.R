
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/CCRScale'
gamma_dat <- read.csv(paste0(path, '/gamma_div_percent.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



p.beta.div <-  brm(log_beta_div_p ~  log_YSA + (1 + log_YSA | Field) + (1 | Year),
                  data = gamma_dat, family=student(), cores = 4, iter=6000, warmup=1000, control =
                    list(adapt_delta = 0.99), chains = 4)


save(p.beta.div,
     file=Sys.getenv('OFILE'))



