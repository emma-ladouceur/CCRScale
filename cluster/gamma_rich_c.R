
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/CCRScale'
gamma_dat <- read.csv(paste0(path, '/gamma_div_percent.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


p.gamma.rich <-  brm(log_gamma_rich_p ~  log_YSA  +  ( 1 + log_YSA  | Field) + (1 | Year),
                     data = gamma_dat_of,family=student(), cores = 4, iter=3000, warmup=1000, chains = 4)


save(p.gamma.rich,
     file=Sys.getenv('OFILE'))



