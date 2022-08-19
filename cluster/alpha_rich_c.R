
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/CCRScale'
alpha_dat <- read.csv(paste0(path, '/alpha_div_percent.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


p.alpha.rich <-  brm(log_alpha_rich_p ~  log_YSA  + ( 1 + log_YSA  | Field/Transect/Plot) + (1 | Year),
                       data = alpha_dat, family=student(), cores = 4, iter=10000,warmup = 1000, control =
                         list(adapt_delta = 0.99), chains = 4)

save(p.alpha.rich,
     file=Sys.getenv('OFILE'))



