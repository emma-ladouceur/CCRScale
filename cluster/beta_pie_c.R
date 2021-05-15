
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/CCRScale'
gamma_dat <- read.csv(paste0(path, '/gamma_div.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


gamma_dat_of <- gamma_dat %>% filter(site_status == "old field") 

gamma_dat_of$beta_ENSPIE_p<-(gamma_dat_of$beta_ENSPIE/4.361521 *100)


gamma_dat_of$YSA <- as.numeric(gamma_dat_of$YSA)
gamma_dat_of$log_beta_ENSPIE_p <- log(gamma_dat_of$beta_ENSPIE_p)
gamma_dat_of$log_YSA <- log(gamma_dat_of$YSA)
gamma_dat_of$c.YSA<-gamma_dat_of$YSA-mean(gamma_dat_of$YSA)
gamma_dat_of$Field<-as.character(gamma_dat_of$Field)
gamma_dat_of$Year<-as.factor(as.character(gamma_dat_of$Year))


p.beta.spie <-  brm(log_beta_ENSPIE_p ~  log_YSA + (1 + log_YSA | Field) + (1 | Year),
                   data = gamma_dat_of, family=student(),cores = 4, iter=10000,warmup=1000, chains = 4)


save(p.beta.spie,
     file=Sys.getenv('OFILE'))



