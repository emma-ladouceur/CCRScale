
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/CCRScale'
gamma_dat <- read.csv(paste0(path, '/gamma_div.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

gamma_dat_np <- gamma_dat %>% filter(site_status == "never-plowed") %>% 
  summarise(gamma_rspie_p_np = mean(gamma_ENSPIE))

gamma_dat_np

gamma_dat_of <- gamma_dat %>% filter(site_status == "old field") %>% 
  summarise(gamma_spie_p = mean(gamma_ENSPIE))

gamma_dat_of

gamma_dat_of <- gamma_dat %>% filter(site_status == "old field") 

gamma_dat_of$gamma_ENSPIE_p<-(gamma_dat_of$gamma_ENSPIE/33.57118 *100)


head(gamma_dat_of)

gamma_dat_of$YSA <- as.numeric(gamma_dat_of$YSA)
gamma_dat_of$log_gamma_ENSPIE_p <- log(gamma_dat_of$gamma_ENSPIE_p)
gamma_dat_of$log_YSA <- log(gamma_dat_of$YSA)
gamma_dat_of$c.YSA<-gamma_dat_of$YSA-mean(gamma_dat_of$YSA)
gamma_dat_of$Field<-as.character(gamma_dat_of$Field)
gamma_dat_of$Year<-as.factor(as.character(gamma_dat_of$Year))


p.gamma.spie<-  brm(log_gamma_ENSPIE_p ~  log_YSA  +  ( 1 + log_YSA  | Field) + (1 | Year),
                    data = gamma_dat_of, control = list(adapt_delta = 0.99), cores = 4, iter=12000, warmup=1000, chains = 4)


save(p.gamma.spie,
     file=Sys.getenv('OFILE'))



