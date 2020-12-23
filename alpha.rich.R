
# This is code to be submitted to the cluster equivalent to  lines 53-55 in percent_models.R
# Since alpha model is the largest running on a server or cluster is recommended
# this code is paired with the cluster submit script 'alpha.rich.sh'

library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/CCRScale'
alpha_dat <- read.csv(paste0(path, '/alpha_div.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


alpha_dat_np <- alpha_dat %>% filter(site_status == "never-plowed") %>% 
  summarise(alpha_rich_p_np = mean(alpha_rich))

alpha_dat_of <- alpha_dat %>% filter(site_status == "old field") %>%  
  summarise(alpha_rich_p = mean(alpha_rich))

alpha_dat_of <- alpha_dat %>% filter(site_status == "old field") 

alpha_dat_of$alpha_rich_p<-(alpha_dat_of$alpha_rich/9.21 * 100)

alpha_dat_of$YSA <- as.numeric(alpha_dat_of$YSA)
alpha_dat_of$log_alpha_rich_p <- log(alpha_dat_of$alpha_rich_p)
alpha_dat_of$log_YSA <- log(alpha_dat_of$YSA)
alpha_dat_of$c.YSA<-alpha_dat_of$YSA-mean(alpha_dat_of$YSA)
alpha_dat_of$Year<-as.factor(as.character(alpha_dat_of$Year))
alpha_dat_of$Field<-as.factor(as.character(alpha_dat_of$Field))


p.alpha.rich.s <-  brm(log_alpha_rich_p ~  log_YSA + ( 1 + log_YSA  | Field/Transect/Plot) + (1 | Year), 
                     data = alpha_dat_of, family=student(), cores = 4, iter=10000,warmup = 1000, control =
                       list(adapt_delta = 0.99), chains = 4)

save(p.alpha.rich.s,
     file=Sys.getenv('OFILE'))



