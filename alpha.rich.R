
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/CCRScale'
plot <- read.csv(paste0(path, '/alpha_div.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


alpha_dat_np <- alpha_dat %>% filter(site_status == "never-plowed") %>% group_by(Field,YSA) %>%
  summarise(alpha_rich_p_np = mean(alpha_rich))

alpha_dat_of <- alpha_dat %>% filter(site_status == "old field") %>%  group_by(Field,YSA) %>%
  summarise(alpha_rich_p = mean(alpha_rich))

alpha_dat_of <- alpha_dat %>% filter(site_status == "old field") 

alpha_dat_of$alpha_rich_p<-(alpha_dat_of$alpha_rich/8.13 *100)

alpha_dat_of$YSA <- as.numeric(alpha_dat_of$YSA)
alpha_dat_of$log_alpha_rich_p <- log(alpha_dat_of$alpha_rich_p)
alpha_dat_of$log_YSA <- log(alpha_dat_of$YSA)
alpha_dat_of$c.YSA<-alpha_dat_of$YSA-mean(alpha_dat_of$YSA)
alpha_dat_of$Year<-as.factor(as.chracter(alpha_dat_of$Year))


p.alpha.rich <-  brm(log_alpha_rich_p ~  log_YSA + Year + ( log_YSA + Year | Field/Transect/Plot), 
                     data = alpha_dat_of, cores = 4, iter=2000, chains = 4)


save(p.alpha.rich,
     file=Sys.getenv('OFILE'))



