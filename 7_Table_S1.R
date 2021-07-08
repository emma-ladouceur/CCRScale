



library(tidyverse)
library(ggplot2)
library(brms)
library(bayesplot)
library(patchwork)
library(RColorBrewer)

ccr_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/e014_e133_cleaned_1983-2016_EL.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
alpha_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/alpha_div.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
gamma_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/gamma_div.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

# SPIE = mobr
# ENSPIE = vegan - inverse Simpson's

View(alpha_dat)
View(gamma_dat)
View(ccr_dat)

View(alpha_dat)



alpha_dat_of <- alpha_dat %>% filter(site_status == "old field") %>% group_by(Exp,Field,site_status) %>%
  summarise(alpha_rich = mean(alpha_rich),
            Year_min = min(Year),
            Year_max = max(Year),
            YSA_min = min(YSA),
            YSA_max = max(YSA)) %>%
       mutate( N_Year = Year_max - Year_min) %>%
  select(-site_status) %>%
  unite(Year_Range, Year_min,Year_max, remove=TRUE, sep="-") %>%
  unite(YSA_Range, YSA_min:YSA_max, remove=FALSE, sep="-")

View(alpha_dat_of)


alpha_dat_np <- alpha_dat %>% filter(site_status == "never-plowed") %>% group_by(Exp,Field,site_status) %>%
  summarise(alpha_rich = mean(alpha_rich),
            Year_min = min(Year),
            Year_max = max(Year)) %>%
  mutate( N_Year = Year_max - Year_min) %>%
  unite(Year_Range, Year_min,Year_max, remove=TRUE, sep="-")  %>%
  mutate(YSA_Range=site_status)


View(alpha_dat_np)

alpha_s1<-alpha_dat_of %>% bind_rows(alpha_dat_np) %>% select(-site_status) %>% arrange(Exp,Field) %>%
  select(Exp,Field,Year_Range,YSA_Range,N_Year,alpha_rich)

View(alpha_s1)
# gamma


gamma_dat_of <- gamma_dat %>% filter(site_status == "old field") %>% group_by(Exp,Field,site_status) %>%
  summarise(gamma_rich = mean(gamma_rich),
            Year_min = min(Year),
            Year_max = max(Year),
            YSA_min = min(YSA),
            YSA_max = max(YSA)) %>%
  mutate( N_Year = Year_max - Year_min) %>%
  select(-site_status) %>%
  unite(Year_Range, Year_min,Year_max, remove=TRUE, sep="-") %>%
  unite(YSA_Range, YSA_min:YSA_max, remove=TRUE, sep="-")

View(gamma_dat_of)


gamma_dat_np <- gamma_dat %>% filter(site_status == "never-plowed") %>% group_by(Exp,Field,site_status) %>%
  summarise(gamma_rich = mean(gamma_rich),
            Year_min = min(Year),
            Year_max = max(Year)) %>%
  mutate( N_Year = Year_max - Year_min) %>%
  unite(Year_Range, Year_min,Year_max, remove=TRUE, sep="-")  %>%
  mutate(YSA_Range=site_status)


View(gamma_dat_np)

gamma_s1<-gamma_dat_of %>% bind_rows(gamma_dat_np) %>% select(-site_status) %>% arrange(Exp,Field)

View(gamma_s1)

#beta


beta_dat_of <- gamma_dat %>% filter(site_status == "old field") %>% group_by(Exp,Field,site_status) %>%
  summarise(beta_rich = mean(beta_rich),
            Year_min = min(Year),
            Year_max = max(Year),
            YSA_min = min(YSA),
            YSA_max = max(YSA)) %>%
  mutate( N_Year = Year_max - Year_min) %>%
  select(-site_status) %>%
  unite(Year_Range, Year_min,Year_max, remove=TRUE, sep="-") %>%
  unite(YSA_Range, YSA_min:YSA_max, remove=TRUE, sep="-")

View(beta_dat_of)


beta_dat_np <- gamma_dat %>% filter(site_status == "never-plowed") %>% group_by(Exp,Field,site_status) %>%
  summarise(beta_rich = mean(beta_rich),
            Year_min = min(Year),
            Year_max = max(Year)) %>%
  mutate( N_Year = Year_max - Year_min) %>%
  unite(Year_Range, Year_min,Year_max, remove=TRUE, sep="-")  %>%
  mutate(YSA_Range=site_status)


View(beta_dat_np)

beta_s1<-beta_dat_of %>% bind_rows(beta_dat_np) %>% select(-site_status) %>% arrange(Exp,Field)

View(beta_s1)


rich_dat<-alpha_s1 %>% left_join(beta_s1) %>% left_join(gamma_s1) %>% 
  arrange(Exp,Field)


View(rich_dat)

rich_dat$YSA_Range<-as.factor(as.character(rich_dat$YSA_Range))


write.csv(rich_dat, "~/Dropbox/Projects/CCRScale/E14 _133/Table_S1.csv")

