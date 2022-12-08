



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

head(alpha_dat)
head(gamma_dat)
head(ccr_dat)

head(alpha_dat)


alpha_dat %>% filter(Field %in% 41)



alpha_dat_of <- alpha_dat %>% filter(site_status == "old field") %>% 
  group_by(Exp, Field) %>%
  mutate(YSA = as.numeric(YSA),
         Year = as.numeric(Year)) %>%
  summarise(alpha_rich = mean(alpha_rich),
            Year_min = min(Year),
            Year_max = max(Year),
            YSA_min = min(YSA),
            YSA_max = max(YSA)) %>%
       mutate( N_Year = Year_max - Year_min) %>%
  #select(-site_status) %>%
  unite(Year_Range, Year_min, Year_max, remove=TRUE, sep="-") %>%
  unite(YSA_Range, YSA_min:YSA_max, remove=FALSE, sep="-")

head(alpha_dat_of)


alpha_dat_np <- alpha_dat %>% filter(site_status == "never-plowed") %>% group_by(Exp,Field,site_status) %>%
  mutate(YSA = as.numeric(Year)) %>%
  summarise(alpha_rich = mean(alpha_rich),
            Year_min = min(Year),
            Year_max = max(Year)) %>%
  mutate( N_Year = Year_max - Year_min) %>%
  unite(Year_Range, Year_min,Year_max, remove=TRUE, sep="-")  %>%
  mutate(YSA_Range=site_status)


head(alpha_dat_np)

alpha_s1 <- alpha_dat_of %>% bind_rows(alpha_dat_np) %>% select(-site_status) %>% arrange(Exp,Field) %>%
  select(Exp,Field,Year_Range,YSA_Range,N_Year,alpha_rich)

head(alpha_s1)
# gamma


gamma_dat_of <- gamma_dat %>% filter(site_status == "old field") %>% group_by(Exp,Field,site_status) %>%
  mutate(YSA = as.numeric(YSA),
         Year = as.numeric(Year)) %>%
  summarise(gamma_rich = mean(gamma_rich),
            Year_min = min(Year),
            Year_max = max(Year),
            YSA_min = min(YSA),
            YSA_max = max(YSA)) %>%
  mutate( N_Year = Year_max - Year_min) %>%
  select(-site_status) %>%
  unite(Year_Range, Year_min, Year_max, remove = TRUE, sep = "-") %>%
  unite(YSA_Range, YSA_min:YSA_max, remove = TRUE, sep = "-")

head(gamma_dat_of)


gamma_dat_np <- gamma_dat %>% filter(site_status == "never-plowed") %>% group_by(Exp,Field,site_status) %>%
  mutate(
         Year = as.numeric(Year)) %>%
  summarise(gamma_rich = mean(gamma_rich),
            Year_min = min(Year),
            Year_max = max(Year)) %>%
  mutate( N_Year = Year_max - Year_min) %>%
  unite(Year_Range, Year_min,Year_max, remove=TRUE, sep="-")  %>%
  mutate(YSA_Range=site_status)


head(gamma_dat_np)

gamma_s1<-gamma_dat_of %>% bind_rows(gamma_dat_np) %>% select(-site_status) %>% arrange(Exp,Field)

head(gamma_s1)

#beta

head(gamma_dat)

beta_dat_of <- gamma_dat %>% filter(site_status == "old field") %>% group_by(Exp,Field,site_status) %>%
  mutate(YSA = as.numeric(YSA),
         Year = as.numeric(Year)) %>%
  summarise(beta_rich = mean(beta_div),
            Year_min = min(Year),
            Year_max = max(Year),
            YSA_min = min(YSA),
            YSA_max = max(YSA)) %>%
  mutate( N_Year = Year_max - Year_min) %>%
  select(-site_status) %>%
  unite(Year_Range, Year_min,Year_max, remove=TRUE, sep="-") %>%
  unite(YSA_Range, YSA_min:YSA_max, remove=TRUE, sep="-")

head(beta_dat_of)


beta_dat_np <- gamma_dat %>% filter(site_status == "never-plowed") %>% group_by(Exp,Field,site_status) %>%
  mutate(
         Year = as.numeric(Year)) %>%
  summarise(beta_rich = mean(beta_div),
            Year_min = min(Year),
            Year_max = max(Year)) %>%
  mutate( N_Year = Year_max - Year_min) %>%
  unite(Year_Range, Year_min,Year_max, remove=TRUE, sep="-")  %>%
  mutate(YSA_Range=site_status)


head(beta_dat_np)

beta_s1 <- beta_dat_of %>% bind_rows(beta_dat_np) %>% select(-site_status) %>% arrange(Exp,Field)

head(beta_s1)


div_sum_prep <- alpha_s1 %>% left_join(beta_s1) %>% left_join(gamma_s1) %>% 
  arrange(Exp,Field) %>% mutate(Site = Field) %>%
  mutate(Site = as.factor(Site)) %>%
  mutate( `Old field` = fct_recode( Site,  "A" = "601",
                                                           "B" = "600",
                                                           "C" = "10",
                                                           "D" = "28",
                                                           "E" = "41",
                                                           "F" = "39",
                                                           "G" = "40",
                                                           "H" = "4",
                                                           "I" = "44",
                                                           "J" = "53",
                                                           "K" = "47",
                                                           "L" = "21",
                                                           "M" = "70",
                                                           "N" = "5",
                                                           "O" = "27",
                                                           "P" = "45",
                                                           "Q" = "32",
                                                           "R" = "35",
                                                           "S" = "72"
  )) %>% select(-Site) %>% arrange(Exp, YSA_Range) 


div_sum_np <- div_sum_prep %>%
  ungroup() %>%
  filter( YSA_Range %in% "never-plowed" ) %>%
mutate(Site = Field) %>% select(-c(Field, `Old field`)) %>%
  mutate(Site = as.factor(Site))

head(div_sum_np)

div_sum_of <- div_sum_prep %>%
  filter( !YSA_Range %in% "never-plowed" ) %>%
  mutate(Field = paste0("(", Field, ")")) %>%
  unite( Site, `Old field`, Field, sep = " ", remove = T)


div_sum <- div_sum_of %>% rbind(div_sum_np) %>% 
  arrange(Exp, Site) %>%
  mutate(alpha_rich = round(alpha_rich, 2),
         beta_div = round(beta_rich, 2),
         gamma_rich = round(gamma_rich, 2)) %>%
  select(-beta_rich) %>% 
  relocate( beta_div, .after = alpha_rich)

head(div_sum)

div_sum$YSA_Range<-as.factor(as.character(div_sum$YSA_Range))


write.csv(div_sum, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/Table_S1.csv")

