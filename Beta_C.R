

library(betaC)
library(vegan)
library(tidyverse)







e1_biomass <- read.csv("~/Dropbox/Projects/CCRScale/E_001/e001.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


# Exp e1


distinct(e1_biomass,Species) %>% arrange(Species)

oldfields_biomass <- e1_biomass %>% filter(!Species %in% c( "Miscellaneous litter", "Mosses & lichens","Mosses & lichens 2") ) %>%
  droplevels()


View(e1_biomass)
colnames(e1_biomass)
e1_biomass$NTrt<-as.factor(as.character(e1_biomass$NTrt))
levels(e1_biomass$NAdd)

e1_biomass$NAdd<-as.factor(as.character(e1_biomass$NAdd))
levels(e1_biomass$NAdd)

e1_biomass$NitrAdd<-as.factor(as.character(e1_biomass$NitrAdd))
levels(e1_biomass$NitrAdd)


e1_summary <- e1_biomass %>% 
  filter(NAdd %in% c("0"),
         NitrAdd %in% c("0"),) %>%
  droplevels() %>%
  mutate(
    YearAbandoned = case_when(
      Field == "A" ~ "1968",
      Field == "B" ~ "1957",
      Field == "C" ~ "1934",
      Field == "D" ~ "never-plowed"),
    site_status = case_when(
      Field == "A" ~ "old field",
      Field == "B" ~ "old field",
      Field == "C" ~ "old field",
      Field == "D" ~ "never-plowed"
    )) %>% select(-NTrt,-NAdd,-NitrAdd,-NAtm.NAdd) 

e1_summary$Year<-as.numeric(e1_summary$Year)
e1_summary$YearAbandoned<-as.numeric(e1_summary$YearAbandoned)

e1<-e1_summary %>% 
  mutate(YSA = (Year-YearAbandoned)) %>%
  mutate( YSA= ifelse(is.na(YSA) & site_status=="never-plowed","never-plowed",YSA)) %>%
  select(Exp,Year,YSA,Field,Plot,Species,Biomass,site_status) %>%
  rename(Transect=Plot)%>%
  arrange(YSA,Field,Transect)


distinct(e1,YSA,Field,Transect)
distinct(oldfields_summary,YSA,Field,Transect)
View(e1)
View(oldfields_summary)

e1$Biomass_mult<-e1$Biomass*10

e1$Biomass_mult<-round(e1$Biomass_mult)

e1<-e1 %>% filter(Year == "1992")

field.A <- e1 %>% filter(Field =="A")
field.B <- e1 %>% filter(Field =="B")
field.C <- e1 %>% filter(Field =="C")
field.D <- e1 %>% filter(Field =="D")

A_wide<-field.A %>% group_by(Exp,Year,YSA,Field,Transect,site_status,Species) %>%
  summarise(Biomass=sum(Biomass_mult)) %>%
  ungroup()%>%
  group_by(Exp,Year,YSA,Field,Transect,site_status) %>%
  spread(Species,Biomass) %>%
  ungroup() %>%   replace(is.na(.), 0) %>%
  group_by(Exp,Year,YSA,Field,site_status) %>%
    sample_n(10) %>%
  unite(site_id,Year,Field,Transect, sep="_",remove=T) %>%
  column_to_rownames(var = "site_id") %>%
  select(-Exp, -YSA, -site_status)


View(A_wide)


B_wide<-field.B %>% group_by(Exp,Year,YSA,Field,Transect,site_status,Species) %>%
  summarise(Biomass=sum(Biomass_mult)) %>%
  ungroup()%>%
  group_by(Exp,Year,YSA,Field,Transect,site_status) %>%
  spread(Species,Biomass) %>%
  ungroup() %>%   replace(is.na(.), 0) %>%
  group_by(Exp,Year,YSA,Field,site_status) %>%
  sample_n(10) %>%
  unite(site_id,Year,Field,Transect, sep="_",remove=T) %>%
  column_to_rownames(var = "site_id") %>%
  select(-Exp, -YSA, -site_status)


C_wide<-field.C %>% group_by(Exp,Year,YSA,Field,Transect,site_status,Species) %>%
  summarise(Biomass=sum(Biomass_mult)) %>%
  ungroup()%>%
  group_by(Exp,Year,YSA,Field,Transect,site_status) %>%
  spread(Species,Biomass) %>%
  ungroup() %>%   replace(is.na(.), 0) %>%
  group_by(Exp,Year,YSA,Field,site_status) %>%
  sample_n(10) %>%
  unite(site_id,Year,Field,Transect, sep="_",remove=T) %>%
  column_to_rownames(var = "site_id") %>%
  select(-Exp, -YSA, -site_status)


D_wide<-field.D %>% group_by(Exp,Year,YSA,Field,Transect,site_status,Species) %>%
  summarise(Biomass=sum(Biomass_mult)) %>%
  ungroup()%>%
  group_by(Exp,Year,YSA,Field,Transect,site_status) %>%
  spread(Species,Biomass) %>%
  ungroup() %>%   replace(is.na(.), 0) %>%
  group_by(Exp,Year,YSA,Field,site_status) %>%
  sample_n(10) %>%
  unite(site_id,Year,Field,Transect, sep="_",remove=T) %>%
  column_to_rownames(var = "site_id") %>%
  select(-Exp, -YSA, -site_status)


C_target(A_wide)
C_target(B_wide)
C_target(C_wide)
C_target(D_wide)


beta_C(A_wide, 0.99)

beta_true(A_wide)

beta_C(B_wide,0.99)

beta_true(B_wide)

beta_C(C_wide,0.99)

beta_true(C_wide)


beta_C(D_wide,0.99)

beta_true(D_wide)




