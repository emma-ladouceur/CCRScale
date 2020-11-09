

# old field wrangle




library(tidyverse)
library(vegan)
library(mobr)


species <- read.csv("~/Dropbox/Projects/CCRScale/Clark_2018/species.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

sites <- read.csv("~/Dropbox/Projects/CCRScale/Clark_2018/sites.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

cover <- read.csv("~/Dropbox/Projects/CCRScale/Clark_2018/sp_pcover.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

biomass <- read.csv("~/Dropbox/Projects/CCRScale/Clark_2018/sp_biomass.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


cover$Field<-as.character(cover$Field)
biomass$Field<-as.character(biomass$Field)


cover$lcd_species<- as.factor(as.character(cover$lcd_species))
levels(cover$lcd_species)

oldfields_cover <- sites %>% left_join(cover) %>% filter(!lcd_species %in% c("Bare ground", "Miscellaneous litter") ) %>%
  droplevels()

View(oldfields_cover)


alpha_summary_cover <- oldfields_cover %>%
  group_by(Field,Transect,Year, Plot) %>%
  summarise(
    S_cover = n_distinct(lcd_species)) %>%
  ungroup() %>%  arrange(Field, Transect, Year, Plot)
  
View(alpha_summary_cover)


biomass$lcd_species<- as.factor(as.character(biomass$lcd_species))
levels(biomass$lcd_species)

oldfields_biomass <- sites %>% left_join(biomass)  %>% filter(!lcd_species %in% c( "Miscellaneous litter","Mosses & lichens") ) %>%
  droplevels()

colnames(oldfields_biomass)

alpha_summary_biomass <- oldfields_biomass %>%
  group_by(Field,Transect,Year) %>%
  summarise(
    S_biomass = n_distinct(lcd_species),
    plot_biomass = sum(sp_biomass),
    S_PIE_biomass = mobr::calc_PIE(sp_biomass, ENS = T),
    ENSPIE_biomass = vegan::diversity(sp_biomass, index='invsimpson')) %>%
  ungroup()  %>% arrange(Field, Transect, Year)


View(alpha_summary_biomass)

View(sites)
sites_sum <- sites %>% select(Field, Transect, YearAbandoned, LastCrop, In_E014,In_E054)

alpha_oldfield_summary <- alpha_summary_cover %>% full_join(alpha_summary_biomass) %>%
  left_join(sites_sum) %>%
  mutate(
    Exp1 = case_when(
      In_E014 == "1" ~ "14"),
    Exp2 = case_when(
      In_E054 == "1" ~ "54"
    )) %>%
  unite(Exp, Exp1,Exp2,sep="&") %>%
  select(-In_E014,-In_E054) %>%
  arrange(Field,Transect,Year,Plot) 


alpha_oldfield_summary$site_status <- "old field"

View(alpha_oldfield_summary)






e1_biomass <- read.csv("~/Dropbox/Projects/CCRScale/E_001/e001.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

e1_biomass<- e1_biomass %>%  filter(!Species %in% c( "Miscellaneous litter","Mosses & lichens","Mosses & lichens 2") ) %>%
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
  
  


View(e1_summary)


alpha_summary_e1 <- e1_summary %>%
  group_by(Exp,site_status,YearAbandoned,Field,Year,Plot) %>%
  summarise(
    S_biomass = n_distinct(Species),
    plot_biomass = sum(Biomass),
    S_PIE_biomass = mobr::calc_PIE(Biomass, ENS = T),
    ENSPIE_biomass = vegan::diversity(Biomass, index='invsimpson')) %>%
  ungroup()  %>% arrange(Field, Plot, Year)


View(alpha_summary_e1)

alpha_summary_e1$Plot<-as.factor(as.character(alpha_summary_e1$Plot))
alpha_summary_e1$Year<-as.numeric(alpha_summary_e1$Year)
alpha_summary_e1$YearAbandoned<-as.numeric(alpha_summary_e1$YearAbandoned)

alpha_summary <- alpha_summary_e1 %>% 
  mutate(YSA = (Year-YearAbandoned)) %>%
  mutate( YSA= ifelse(is.na(YSA) & site_status=="never-plowed","100",YSA))


summary(alpha_summary)



write.csv(alpha_summary, "~/Dropbox/Projects/CCRScale/data/alpha_summary.csv")








# SAR Wrangle

View(e1_summary)

View(oldfields_biomass)


