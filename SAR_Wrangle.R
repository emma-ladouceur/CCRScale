

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
  # filter(!Burned %in% c("0") ) %>%
  # filter(!LastCrop %in% c("unknown") ) %>%
  droplevels() %>%
  unite(Plot_id,Transect,Plot,sep="_") 

View(oldfields_cover)

oldfields_cover %>% distinct(Field,YearAbandoned,LastCrop)

oldfield_cover_summary<-oldfields_cover %>% group_by(Field,Year) %>%
  summarise(#min_transect=n_distinct(Transect),
    min_plot=n_distinct(Plot_id))

View(oldfield_cover_summary)

gamma_summary_cover_1 <- oldfields_cover %>%
  group_by(Field,Year) %>%
  nest(lcd_species,sp_pcover) %>%
  ungroup() %>%
  group_by(Field,Year) %>%
  sample_n(1,replace=F) %>% ungroup() %>%
  unnest() %>% 
  group_by(Field,Year) %>%
  summarise(
    N_plot = n_distinct(Plot_id),
    S_cover = n_distinct(lcd_species)) %>%
  ungroup() %>%  arrange(Field, Year)

gamma_summary_cover_2 <- oldfields_cover %>%
  group_by(Field,Year) %>%
  nest(lcd_species,sp_pcover) %>%
  ungroup() %>%
  group_by(Field,Year) %>%
  sample_n(2,replace=F) %>% ungroup() %>%
  unnest() %>% 
  group_by(Field,Year) %>%
  summarise(
    N_plot = n_distinct(Plot_id),
    S_cover = n_distinct(lcd_species)) %>%
  ungroup() %>%  arrange(Field, Year)

gamma_summary_cover_3 <- oldfields_cover %>%
  group_by(Field,Year) %>%
  nest(lcd_species,sp_pcover) %>%
  ungroup() %>%
  group_by(Field,Year) %>%
  sample_n(3,replace=F) %>% ungroup() %>%
  unnest() %>% 
  group_by(Field,Year) %>%
  summarise(
    N_plot = n_distinct(Plot_id),
    S_cover = n_distinct(lcd_species)) %>%
  ungroup() %>%  arrange(Field, Year)

gamma_summary_cover_4 <- oldfields_cover %>%
  group_by(Field,Year) %>%
  nest(lcd_species,sp_pcover) %>%
  ungroup() %>%
  group_by(Field,Year) %>%
  sample_n(4,replace=F) %>% ungroup() %>%
  unnest() %>% 
  group_by(Field,Year) %>%
  summarise(
    N_plot = n_distinct(Plot_id),
    S_cover = n_distinct(lcd_species)) %>%
  ungroup() %>%  arrange(Field, Year)

View(gamma_summary_cover)


sar_cover<- gamma_summary_cover_1 %>% bind_rows(gamma_summary_cover_2)%>% bind_rows(gamma_summary_cover_3)%>% bind_rows(gamma_summary_cover_4)

biomass$lcd_species<- as.factor(as.character(biomass$lcd_species))
levels(biomass$lcd_species)
View(biomass)

oldfields_biomass <- sites %>% left_join(biomass)  %>% filter(!lcd_species %in% c( "Miscellaneous litter") ) %>%
  # filter(!Burned %in% c("0") ) %>%
  # filter(!LastCrop %in% c("unknown") ) %>%
  droplevels()  %>% arrange(Field,Year)

colnames(oldfields_biomass)
View(oldfields_biomass)

# same number of plots per field?
oldfield_summary<-oldfields_biomass %>% group_by(Field,Year) %>%
  summarise(min_trasect=n_distinct(Transect) )
# 4 and this is the minimum, so we dont have to sub sample


View(oldfield_summary)


gamma_summary_biomass_1 <- oldfields_biomass %>%
  group_by(Field,Year) %>%
  nest(lcd_species,sp_biomass) %>%
  ungroup() %>%
  group_by(Field,Year) %>%
  sample_n(1,replace=F) %>% ungroup() %>%
  unnest() %>% 
  group_by(Field,Year) %>%
  summarise(
    N_plot = n_distinct(Transect),
    S_biomass = n_distinct(lcd_species)) %>%
  ungroup() %>%  arrange(Field, Year)

gamma_summary_biomass_2 <- oldfields_biomass %>%
  group_by(Field,Year) %>%
  nest(lcd_species,sp_biomass) %>%
  ungroup() %>%
  group_by(Field,Year) %>%
  sample_n(2,replace=F) %>% ungroup() %>%
  unnest() %>% 
  group_by(Field,Year) %>%
  summarise(
    N_plot = n_distinct(Transect),
    S_biomass = n_distinct(lcd_species)) %>%
  ungroup() %>%  arrange(Field, Year)

gamma_summary_biomass_3 <- oldfields_biomass %>%
  group_by(Field,Year) %>%
  nest(lcd_species,sp_biomass) %>%
  ungroup() %>%
  group_by(Field,Year) %>%
  sample_n(3,replace=F) %>% ungroup() %>%
  unnest() %>% 
  group_by(Field,Year) %>%
  summarise(
    N_plot = n_distinct(Transect),
    S_biomass = n_distinct(lcd_species)) %>%
  ungroup() %>%  arrange(Field, Year)

gamma_summary_biomass_4 <- oldfields_biomass %>%
  group_by(Field,Year) %>%
  summarise(
    N_plot = n_distinct(Transect),
    S_biomass = n_distinct(lcd_species))%>%
  ungroup()  %>% arrange(Field,  Year)

View(gamma_summary_biomass)
View(gamma_summary_biomass)
sar_biomass<- gamma_summary_biomass_1 %>% bind_rows(gamma_summary_biomass_2)%>% bind_rows(gamma_summary_biomass_3)%>% bind_rows(gamma_summary_biomass_4)



View(sar_biomass)




ggplot()+
  






View(sites)
sites_sum <- sites %>% select(Field, YearAbandoned, LastCrop, In_E014,In_E054)

gamma_oldfield_summary <- gamma_summary_cover %>% full_join(gamma_summary_biomass) %>%
  left_join(sites_sum) %>%
  mutate(
    Exp1 = case_when(
      In_E014 == "1" ~ "14"),
    Exp2 = case_when(
      In_E054 == "1" ~ "54"
    )) %>%
  unite(Exp, Exp1,Exp2,sep="&") %>%
  select(-In_E014,-In_E054) %>%
  distinct() %>%
  arrange(Field,Year) 


gamma_oldfield_summary$site_status <- "old field"

View(gamma_oldfield_summary)






e1_biomass <- read.csv("~/Dropbox/Projects/CCRScale/E_001/e001.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

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



# same number of plots per field?
e1_plot_summary<-e1_summary %>% group_by(Field,Year) %>%
  summarise(min_plot=n_distinct(Plot))

View(e1_plot_summary)
View(e1_summary)

gamma_summary_e1 <- e1_summary %>%
  group_by(Exp,site_status,YearAbandoned,Field,Year,Plot) %>%
  nest(Species,Biomass) %>%
  ungroup() %>%
  group_by(Exp,Field,Year) %>%
  sample_n(4,replace=F) %>% ungroup() %>%
  unnest() %>% 
  group_by(Exp,site_status,YearAbandoned,Field,Year) %>%
  summarise(
    N_plot = n_distinct(Plot),
    S_biomass = n_distinct(Species),
    sum_biomass = sum(Biomass),
    S_PIE_biomass = mobr::calc_PIE(Biomass, ENS = T),
    ENSPIE_biomass = vegan::diversity(Biomass, index='invsimpson')) %>%
  ungroup()  %>% arrange(Field,  Year)



View(gamma_summary_e1)




gamma_summary_e1$Year<-as.numeric(gamma_summary_e1$Year)
gamma_summary_e1$YearAbandoned<-as.numeric(gamma_summary_e1$YearAbandoned)

gamma_summary <- gamma_oldfield_summary %>% bind_rows(gamma_summary_e1) %>%
  mutate(YSA = (Year-YearAbandoned)) %>%
  mutate( YSA= ifelse(is.na(YSA) & site_status=="never-plowed","100",YSA))


View(gamma_summary)



write.csv(gamma_summary, "~/Dropbox/Projects/CCRScale/data/gamma_summary.csv")





