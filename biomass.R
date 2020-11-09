

library(tidyverse)
library(patchwork)
library(iNEXT)
library(ggplot2)
library(viridis)


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

e1_wide<-e1 %>% group_by(Exp,Year,YSA,Field,Transect,site_status,Species) %>%
  summarise(Biomass=sum(Biomass)) %>%
  ungroup()%>%
  group_by(Exp,Year,YSA,Field,Transect,site_status) %>%
  spread(Species,Biomass) %>%
  ungroup()



e1_select<- e1_wide %>% group_by(Exp,Year,YSA,Field,site_status) %>%
  sample_n(10) 


e1_long<-e1_select %>% gather(Species,Biomass, "Achillea millefolium(lanulosa)":"Viola sp.") %>%
  filter(!is.na(Biomass)) %>% droplevels() %>%
  arrange(YSA,Field,Transect)

e1_long$pres<-1

e1_long$YSA<-as.numeric(e1_long$YSA)

plot_prep<-e1_long %>% distinct(Year,YSA,Field, Transect,site_status)%>%
  group_by(YSA,Field) %>% mutate(y = 1:n()) %>%
  arrange(YSA,Field,Transect) %>%
  ungroup()

plot_attr<- plot_prep %>% distinct(Year,Field) %>%  mutate(x = 1:n()) %>%
  left_join(plot_prep) %>%
  rename(group=site_status) %>%
  arrange(YSA,Field,Transect) 

View(plot_attr)


ccr_prep <- plot_attr %>% left_join(e1_long) %>%
  group_by(Year,YSA,Field,site_status,Transect)%>%
  summarise(Biomass = sum(Biomass))


colnames(ccr_prep)
View(ccr_prep)



of_bm <- ccr_prep %>% filter(site_status == "old field" ) 
np_bm <- ccr_prep %>% filter(site_status == "never-plowed" ) 


plot_bm<-ggplot() +
  geom_point(data = ccr_prep,
             aes(x = Year, y = Biomass,
                 colour = Field, shape=Field), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_smooth(data = ccr_prep,
              aes(x = Year, y = Biomass,
                  colour = Field,group=Field))+
  # stat_summary(data = of_bm,
  #              aes(x = Year, y = Biomass),fun.data=mean_sdl, fun.args = list(mult=1), 
  #              geom="pointrange", color="black",shape=16,size=0.25)+
  # stat_summary(data = of_bm,
  #              aes(x = Year, y = Biomass),fun = mean, geom="line",size=0.55) +
  # stat_summary(data = np_bm,
  #              aes(x = Year, y = Biomass),fun.data=mean_sdl, fun.args = list(mult=1), 
  #              geom="pointrange", color="black",shape=16,size=0.25)+
  # stat_summary(data = np_bm,
  #              aes(x = Year, y = Biomass),fun = mean, geom="line",size=0.55) +
  scale_color_manual(values =  c("#A1C720FF","#15983DFF","#00AFBB", "#E7B800", "#FC4E07"))  + 
  #scale_x_continuous(breaks=c(0,25,50,75,89)) +
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), #plot.margin=margin(t=4,1.5,1,1, "lines"),
                        legend.direction = "horizontal", legend.position = "top" )+
  labs(title =  'Plot Biomass') + ylab((expression(paste('Biomass g/',m^2, '')))) + xlab("Year") 


plot_bm

