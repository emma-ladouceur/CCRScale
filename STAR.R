
library(tidyverse)
library(patchwork)
library(iNEXT)
library(ggplot2)
library(viridis)


e1_biomass <- read.csv("~/Dropbox/Projects/CCRScale/E_001/e001.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


# Exp e1


distinct(e1_biomass,Species) %>% arrange(Species)

oldfields_biomass <- e1_biomass %>% filter(!Species %in% c("Miscellaneous litter", "Mosses & lichens","Mosses & lichens 2") ) %>%
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


ccr_prep <- plot_attr %>% left_join(e1_long) 


View(ccr_prep)


ccr_comm_prep <- ccr_prep %>%  group_by(group,x,y,Year,YSA,Field,Transect,Species) %>%
  summarise(pres=sum(pres))%>%
  ungroup() %>%
  unite(sample_id,Year,Field,Transect, sep="_",remove=F) %>%
  # spread(sample_id,pres) %>%
  # replace(is.na(.), 0) %>%
  unite(group,group,Field,sep="_",remove=F) %>%
  select(-group,-x,-y) %>%
  unite(site_id,Year,Field, sep="_",remove=F) %>%
  arrange(Year,YSA,Field,Transect) %>%
  #select(-Year,-Field,-YSA,-Transect)  %>%
  distinct() 

View(ccr_comm_prep)


View(time.yr)


basic_dat<-ccr_comm_prep %>% select(Year,YSA,Field,site_id) %>%
  rename(site = site_id) %>% distinct()

View(basic_dat)

ccr.list.all <- ccr_comm_prep %>%
  split(.$site_id)

View(ccr.list.all)

ccr.list.all <- map(ccr.list.all, ~ .x %>% 
                      select(-site_id,-Year,-Field,-YSA,-Transect) %>%
                      spread(key = sample_id, value = pres) %>%
                      replace(is.na(.), 0) %>%
                      column_to_rownames(var = "Species") )

View(ccr.list.all)

ccr.all.out <- iNEXT(ccr.list.all, q=0, datatype="incidence_raw")

ccr.all.df <- fortify(ccr.all.out, type=1)

View(ccr.all.df)

ccr.star <- ccr.all.df %>% filter(method == "interpolated" | method == "observed") %>% distinct ()

View(ccr.star)

time.yr <- ccr.star %>% left_join(basic_dat) %>%
select(Year) %>% distinct() %>% mutate(Time = 1:n())

time.yr



dat <- ccr.star %>% left_join(basic_dat) %>%
  left_join(time.yr) 

View(dat)
head(dat)

dat$log.y<-log(dat$y)
dat$log.Time<-log(dat$Time)
dat$log10.y<-log10(dat$y)
dat$log10.Time<-log10(dat$Time)
dat$log.x<-log(dat$x)
dat$log10.x<-log10(dat$x)

dat$x<-as.factor(as.character(dat$x))

star.a<-dat %>% filter(Field=="A")
star.b<-dat %>% filter(Field=="B")
star.c<-dat %>% filter(Field=="C")
star.d<-dat %>% filter(Field=="D")

View(dat)

dat$x<-factor(dat$x,  levels=c("1" , "2" ,"3" ,"4" , "5" ,"6" ,"7" ,"8" , "9" ,"10" ))


star.ysa<-dat %>% filter(!Field=="D")

star.ysa$YSA.f<-as.factor(as.character(star.ysa$YSA))
levels(star.ysa$YSA.f)
star.ysa$YSA.f<-factor(star.ysa$YSA.f,  levels=c("14" , "15" , "16" , "17" , "18" , "19" ,"20" , "21" , "22" , "23" , "24" , "25" , "26" , "27" , "28" , "29" ,   "30" , "31" , "32" , "33" , "34" , "35" , "36" , "37" , "38" , "39" ,   "40" , "41" , "42" , "43" , "44" , "45" , "46" , "47" , "48" , "49" ,   "50" , "51" , "52" , "53" , "54" , "55" ,"56" , "57" , "58" , "59" ,    "60" , "61" , "62" , "63" , "64" , "65" , "66" , "67" , "68" , "69" ,    "70" , "71" , "72" , "73" , "74" , "75" , "76" , "77" , "78" , "79" , "80" , "81"))

View(star.ysa)

head(dat)
species.area.ysa<-ggplot() +
 # facet_grid(.~Field)+
  geom_point(data = star.ysa,
             aes(x = log10.x, y = log10.y,
                 colour = YSA.f,line=Field), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_line(data = star.ysa,
            aes(x = log10.x, y = log10.y,
                colour = YSA.f,line=Field))+
  scale_color_viridis(discrete=TRUE,option = "plasma") +
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), 
                        legend.direction = "horizontal", legend.position = "bottom")+
  guides(col = guide_legend(ncol = 13))+
  labs(title = "Species-Area",color="Year Since Abandonment") + ylab("Log10 Species Richness") + xlab("Log10 Area") 

species.area.ysa

star.ysa$YSA<-as.numeric(star.ysa$YSA)
star.ysa$log10.YSA<-log10(star.ysa$YSA)

species.time.ysa<-ggplot() +
  facet_grid(.~Field)+
  geom_point(data = star.ysa,
             aes(x = log10.YSA, y = log10.y,
                 colour = `x`), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_line(data = star.ysa,
            aes(x = log10.YSA, y = log10.y,
                colour = `x`))+
  scale_color_viridis(discrete=TRUE) +
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), 
                        legend.direction = "horizontal", legend.position = "bottom" )+
  guides(col = guide_legend(ncol = 10))+
  labs(title = "Species-Time",color= "Number of Samples") + ylab("Log10 Species Richness") + xlab("Log10 Time") 

species.time.ysa


