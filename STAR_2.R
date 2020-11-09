



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

head(ccr_comm_prep)


time.yr <- ccr_comm_prep %>% 
  select(Field,Year) %>% distinct() %>% arrange(Field,Year) %>% group_by(Field) %>%
  mutate(Time = 1:n()) %>% arrange(Field,Year,Time)

View(time.yr)

plot.n <- ccr_comm_prep %>% 
  select(Field,Year,Transect) %>% distinct() %>% arrange(Field,Year,Transect) %>% group_by(Field,Year) %>%
  mutate(Plot.n = 1:n()) %>% arrange(Field,Year,Transect,Plot.n)

View(plot.n)

ccr_prep <- ccr_comm_prep %>% left_join(time.yr) %>% left_join(plot.n) 

View(ccr_prep)

colnames(ccr_prep)


ccr.list.1 <- ccr_prep %>% filter( Time == "1") %>%
  split(.$Field)


ccr.list.1 <- map(ccr.list.1, ~ .x %>% 
                           select(Plot.n,Species,pres) %>%
                           spread(key = Plot.n, value = pres) %>%
                           replace(is.na(.), 0) %>%
                           column_to_rownames(var = "Species") )

View(ccr.list.1)

ccr.all.out.time.1 <- iNEXT(ccr.list.1, q=0, datatype="incidence_raw")

View(ccr.all.out.time)


ccr.all.time.df.1 <- fortify(ccr.all.out.time.1, type=1)

View(ccr.all.time.df.1)

ccr.star.time.1 <- ccr.all.time.df.1 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "1")

View(ccr.star.time.1)


# 2
ccr.list.2 <- ccr_prep %>% filter( Time == "1" | Time =="2") %>%
  split(.$Field)

ccr.list.2 <- map(ccr.list.2, ~ .x %>% 
                    select(Plot.n,Species,pres) %>%
                    distinct() %>%
                    spread(key = Plot.n, value = pres) %>%
                    replace(is.na(.), 0) %>%
                    column_to_rownames(var = "Species") )

ccr.all.out.time.2 <- iNEXT(ccr.list.2, q=0, datatype="incidence_raw")

ccr.all.time.df.2 <- fortify(ccr.all.out.time.2, type=1)


ccr.star.time.2 <- ccr.all.time.df.2 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "2")


# 3
ccr.list.3 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3") %>%
  split(.$Field)

ccr.list.3 <- map(ccr.list.3, ~ .x %>% 
                    select(Plot.n,Species,pres) %>%
                    distinct() %>%
                    spread(key = Plot.n, value = pres) %>%
                    replace(is.na(.), 0) %>%
                    column_to_rownames(var = "Species") )

ccr.all.out.time.3 <- iNEXT(ccr.list.3, q=0, datatype="incidence_raw")

ccr.all.time.df.3 <- fortify(ccr.all.out.time.3, type=1)


ccr.star.time.3 <- ccr.all.time.df.3 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "3")



# 4
ccr.list.4 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4") %>%
  split(.$Field)

ccr.list.4 <- map(ccr.list.4, ~ .x %>% 
                    select(Plot.n,Species,pres) %>%
                    distinct() %>%
                    spread(key = Plot.n, value = pres) %>%
                    replace(is.na(.), 0) %>%
                    column_to_rownames(var = "Species") )

ccr.all.out.time.4 <- iNEXT(ccr.list.4, q=0, datatype="incidence_raw")

ccr.all.time.df.4 <- fortify(ccr.all.out.time.4, type=1)


ccr.star.time.4 <- ccr.all.time.df.4 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "4")


# 5
ccr.list.5 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5") %>%
  split(.$Field)

ccr.list.5 <- map(ccr.list.5, ~ .x %>% 
                    select(Plot.n,Species,pres) %>%
                    distinct() %>%
                    spread(key = Plot.n, value = pres) %>%
                    replace(is.na(.), 0) %>%
                    column_to_rownames(var = "Species") )

ccr.all.out.time.5 <- iNEXT(ccr.list.5, q=0, datatype="incidence_raw")

ccr.all.time.df.5 <- fortify(ccr.all.out.time.5, type=1)


ccr.star.time.5 <- ccr.all.time.df.5 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "5")



# 6
ccr.list.6 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6") %>%
  split(.$Field)

ccr.list.6 <- map(ccr.list.6, ~ .x %>% 
                    select(Plot.n,Species,pres) %>%
                    distinct() %>%
                    spread(key = Plot.n, value = pres) %>%
                    replace(is.na(.), 0) %>%
                    column_to_rownames(var = "Species") )

ccr.all.out.time.6 <- iNEXT(ccr.list.6, q=0, datatype="incidence_raw")

ccr.all.time.df.6 <- fortify(ccr.all.out.time.6, type=1)


ccr.star.time.6 <- ccr.all.time.df.6 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "6")


# 7
ccr.list.7 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6"| Time =="7") %>%
  split(.$Field)

ccr.list.7 <- map(ccr.list.7, ~ .x %>% 
                    select(Plot.n,Species,pres) %>%
                    distinct() %>%
                    spread(key = Plot.n, value = pres) %>%
                    replace(is.na(.), 0) %>%
                    column_to_rownames(var = "Species") )

ccr.all.out.time.7 <- iNEXT(ccr.list.7, q=0, datatype="incidence_raw")

ccr.all.time.df.7 <- fortify(ccr.all.out.time.7, type=1)


ccr.star.time.7 <- ccr.all.time.df.7 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "7")



# 8
ccr.list.8 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6"| Time =="7"| Time =="8") %>%
  split(.$Field)

ccr.list.8 <- map(ccr.list.8, ~ .x %>% 
                    select(Plot.n,Species,pres) %>%
                    distinct() %>%
                    spread(key = Plot.n, value = pres) %>%
                    replace(is.na(.), 0) %>%
                    column_to_rownames(var = "Species") )

ccr.all.out.time.8 <- iNEXT(ccr.list.8, q=0, datatype="incidence_raw")

ccr.all.time.df.8 <- fortify(ccr.all.out.time.8, type=1)


ccr.star.time.8 <- ccr.all.time.df.8 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "8")


# 9
ccr.list.9 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6"| Time =="7"| Time =="8"| Time =="9") %>%
  split(.$Field)

ccr.list.9 <- map(ccr.list.9, ~ .x %>% 
                    select(Plot.n,Species,pres) %>%
                    distinct() %>%
                    spread(key = Plot.n, value = pres) %>%
                    replace(is.na(.), 0) %>%
                    column_to_rownames(var = "Species") )

ccr.all.out.time.9 <- iNEXT(ccr.list.9, q=0, datatype="incidence_raw")

ccr.all.time.df.9 <- fortify(ccr.all.out.time.9, type=1)


ccr.star.time.9 <- ccr.all.time.df.9 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "9")



# 10
ccr.list.10 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6"| Time =="7"| Time =="8"| Time =="9"| Time =="10") %>%
  split(.$Field)

ccr.list.10 <- map(ccr.list.10, ~ .x %>% 
                    select(Plot.n,Species,pres) %>%
                    distinct() %>%
                    spread(key = Plot.n, value = pres) %>%
                    replace(is.na(.), 0) %>%
                    column_to_rownames(var = "Species") )

ccr.all.out.time.10 <- iNEXT(ccr.list.10, q=0, datatype="incidence_raw")

ccr.all.time.df.10 <- fortify(ccr.all.out.time.10, type=1)


ccr.star.time.10 <- ccr.all.time.df.10 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "10")


# 11
ccr.list.11 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6"| Time =="7"| Time =="8"| Time =="9"| Time =="10"| Time =="11") %>%
  split(.$Field)

ccr.list.11 <- map(ccr.list.11, ~ .x %>% 
                     select(Plot.n,Species,pres) %>%
                     distinct() %>%
                     spread(key = Plot.n, value = pres) %>%
                     replace(is.na(.), 0) %>%
                     column_to_rownames(var = "Species") )

ccr.all.out.time.11 <- iNEXT(ccr.list.11, q=0, datatype="incidence_raw")

ccr.all.time.df.11 <- fortify(ccr.all.out.time.11, type=1)


ccr.star.time.11 <- ccr.all.time.df.11 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "11")



# 12
ccr.list.12 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6"| Time =="7"| Time =="8"| Time =="9"| Time =="10"| Time =="11"| Time =="12") %>%
  split(.$Field)

ccr.list.12 <- map(ccr.list.12, ~ .x %>% 
                     select(Plot.n,Species,pres) %>%
                     distinct() %>%
                     spread(key = Plot.n, value = pres) %>%
                     replace(is.na(.), 0) %>%
                     column_to_rownames(var = "Species") )

ccr.all.out.time.12 <- iNEXT(ccr.list.12, q=0, datatype="incidence_raw")

ccr.all.time.df.12 <- fortify(ccr.all.out.time.12, type=1)


ccr.star.time.12 <- ccr.all.time.df.12 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "12")


# 13
ccr.list.13 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6"| Time =="7"| Time =="8"| Time =="9"| Time =="10"| Time =="11"| Time =="12"| Time =="13") %>%
  split(.$Field)

ccr.list.13 <- map(ccr.list.13, ~ .x %>% 
                     select(Plot.n,Species,pres) %>%
                     distinct() %>%
                     spread(key = Plot.n, value = pres) %>%
                     replace(is.na(.), 0) %>%
                     column_to_rownames(var = "Species") )

ccr.all.out.time.13 <- iNEXT(ccr.list.13, q=0, datatype="incidence_raw")

ccr.all.time.df.13 <- fortify(ccr.all.out.time.13, type=1)


ccr.star.time.13 <- ccr.all.time.df.13 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "13")



# 14
ccr.list.14 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6"| Time =="7"| Time =="8"| Time =="9"| Time =="10"| Time =="11"| Time =="12"| Time =="13"| Time =="14") %>%
  split(.$Field)

ccr.list.14 <- map(ccr.list.14, ~ .x %>% 
                     select(Plot.n,Species,pres) %>%
                     distinct() %>%
                     spread(key = Plot.n, value = pres) %>%
                     replace(is.na(.), 0) %>%
                     column_to_rownames(var = "Species") )

ccr.all.out.time.14 <- iNEXT(ccr.list.14, q=0, datatype="incidence_raw")

ccr.all.time.df.14 <- fortify(ccr.all.out.time.14, type=1)


ccr.star.time.14 <- ccr.all.time.df.14 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "14")


# 15
ccr.list.15 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6"| Time =="7"| Time =="8"| Time =="9"| Time =="10"| Time =="11"| Time =="12"| Time =="13"| Time =="14"| Time =="15") %>%
  split(.$Field)

ccr.list.15 <- map(ccr.list.15, ~ .x %>% 
                     select(Plot.n,Species,pres) %>%
                     distinct() %>%
                     spread(key = Plot.n, value = pres) %>%
                     replace(is.na(.), 0) %>%
                     column_to_rownames(var = "Species") )

ccr.all.out.time.15 <- iNEXT(ccr.list.15, q=0, datatype="incidence_raw")

ccr.all.time.df.15 <- fortify(ccr.all.out.time.15, type=1)


ccr.star.time.15 <- ccr.all.time.df.15 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "15")

# 16
ccr.list.16 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6"| Time =="7"| Time =="8"| Time =="9"| Time =="10"| Time =="11"| Time =="12"| Time =="13"| Time =="14"| Time =="15"| Time =="16") %>%
  split(.$Field)

ccr.list.16 <- map(ccr.list.16, ~ .x %>% 
                     select(Plot.n,Species,pres) %>%
                     distinct() %>%
                     spread(key = Plot.n, value = pres) %>%
                     replace(is.na(.), 0) %>%
                     column_to_rownames(var = "Species") )

ccr.all.out.time.16 <- iNEXT(ccr.list.16, q=0, datatype="incidence_raw")

ccr.all.time.df.16 <- fortify(ccr.all.out.time.16, type=1)


ccr.star.time.16 <- ccr.all.time.df.16 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "16")

# 17
ccr.list.17 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6"| Time =="7"| Time =="8"| Time =="9"| Time =="10"| Time =="11"| Time =="12"| Time =="13"| Time =="14"| Time =="15"| Time =="16"| Time =="17") %>%
  split(.$Field)

ccr.list.17 <- map(ccr.list.17, ~ .x %>% 
                     select(Plot.n,Species,pres) %>%
                     distinct() %>%
                     spread(key = Plot.n, value = pres) %>%
                     replace(is.na(.), 0) %>%
                     column_to_rownames(var = "Species") )

ccr.all.out.time.17 <- iNEXT(ccr.list.17, q=0, datatype="incidence_raw")

ccr.all.time.df.17 <- fortify(ccr.all.out.time.17, type=1)


ccr.star.time.17 <- ccr.all.time.df.17 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "17")



# 18
ccr.list.18 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6"| Time =="7"| Time =="8"| Time =="9"| Time =="10"| Time =="11"| Time =="12"| Time =="13"| Time =="14"| Time =="15"| Time =="16"| Time =="17"| Time =="18") %>%
  split(.$Field)

ccr.list.18 <- map(ccr.list.18, ~ .x %>% 
                     select(Plot.n,Species,pres) %>%
                     distinct() %>%
                     spread(key = Plot.n, value = pres) %>%
                     replace(is.na(.), 0) %>%
                     column_to_rownames(var = "Species") )

ccr.all.out.time.18 <- iNEXT(ccr.list.18, q=0, datatype="incidence_raw")

ccr.all.time.df.18 <- fortify(ccr.all.out.time.18, type=1)


ccr.star.time.18 <- ccr.all.time.df.18 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "18")


# 19
ccr.list.19 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6"| Time =="7"| Time =="8"| Time =="9"| Time =="10"| Time =="11"| Time =="12"| Time =="13"| Time =="14"| Time =="15"| Time =="16"| Time =="17"| Time =="18"| Time =="19") %>%
  split(.$Field)

ccr.list.19 <- map(ccr.list.19, ~ .x %>% 
                     select(Plot.n,Species,pres) %>%
                     distinct() %>%
                     spread(key = Plot.n, value = pres) %>%
                     replace(is.na(.), 0) %>%
                     column_to_rownames(var = "Species") )

ccr.all.out.time.19 <- iNEXT(ccr.list.19, q=0, datatype="incidence_raw")

ccr.all.time.df.19 <- fortify(ccr.all.out.time.19, type=1)


ccr.star.time.19 <- ccr.all.time.df.19 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "19")


# 20
ccr.list.20 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6"| Time =="7"| Time =="8"| Time =="9"| Time =="10"| Time =="11"| Time =="12"| Time =="13"| Time =="14"| Time =="15"| Time =="16"| Time =="17"| Time =="18"| Time =="19"| Time =="20") %>%
  split(.$Field)

ccr.list.20 <- map(ccr.list.20, ~ .x %>% 
                     select(Plot.n,Species,pres) %>%
                     distinct() %>%
                     spread(key = Plot.n, value = pres) %>%
                     replace(is.na(.), 0) %>%
                     column_to_rownames(var = "Species") )

ccr.all.out.time.20 <- iNEXT(ccr.list.20, q=0, datatype="incidence_raw")

ccr.all.time.df.20 <- fortify(ccr.all.out.time.20, type=1)


ccr.star.time.20 <- ccr.all.time.df.20 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "20")


# 21
ccr.list.21 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6"| Time =="7"| Time =="8"| Time =="9"| Time =="10"| Time =="11"| Time =="12"| Time =="13"| Time =="14"| Time =="15"| Time =="16"| Time =="17"| Time =="18"| Time =="19"| Time =="20"| Time =="21") %>%
  split(.$Field)

ccr.list.21 <- map(ccr.list.21, ~ .x %>% 
                     select(Plot.n,Species,pres) %>%
                     distinct() %>%
                     spread(key = Plot.n, value = pres) %>%
                     replace(is.na(.), 0) %>%
                     column_to_rownames(var = "Species") )

ccr.all.out.time.21 <- iNEXT(ccr.list.21, q=0, datatype="incidence_raw")

ccr.all.time.df.21 <- fortify(ccr.all.out.time.21, type=1)


ccr.star.time.21 <- ccr.all.time.df.21 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "21")

# 22
ccr.list.22 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6"| Time =="7"| Time =="8"| Time =="9"| Time =="10"| Time =="11"| Time =="12"| Time =="13"| Time =="14"| Time =="15"| Time =="16"| Time =="17"| Time =="18"| Time =="19"| Time =="20"| Time =="21"| Time =="22") %>%
  split(.$Field)

ccr.list.22 <- map(ccr.list.22, ~ .x %>% 
                     select(Plot.n,Species,pres) %>%
                     distinct() %>%
                     spread(key = Plot.n, value = pres) %>%
                     replace(is.na(.), 0) %>%
                     column_to_rownames(var = "Species") )

ccr.all.out.time.22 <- iNEXT(ccr.list.22, q=0, datatype="incidence_raw")

ccr.all.time.df.22 <- fortify(ccr.all.out.time.22, type=1)


ccr.star.time.22 <- ccr.all.time.df.22 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "22")

# 23
ccr.list.23 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6"| Time =="7"| Time =="8"| Time =="9"| Time =="10"| Time =="11"| Time =="12"| Time =="13"| Time =="14"| Time =="15"| Time =="16"| Time =="17"| Time =="18"| Time =="19"| Time =="20"| Time =="21"| Time =="22"| Time =="23") %>%
  split(.$Field)

ccr.list.23 <- map(ccr.list.23, ~ .x %>% 
                     select(Plot.n,Species,pres) %>%
                     distinct() %>%
                     spread(key = Plot.n, value = pres) %>%
                     replace(is.na(.), 0) %>%
                     column_to_rownames(var = "Species") )

ccr.all.out.time.23 <- iNEXT(ccr.list.23, q=0, datatype="incidence_raw")

ccr.all.time.df.23 <- fortify(ccr.all.out.time.23, type=1)


ccr.star.time.23 <- ccr.all.time.df.23 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "23")

# 24
ccr.list.24 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6"| Time =="7"| Time =="8"| Time =="9"| Time =="10"| Time =="11"| Time =="12"| Time =="13"| Time =="14"| Time =="15"| Time =="16"| Time =="17"| Time =="18"| Time =="19"| Time =="20"| Time =="21"| Time =="22"| Time =="23"| Time =="24") %>%
  split(.$Field)

ccr.list.24 <- map(ccr.list.24, ~ .x %>% 
                     select(Plot.n,Species,pres) %>%
                     distinct() %>%
                     spread(key = Plot.n, value = pres) %>%
                     replace(is.na(.), 0) %>%
                     column_to_rownames(var = "Species") )

ccr.all.out.time.24 <- iNEXT(ccr.list.24, q=0, datatype="incidence_raw")

ccr.all.time.df.24 <- fortify(ccr.all.out.time.24, type=1)


ccr.star.time.24 <- ccr.all.time.df.24 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "24")


# 25
ccr.list.25 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6"| Time =="7"| Time =="8"| Time =="9"| Time =="10"| Time =="11"| Time =="12"| Time =="13"| Time =="14"| Time =="15"| Time =="16"| Time =="17"| Time =="18"| Time =="19"| Time =="20"| Time =="21"| Time =="22"| Time =="23"| Time =="24"| Time =="25") %>%
  split(.$Field)

ccr.list.25 <- map(ccr.list.25, ~ .x %>% 
                     select(Plot.n,Species,pres) %>%
                     distinct() %>%
                     spread(key = Plot.n, value = pres) %>%
                     replace(is.na(.), 0) %>%
                     column_to_rownames(var = "Species") )

ccr.all.out.time.25 <- iNEXT(ccr.list.25, q=0, datatype="incidence_raw")

ccr.all.time.df.25 <- fortify(ccr.all.out.time.25, type=1)


ccr.star.time.25 <- ccr.all.time.df.25 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "25")

View(ccr_prep)

ccr.star <- ccr.star.time.1 %>% bind_rows(ccr.star.time.2) %>% bind_rows(ccr.star.time.3)%>% bind_rows(ccr.star.time.4)%>% 
  bind_rows(ccr.star.time.5) %>% bind_rows(ccr.star.time.6)%>% bind_rows(ccr.star.time.7)%>% bind_rows(ccr.star.time.8)%>%
  bind_rows(ccr.star.time.9) %>% bind_rows(ccr.star.time.10)%>% bind_rows(ccr.star.time.11)%>% bind_rows(ccr.star.time.12)%>% 
  bind_rows(ccr.star.time.13)%>% bind_rows(ccr.star.time.14)%>% bind_rows(ccr.star.time.15) %>% bind_rows(ccr.star.time.16)%>% 
  bind_rows(ccr.star.time.17)%>% bind_rows(ccr.star.time.18)%>% bind_rows(ccr.star.time.19)%>% bind_rows(ccr.star.time.20)%>% 
  bind_rows(ccr.star.time.21)%>% bind_rows(ccr.star.time.22)%>% 
  bind_rows(ccr.star.time.23)%>% bind_rows(ccr.star.time.24)%>% bind_rows(ccr.star.time.25) 

head(ccr.star)  
  
ccr.star$log10.plot<-log10(ccr.star$plot)
ccr.star$log10.rich<-log10(ccr.star$richness)
ccr.star$time.f<-as.factor(ccr.star$time)

ccr.star$time.f<-factor(ccr.star$time.f,  levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14" , "15" , "16" , "17" , "18" , "19" ,"20" , "21" , "22" , "23" , "24" , "25" ))

star.species.area<-ggplot() +
   facet_grid(.~site)+
  geom_point(data = ccr.star,
             aes(x = log10.plot, y = log10.rich,
                 colour = time.f), alpha=0.6,
             size = 1.3)+
  geom_line(data = ccr.star,
            aes(x = log10.plot, y = log10.rich,
                colour = time.f))+
  scale_color_viridis(discrete=TRUE,option = "plasma") +
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), 
                        legend.direction = "horizontal", legend.position = "bottom")+
  guides(col = guide_legend(ncol = 13))+
  labs(title = "Species-Area",color="Year Sampled (Time)") + ylab("Log10 Species Richness") + xlab("Log10 Area") 

star.species.area


head(ccr.star)

ccr.star$time.n<-as.numeric(ccr.star$time)
ccr.star$log10.time<-log10(ccr.star$time.n)
ccr.star$plot.f<-as.factor(ccr.star$plot)

star.species.time<-ggplot() +
  facet_grid(.~site)+
  geom_point(data = ccr.star,
             aes(x = log10.time, y = log10.rich,
                 colour = `plot.f`), alpha=0.6,
             size = 1.3)+
  geom_line(data = ccr.star,
            aes(x = log10.time, y = log10.rich,
                colour = `plot.f`))+
  scale_color_viridis(discrete=TRUE) +
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), 
                        legend.direction = "horizontal", legend.position = "bottom" )+
  guides(col = guide_legend(ncol = 10))+
  labs(title = "Species-Time",color= "Number of Samples (Area)") + ylab("Log10 Species Richness") + xlab("Log10 Time") 

star.species.time


library(patchwork)

(star.species.area )/ (star.species.time)




View(ccr.star)
colnames(ccr.star)


ccr.star.area<- ccr.star %>% filter(time.f == "24")

ccr.star.time<- ccr.star %>% filter(plot.f == "10")

star.species.area.mx <-ggplot() +
  #facet_grid(.~site)+
  geom_point(data = ccr.star.area,
             aes(x = log10.plot, y = log10.rich,
                 colour = site), alpha=0.6,
             size = 1.3)+
  geom_line(data = ccr.star.area,
            aes(x = log10.plot, y = log10.rich,
                colour = site))+
  scale_color_viridis(discrete=TRUE,option = "plasma") +
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), 
                        legend.direction = "horizontal", legend.position = "bottom")+
  guides(col = guide_legend(ncol = 13))+
  labs(title = "Species-Area",color="Site") + ylab("Log10 Species Richness") + xlab("Log10 Area") 

star.species.area.mx




star.species.time.mx<-ggplot() +
  #facet_grid(.~site)+
  geom_point(data = ccr.star.time,
             aes(x = log10.time, y = log10.rich,
                 colour = `site`), alpha=0.6,
             size = 1.3)+
  geom_line(data = ccr.star.time,
            aes(x = log10.time, y = log10.rich,
                colour = `site`))+
  scale_color_viridis(discrete=TRUE) +
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), 
                        legend.direction = "horizontal", legend.position = "bottom" )+
  guides(col = guide_legend(ncol = 10))+
  labs(title = "Species-Time",color= "Site") + ylab("Log10 Species Richness") + xlab("Log10 Time") 

star.species.time.mx


