


library(tidyverse)
library(vegan)
library(mobr)
library(patchwork)

species <- read.csv("~/Dropbox/Projects/CCRScale/Clark_2018/species.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

sites <- read.csv("~/Dropbox/Projects/CCRScale/Clark_2018/sites.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


biomass <- read.csv("~/Dropbox/Projects/CCRScale/Clark_2018/sp_biomass.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

e1_biomass <- read.csv("~/Dropbox/Projects/CCRScale/E_001/e001.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))



oldfields_biomass <- sites %>% left_join(biomass)  %>% filter(!lcd_species %in% c( "Miscellaneous litter", "Mosses & lichens") ) %>%
  droplevels()


View(oldfields_biomass)

oldfields_summary <- oldfields_biomass %>% 
  mutate(
    Exp1 = case_when(
      In_E014 == "1" ~ "14"),
    Exp2 = case_when(
      In_E054 == "1" ~ "54"
    )) %>%
  unite(Exp, Exp1,Exp2,sep="&") %>%
  select(-In_E014,-In_E054) %>%
  mutate(YSA = (Year-YearAbandoned)) %>%
  #unite(id,Exp,Year,Field,Transect,sep="_") %>%
  rename(Species=lcd_species) %>%
  rename(Biomass=sp_biomass) %>%
  select(Exp,Year,YSA,Field,Transect,Species,Biomass) %>%
arrange(YSA,Field,Transect)

oldfields_summary$site_status <- "old field"

View(oldfields_summary)







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

  
  View(e1_wide)

e1_select<- e1_wide %>% group_by(Exp,Year,YSA,Field,site_status) %>%
  sample_n(4)
  

colnames(e1_select)
e1_long<-e1_select %>% gather(Species,Biomass, "Achillea millefolium(lanulosa)":"Viola sp.") %>%
  filter(!is.na(Biomass)) %>% droplevels() %>%
  arrange(YSA,Field,Transect)
  

View(e1_long)

e1_long$YSA<-as.numeric(e1_long$YSA)

biomass_together <- oldfields_summary %>% bind_rows(e1_long) 


View(biomass_together)

plot_prep<-biomass_together %>% distinct(Year,YSA,Field, Transect,site_status)%>%
  group_by(YSA,Field) %>% mutate(y = 1:n()) %>%
  arrange(YSA,Field,Transect) %>%
  ungroup()
  
 View(plot_prep)

plot_attr<- plot_prep %>% distinct(Year,Field) %>%  mutate(x = 1:n()) %>%
  left_join(plot_prep) %>%
  rename(group=site_status) %>%
  arrange(YSA,Field,Transect) 
  
View(plot_attr)


ccr_prep <- plot_attr %>% left_join(biomass_together) 


ccr_comm_prep <- ccr_prep %>%  group_by(group,x,y,Year,YSA,Field,Transect,Species) %>%
  summarise(Biomass=sum(Biomass))%>%
  ungroup() %>%
  spread(Species,Biomass) %>%
  replace(is.na(.), 0) %>%
  unite(group,group,Field,sep="_",remove=F) %>%
  arrange(Year,YSA,Field,Transect)


View(ccr_comm_prep)

nrow(ccr_comm_prep)

ccr_comm <- ccr_comm_prep %>% select(-group,-x,-y,-Year,-YSA,-Field,-Transect) 


View(ccr_comm)  

nrow(ccr_comm)


View(ccr_comm_prep)  

ccr_plot_attr <- ccr_comm_prep %>% 
  select(group,x,y) 

View(ccr_plot_attr)

nrow(ccr_plot_attr)


# trying with mobr
ccr_mob_in = make_mob_in(ccr_comm, ccr_plot_attr)



par(mfrow=c(1,1))
plot_rarefaction(ccr_mob_in, 'group', 'samp', leg_loc = 'topright')



# 10 SAMPLES E001 ONLY


e1_select<- e1_wide %>% group_by(Exp,Year,YSA,Field,site_status) %>%
  sample_n(10)


colnames(e1_select)
e1_long<-e1_select %>% gather(Species,Biomass, "Achillea millefolium(lanulosa)":"Viola sp.") %>%
  filter(!is.na(Biomass)) %>% droplevels() %>%
  arrange(YSA,Field,Transect)


View(e1_long)

e1_long$YSA<-as.numeric(e1_long$YSA)



plot_prep<-e1_long %>% distinct(Year,YSA,Field, Transect,site_status)%>%
  group_by(YSA,Field) %>% mutate(y = 1:n()) %>%
  arrange(YSA,Field,Transect) %>%
  ungroup()

View(plot_prep)

plot_attr<- plot_prep %>% distinct(Year,Field) %>%  mutate(x = 1:n()) %>%
  left_join(plot_prep) %>%
  rename(group=site_status) %>%
  arrange(YSA,Field,Transect) 

View(plot_attr)


ccr_prep <- plot_attr %>% left_join(e1_long) 


ccr_comm_prep <- ccr_prep %>%  group_by(group,x,y,Year,YSA,Field,Transect,Species) %>%
  summarise(Biomass=sum(Biomass))%>%
  ungroup() %>%
  spread(Species,Biomass) %>%
  replace(is.na(.), 0) %>%
  #unite(group,group,Field,sep="_",remove=F) %>%
  arrange(Field,YSA,Year,Transect)


fields.years<-ccr_comm_prep %>% distinct (Field,YSA,Year)
View(fields.years)


View(ccr_comm_prep)

nrow(ccr_comm_prep)

ccr_comm <- ccr_comm_prep %>% 
  #filter(Year== "1982") %>%
  #filter(Year== "1992") %>%
   #filter(Year== "2002") %>%
   filter(Year== "2008") %>%
  select(-group,-x,-y,-Year,-YSA,-Field,-Transect) 


View(ccr_comm)  

nrow(ccr_comm)


View(ccr_comm_prep)  

ccr_plot_attr <- ccr_comm_prep %>% 
  #filter(Year== "1982") %>%
  #filter(Year== "1992") %>%
   #filter(Year== "2002") %>%
   filter(Year== "2008") %>%
  select(Field,x,y) %>%
  rename(group = Field)

View(ccr_plot_attr)

nrow(ccr_plot_attr)


# trying with mobr
ccr_mob_in = make_mob_in(ccr_comm, ccr_plot_attr)



par(mfrow=c(1,1))
plot_rarefaction(ccr_mob_in, 'group', 'samp', col=c("#00AFBB", "#E7B800", "#FC4E07","#52854C"),leg_loc = 'topright') 


plot_rarefaction(ccr_mob_in, 'group', 'samp', leg_loc = 'topright')


plot_rarefaction(ccr_mob_in, 'group', 'samp', leg_loc = 'topright')


plot_rarefaction(ccr_mob_in, 'group', 'samp', leg_loc = 'topright')


par(mfrow=c(2,2))

(field_1982 | field_1992)/(field_2002 | field_2008)



# vegan / ggplot2
colnames(ccr_comm_prep)
All_ccr = ccr_comm_prep[8:200]


curve_all = specaccum(All_ccr, permutations = 100)

#plot curve_all first
plot(curve_all)

View(ccr_comm_prep)
#subset each habitat into its own df
ccr_comm_prep %>% filter(group == "never-plowed") -> neverplowed
ccr_comm_prep %>% filter(group == "old field") -> oldfield

#calc species accumulation curve for each habitat
curve_neverplowed = poolaccum(neverplowed[, 8:200])
curve_oldfield = poolaccum(oldfield[,8:200])

np<-plot(curve_neverplowed, display = c("chao", "jack1", "jack2"))
of<-plot(curve_oldfield, display = c("chao", "jack1", "jack2"))

np_chao <- data.frame(summary(curve_neverplowed)$chao,check.names = FALSE)
of_chao <- data.frame(summary(curve_oldfield)$chao,check.names = FALSE)

View(np_chao)

colnames(np_chao) <- c("N", "Chao", "lower2.5", "higher97.5", "std")
np_chao$'Site Status'<-"never-plowed"
colnames(of_chao) <- c("N", "Chao", "lower2.5", "higher97.5", "std")
of_chao$'Site Status'<-"old field"

View(np_chao)

chao<-np_chao %>% bind_rows(of_chao)

View(chao)

chao_melt <- melt(chao, id.vars = c("N","std","Site Status"))

View(chao_melt)

ss<-ggplot(data = chao_melt %>% filter(variable == "Chao"), aes(x = N, y = value, group = `Site Status`)) +
  geom_line(aes(color = `Site Status`)) +
  scale_color_manual(values =  c("#A1C720FF","#15983DFF"))  + 
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), plot.margin=margin(t=4,1,1,1, "lines"),
                        legend.direction = "horizontal", legend.position = "top" ) +
  labs(title = 'E001 Only' ) + ylab("Species Richness") + xlab("Samples") 



# by field


ccr_comm_prep %>% filter(Field == "A") -> a
ccr_comm_prep %>% filter(Field == "B") -> b
ccr_comm_prep %>% filter(Field == "C") -> c
ccr_comm_prep %>% filter(Field == "D") -> d

#calc species accumulation curve for each habitat
curve_a = poolaccum(a[, 8:200])
curve_b = poolaccum(b[,8:200])
curve_c = poolaccum(c[, 8:200])
curve_d = poolaccum(d[,8:200])

a_chao <- data.frame(summary(curve_a)$chao,check.names = FALSE)
b_chao <- data.frame(summary(curve_b)$chao,check.names = FALSE)
c_chao <- data.frame(summary(curve_c)$chao,check.names = FALSE)
d_chao <- data.frame(summary(curve_d)$chao,check.names = FALSE)


colnames(a_chao) <- c("N", "Chao", "lower2.5", "higher97.5", "std")
a_chao$Field<-"A"
colnames(b_chao) <- c("N", "Chao", "lower2.5", "higher97.5", "std")
b_chao$Field<-"B"
colnames(c_chao) <- c("N", "Chao", "lower2.5", "higher97.5", "std")
c_chao$Field<-"C"
colnames(d_chao) <- c("N", "Chao", "lower2.5", "higher97.5", "std")
d_chao$Field<-"D"


chao<-a_chao %>% bind_rows(b_chao) %>% bind_rows(c_chao) %>% bind_rows(d_chao)

View(chao)

chao_melt <- melt(chao, id.vars = c("N","std","Field"))

View(chao_melt)

f<-ggplot(data = chao_melt %>% filter(variable == "Chao"), aes(x = N, y = value, group = Field)) +
  geom_line(aes(color = Field)) +
  #scale_color_manual(values =  c("#A1C720FF","#15983DFF"))  + 
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), plot.margin=margin(t=4,1,1,1, "lines"),
                        legend.direction = "horizontal", legend.position = "top" ) +
  labs(title = 'E001 Only' ) + ylab("Species Richness") + xlab("Samples") 
f


(ss)/(f)




# time ? most recent year only

View(ccr_comm_prep)

fy<-ccr_comm_prep %>% distinct(Field,Year) %>% arrange(Field,Year)

is.numeric(ccr_comm_prep$Year)

ccr_comm_prep %>% filter(Field == "A")  %>% filter( Year == max(Year)) -> a
ccr_comm_prep %>% filter(Field == "B") %>% filter( Year == max(Year)) -> b
ccr_comm_prep %>% filter(Field == "C") %>% filter( Year == max(Year))  -> c
ccr_comm_prep %>% filter(Field == "D")%>% filter( Year == max(Year)) -> d

View(a)
#  calc species accumulation curve for each habitat
curve_a = poolaccum(a[, 8:200])
curve_b = poolaccum(b[,8:200])
curve_c = poolaccum(c[, 8:200])
curve_d = poolaccum(d[,8:200])

a_chao <- data.frame(summary(curve_a)$chao,check.names = FALSE)
b_chao <- data.frame(summary(curve_b)$chao,check.names = FALSE)
c_chao <- data.frame(summary(curve_c)$chao,check.names = FALSE)
d_chao <- data.frame(summary(curve_d)$chao,check.names = FALSE)


colnames(a_chao) <- c("N", "Chao", "lower2.5", "higher97.5", "std")
a_chao$Field<-"A"
colnames(b_chao) <- c("N", "Chao", "lower2.5", "higher97.5", "std")
b_chao$Field<-"B"
colnames(c_chao) <- c("N", "Chao", "lower2.5", "higher97.5", "std")
c_chao$Field<-"C"
colnames(d_chao) <- c("N", "Chao", "lower2.5", "higher97.5", "std")
d_chao$Field<-"D"


chao<-a_chao %>% bind_rows(b_chao) %>% bind_rows(c_chao) %>% bind_rows(d_chao)

View(chao)

chao_melt <- melt(chao, id.vars = c("N","std","Field"))

View(chao_melt)

t<-ggplot(data = chao ,aes(x = N, y = Chao, group = Field)) +
  geom_line(aes(color = Field)) +
  # geom_ribbon(data= chao,
  #             aes(x = N, ymin =lower2.5, ymax = higher97.5),
  #             alpha = 0.2)+
  #scale_color_manual(values =  c("#A1C720FF","#15983DFF"))  + 
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), plot.margin=margin(t=4,1,1,1, "lines"),
                        legend.direction = "horizontal", legend.position = "top" ) +
  labs(title = 'E001 Only: Most Recent Measurement' ) + ylab("Species Richness") + xlab("Samples") 
t


(ss)/(f)/(t)

