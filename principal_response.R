
rm(list=ls())

library(ggrepel)
library(ggvegan)


library(tidyverse)
library(patchwork)
library(iNEXT)
library(ggplot2)
library(vegan)





# emmas data

# Cedar Creek Exp e1 : old fields (field a, b, c) vs never plowed field (d)
# response is biomass per species 

e1_biomass <- read.csv("~/Dropbox/Projects/CCRScale/E_001/e001.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


distinct(e1_biomass,Species) %>% arrange(Species)

oldfields_biomass <- e1_biomass %>% filter(!Species %in% c( "Miscellaneous litter", "Mosses & lichens","Mosses & lichens 2","Miscellaneous herbs") ) %>%
  droplevels()


e1_summary <- oldfields_biomass %>% 
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
  arrange(YSA,Field,Transect) %>% ungroup()


View(e1_long)


e1_rel <- e1_long %>% group_by(Field, Year, Transect) %>%
  summarise(Biomass_plot_sum=sum(Biomass)) %>%
  left_join(e1_long) %>%
mutate( Relative_Plot_Biomass = (Biomass/Biomass_plot_sum) *100 ) %>%
  arrange(Field,Year,Transect,Species)


e1_rel2 <- e1_long %>% group_by(Species) %>%
  summarise(Biomass_ind_sum=sum(Biomass)) %>%
  left_join(e1_rel) %>%
  mutate( Relative_ind_Biomass = (Biomass/Biomass_ind_sum) *100 ) %>% 
  arrange(Field,Year,Transect,Species)




e1$pres<-1
year_n <- e1 %>% select(Field,Year,pres)  %>% arrange(Field,Year) %>%
  distinct() %>%
  spread(Year,pres)


View(year_n)

e1_rel2$log_Relative_Plot_Biomass<-log(e1_rel2$Relative_Plot_Biomass)
e1_rel2$log_Relative_ind_Biomass<-log(e1_rel2$Relative_ind_Biomass)
e1_rel2$log_Biomass<-log(e1_rel2$Biomass)


View(e1_rel2)

e1_rel2$pres<-1

colnames(e1_rel2)


 e1_summary <- e1_rel2 %>% group_by(Field,Species) %>%
  summarise(Biomass_sum = round(sum(Relative_Plot_Biomass,2)),
            pres_sum = round(sum(pres,2)) ) 
 
 e1_bm <- e1_summary %>% select(Field, Species, Biomass_sum) %>%
   spread(Field, Biomass_sum)
 
 e1_pres <- e1_summary %>% select(Field, Species, pres_sum) %>%
   spread(Field, pres_sum) %>%
   rename(A.pres= A,
          B.pres=B,
          C.pres=C,
          D.pres=D) %>% left_join(e1_bm) %>%
   rename(A.bm= A,
          B.bm=B,
          C.bm=C,
          D.bm=D) %>%
   replace(is.na(.), 0) %>%
   mutate

View(e1_pres)

write.csv(e1_pres,"~/Dropbox/Projects/CCRScale/data/species_sppra.csv")

View(e1_rel2)

e1_wide_select <- e1_rel2 %>% filter(!Year %in% c("2005","2006","2007","2009","2010","2011","2012","2013","2014", "2015" )) %>%
  unite(site_id,Year,Field,Transect, sep="_",remove=F) %>%
  group_by(site_id,Year, Field,Transect,Species) %>%
  summarise(Biomass=sum(log_Relative_Plot_Biomass)) %>% # reverses results
 # summarise(Biomass=sum(Relative_Plot_Biomass)) %>% 
 # summarise(Biomass=sum(Biomass)) %>% 
 # summarise(Biomass=sum(Relative_ind_Biomass)) %>% # reverses results
  ungroup() %>%
  group_by(site_id, Year, Field,Transect) %>%
  spread(Species,Biomass) %>%
  ungroup() %>%
  replace(is.na(.), 0) %>%
  column_to_rownames(var = "site_id")  %>% 
  arrange(Year,Field) %>%
  select(-Field,-Year,-Transect)

View(e1_wide_select)


yr.dat<-e1_long %>% ungroup() %>% filter(!Year %in% c("2005","2006","2007","2009","2010","2011","2012","2013","2014", "2015" )) %>%
  select(Year) %>% distinct()

yr.dat$Year<-as.factor(as.character(yr.dat$Year))
levels(yr.dat$Year)

# time
yr <- gl(24, 40, labels = c(1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2008))
# treatment
field <- factor(rep(c("A","B","C","D"), each=10 ,times=24))


field <- relevel(field, ref = "D")


e1_prc_bray_rel <- prc(response = e1_wide_select, treatment = field, time = yr, dist="bray")

summary(e1_prc_bray_rel)

# plot(e1_prc, scaling = 1, ylim=c(-3,10))
# sum_prc <- summary(e1_prc)
# # Species scores:
# sum_prc$sp
# 
# head(sum_prc$sp)
# plot(e1_prc, select = abs(sum_prc$sp) > 0.3, scaling = 1, ylim=c(-5,10))

 # alt way
 # extract species scores

 
 # df1 <- data.frame(prc.scores = scores(e1_prc)$sites[ , 1])
 # df2 <- data.frame(spec_scores = scores(e1_prc)$species[ , 1], taxa = rownames(scores(e1_prc)$species))
 # 
 View(df2)
 
 
 # custom
 # modified from https://github.com/gavinsimpson/ggvegan/blob/master/R/autoplot.prc.R

 fobr <- fortify(e1_prc_bray_rel)
 
 ## levels of factors - do this now before we convert things
 TimeLevs <- levels(fobr$Time)
 TreatLevs <- levels(fobr$Treatment)
 
 ## convert Time to a numeric
 fobr$Time <- as.numeric(as.character(fobr$Time))
 
 ## process select
 ind <- fobr$Score != "Sample"
 
 ## samples and species "scores"
 sampr <- fobr[!ind, ]
 sppr <- fobr[ind,]
 

 a_sp <- read.csv("~/Dropbox/Projects/CCRScale/Clark_2018/species.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
 
 colnames(a_sp)
 
 sppr2<- sppr %>% mutate(Response2 = abs(Response)) %>%
   filter(Response2 >= abs(0.3))
 
 sp_r <- sppr2 %>% mutate(Species = Label)%>%
   left_join(a_sp) 
   
   
# piratepal(palette = "rat")
 
 sp_rel <- ggplot(data = sp_r,
        aes(x = reorder(Label, Response), y = Response,color=Origin),alpha=0.5,line=2)+
   geom_bar(stat = "identity",aes(fill=Origin),alpha=0.5)+
    scale_color_manual(values = c("#B21D13FF","#7F8624FF" )) +
    scale_fill_manual(values = c("#B21D13FF","#7F8624FF" )) +
   coord_flip()  + 
    labs(x = "", y = "Response", title = "b) Relative Biomass", subtitle = "")+
   theme_classic()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
          plot.margin=margin(t=1,1,1,1, "lines"),
           legend.position="bottom")
  
 
 
 

sp_rel#ggplot prc curve
 
 # ggplot(data = samp,
 #        aes_string(x = 'Time', y = 'Response',
 #                   colour = 'Treatment')) + 
 #   #geom_segment(y=0,yend=0,x=0,xend=2009, lty=2, size = I(0.2)) + 
 #   ## add the control
 #   geom_hline(yintercept = 0) +
 #   ## add the coefficients
 #   geom_line() +
 #   theme(legend.position = "top") +
 #   scale_x_continuous(breaks = as.numeric(TimeLevs), minor_breaks = NULL) +
 #   ## rug the species scores
 #   geom_rug(data = spp %>% mutate(Response2 = abs(Response)) %>%
 #              filter(Response2 >= abs(0.5)),
 #            sides = "r",
 #            mapping = aes_string(group = NULL, x = NULL, y='Response',
 #                                 colour = NULL, linetype = NULL)) +
 #   # repel text
 #   geom_text_repel(data=spp %>% mutate(Response2 = abs(Response)) %>%
 #                     filter(Response2 >= abs(0.5)), mapping=aes(x=2009,y=Response,label=Label),xlim=c(0,2013),
 #                   size=3,segment.size=0.2,
 #                   direction="y", nudge_x = 10, hjust = 0) +
 #   theme(axis.line=element_blank(),
 #         panel.grid=element_blank(),
 #         panel.background=element_blank(),
 #         plot.margin = margin(0,6,0,0,"cm")  ) +
 #   ylim(-10,10)+
 #   coord_cartesian(xlim = c(1982, 2008), # This focuses the x-axis on the range of interest
 #                   clip = 'off') +   # This keeps the labels from disappearing
 #   labs(x = "Treatment", y = "Time", title = "", subtitle = "")
 # 
 # 
 
 #presence
 
 e1_wide<-e1 %>% group_by(Exp,Year,YSA,Field,Transect,site_status,Species) %>%
   summarise(pres=sum(pres)) %>%
   ungroup()%>%
   group_by(Exp,Year,YSA,Field,Transect,site_status) %>%
   spread(Species,pres) %>%
   ungroup()
 
 e1_select<- e1_wide %>% group_by(Exp,Year,YSA,Field,site_status) %>%
   sample_n(10) 
 
 e1_long<-e1_select %>% gather(Species,pres, "Achillea millefolium(lanulosa)":"Viola sp.") %>%
   filter(!is.na(pres)) %>% droplevels() %>%
   arrange(YSA,Field,Transect) %>% ungroup()
 
 
 e1_wide_inc <- e1_long %>% filter(!Year %in% c("2005","2006","2007","2009","2010","2011","2012","2013","2014", "2015" )) %>%
   unite(site_id,Year,Field,Transect, sep="_",remove=F) %>%
   group_by(site_id,Year, Field,Transect,Species) %>%
   summarise(pres=sum(pres)) %>%
   ungroup() %>%
   mutate( pres = case_when( pres >= 1 ~ 1) ) %>%
   group_by(site_id, Year, Field,Transect) %>%
   spread(Species,pres) %>%
   ungroup() %>%
   replace(is.na(.), 0) %>%
   column_to_rownames(var = "site_id")  %>% 
   arrange(Year,Field) %>%
   select(-Field,-Year,-Transect)
 
 
 summary(e1_wide_inc)
 
 View(e1_wide_inc)
 
 field <- relevel(field, ref = "D")
 
 e1_prc_inc <- prc(response = e1_wide_inc, treatment = field, time = yr, binary=TRUE )

 ??dist
 
summary(e1_prc_inc)
 
 fobj <- fortify(e1_prc_inc)
 
 ## levels of factors - do this now before we convert things
 TimeLevs <- levels(fobj$Time)
 TreatLevs <- levels(fobj$Treatment)
 
 ## convert Time to a numeric
 fobj$Time <- as.numeric(as.character(fobj$Time))
 
 ## process select
 ind <- fobj$Score != "Sample"
 
 ## samples and species "scores"
 samp <- fobj[!ind, ]
 spp <- fobj[ind,]
 
 
 a_sp <- read.csv("~/Dropbox/Projects/CCRScale/Clark_2018/species.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
 
 colnames(a_sp)
 
 spp2<- spp %>% mutate(Response2 = abs(Response)) %>%
   filter(Response2 >= abs(0.3))
 
 View(spp2)
 
 sp_i <- spp2 %>% mutate(Species = Label)%>%
   left_join(a_sp) %>% distinct()
 
 
 View(sp_i)
 
 piratepal(palette = "rat")
 
sp_inc <-  ggplot(data = sp_i,
        aes(x = reorder(Label, Response), y = Response,color=Origin),alpha=0.5,line=2)+
   geom_bar(stat = "identity",aes(fill=Origin),alpha=0.5)+
   scale_color_manual(values = c("#B21D13FF","#7F8624FF" )) +
   scale_fill_manual(values = c("#B21D13FF","#7F8624FF" )) +
   coord_flip()  + 
   labs(x = "Species", y = "Response", title = "a) Incidence", subtitle = "") +
  theme_classic() +
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
         plot.margin=margin(t=1,1,1,1, "lines"),
          legend.position="none") 
   


sp_inc

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# make legend an object
sp.legend<-g_legend(sp_rel)


(sp_inc  | sp_rel + theme(legend.position="none") )/ (sp.legend) +  plot_layout(heights = c(10,0.5))
 
 
          
          