
rm(list=ls())

library(ggrepel)
library(ggvegan)


library(tidyverse)
library(patchwork)
library(iNEXT)
library(ggplot2)
library(vegan)





# emmas data

# Cedar Creek Exp e133 & e014 



ccr_dat <- read.csv("~/Dropbox/Projects/CCRScale/E14 _133/e014_e133_cleaned_1983-2016_EL.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


View(ccr_dat)


ccr_rel <- ccr_dat %>% group_by(Field, Year, Transect, Plot) %>%
  summarise(pCover_plot_sum=sum(pCover)) %>%
  left_join(ccr_dat) %>%
mutate( Relative_pCover = (pCover/pCover_plot_sum) *100 ) %>%
  arrange(Field,Year,Transect,Plot,Species)


ccr_rel$pres<-1

time.yr <- ccr_rel %>% ungroup() %>%
  select(Field,Year, site_status) %>% distinct() %>% 
  unite(field_stat, Field, site_status,sep="_",remove=F) %>%
  arrange(field_stat,Year) %>% group_by(field_stat) %>%
  mutate(Time = 1:n()) %>% arrange(field_stat,Year,Time)

View(time.yr)

ccr <- ccr_rel %>% left_join(time.yr) %>% filter(Time <= 4) %>%
  unite(field_stat, Field, site_status, sep="_",remove=F) %>%
  filter(field_stat %in% c("101_never-plowed","21_old field","27_old field" ,"28_old field","32_old field","35_old field" , "39_old field",
                           "40_old field"   ,  "41_old field"   ,  "44_old field"   ,  "45_old field"  ,   "47_old field" ,
                           "53_old field"   , "70_old field"))

View(ccr)


ccr$log_Relative_pCover<-log(ccr$Relative_pCover)


ccr_wide_select <- ccr %>% #filter(!Year %in% c("2005","2006","2007","2009","2010","2011","2012","2013","2014", "2015" )) %>%
  unite(site_id,Year,Field,Transect,Plot, sep="_",remove=F) %>%
  group_by(site_id,Year, Field,Transect,Plot,Species) %>%
  summarise(Biomass=sum(log_Relative_pCover)) %>% # reverses results
  #summarise(pCover=sum(Relative_pCover)) %>% 
 # summarise(Biomass=sum(Biomass)) %>% 
 # summarise(Biomass=sum(Relative_ind_Biomass)) %>% # reverses results
  ungroup() %>%
  group_by(site_id, Year, Field,Transect) %>%
  spread(Species,pCover) %>%
  ungroup() %>%
  replace(is.na(.), 0) %>%
  column_to_rownames(var = "site_id")  %>% 
  arrange(Year,Field) %>%
  select(-Field,-Year,-Transect,-Plot)

View(ccr_wide_select)

nrow(ccr_wide_select)


yr.dat<-ccr %>% ungroup() %>% 
  select(Year) %>% distinct()

yr.dat$Year<-as.factor(as.character(yr.dat$Year))
levels(yr.dat$Year)

ccrdeets<-ccr %>% ungroup() %>% select(Field, site_status) %>% distinct(Field, site_status) %>%
  unite(field_stat, Field, site_status)

ccrdeets$field_stat<- as.factor(as.character(ccrdeets$field_stat))
levels(ccrdeets$field_stat)




# time
yr <- gl(4, 280, labels = c(1,2,3,4))
# treatment
field <- factor(rep(c("101_never-plowed","21_old field","27_old field" ,"28_old field","32_old field","35_old field" , "39_old field",
                      "40_old field"   ,  "41_old field"   ,  "44_old field"   ,  "45_old field"  ,   "47_old field" ,
                      "53_old field"   , "70_old field" ), each=20 , times=4))


field <- relevel(field, ref = "101_never-plowed")


ccr_prc_bray_rel <- prc(response = ccr_wide_select, treatment = field, time = yr, dist="bray")

 # custom
 # modified from https://github.com/gavinsimpson/ggvegan/blob/master/R/autoplot.prc.R

 fobr <- fortify(ccr_prc_bray_rel)
 
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
    labs(x = "", y = "Response", title = "b) Relative Cover", subtitle = "")+
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

ccr_wide_inc <- ccr %>% #filter(!Year %in% c("2005","2006","2007","2009","2010","2011","2012","2013","2014", "2015" )) %>%
  select(Year, Field,Transect,Plot,Species,pres) %>%
  unite(site_id,Year,Field,Transect,Plot, sep="_",remove=F) %>%
  group_by(site_id,Year, Field,Transect,Plot,Species) %>%
 distinct() %>%  ungroup() %>%
  group_by(site_id, Year, Field,Transect) %>%
  spread(Species,pres) %>%
  ungroup() %>%
  replace(is.na(.), 0) %>%
  column_to_rownames(var = "site_id")  %>% 
  arrange(Year,Field) %>%
  select(-Field,-Year,-Transect,-Plot)

 
 View(ccr_wide_inc)
 nrow(ccr_wide_inc)
 

ccr_prc_inc <- prc(response = ccr_wide_inc, treatment = field, time = yr, binary=TRUE )


 
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
 
 
          
          