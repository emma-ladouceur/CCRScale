
library(yarrr)
library(ggplot2)
library(ggforce)
library(patchwork)
library(tidyverse)
library(Hmisc)

alpha_dat <- read.csv("~/Dropbox/Projects/CCRScale/data/alpha_summary.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
#gamma_dat <- read.csv("~/Dropbox/Projects/CCRScale/data/gamma_summary.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
gamma_dat <- read.csv("~/Dropbox/Projects/CCRScale/data/e1_gamma_summary.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


View(gamma_dat)
View(alpha_dat)

alpha_dat$Scale<-"alpha"
gamma_dat$Scale<-"gamma"

gamma_dat$Exp<-as.factor(as.character(gamma_dat$Exp))

gamma_dat$S_biomass<-gamma_dat$gamma_S_biomass
gamma_dat$ENSPIE_biomass<-gamma_dat$gamma_ENSPIE_biomass

dat<- bind_rows(alpha_dat,gamma_dat) %>% filter(!is.na(YSA)) %>%
  mutate( biomass= ifelse(is.na(biomass) , gamma_sum_biomass, biomass))

View(alpha_dat)


alpha_dat<- alpha_dat %>% group_by(Exp,Year,YSA,Field,site_status) %>%
  sample_n(10) 


of_alpha <- alpha_dat %>% filter(site_status == "old field" ) 
np_alpha <- alpha_dat %>% filter(site_status == "never-plowed" ) 

gamma_dat<- gamma_dat %>% filter(Field == "A" | Field == "B" | Field == "C" | Field == "D") 

of_gamma <- gamma_dat %>% filter(site_status == "old field" ) 
np_gamma <- gamma_dat %>% filter(site_status == "never-plowed" ) 

#gamma_dat<- gamma_dat %>% filter(Field == "A" | Field == "B" | Field == "C" | Field == "D")

dat<- dat %>% filter(Field == "A" | Field == "B" | Field == "C" | Field == "D") 

View(dat)



years<-alpha_dat %>% distinct(Field, YSA,Year, YearAbandoned)

View(years)
# alpha_dat$YSA<-as.factor(as.character(alpha_dat$YSA))
# gamma_dat$YSA<-as.factor(as.character(gamma_dat$YSA))
# dat$YSA<-as.factor(as.character(dat$YSA))
# 
# levels(dat$YSA)
# 
# alpha_dat$YSA<-factor(alpha_dat$YSA,  levels=c("1" , "2" ,"3" ,"4" , "5" ,"6" ,"7" ,"8" , "9" ,"10"  ,"11" , "12" , "13" , "14" , "15" , "16" , "17" , "18" , "19" ,"20" , "21" , "22" , "23" , "24" , "25" , "26" , "27" , "28" , "29" ,   "30" , "31" , "32" , "33" , "34" , "35" , "36" , "37" , "38" , "39" ,   "40" , "41" , "42" , "43" , "44" , "45" , "46" , "47" , "48" , "49" ,   "50" , "51" , "52" , "53" , "54" , "55" ,"56" , "57" , "58" , "59" ,    "60" , "61" , "62" , "63" , "64" , "65" , "66" , "67" , "68" , "69" ,    "70" , "71" , "72" , "73" , "74" , "75" , "76" , "77" , "78" , "79" , "80" , "81" , "82" , "83" , "84" , "85" , "86" , "87" , "88" , "89", "never-plowed"))
# gamma_dat$YSA<-factor(gamma_dat$YSA,  levels=c("1" , "2" ,"3" ,"4" , "5" ,"6" ,"7" ,"8" , "9" ,"10"  ,"11" , "12" , "13" , "14" , "15" , "16" , "17" , "18" , "19" ,"20" , "21" , "22" , "23" , "24" , "25" , "26" , "27" , "28" , "29" ,   "30" , "31" , "32" , "33" , "34" , "35" , "36" , "37" , "38" , "39" ,   "40" , "41" , "42" , "43" , "44" , "45" , "46" , "47" , "48" , "49" ,   "50" , "51" , "52" , "53" , "54" , "55" ,"56" , "57" , "58" , "59" ,    "60" , "61" , "62" , "63" , "64" , "65" , "66" , "67" , "68" , "69" ,    "70" , "71" , "72" , "73" , "74" , "75" , "76" , "77" , "78" , "79" , "80" , "81" , "82" , "83" , "84" , "85" , "86" , "87" , "88" , "89", "never-plowed"))
# dat$YSA<-factor(dat$YSA,  levels=c("1" , "2" ,"3" ,"4" , "5" ,"6" ,"7" ,"8" , "9" ,"10"  ,"11" , "12" , "13" , "14" , "15" , "16" , "17" , "18" , "19" ,"20" , "21" , "22" , "23" , "24" , "25" , "26" , "27" , "28" , "29" ,   "30" , "31" , "32" , "33" , "34" , "35" , "36" , "37" , "38" , "39" ,   "40" , "41" , "42" , "43" , "44" , "45" , "46" , "47" , "48" , "49" ,   "50" , "51" , "52" , "53" , "54" , "55" ,"56" , "57" , "58" , "59" ,    "60" , "61" , "62" , "63" , "64" , "65" , "66" , "67" , "68" , "69" ,    "70" , "71" , "72" , "73" , "74" , "75" , "76" , "77" , "78" , "79" , "80" , "81" , "82" , "83" , "84" , "85" , "86" , "87" , "88" , "89", "never-plowed"))

View(dat)

summary(dat)


piratepal(palette = "all")

piratepal(palette = "google")




alpha_bm_sp<-ggplot() +
  geom_point(data = alpha_dat,
             aes(x = Year, y = S_biomass,
                 colour = site_status, shape=site_status), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_smooth(data = alpha_dat,
              aes(x = Year, y = S_biomass,
                  colour = site_status,group=site_status))+
  stat_summary(data = of_alpha,
               aes(x = Year, y = S_biomass),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",shape=16,size=0.25)+
  stat_summary(data = of_alpha,
               aes(x = Year, y = S_biomass),fun = mean, geom="line",size=0.55) +
  stat_summary(data = np_alpha,
               aes(x = Year, y = S_biomass),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",shape=16,size=0.25)+
  stat_summary(data = np_alpha,
               aes(x = Year, y = S_biomass),fun = mean, geom="line",size=0.55) +
  scale_color_manual(values =  c("#15983DFF","#A1C720FF"))  + 
  scale_fill_manual(values =  c("#15983DFF","#A1C720FF"))  + 
  #scale_x_continuous(breaks=c(0,25,50,75,89)) +
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), plot.margin=margin(t=2,1,1,1, "lines"),
                        legend.direction = "horizontal", legend.position = "none" )+
  labs(title = (expression(paste(italic(alpha), '-scale', sep = '')))
       ) + ylab("Species Richness") + xlab("Year") 


alpha_bm_sp

View(gamma_dat)
gamma_bm_sp<-ggplot() +
  geom_point(data = gamma_dat,
             aes(x = Year, y = gamma_S_biomass,
                 colour = site_status, shape=site_status), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_smooth(data = gamma_dat,
              aes(x = Year, y = gamma_S_biomass,
                  colour = site_status,group=site_status))+
  stat_summary(data = of_gamma,
               aes(x = Year, y = gamma_S_biomass),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",shape=17,size=0.25)+
  stat_summary(data = of_gamma,
               aes(x = Year, y = gamma_S_biomass),fun = mean, geom="line",size=0.55) +
  stat_summary(data = np_gamma,
               aes(x = Year, y = gamma_S_biomass),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",shape=17,size=0.25)+
  stat_summary(data = np_gamma,
               aes(x = Year, y = gamma_S_biomass),fun = mean, geom="line",size=0.55) +
  scale_color_manual(values =  c("#15983DFF","#A1C720FF"))  + 
 # scale_x_continuous(breaks=c(0,25,50,75,89)) +
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), plot.margin=margin(t=2,1,1,1, "lines"),
                        legend.direction = "horizontal", legend.position = "none" )+
  labs(title = (expression(paste(italic(gamma), '-scale', sep = '')))
       ) + ylab("Species Richness") + xlab("Year") 


gamma_bm_sp


(alpha_bm_sp)/(gamma_bm_sp)


gamma_dat$S_biomass<-gamma_dat$gamma_S_biomass
gamma_dat$ENSPIE_biomass<-gamma_dat$gamma_ENSPIE_biomass

together_dat<-alpha_dat %>% bind_rows(gamma_dat)


together_dat <-  unite(together_dat, `Site & Scale`, site_status, Scale, sep=" ", remove=F)

View(together_dat)

# together
together_rich<-ggplot() +
  #facet_grid(.~Field)+
  geom_point(data = together_dat,
             aes(x = Year, y = S_biomass,
                 colour = `Site & Scale`, shape=`Site & Scale`), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_smooth(data = together_dat,
              aes(x = Year, y = S_biomass,
                  colour = `Site & Scale`,group=`Site & Scale`, line=`Site & Scale`))+
  stat_summary(data = of_alpha,
               aes(x = Year, y = S_biomass),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",shape=16,size=0.25)+
  stat_summary(data = of_alpha,
               aes(x = Year, y = S_biomass),fun = mean, geom="line",size=0.55) +
  stat_summary(data = np_alpha,
               aes(x = Year, y = S_biomass),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",shape=16,size=0.25)+
  stat_summary(data = np_alpha,
               aes(x = Year, y = S_biomass),fun = mean, geom="line",size=0.55) +
  # gamma
  stat_summary(data = of_gamma,
               aes(x = Year, y = S_biomass),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",shape=17,size=0.25)+
  stat_summary(data = of_gamma,
               aes(x = Year, y = S_biomass),fun = mean, geom="line",size=0.55) +
  stat_summary(data = np_gamma,
               aes(x = Year, y = S_biomass),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",shape=17,size=0.25)+
  stat_summary(data = np_gamma,
               aes(x = Year, y = S_biomass),fun = mean, geom="line",size=0.55) +
  scale_color_manual(values =  c("#A1C720FF","#15983DFF","#00AFBB", "#E7B800", "#FC4E07"))  + 
  #scale_x_continuous(breaks=c(0,25,50,75,89)) +
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), plot.margin=margin(t=1,1,1,1, "lines"),
                        legend.direction = "horizontal", legend.position = "bottom" )+
  labs(title = (expression(paste('Richness'))) ) + ylab("Species Richness") + xlab("Year") 


together_rich

# pie

alpha_pie<-ggplot() +
  geom_point(data = alpha_dat,
             aes(x = Year, y = ENSPIE_biomass,
                 colour = site_status, shape=site_status), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_smooth(data = alpha_dat,
              aes(x = Year, y = ENSPIE_biomass,
                  colour = site_status,group=site_status))+
  stat_summary(data = of_alpha,
               aes(x = Year, y = ENSPIE_biomass),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",shape=16,size=0.25)+
  stat_summary(data = of_alpha,
               aes(x = Year, y = ENSPIE_biomass),fun = mean, geom="line",size=0.55) +
  stat_summary(data = np_alpha,
               aes(x = Year, y = ENSPIE_biomass),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",shape=16,size=0.25)+
  stat_summary(data = np_alpha,
               aes(x = Year, y = ENSPIE_biomass),fun = mean, geom="line",size=0.55) +
  scale_color_manual(values =  c("#15983DFF","#A1C720FF"))  + 
  #scale_x_continuous(breaks=c(0,25,50,75,89)) +
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), plot.margin=margin(t=2,1,1,1, "lines"),
                        legend.direction = "horizontal", legend.position = "none" )+
  labs(title =  ''
       #(expression(paste(italic(alpha), '-scale', sep = '')))
       ) + ylab(expression(ENS[PIE])) + xlab("Year") 


alpha_pie


gamma_pie<-ggplot() +
  geom_point(data = gamma_dat,
             aes(x = Year, y = ENSPIE_biomass,
                 colour = site_status, shape=site_status), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_smooth(data = gamma_dat,
              aes(x = Year, y = ENSPIE_biomass,
                  colour = site_status,group=site_status))+
  stat_summary(data = of_gamma,
               aes(x = Year, y = ENSPIE_biomass),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",shape=17,size=0.25)+
  stat_summary(data = of_gamma,
               aes(x = Year, y = ENSPIE_biomass),fun = mean, geom="line",size=0.55) +
  stat_summary(data = np_gamma,
               aes(x = Year, y = ENSPIE_biomass),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",shape=17,size=0.25)+
  stat_summary(data = np_gamma,
               aes(x = Year, y = ENSPIE_biomass),fun = mean, geom="line",size=0.55) +
  scale_color_manual(values =  c("#15983DFF","#A1C720FF"))  + 
  # scale_x_continuous(breaks=c(0,25,50,75,89)) +
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), plot.margin=margin(t=2,1,1,1, "lines"),
                        legend.direction = "horizontal", legend.position = "bottom" )+
  labs(title = '', color= "Site Status", shape= "Site Status"
         #(expression(paste(italic(gamma), '-scale', sep = '')))
       )  + ylab(expression(ENS[PIE])) + xlab("Year") 


gamma_pie


#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

ccr.legend<-g_legend(gamma_pie)




(alpha_bm_sp | alpha_pie ) / (gamma_bm_sp | gamma_pie + theme(legend.position="none"))/(ccr.legend) + plot_layout(heights = c(10,10,1)) 








together_pie<-ggplot() +
  #facet_grid(.~Field)+
  geom_point(data = together_dat,
             aes(x = Year, y = ENSPIE_biomass,
                 colour = `Site & Scale`, shape=`Site & Scale`), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_smooth(data = together_dat,
              aes(x = Year, y = ENSPIE_biomass,
                  colour = `Site & Scale`,group=`Site & Scale`))+
  stat_summary(data = of_alpha,
               aes(x = Year, y = ENSPIE_biomass),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",shape=16,size=0.25)+
  stat_summary(data = of_alpha,
               aes(x = Year, y = ENSPIE_biomass),fun = mean, geom="line",size=0.55) +
  stat_summary(data = np_alpha,
               aes(x = Year, y = ENSPIE_biomass),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",shape=16,size=0.25)+
  stat_summary(data = np_alpha,
               aes(x = Year, y = ENSPIE_biomass),fun = mean, geom="line",size=0.55) +
  stat_summary(data = of_gamma,
               aes(x = Year, y = ENSPIE_biomass),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",shape=17,size=0.25)+
  stat_summary(data = of_gamma,
               aes(x = Year, y = ENSPIE_biomass),fun = mean, geom="line",size=0.55) +
  stat_summary(data = np_gamma,
               aes(x = Year, y = ENSPIE_biomass),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",shape=17,size=0.25)+
  stat_summary(data = np_gamma,
               aes(x = Year, y = ENSPIE_biomass),fun = mean, geom="line",size=0.55) +
  scale_color_manual(values =  c("#A1C720FF","#15983DFF","#00AFBB", "#E7B800", "#FC4E07"))  + 
  #scale_x_continuous(breaks=c(0,25,50,75,89)) +
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), plot.margin=margin(t=1,1,1,1, "lines"),
                        legend.direction = "horizontal", legend.position = c(0.5,1.2) )+
  labs(title = "Evenness") + ylab(expression(S[PIE])) + xlab("Year") 


together_pie



(together_rich)/(together_pie + theme(legend.position ="none"))




beta<-read.csv("~/Dropbox/Projects/CCRScale/data/beta.df.csv", header=TRUE) %>%
  as_tibble()


View(beta)


beta.plot.facet<-ggplot() +
  facet_grid(.~Field)+
  geom_point(data = beta,
             aes(x = Year, y = jac,
                 colour = `Field`, shape=`Field`), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_smooth(data = beta,
              aes(x = Year, y = jac,
                  colour = `Field`,group=`Field`))+
  stat_summary(data = beta,
               aes(x = Year, y = jac,color=Field,shape=Field),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,size=0.25)+
  stat_summary(data = beta,
               aes(x = Year, y = jac,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  scale_color_manual(values =  c("#A1C720FF","#15983DFF","#00AFBB", "#E7B800", "#FC4E07"))  + 
  #scale_x_continuous(breaks=c(0,25,50,75,89)) +
  #scale_x_continuous(sec.axis = sec_axis("Year"))+
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), 
                        legend.direction = "horizontal", legend.position = "bottom" )+
  labs(title = "Dissimilarity to Field D") + ylab("Jaccard's Dissimilarity") + xlab("Year") 

beta.plot.facet


beta.a<-beta %>% filter(Field=="A")
beta.b<-beta %>% filter(Field=="B")
beta.c<-beta %>% filter(Field=="C")

beta.plot<-ggplot() +
  #facet_grid(.~Field)+
  geom_point(data = beta,
             aes(x = YSA, y = jac,
                 colour = `Field`, shape=`Field`), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_smooth(data = beta,
              aes(x = YSA, y = jac,
                  colour = `Field`,group=`Field`))+
  stat_summary(data = beta.a,
               aes(x = YSA, y = jac,color=Field,shape=Field),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,size=0.25)+
  stat_summary(data = beta.a,
               aes(x = YSA, y = jac,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  stat_summary(data = beta.b,
               aes(x = YSA, y = jac,color=Field,shape=Field),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,size=0.25)+
  stat_summary(data = beta.c,
               aes(x = YSA, y = jac,color=Field,shape=Field),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,size=0.25)+
  stat_summary(data = beta.c,
               aes(x = YSA, y = jac,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  stat_summary(data = beta.b,
               aes(x = YSA, y = jac,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  scale_color_manual(values =  c("#A1C720FF","#15983DFF","#00AFBB", "#E7B800", "#FC4E07"))  + 
  #scale_x_continuous(breaks=c(0,25,50,75,89)) +
  #scale_x_continuous(sec.axis = sec_axis("Year"))+
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), 
                        legend.direction = "horizontal", legend.position = "bottom" )+
  labs(title = "Dissimilarity to Field D") + ylab("Jaccard's Dissimilarity") + xlab("Year Since Abdandonment") 

beta.plot

(beta.plot)/(beta.plot.facet)




View(alpha_dat)
