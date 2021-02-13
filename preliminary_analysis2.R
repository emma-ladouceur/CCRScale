

library(yarrr)
library(ggplot2)
library(ggforce)
library(patchwork)
library(tidyverse)
library(Hmisc)



alpha_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/alpha_div.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
gamma_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/gamma_div.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


colnames(alpha_dat)

of_alpha <- alpha_dat %>% filter(site_status == "old field" ) 
np_alpha <- alpha_dat %>% filter(site_status == "never-plowed" ) 

of_gamma <- gamma_dat %>% filter(site_status == "old field" ) 
np_gamma <- gamma_dat %>% filter(site_status == "never-plowed" ) 


levels(alpha_dat$YSA)
alpha_dat$YSA<-factor(alpha_dat$YSA,  levels=c("1" , "3" , "5" , "6" , "7" , "8" , "9" , "11" , "12" , "14" , "15" , "17" , "18" , "19" , "20" , "21" , "22" , "23" , "24" , "25" , "26" , "27" , "28" , "29" , "30" , "31" , "32" , "33" , "34" , "35" , "36" , "37" ,"38" , "39" , "40" , "41" , "42" , "43" , "45" , "46" , "47", "48" , "49" , "50" , "51" , "53" , "54" , "55" , "56" ,"59" ,  "61" , "62" , "63" , "65" , "67" , "70",  "75" , "79" , "never-plowed"))


alpha_plot<-ggplot() +
  #facet_grid(.~Field)+
  geom_point(data = alpha_dat,
             aes(x = YSA, y = alpha_rich,
                 colour = `site_status`, shape=`site_status`), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_smooth(data = alpha_dat,
              aes(x = YSA, y = alpha_rich,
              ))+
  stat_summary(data = alpha_dat,
               aes(x = YSA, y = alpha_rich,color=site_status,group=site_status),fun = mean, geom="line",size=0.55) +
  stat_summary(data = alpha_dat,
               aes(x = YSA, y = alpha_rich),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black", shape=16,size=0.25)+
  scale_color_manual(values =  c("#228B22", 	"#6B8E23"))  + 
  scale_x_discrete(breaks=c(1,9,20,20,30,40,50,61,70,79)) +
  #ylim(0,60) +
  theme_classic()+theme(axis.text.x = element_text(size=7), plot.margin=margin(t=2,1,1,1, "lines"),
                        legend.direction = "horizontal", legend.position = "none" )+
  labs(title = (expression(paste(italic(alpha), '-scale', sep = '')))
  ) + ylab("Species Richness") + xlab("Years since agricultural abandonment") 

alpha_plot


alpha_PIE<-ggplot() +
  #facet_grid(.~Field)+
  geom_point(data = alpha_dat,
             aes(x = YSA, y = alpha_ENSPIE,
                 colour = `site_status`, shape=`site_status`), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_smooth(data = alpha_dat,
              aes(x = YSA, y = alpha_ENSPIE,
              ))+
  stat_summary(data = alpha_dat,
               aes(x = YSA, y = alpha_ENSPIE,color=site_status,group=site_status),fun = mean, geom="line",size=0.55) +
  stat_summary(data = alpha_dat,
               aes(x = YSA, y = alpha_ENSPIE),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black", shape=16,size=0.25)+
  scale_color_manual(values =  c("#228B22", 	"#6B8E23"))  + 
  scale_x_discrete(breaks=c(1,9,20,20,30,40,50,61,70,79)) +
  #ylim(0,200) +
  theme_classic()+theme(axis.text.x = element_text(size=7), plot.margin=margin(t=2,1,1,1, "lines"),
                        legend.direction = "horizontal", legend.position = "none" )+
  labs(title =  ''
       #(expression(paste(italic(alpha), '-scale', sep = '')))
  ) +  labs(title = (expression(paste(italic(alpha), '-scale', sep = '')))
  ) + ylab(expression(ENS[PIE])) + xlab("Years since agricultural abandonment") 

alpha_PIE


gamma_dat$YSA<-factor(gamma_dat$YSA,  levels=c("1" , "3" , "5" , "6" , "7" , "8" , "9" , "11" , "12" , "14" , "15" , "17" , "18" , "19" , "20" , "21" , "22" , "23" , "24" , "25" , "26" , "27" , "28" , "29" , "30" , "31" , "32" , "33" , "34" , "35" , "36" , "37" ,"38" , "39" , "40" , "41" , "42" , "43" , "45" , "46" , "47", "48" , "49" , "50" , "51" , "53" , "54" , "55" , "56" ,"59" ,  "61" , "62" , "63" , "65" , "67" , "70",  "75" , "79",  "never-plowed"))

gamma_plot<-ggplot() +
  #facet_grid(.~Field)+
  geom_point(data = gamma_dat,
             aes(x = YSA, y = gamma_rich,
                 colour = `site_status`, shape=`site_status`), gamma=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_smooth(data = gamma_dat,
              aes(x = YSA, y = gamma_rich,
              ))+
  stat_summary(data = gamma_dat,
               aes(x = YSA, y = gamma_rich,color=site_status,group=site_status),fun = mean, geom="line",size=0.55) +
  stat_summary(data = gamma_dat,
               aes(x = YSA, y = gamma_rich),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black", shape=16,size=0.25)+
  scale_color_manual(values =  c("#228B22", 	"#6B8E23"))  + 
  scale_x_discrete(breaks=c(1,9,20,20,30,40,50,61,70,79)) +
  #ylim(0,60) +
  theme_classic()+theme(axis.text.x = element_text(size=7), plot.margin=margin(t=2,1,1,1, "lines"),
                        legend.direction = "horizontal", legend.position = "none" )+
  labs(title = (expression(paste(italic(gamma), '-scale', sep = '')))
  ) + ylab("Species Richness") + xlab("Years since agricultural abandonment") 

gamma_plot


gamma_PIE<-ggplot() +
  #facet_grid(.~Field)+
  geom_point(data = gamma_dat,
             aes(x = YSA, y = gamma_ENSPIE,
                 colour = `site_status`, shape=`site_status`), gamma=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_smooth(data = gamma_dat,
              aes(x = YSA, y = gamma_ENSPIE,
              ))+
  stat_summary(data = gamma_dat,
               aes(x = YSA, y = gamma_ENSPIE,color=site_status,group=site_status),fun = mean, geom="line",size=0.55) +
  stat_summary(data = gamma_dat,
               aes(x = YSA, y = gamma_ENSPIE),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black", shape=16,size=0.25)+
  scale_color_manual(values =  c("#228B22", 	"#6B8E23"))  + 
  scale_x_discrete(breaks=c(1,9,20,20,30,40,50,61,70,79)) +
  #ylim(0,200) +
  theme_classic()+theme(axis.text.x = element_text(size=7), plot.margin=margin(t=2,1,1,1, "lines"),
                         legend.direction = "horizontal", legend.position = "bottom" )+
  labs(title = '', color= "Site Status", shape= "Site Status"
       #(expression(paste(italic(gamma), '-scale', sep = '')))
  )  +   labs(title = (expression(paste(italic(gamma), '-scale', sep = '')))
  ) + ylab(expression(ENS[PIE])) + xlab("Years since agricultural abandonment") 

gamma_PIE


beta.plot<-ggplot() +
  #facet_grid(.~Field)+
  geom_point(data = gamma_dat,
             aes(x = YSA, y = beta_rich,
                 colour = `site_status`, shape=`site_status`), alpha=0.6,
             size = 1.3)+
  geom_smooth(data = gamma_dat,
              aes(x = YSA, y = beta_rich,
                  ))+
  stat_summary(data = gamma_dat,
               aes(x = YSA, y = beta_rich,color=site_status,group=site_status),fun = mean, geom="line",size=0.55) +
  stat_summary(data = gamma_dat,
               aes(x = YSA, y = beta_rich),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,color="black",size=0.25)+
  scale_color_manual(values =  c("#228B22", 	"#6B8E23"))  + 
  #ylim(2,20) +
  scale_x_discrete(breaks=c(1,9,20,20,30,40,50,61,70,79)) +
  theme_classic()+theme(axis.text.x = element_text(size=7), plot.margin=margin(t=1,1,2,1, "lines"),
                        legend.direction = "horizontal", legend.position = "none")+
  labs(title = (expression(paste('', italic(beta), '-scale', sep = '')))) + ylab((expression(paste(italic(beta), '-Diversity', sep = '')))) + xlab("Years since agricultural abandonment") 



beta.plot


beta.PIE<-ggplot() +
  #facet_grid(.~Field)+
  geom_point(data = gamma_dat,
             aes(x = YSA, y = beta_ENSPIE,
                 colour = `site_status`, shape=`site_status`), alpha=0.6,
             size = 1.3)+
  geom_smooth(data = gamma_dat,
              aes(x = YSA, y = beta_ENSPIE,
              ))+
  stat_summary(data = gamma_dat,
               aes(x = YSA, y = beta_ENSPIE,color=site_status,group=site_status),fun = mean, geom="line",size=0.55) +
  stat_summary(data = gamma_dat,
               aes(x = YSA, y = beta_ENSPIE),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,color="black",size=0.25)+
  scale_color_manual(values =  c("#228B22", 	"#6B8E23"))  + 
  scale_x_discrete(breaks=c(1,9,20,20,30,40,50,61,70,79)) +
  #ylim(2,20) +
  theme_classic()+theme(axis.text.x = element_text(size=7), plot.margin=margin(t=1,1,2,1, "lines"),
                        legend.direction = "horizontal", legend.position = "none")+
  #labs(title =  (expression(paste(' ', italic(beta), -ENS[PIE], sep = ' ')))) + 
  labs(title = (expression(paste('', italic(beta), '-scale', sep = '')))) +
  ylab((expression(paste(italic(beta), -ENS[PIE], sep = ' ')))) + xlab("Years since agricultural abandonment") 



beta.PIE

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

ccr.legend<-g_legend(gamma_PIE)


# Figure S1
(alpha_plot | alpha_PIE ) /  (gamma_plot | gamma_PIE + theme(legend.position="none"))/ (beta.plot | beta.PIE) /(ccr.legend) + plot_layout(heights = c(10,10,10,1)) 

# Figure S1 alternative
(alpha_plot | gamma_plot  ) / (alpha_PIE | gamma_PIE + theme(legend.position="none"))/ (beta.plot | beta.PIE) / (ccr.legend) + plot_layout(heights = c(10,10,10,1)) 



