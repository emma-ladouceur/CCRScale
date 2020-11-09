# Cedar Creek Beta Diversity

# Libraries
library(tidyverse)
library(betapart)
library(bayesplot)
library(patchwork)
library(ggplot2)
library(purrr)
library(vegan)


alpha_dat <-read.csv("~/Dropbox/Projects/CCRScale/data/e1_alpha_summary.csv", header=TRUE) %>%
  as_tibble()
gamma_dat <-read.csv("~/Dropbox/Projects/CCRScale/data/e1_gamma_summary.csv", header=TRUE) %>%
  as_tibble()


alpha_dat$Year<-as.character(as.factor(alpha_dat$Year))
alpha_dat$Plot<-as.character(as.factor(alpha_dat$Plot))
alpha_dat$Exp<-as.character(as.factor(alpha_dat$Exp))

gamma_dat$Year<-as.character(as.factor(gamma_dat$Year))
gamma_dat$Exp<-as.character(as.factor(gamma_dat$Exp))


alpha_dat <- alpha_dat %>% group_by(Field, Year, YSA, site_status) %>%
  summarise(mean_alpha = mean(alpha_S_biomass),
            mean_alpha_ENSPIE = mean(alpha_ENSPIE_biomass))



dat <- alpha_dat %>% left_join(gamma_dat)%>%
 # gather(S_measure,S_biomass,alpha_S_biomass,gamma_S_biomass ) %>%
  arrange(Field,Year) 

View(dat)



dat$beta <- dat$gamma_S_biomass/dat$mean_alpha



beta.plot.facet<-ggplot() +
  facet_grid(.~Field)+
  geom_point(data = dat,
             aes(x = Year, y = beta,
                 colour = `Field`, shape=`Field`), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  # geom_smooth(data = dat,
  #             aes(x = Year, y = beta,
  #                 colour = `Field`,group=`Field`))+
  stat_summary(data = dat,
               aes(x = Year, y = beta),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",shape=16,size=0.25)+
  stat_summary(data = dat,
               aes(x = Year, y = beta,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  scale_color_manual(values =  c("#A1C720FF","#15983DFF","#00AFBB", "#E7B800", "#FC4E07"))  + 
  #scale_x_continuous(breaks=c(0,25,50,75,89)) +
  theme_classic()+theme(axis.text.x = element_text(size=7, angle=90), plot.margin=margin(t=1,1,1,1, "lines"),
                        legend.direction = "horizontal", legend.position = "none" )+
labs(title = "Whittaker's Beta") + ylab("Beta Diversity") + xlab("Year") 

beta.plot.facet

piratepal(palette = "info2")


beta.plot<-ggplot() +
  #facet_grid(.~Field)+
  geom_point(data = dat,
             aes(x = Year, y = beta,
                 colour = `Field`, shape=`Field`), alpha=0.6,
             size = 1.3)+
  # geom_smooth(data = dat,
  #             aes(x = Year, y = beta,
  #                 colour = `Field`,group=`Field`))+
  stat_summary(data = dat,
               aes(x = Year, y = beta,color=Field,shape=Field),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,size=0.25)+
  stat_summary(data = dat,
               aes(x = Year, y = beta,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  scale_color_manual(values =  c("#E7B800", "#972C8DFF" ,"#00AFBB","#15983DFF"))  + 
  #scale_x_continuous(breaks=c(0,25,50,75,89)) +
  theme_classic()+theme(axis.text.x = element_text(size=7, angle=90), plot.margin=margin(t=1,1,2,1, "lines"),
                        legend.direction = "horizontal", legend.position = "none")+
labs(title = (expression(paste('b) ', italic(beta), '-Diversity', sep = '')))) + ylab((expression(paste(italic(beta), '-Diversity', sep = '')))) + xlab("Year") 

beta.plot

(beta.plot.facet)/(beta.plot)



# Beta ENSPIE with grams



colnames(dat)

dat$beta_ENSPIE <- dat$gamma_ENSPIE_biomass/dat$mean_alpha_ENSPIE



beta.SPIE.plot.facet<-ggplot() +
  facet_grid(.~Field)+
  geom_point(data = dat,
             aes(x = Year, y = beta_ENSPIE,
                 colour = `Field`, shape=`Field`), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  # geom_smooth(data = dat,
  #             aes(x = Year, y = beta,
  #                 colour = `Field`,group=`Field`))+
  stat_summary(data = dat,
               aes(x = Year, y = beta_ENSPIE),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", color="black",shape=16,size=0.25)+
  stat_summary(data = dat,
               aes(x = Year, y = beta_ENSPIE,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  scale_color_manual(values =  c("#D86C4FFF","#972C8DFF" ,"#00AFBB", "#E7B800", "#FC4E07"))  + 
  #scale_x_continuous(breaks=c(0,25,50,75,89)) +
  theme_classic()+theme(axis.text.x = element_text(size=7, angle=90), plot.margin=margin(t=1,1,1,1, "lines"),
                        legend.direction = "horizontal", legend.position = "none" )+
labs(title = (expression(S[PIE]))) + ylab(expression(S[PIE])) + ylab((expression(S[PIE]))) + ylab(expression(S[PIE]))+ xlab("Year") 

beta.SPIE.plot.facet




beta.SPIE.plot<-ggplot() +
  #facet_grid(.~Field)+
  geom_point(data = dat,
             aes(x = Year, y = beta_ENSPIE,
                 colour = `Field`, shape=`Field`), alpha=0.6,
             size = 1.3)+
  # geom_smooth(data = dat,
  #             aes(x = Year, y = beta,
  #                 colour = `Field`,group=`Field`))+
  stat_summary(data = dat,
               aes(x = Year, y = beta_ENSPIE,color=Field,shape=Field),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,size=0.25)+
  stat_summary(data = dat,
               aes(x = Year, y = beta_ENSPIE,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  scale_color_manual(values =  c("#E7B800", "#972C8DFF" ,"#00AFBB","#15983DFF"))  + 
  #scale_x_continuous(breaks=c(0,25,50,75,89)) +
  theme_classic()+theme(axis.text.x = element_text(size=7, angle=90), plot.margin=margin(t=1,1,2,1, "lines"),
                        legend.direction = "horizontal", legend.position = "bottom")+
  labs(title =  (expression(paste('c) ', italic(beta), -ENS[PIE], sep = ' ')))) + ylab((expression(paste(italic(beta), -ENS[PIE], sep = ' ')))) + xlab("Year") 

beta.SPIE.plot

(beta.plot)/(beta.SPIE.plot)

(sa.all)/(beta.plot)/(beta.SPIE.plot)



beta<-read.csv("~/Dropbox/Projects/CCRScale/data/beta.df.csv", header=TRUE) %>%
  as_tibble()

beta.a<-beta %>% filter(Field=="A")
beta.b<-beta %>% filter(Field=="B")
beta.c<-beta %>% filter(Field=="C")

head(beta)

beta.nest<-ggplot() +
  #facet_grid(.~Field)+
  geom_point(data = beta,
             aes(x = Year, y = jne,
                 colour = `Field`, shape=`Field`), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_smooth(data = beta,
              aes(x = Year, y = jne,
                  colour = `Field`,group=`Field`))+
  stat_summary(data = beta.a,
               aes(x = Year, y = jne,color=Field,shape=Field),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,size=0.25)+
  stat_summary(data = beta.a,
               aes(x = Year, y = jne,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  stat_summary(data = beta.b,
               aes(x = Year, y = jne,color=Field,shape=Field),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,size=0.25)+
  stat_summary(data = beta.c,
               aes(x = Year, y = jne,color=Field,shape=Field),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,size=0.25)+
  stat_summary(data = beta.c,
               aes(x = Year, y = jne,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  stat_summary(data = beta.b,
               aes(x = Year, y = jne,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  scale_color_manual(values =  c("#A1C720FF","#15983DFF","#00AFBB", "#E7B800", "#FC4E07"))  + 
  #scale_x_continuous(breaks=c(0,25,50,75,89)) +
  #scale_x_continuous(sec.axis = sec_axis("Year"))+
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), 
                        legend.direction = "horizontal", legend.position = "none" )+
  labs(title = "Nestedness") + ylab("Nestedness") + xlab("Year") 

beta.nest



beta.turn<-ggplot() +
  #facet_grid(.~Field)+
  geom_point(data = beta,
             aes(x = Year, y = jtu,
                 colour = `Field`, shape=`Field`), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_smooth(data = beta,
              aes(x = Year, y = jtu,
                  colour = `Field`,group=`Field`))+
  stat_summary(data = beta.a,
               aes(x = Year, y = jtu,color=Field,shape=Field),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,size=0.25)+
  stat_summary(data = beta.a,
               aes(x = Year, y = jtu,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  stat_summary(data = beta.b,
               aes(x = Year, y = jtu,color=Field,shape=Field),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,size=0.25)+
  stat_summary(data = beta.c,
               aes(x = Year, y = jtu,color=Field,shape=Field),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,size=0.25)+
  stat_summary(data = beta.c,
               aes(x = Year, y = jtu,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  stat_summary(data = beta.b,
               aes(x = Year, y = jtu,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  scale_color_manual(values =  c("#A1C720FF","#15983DFF","#00AFBB", "#E7B800", "#FC4E07"))  + 
  #scale_x_continuous(breaks=c(0,25,50,75,89)) +
  #scale_x_continuous(sec.axis = sec_axis("Year"))+
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), 
                        legend.direction = "horizontal", legend.position = "none" )+
  labs(title = "Turnover") + ylab("Turnover") + xlab("Year") 

beta.turn


beta.dis<-ggplot() +
  #facet_grid(.~Field)+
  geom_point(data = beta,
             aes(x = Year, y = jac,
                 colour = `Field`, shape=`Field`), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_smooth(data = beta,
              aes(x = Year, y = jac,
                  colour = `Field`,group=`Field`))+
  stat_summary(data = beta.a,
               aes(x = Year, y = jac,color=Field,shape=Field),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,size=0.25)+
  stat_summary(data = beta.a,
               aes(x = Year, y = jac,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  stat_summary(data = beta.b,
               aes(x = Year, y = jac,color=Field,shape=Field),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,size=0.25)+
  stat_summary(data = beta.c,
               aes(x = Year, y = jac,color=Field,shape=Field),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,size=0.25)+
  stat_summary(data = beta.c,
               aes(x = Year, y = jac,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  stat_summary(data = beta.b,
               aes(x = Year, y = jac,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  scale_color_manual(values =  c("#A1C720FF","#15983DFF","#00AFBB", "#E7B800", "#FC4E07"))  + 
  #scale_x_continuous(breaks=c(0,25,50,75,89)) +
  #scale_x_continuous(sec.axis = sec_axis("Year"))+
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), 
                        legend.direction = "horizontal", legend.position = "bottom" )+
  labs(title = "Dissimilarity") + ylab("Dissimilarity") + xlab("Year") 


(beta.nest)/(beta.turn)/(beta.dis)


