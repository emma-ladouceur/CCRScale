

# load packagaes
library(tidyverse)
library(ggplot2)
library(brms)
library(bayesplot)
library(patchwork)

# cleaned data and alpha, beta gamma div datasets
#ccr_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/e014_e133_cleaned_1983-2016_EL.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
alpha_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/alpha_div.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
gamma_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/gamma_div.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

# SPIE = mobr
# ENSPIE = vegan - inverse Simpson's

# Note: Models are computationally intensive but in this case don't take too long to run
# gamma and beta models are the fastest if you want to give it a try

#----------------------------------------------------------------------------------------------
# alpha rich 
alpha_dat$site_status <- factor(alpha_dat$site_status  , levels=c("never-plowed","old field"))
# 
# d.alpha.rich <-  brm(alpha_rich ~  site_status +  ( 1 | Field) + (1 | Year),
#                   data = alpha_dat, family = 'poisson', cores = 4, iter=3000, warmup=1000, chains = 4)

# 
# save(d.alpha.rich, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/d.alpha.rich.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/alpha_rich_d.Rdata')

summary(d.alpha.rich) # model summary


color_scheme_set("darkgray")
fig_s4a <-pp_check(d.alpha.rich) +   
  xlab( "Species richness") + ylab("Density") + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  labs(subtitle= "a)")+
  theme_classic() +  theme( plot.title=element_text(size=18, hjust=0.5), legend.position= "none")# predicted vs. observed values

fig_s4a


 alpha.rich_fixef <- fixef(d.alpha.rich) # look @ fixed effects
 alpha.rich_fixef


alpha_c <- conditional_effects(d.alpha.rich, effects = 'site_status', re_formula = NA, method = 'fitted')  # conditional effects

head(alpha_c)

alpha_dat$site_status <- factor(alpha_dat$site_status  , levels=c("old field","never-plowed"))


#head(alpha_c)

fig_1a <- ggplot() + 
  geom_point(data = alpha_dat,
             aes(x = site_status, y = alpha_rich, colour = 	"#C0C0C0"), 
             size = 0.25, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = alpha_c$site_status,
             aes(x = site_status, y = estimate__, colour = site_status), size = 3) +
  geom_errorbar(data = alpha_c$site_status,
                aes(x = site_status, ymin = lower__, ymax = upper__, colour = site_status),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  labs( subtitle= 'a)'
  ) + ylab("Species Richness") 


fig_1a



#----------------------------------------------------------------------------------------------

# alpha enspie

colnames(alpha_dat)

# alpha_dat$site_status <- factor(alpha_dat$site_status  , levels=c("never-plowed","old field"))
# 
# 
# d.alpha.spie <-  brm(alpha_ENSPIE ~  site_status +  ( 1 | Field) + (1 | Year), 
#                   data = alpha_dat,cores = 4, family = 'lognormal', iter=3000, warmup=1000, chains = 4)
# 
# save(d.alpha.spie, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/d.alpha.spie.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/alpha_pie_d.Rdata") 


summary(d.alpha.spie)


color_scheme_set("darkgray")
fig_s5a <- pp_check(d.alpha.spie)+  xlab( expression(paste(ENS[PIE])) ) + ylab("Density") + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  labs(subtitle= "a)")+
  xlim(-20,20) + theme_classic() +  theme(plot.title=element_text(size=18, hjust=0.5), legend.position= "none")# predicted vs. observed values
fig_s5a

d.alpha.spie_c <- conditional_effects(d.alpha.spie, effects = 'site_status', re_formula = NA, method = 'fitted')  

head(d.alpha.spie_c)

alpha_dat$site_status <- factor(alpha_dat$site_status  , levels=c("old field","never-plowed"))

#head(d.alpha.spie_c)

fig_2a <- ggplot() + 
  geom_point(data = alpha_dat,
             aes(x = site_status, y = alpha_rich, colour = 	"#C0C0C0"), 
             size = 0.25, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = d.alpha.spie_c$site_status,
             aes(x = site_status, y = estimate__, colour = site_status), size = 3) +
  geom_errorbar(data = d.alpha.spie_c$site_status,
                aes(x = site_status, ymin = lower__, ymax = upper__, colour = site_status),
                size = 1, width = 0) +
  labs(x = '',
       y='') +
  scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18 )+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  labs( subtitle= 'a)' ) +  
  ylab( expression(paste(ENS[PIE])) ) 



fig_2a

#----------------------------------------------------------------------------------------------
# gamma rich 
# gamma_dat$site_status <- factor(gamma_dat$site_status  , levels=c("never-plowed","old field"))
# 
# head(gamma_dat)
# 
d.gamma.rich <-  brm(gamma_rich ~  site_status +  (1 | Field) +  (1 | Year),
                     data = gamma_dat,family = 'poisson', cores = 4, iter=2000, chains = 4)



# save(d.gamma.rich, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/d.gamma.rich.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/gamma_rich_d.Rdata") 



summary(d.gamma.rich)


color_scheme_set("darkgray")
fig_s4b <- pp_check(d.gamma.rich)+  xlab( "Species richness") + ylab("") + 
  ggtitle((expression(paste(italic(gamma), '-scale', sep = ''))))+
  labs(subtitle= "b)")+
  xlim(0,70) + theme_classic() +  theme( plot.title=element_text(size=18, hjust=0.5),legend.position= "none")# predicted vs. observed values

fig_s4b


gamma_c <- conditional_effects(d.gamma.rich.test, effects = 'site_status', re_formula = NA, method = 'fitted')  
head(gamma_c)

gamma_dat$site_status <- factor(gamma_dat$site_status  , levels=c("old field","never-plowed"))


head(gamma_dat)

fig_1b <- ggplot() + 
  geom_point(data = gamma_dat,
             aes(x = site_status, y = gamma_rich, colour = 	"#C0C0C0"), 
             size = 0.25, gamma = 0.2, position = position_jitter(width = 0.02, height=0.05)) +
  geom_point(data = gamma_c$site_status,
             aes(x = site_status, y = estimate__, colour = site_status), size = 3) +
  geom_errorbar(data = gamma_c$site_status,
                aes(x = site_status, ymin = lower__, ymax = upper__, colour = site_status),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
  ggtitle((expression(paste(italic(gamma), '-scale', sep = ''))))+
  theme_bw(base_size=18 )+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  labs(subtitle= 'b)'
  ) + ylab("Species Richness")  + xlab("")


fig_1b

#----------------------------------------------------------------------------------------------

# gamma pie

# colnames(gamma_dat)
# gamma_dat$Field<-as.factor(as.character(gamma_dat$Field))
# gamma_dat$Year<-as.factor(as.character(gamma_dat$Year))
# gamma_dat$site_status <- factor(gamma_dat$site_status  , levels=c("never-plowed","old field"))
# 
# head(gamma_dat)
# 
# d.gamma.spie <-  brm(gamma_ENSPIE ~  site_status + (1 | Field)  + (1 | Year),
#                   data = gamma_dat, family = student(), cores = 4, iter=3000, warmup = 1000, chains = 4)
# 
# save(d.gamma.spie, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/d.gamma.spie.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/gamma_pie_d.Rdata") 



summary(d.gamma.spie)

color_scheme_set("darkgray")
fig_s5b <- pp_check(d.gamma.spie)+  xlab( expression(paste(ENS[PIE])) ) + ylab("") + 
  ggtitle((expression(paste(italic(gamma), '-scale', sep = ''))))+
  labs(subtitle= "b)")+  scale_x_continuous(limits = c(-20,25))+ 
  theme_classic() +  theme(plot.title=element_text(size=18, hjust=0.5),legend.position= "none")# predicted vs. observed values
fig_s5b

d.gamma.spie_c <- conditional_effects(d.gamma.spie, effects = 'site_status', re_formula = NA, method = 'fitted')  

head(d.gamma.spie_c)

gamma_dat$site_status <- factor(gamma_dat$site_status  , levels=c("old field","never-plowed"))

#head(d.gamma.spie_c)

fig_2b <- ggplot() + 
  geom_point(data = gamma_dat,
             aes(x = site_status, y = gamma_ENSPIE, colour = 	"#C0C0C0"), 
             size = 0.25, gamma = 0.2, position = position_jitter(width = 0.02, height=0.05)) +
  geom_point(data = d.gamma.spie_c$site_status,
             aes(x = site_status, y = estimate__, colour = site_status), size = 3) +
  geom_errorbar(data = d.gamma.spie_c$site_status,
                aes(x = site_status, ymin = lower__, ymax = upper__, colour = site_status),
                size = 1, width = 0) +
  labs(x = '',
      y='') +
  scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
  ggtitle((expression(paste(italic(gamma), '-scale', sep = ''))))+
  theme_bw(base_size=18 )+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  labs(subtitle= 'b)'
  ) +  ylab(expression(ENS[PIE])) 

fig_2b


#----------------------------------------------------------------------------------------------
# beta div

colnames(gamma_dat)
# gamma_dat$site_status <- factor(gamma_dat$site_status  , levels=c("never-plowed","old field"))
# 
# 
# d.beta.div <-  brm(beta_rich ~  site_status +  (1 | Field) + (1 | Year), 
#                   data = gamma_dat,family=student(), cores = 4, iter=10000,warmup=1000, chains = 4)
# 
# save(d.beta.div, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/d.beta.div.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/beta_div_d.Rdata") 


summary(d.beta.div)


fig_s6a <- pp_check(d.beta.div)+ xlab((expression(paste(italic(beta), '-Diversity', sep = '')))) +  ylab("Density") + 
  labs(title= "a)")+
  theme_classic() +  theme(legend.position= "none")# predicted vs. observed values

fig_s6a

d.beta_c <- conditional_effects(d.beta.div, effects = 'site_status', re_formula = NA, method = 'fitted')  


head(d.beta_c)
gamma_dat$site_status <- factor(gamma_dat$site_status  , levels=c("old field","never-plowed"))


#head(d.beta_c)
colnames(gamma_dat)

fig_3a <- ggplot() + 
  geom_point(data = gamma_dat,
             aes(x = site_status, y = beta_div, colour = 	"#C0C0C0"), 
             size = 0.25, gamma = 0.2, position = position_jitter(width = 0.02, height=0.05)) +
  geom_point(data = d.beta_c$site_status,
             aes(x = site_status, y = estimate__, colour = site_status), size = 3) +
  geom_errorbar(data = d.beta_c$site_status,
                aes(x = site_status, ymin = lower__, ymax = upper__, colour = site_status),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
  theme_bw(base_size=18 )+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.3, unit = "cm"),
                               strip.background = element_blank(),legend.position="none") +
  labs(subtitle= 'a)') +
  ylab((expression(paste(italic(beta), '-Diversity', sep = '')))) + xlab('')


fig_3a


#----------------------------------------------------------------------------------------------
# beta spie

# colnames(gamma_dat)
# gamma_dat$site_status <- factor(gamma_dat$site_status  , levels=c("never-plowed","old field"))
# 
# 
# d.beta.spie <-  brm(beta_ENSPIE ~  site_status + (1 | Field) + (1 | Year), 
#                  data = gamma_dat, family=student(),cores = 4, iter=4000,warmup=1000, chains = 4)
# 
# save(d.beta.spie, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/d.beta.spie.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/beta_pie_d.Rdata") 


summary(d.beta.spie)


fig_s6c <- pp_check(d.beta.spie)+ xlab((expression(paste(italic(beta), -ENS[PIE], sep = ' ')))) + ylab("Density")+
  labs(title= "c)")+
  theme_classic() +  theme(legend.position= "none")# predicted vs. observed values
fig_s6c

d.beta.spie_c <- conditional_effects(d.beta.spie, effects = 'site_status', re_formula = NA, method = 'fitted')  


head(d.beta.spie_c)
gamma_dat$site_status <- factor(gamma_dat$site_status  , levels=c("old field","never-plowed"))


#head(d.beta.spie_c)
colnames(gamma_dat)

fig_3c <- ggplot() + 
  geom_point(data = gamma_dat,
             aes(x = site_status, y = beta_ENSPIE, colour = 	"#C0C0C0"), 
             size = 0.25, gamma = 0.2, position = position_jitter(width = 0.02, height=0.05)) +
  geom_point(data = d.beta.spie_c$site_status,
             aes(x = site_status, y = estimate__, colour = site_status), size = 3) +
  geom_errorbar(data = d.beta.spie_c$site_status,
                aes(x = site_status, ymin = lower__, ymax = upper__, colour = site_status),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               strip.background = element_blank(),legend.position="none") +
  labs(title =  '',
       subtitle= 'c)'
  )+
  ylab((expression(paste(italic(beta), -ENS[PIE], sep = ' ')))) + xlab('')


fig_3c



