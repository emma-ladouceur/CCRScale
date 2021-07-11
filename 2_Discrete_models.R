

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
# alpha_dat$site_status <- factor(alpha_dat$site_status  , levels=c("never-plowed","old field"))
# 
# d.alpha.rich <-  brm(alpha_rich ~  site_status +  ( 1 | Field) + (1 | Year), 
#                   data = alpha_dat, family = 'poisson', cores = 4, iter=3000, warmup=1000, chains = 4)
# 
# 
# save(d.alpha.rich, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/d.alpha.rich.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/alpha_rich_d.Rdata')

summary(d.alpha.rich) # model summary


color_scheme_set("darkgray")
pp_check(d.alpha.rich) +   
  xlab( "Species richness") + ylab("Density") + theme_classic() # predicted vs. observed values



 alpha.rich_fixef <- fixef(d.alpha.rich) # look @ fixed effects
 alpha.rich_fixef


alpha_c <- conditional_effects(d.alpha.rich, effects = 'site_status', re_formula = NA, method = 'fitted')  # conditional effects

View(alpha_c)

alpha_dat$site_status <- factor(alpha_dat$site_status  , levels=c("old field","never-plowed"))


#View(alpha_c)

d.alpha.rich.eff<-ggplot() + 
  geom_point(data = alpha_dat,
             aes(x = site_status, y = alpha_rich, colour = 	"#C0C0C0"), 
             size = 0.25, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = alpha_c$site_status,
             aes(x = site_status, y = estimate__, colour = site_status), size = 3) +
  geom_errorbar(data = alpha_c$site_status,
                aes(x = site_status, ymin = lower__, ymax = upper__, colour = site_status),
                size = 1, width = 0) +
  #facet_wrap(~Model)+
  labs(x = '',
       # y= expression(paste('Effect of NPK on Species Loss'))
       y='') +
  #geom_hline(yintercept = 0, lty = 2) +
 # scale_y_continuous(trans = 'log2', breaks = c(4,8, 16, 24)) +
  #ylim(0,60)+ 
  scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               # axis.text.y = element_text(size=6),
                               # axis.text.x = element_text(size=6),
                                plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  labs(#title = (expression(paste(italic(alpha), '-scale', sep = ''))),
       subtitle= 'a)'
  ) + ylab("Species Richness") 


d.alpha.rich.eff



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
pp_check(d.alpha.spie)+  xlab( expression(paste(ENS[PIE])) ) + ylab("Density") + xlim(-20,20) + theme_classic() # predicted vs. observed values


d.alpha.spie_c <- conditional_effects(d.alpha.spie, effects = 'site_status', re_formula = NA, method = 'fitted')  

View(d.alpha.spie_c)

alpha_dat$site_status <- factor(alpha_dat$site_status  , levels=c("old field","never-plowed"))

#View(d.alpha.spie_c)

d.alpha.spie.eff<-ggplot() + 
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
  #geom_hline(yintercept = 0, lty = 2) +
  #scale_y_continuous(trans = 'log2', breaks = c(4,8, 16, 24)) +
  scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
  #ylim(0,200) +
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18 )+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               # axis.text.y = element_text(size=6),
                               # axis.text.x = element_text(size=6),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  labs(#title = (expression(paste(italic(alpha), '-scale', sep = ''))),
       subtitle= 'a)'
  ) +  
  ylab( expression(paste(ENS[PIE])) ) 



d.alpha.spie.eff

#----------------------------------------------------------------------------------------------
# gamma rich 
# gamma_dat$site_status <- factor(gamma_dat$site_status  , levels=c("never-plowed","old field"))
# 
# d.gamma.rich <-  brm(gamma_rich ~  site_status +  (1 | Field) + (1 | Year), 
#                   data = gamma_dat,family = 'poisson',cores = 4, iter=2000, chains = 4)
# 
# save(d.gamma.rich, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/d.gamma.rich.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/gamma_rich_d.Rdata") 



summary(d.gamma.rich)


color_scheme_set("darkgray")
pp_check(d.gamma.rich)+  xlab( "Species richness") + ylab("Density") + xlim(0,70) + theme_classic() # predicted vs. observed values


gamma_c <- conditional_effects(d.gamma.rich, effects = 'site_status', re_formula = NA, method = 'fitted')  
View(gamma_c)

gamma_dat$site_status <- factor(gamma_dat$site_status  , levels=c("old field","never-plowed"))


View(gamma_dat)

d.gamma.rich.eff<-ggplot() + 
  geom_point(data = gamma_dat,
             aes(x = site_status, y = gamma_rich, colour = 	"#C0C0C0"), 
             size = 0.25, gamma = 0.2, position = position_jitter(width = 0.02, height=0.05)) +
  geom_point(data = gamma_c$site_status,
             aes(x = site_status, y = estimate__, colour = site_status), size = 3) +
  geom_errorbar(data = gamma_c$site_status,
                aes(x = site_status, ymin = lower__, ymax = upper__, colour = site_status),
                size = 1, width = 0) +
  #geom_hline(yintercept = 0, lty = 2) +
  #scale_y_continuous(trans = 'log2', breaks = c(12, 16, 24,36,48,66)) +
  scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
  #ylim(0,60) +
  ggtitle((expression(paste(italic(gamma), '-scale', sep = ''))))+
  theme_bw(base_size=18 )+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               # axis.text.y = element_text(size=6),
                               # axis.text.x = element_text(size=6),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  labs(#title = (expression(paste(italic(gamma), '-scale', sep = ''))),
       subtitle= 'b)'
  ) + ylab("Species Richness")  + xlab("")


d.gamma.rich.eff

#----------------------------------------------------------------------------------------------

# gamma pie

# colnames(gamma_dat)
# gamma_dat$Field<-as.factor(as.character(gamma_dat$Field))
# gamma_dat$Year<-as.factor(as.character(gamma_dat$Year))
# gamma_dat$site_status <- factor(gamma_dat$site_status  , levels=c("never-plowed","old field"))
# 
# View(gamma_dat)
# 
# d.gamma.spie <-  brm(gamma_ENSPIE ~  site_status + (1 | Field)  + (1 | Year),
#                   data = gamma_dat, family = student(), cores = 4, iter=3000, warmup = 1000, chains = 4)
# 
# save(d.gamma.spie, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/d.gamma.spie.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/gamma_pie_d.Rdata") 



summary(d.gamma.spie)

color_scheme_set("darkgray")
pp_check(d.gamma.spie)+  xlab( expression(paste(ENS[PIE])) ) + ylab("Density") + theme_classic() # predicted vs. observed values


d.gamma.spie_c <- conditional_effects(d.gamma.spie, effects = 'site_status', re_formula = NA, method = 'fitted')  

View(d.gamma.spie_c)

gamma_dat$site_status <- factor(gamma_dat$site_status  , levels=c("old field","never-plowed"))

#View(d.gamma.spie_c)

d.gamma.spie.eff<-ggplot() + 
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
 # geom_hline(yintercept = 0, lty = 2) +
  #ylim(0,200)+
  #scale_y_continuous( breaks = c(10, 20,50,100,150,200)) +
  scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
  ggtitle((expression(paste(italic(gamma), '-scale', sep = ''))))+
  theme_bw(base_size=18 )+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               # axis.text.y = element_text(size=6),
                               # axis.text.x = element_text(size=6),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  labs(#title = (expression(paste(italic(gamma), '-scale', sep = ''))),
       subtitle= 'b)'
  ) +  ylab(expression(ENS[PIE])) 


# theme_bw(base_size=18)

d.gamma.spie.eff


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


pp_check(d.beta.div)+ xlab((expression(paste(italic(beta), '-Diversity', sep = '')))) +  ylab("Density") + theme_classic() # predicted vs. observed values


d.beta_c <- conditional_effects(d.beta.div, effects = 'site_status', re_formula = NA, method = 'fitted')  


View(d.beta_c)
gamma_dat$site_status <- factor(gamma_dat$site_status  , levels=c("old field","never-plowed"))


#View(d.beta_c)
colnames(gamma_dat)

d.beta.div.eff<-ggplot() + 
  geom_point(data = gamma_dat,
             aes(x = site_status, y = beta_div, colour = 	"#C0C0C0"), 
             size = 0.25, gamma = 0.2, position = position_jitter(width = 0.02, height=0.05)) +
  geom_point(data = d.beta_c$site_status,
             aes(x = site_status, y = estimate__, colour = site_status), size = 3) +
  geom_errorbar(data = d.beta_c$site_status,
                aes(x = site_status, ymin = lower__, ymax = upper__, colour = site_status),
                size = 1, width = 0) +
 # geom_hline(yintercept = 0, lty = 2) +
 # scale_y_continuous(trans = 'log2', breaks = c(12, 16, 24,36,48,66)) +
  scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
 # ggtitle((expression(paste('', italic(beta), '-scale', sep = ''))))+
  theme_bw(base_size=18 )+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.3, unit = "cm"),
                               # axis.text.y = element_text(size=6),
                               # axis.text.x = element_text(size=6),
                               #plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  labs(#title = (expression(paste('', italic(beta), '-scale', sep = ''))),
       subtitle= 'a)') +
  ylab((expression(paste(italic(beta), '-Diversity', sep = '')))) + xlab('')


d.beta.div.eff


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


pp_check(d.beta.spie)+ xlab((expression(paste(italic(beta), -ENS[PIE], sep = ' ')))) + ylab("Density")+theme_classic() # predicted vs. observed values


d.beta.spie_c <- conditional_effects(d.beta.spie, effects = 'site_status', re_formula = NA, method = 'fitted')  


View(d.beta.spie_c)
gamma_dat$site_status <- factor(gamma_dat$site_status  , levels=c("old field","never-plowed"))


#View(d.beta.spie_c)
colnames(gamma_dat)

d.beta.spie.eff <- ggplot() + 
  geom_point(data = gamma_dat,
             aes(x = site_status, y = beta_ENSPIE, colour = 	"#C0C0C0"), 
             size = 0.25, gamma = 0.2, position = position_jitter(width = 0.02, height=0.05)) +
  geom_point(data = d.beta.spie_c$site_status,
             aes(x = site_status, y = estimate__, colour = site_status), size = 3) +
  geom_errorbar(data = d.beta.spie_c$site_status,
                aes(x = site_status, ymin = lower__, ymax = upper__, colour = site_status),
                size = 1, width = 0) +
 # geom_hline(yintercept = 0, lty = 2) +
  #scale_y_continuous(trans = 'log2', breaks = c(8,12, 16, 20)) +
  scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
  theme_bw(base_size=18 )+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               # axis.text.y = element_text(size=6),
                               # axis.text.x = element_text(size=6),
                               # title=element_text(size=8),
                               strip.background = element_blank(),legend.position="none") +
  labs(title =  '',
       subtitle= 'c)'
       #(expression(paste(italic(gamma), '-scale', sep = '')))+
  )+
  ylab((expression(paste(italic(beta), -ENS[PIE], sep = ' ')))) + xlab('')


d.beta.spie.eff


# Don't need a legend, really
# 
# g_legend<-function(a.gplot){
#   tmp <- ggplot_gtable(ggplot_build(a.gplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)}
# 
# ccr.legend<-g_legend(gamma.spie.eff)


# save as 10X10 portrait

(d.alpha.rich.eff | d.alpha.spie.eff ) / (d.gamma.rich.eff | d.gamma.spie.eff) / (d.beta.div.eff | d.beta.spie.eff + theme(legend.position="none")) + plot_layout(heights = c(10,10,10)) 


# alternative arrangement
(d.alpha.rich.eff | d.gamma.rich.eff  ) / (d.alpha.spie.eff | d.gamma.spie.eff) / (d.beta.div.eff | d.beta.spie.eff + theme(legend.position="none")) + plot_layout(heights = c(10,10,10)) 



