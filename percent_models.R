

library(tidyverse)
library(ggplot2)
library(brms)
library(bayesplot)
library(patchwork)
library(RColorBrewer)

ccr_dat <- read.csv("~/Dropbox/Projects/CCRScale/E14 _133/e014_e133_cleaned_1983-2016_EL.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
alpha_dat <- read.csv("~/Dropbox/Projects/CCRScale/E14 _133/alpha_div.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
gamma_dat <- read.csv("~/Dropbox/Projects/CCRScale/E14 _133/gamma_div.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

# SPIE = mobr
# ENSPIE = vegan - inverse Simpson's

View(alpha_dat)
View(gamma_dat)
View(ccr_dat)


alpha_dat_np <- alpha_dat %>% filter(site_status == "never-plowed") %>% group_by(Field,YSA) %>%
  summarise(alpha_rich_p_np = mean(alpha_rich))

View(alpha_dat_np)

alpha_dat_of <- alpha_dat %>% filter(site_status == "old field") %>%  group_by(Field,YSA) %>%
  summarise(alpha_rich_p = mean(alpha_rich))

alpha_dat_of

alpha_dat_of <- alpha_dat %>% filter(site_status == "old field") 

alpha_dat_of$alpha_rich_p<-(alpha_dat_of$alpha_rich/8.13 *100)
  
View(alpha_dat_of)

alpha_dat_of$YSA <- as.numeric(alpha_dat_of$YSA)
alpha_dat_of$log_alpha_rich_p <- log(alpha_dat_of$alpha_rich_p)
alpha_dat_of$log_YSA <- log(alpha_dat_of$YSA)
alpha_dat_of$c.YSA<-alpha_dat_of$YSA-mean(alpha_dat_of$YSA)



p.alpha.rich <-  brm(log_alpha_rich_p ~  log_YSA + ( log_YSA | Field/Transect/Plot/Year), 
                    data = alpha_dat_of, cores = 4, iter=2000, chains = 4)



#save(p.alpha.rich, file = '~/Dropbox/Projects/CCRScale/data/model_fits/percent/p.alpha.rich.Rdata')
load("~/Dropbox/Projects/CCRScale/data/model_fits/percent/p.alpha.rich.Rdata") 

summary(p.alpha.rich)


color_scheme_set("darkgray")
pp_check(p.alpha.rich)+ theme_classic() # predicted vs. observed values



alpha_dat_of$Field<-as.numeric(alpha_dat_of$Field)


# for plotting fixed effects
p.alpha.rich_fitted <- cbind(p.alpha.rich$data,
                          fitted(p.alpha.rich, re_formula = NA)) %>% 
  as_tibble() %>% 
  # join with plot data for figures
  inner_join(alpha_dat_of %>% distinct(Field, Year, log_YSA, YSA, log_alpha_rich_p, alpha_rich_p,alpha_rich),
             by= c("Field", "Year", "log_YSA", "log_alpha_rich_p"))


View(p.alpha.rich_fitted)

# fixed effect coefficients
p.alpha.rich_fixef <- fixef(p.alpha.rich)


p.alpha.rich_coef <- coef(p.alpha.rich)
p.alpha.rich_coef 

alpha_dat_of$Field<-as.character(alpha_dat_of$Field)


p.alpha.rich_coef2 <-  bind_cols(p.alpha.rich_coef$Field[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       Field = rownames(p.alpha.rich_coef$Field[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              p.alpha.rich_coef$Field[,,'log_YSA'] %>% 
                                as_tibble() %>% 
                                mutate(Slope = Estimate,
                                       Slope_lower = Q2.5,
                                       Slope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(alpha_dat_of %>% 
               group_by(Field) %>% 
               summarise(xmin = min(YSA),
                         xmax = max(YSA),
                         lxmin = min(log_YSA),
                         lxmax = max(log_YSA)
               ),
             by = 'Field')


 alpha_dat_sum   <- alpha_dat_of %>% 
                      group_by(Field,YSA) %>% 
                        summarise(alpha_rich_p = mean(alpha_rich_p),
                          alpha_rich = mean(alpha_rich),
                            xmin = min(YSA),
                                xmax = max(YSA))

View(alpha_dat_sum)

p.alpha.rich_fitted$YSA<- as.numeric(p.alpha.rich_fitted$YSA)
p.alpha.rich_fitted$Field<-as.character(p.alpha.rich_fitted$Field)
p.alpha.rich_coef2$Field<-as.character(p.alpha.rich_coef2$Field)


View(p.alpha.rich_coef2)


# Define the number of colors you want
nb.cols <- 17
mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)


p.alpha.rich.fig<-ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = p.alpha.rich_fitted,
             aes(x = YSA, y = alpha_rich_p,
                 colour = Field),
             size = 1.2, shape=1,position = position_jitter(width = 0.95, height = 0.95)) +
  geom_line(data = alpha_dat_sum,aes(x = YSA, y= alpha_rich_p,
                     group = Field,
                     colour = Field),
                     size = 0.75)+
  geom_segment(data = p.alpha.rich_coef2,
               aes(x = xmin,
                   xend = xmax,
                   y = exp(Intercept + Slope * lxmin),
                   yend = exp(Intercept + Slope * lxmax ),
                   group = Field,
                   colour = Field),
               size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = p.alpha.rich_fitted,
              aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = p.alpha.rich_fitted,
            aes(x = YSA, y = exp(Estimate)),
            size = 1.5) +
  # scale_y_continuous(trans = 'log2', breaks = c(25,50,100,150)) +
  scale_color_manual(values = mycolors) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.position="none") +
  labs(title = (expression(paste(italic(alpha), '-scale', sep = ''))) ) +
 ylab("log(Species Richness) (%)")  + xlab("")

p.alpha.rich.fig



#----------------------------------------------------------------------------------------------
gamma_dat_np <- gamma_dat %>% filter(site_status == "never-plowed") %>% 
  summarise(gamma_rich_p_np = mean(gamma_rich))

gamma_dat_np

gamma_dat_of <- gamma_dat %>% filter(site_status == "old field") %>% 
  summarise(gamma_rich_p = mean(gamma_rich))

gamma_dat_of

gamma_dat_of <- gamma_dat %>% filter(site_status == "old field") 

gamma_dat_of$gamma_rich_p<-(gamma_dat_of$gamma_rich/40.82 *100)


View(gamma_dat_of)

gamma_dat_of$YSA <- as.numeric(gamma_dat_of$YSA)
gamma_dat_of$log_gamma_rich_p <- log(gamma_dat_of$gamma_rich_p)
gamma_dat_of$log_YSA <- log(gamma_dat_of$YSA)
gamma_dat_of$c.YSA<-gamma_dat_of$YSA-mean(gamma_dat_of$YSA)
gamma_dat_of$Field<-as.character(gamma_dat_of$Field)

# ar(time = Year, gr = Field, p = 1),

p.gamma.rich <-  brm(log_gamma_rich_p ~  log_YSA + (log_YSA | Field/Year), 
                  data = gamma_dat_of,cores = 4, iter=2000, chains = 4)


save(p.gamma.rich, file = '~/Dropbox/Projects/CCRScale/data/model_fits/percent/p.gamma.rich.Rdata')
load("~/Dropbox/Projects/CCRScale/data/model_fits/percent/p.gamma.rich.Rdata") 

summary(p.gamma.rich)

color_scheme_set("darkgray")
pp_check(p.gamma.rich)+ theme_classic() # predicted vs. observed values

# models residuals
m1<-residuals(p.gamma.rich)
m1<-as.data.frame(m1)
rr.plot<-cbind(gamma_dat_of,m1$Estimate)

par(mfrow=c(1,2))
with(rr.plot, plot(Field, m1$Estimate))
with(rr.plot, plot(Year, m1$Estimate))




# for plotting fixed effects
p.gamma.rich_fitted <- cbind(p.gamma.rich$data,
                       fitted(p.gamma.rich, re_formula = NA)) %>% 
  as_tibble() %>% 
  # join with plot data for figures
  inner_join(gamma_dat_of %>% distinct(Field, Year, log_YSA, YSA, log_gamma_rich_p, gamma_rich_p, gamma_rich),
  by= c("Field", "Year", "log_YSA", "log_gamma_rich_p"))


View(p.gamma.rich_fitted)

# fixed effect coefficients
p.gamma.rich_fixef <- fixef(p.gamma.rich)


p.gamma.rich_coef <- coef(p.gamma.rich)
p.gamma.rich_coef 

p.gamma.rich_coef2 <-  bind_cols(p.gamma.rich_coef$Field[,,'Intercept'] %>% 
                                 as_tibble() %>% 
                                 mutate(Intercept = Estimate,
                                        Intercept_lower = Q2.5,
                                        Intercept_upper = Q97.5,
                                        Field = rownames(p.gamma.rich_coef$Field[,,'Intercept'])) %>% 
                                 select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              p.gamma.rich_coef$Field[,,'log_YSA'] %>% 
                                 as_tibble() %>% 
                                 mutate(Slope = Estimate,
                                        Slope_lower = Q2.5,
                                        Slope_upper = Q97.5) %>% 
                                 select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(gamma_dat_of %>% 
               group_by(Field) %>% 
               summarise(xmin = min(YSA),
                         xmax = max(YSA),
                         lxmin = min(log_YSA),
                         lxmax = max(log_YSA)
                        ),
             by = 'Field')


gamma_dat_sum   <- gamma_dat_of %>% 
  group_by(Field,YSA) %>% 
  summarise(gamma_rich_p = mean(gamma_rich_p),
            gamma_rich = mean(gamma_rich),
            xmin = min(YSA),
            xmax = max(YSA))

View(gamma_dat_sum)

p.gamma.rich_fitted$YSA<- as.numeric(p.gamma.rich_fitted$YSA)
p.gamma.rich_fitted$Field<-as.character(p.gamma.rich_fitted$Field)
p.gamma.rich_coef2$Field<-as.character(p.gamma.rich_coef2$Field)


View(p.gamma.rich_coef2)

p.gamma.rich.fig<-ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = p.gamma.rich_fitted,
             aes(x = YSA, y = gamma_rich_p,
                 colour = Field),
             size = 1.2, shape=1,position = position_jitter(width = 0.95, height = 0.95)) +
  geom_line(data = p.gamma.rich_fitted,aes(x = YSA, y= gamma_rich_p,
                                     group = Field,
                                     colour = Field),
            size = 0.75)+
  geom_segment(data = p.gamma.rich_coef2,
               aes(x = xmin,
                   xend = xmax,
                   y = exp(Intercept + Slope * lxmin),
                   yend = exp(Intercept + Slope * lxmax),
                   group = Field,
                   colour = Field),
               size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = p.gamma.rich_fitted,
              aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = p.gamma.rich_fitted,
            aes(x = YSA, y = exp(Estimate)),
            size = 1.5) +
  #scale_y_continuous(trans = 'log2', breaks = c(25,50,100,150)) +
  scale_color_manual(values = mycolors) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.position="none")  +
  labs(title = (expression(paste(italic(gamma), '-scale', sep = '')))
                     ) + ylab("log(Species Richness) (%)")  + xlab("")


p.gamma.rich.fig




#----------------------------------------------------------------------------------------------
gamma_dat_np <- gamma_dat %>% filter(site_status == "never-plowed") %>% 
  summarise(beta_rich_p_np = mean(beta_rich))

gamma_dat_np

gamma_dat_of <- gamma_dat %>% filter(site_status == "old field") %>% 
  summarise(beta_rich_p = mean(beta_rich))

gamma_dat_of

gamma_dat_of <- gamma_dat %>% filter(site_status == "old field") 

gamma_dat_of$beta_rich_p<-(gamma_dat_of$beta_rich/5.23 *100)

View(gamma_dat_of)

gamma_dat_of$YSA <- as.numeric(gamma_dat_of$YSA)
gamma_dat_of$log_beta_rich_p <- log(gamma_dat_of$beta_rich_p)
gamma_dat_of$log_YSA <- log(gamma_dat_of$YSA)
gamma_dat_of$c.YSA<-gamma_dat_of$YSA-mean(gamma_dat_of$YSA)


p.beta.div <-  brm(log_beta_rich_p ~  log_YSA + (log_YSA | Field/Year), 
                  data = gamma_dat_of,cores = 4, iter=2000, chains = 4)

save(p.beta.div, file = '~/Dropbox/Projects/CCRScale/data/model_fits/percent/p.beta.div.Rdata')
load("~/Dropbox/Projects/CCRScale/data/model_fits/percent/p.beta.div.Rdata") 

summary(p.beta.div)




color_scheme_set("darkgray")
pp_check(p.beta.div)+ theme_classic() # predicted vs. observed values

# models residuals
m1<-residuals(p.beta.div)
m1<-as.data.frame(m1)
rr.plot<-cbind(gamma_dat_of,m1$Estimate)

par(mfrow=c(1,2))
with(rr.plot, plot(Field, m1$Estimate))
with(rr.plot, plot(Year, m1$Estimate))




# for plotting fixed effects
p.beta.div_fitted <- cbind(p.beta.div$data,
                          fitted(p.beta.div, re_formula = NA)) %>% 
  as_tibble() %>% 
  # join with plot data for figures
  inner_join(gamma_dat_of %>% distinct(Field, Year, log_YSA, YSA, log_beta_rich_p, beta_rich_p, beta_rich),
             by= c("Field", "Year", "log_YSA", "log_beta_rich_p"))


View(p.beta.div_fitted)

# fixed effect coefficients
p.beta.div_fixef <- fixef(p.beta.div)


p.beta.div_coef <- coef(p.beta.div)
p.beta.div_coef 

gamma_dat_of$Field<-as.character(gamma_dat_of$Field)


p.beta.div_coef2 <-  bind_cols(p.beta.div_coef$Field[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       Field = rownames(p.beta.div_coef$Field[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             p.beta.div_coef$Field[,,'log_YSA'] %>% 
                                as_tibble() %>% 
                                mutate(Slope = Estimate,
                                       Slope_lower = Q2.5,
                                       Slope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(gamma_dat_of %>% 
               group_by(Field) %>% 
               summarise(xmin = min(YSA),
                         xmax = max(YSA),
                         lxmin = min(log_YSA),
                         lxmax = max(log_YSA)
               ),
             by = 'Field')


beta_dat_sum   <- gamma_dat_of %>% 
  group_by(Field,YSA) %>% 
  summarise(beta_rich_p = mean(beta_rich_p),
            beta_rich = mean(beta_rich),
            xmin = min(YSA),
            xmax = max(YSA),
            lxmin = min(log_YSA),
            lxmax = max(log_YSA))

View(beta_dat_sum)

p.beta.div_fitted$YSA<- as.numeric(p.beta.div_fitted$YSA)
p.beta.div_fitted$Field<-as.character(p.beta.div_fitted$Field)
p.beta.div_coef2$Field<-as.character(p.beta.div_coef2$Field)


View(p.beta.div_coef2)

View(p.beta.div_fitted)

p.beta.div.fig<-ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = p.beta.div_fitted,
             aes(x = YSA, y = beta_rich_p,
                 colour = Field),
             size = 1.2, shape=1,position = position_jitter(width = 0.95, height = 0.95)) +
  geom_line(data = p.beta.div_fitted,aes(x = YSA, y= beta_rich_p,
                                        group = Field,
                                        colour = Field),
             size = 0.75)+
  geom_segment(data = p.beta.div_coef2,
               aes(x = xmin,
                   xend = xmax,
                   y = exp(Intercept + Slope * lxmin),
                   yend = exp(Intercept + Slope * lxmax),
                   group = Field,
                   colour = Field),
               size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = p.beta.div_fitted,
              aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = p.beta.div_fitted,
            aes(x = YSA, y = exp(Estimate)),
            size = 1.5) +
  #scale_y_continuous(trans = 'log', breaks = c(25,50,100,150)) +
  scale_color_manual(values = mycolors) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                    legend.direction = "horizontal", legend.position="bottom")  +
  labs(title = (expression(paste('', italic(beta), '-scale', sep = '')))) +
  ylab((expression('log('~paste(italic(beta), '-Diversity) (%)', sep = '')))) +  xlab("Years since agricultural abandonment") + guides(col = guide_legend(ncol = 13))



p.beta.div.fig


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

ysa.legend<-g_legend(p.beta.div.fig)

(p.alpha.rich.fig |  p.beta.div.fig + theme(legend.position="none") | p.gamma.rich.fig )/(ysa.legend) + plot_layout(heights = c(10,1)) 
