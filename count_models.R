

library(tidyverse)
library(ggplot2)
library(brms)
library(bayesplot)
library(patchwork)

ccr_dat <- read.csv("~/Dropbox/Projects/CCRScale/E14 _133/e014_e133_cleaned_1983-2016_EL.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
alpha_dat <- read.csv("~/Dropbox/Projects/CCRScale/E14 _133/alpha_div.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
gamma_dat <- read.csv("~/Dropbox/Projects/CCRScale/E14 _133/gamma_div.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

# SPIE = mobr
# ENSPIE = vegan - inverse Simpson's

View(alpha_dat)
View(gamma_dat)
View(ccr_dat)


alpha_dat_np <- alpha_dat %>% filter(site_status == "never-plowed") %>% 
  summarise(alpha_rich_p_np = mean(alpha_rich))

alpha_dat_np

alpha_dat_of <- alpha_dat %>% filter(site_status == "old field") %>% 
  summarise(alpha_rich_p = mean(alpha_rich))

alpha_dat_of

alpha_dat_of <- alpha_dat %>% filter(site_status == "old field") 

alpha_dat_of$alpha_rich_p<-(alpha_dat_of$alpha_rich/8.13 *100)
  
View(alpha_dat_of)

alpha_dat_of$YSA <- as.numeric(alpha_dat_of$YSA)
alpha_dat_of$log_alpha_rich <- log(alpha_dat_of$alpha_rich)
alpha_dat_of$log_alpha_rich_p <- log(alpha_dat_of$alpha_rich_p)
alpha_dat_of$log_YSA <- log(alpha_dat_of$YSA)
alpha_dat_of$c.YSA<-alpha_dat_of$YSA-mean(alpha_dat_of$YSA)



c.alpha.rich <-  brm(log_alpha_rich ~  log_YSA + ( log_YSA | Field/Transect/Plot/Year), 
                    data = alpha_dat_of, cores = 4, iter=2000, chains = 4)

save(c.alpha.rich, file = '~/Dropbox/Projects/CCRScale/data/model_fits/count/c.alpha.rich.Rdata')
load("~/Dropbox/Projects/CCRScale/data/model_fits/count/c.alpha.rich.Rdata") 

summary(c.alpha.rich)


color_scheme_set("darkgray")
pp_check(c.alpha.rich)+ theme_classic() # predicted vs. observed values



alpha_dat_of$Field<-as.numeric(alpha_dat_of$Field)


# for plotting fixed effects
c.alpha.rich_fitted <- cbind(c.alpha.rich$data,
                          fitted(c.alpha.rich, re_formula = NA)) %>% 
  as_tibble() %>% 
  # join with plot data for figures
  inner_join(alpha_dat_of %>% distinct(Field, Year, log_YSA, YSA, log_alpha_rich_p, alpha_rich_p,alpha_rich),
             by= c("Field", "Year", "log_YSA", "log_alpha_rich_p"))


View(c.alpha.rich_fitted)

# fixed effect coefficients
c.alpha.rich_fixef <- fixef(c.alpha.rich)


c.alpha.rich_coef <- coef(c.alpha.rich) # coefficients
c.alpha.rich_coef 

alpha_dat_of$Field<-as.character(alpha_dat_of$Field)

# pull out the coefficients we want
c.alpha.rich_coef2 <-  bind_cols(c.alpha.rich_coef$Field[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       Field = rownames(c.alpha.rich_coef$Field[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              c.alpha.rich_coef$Field[,,'log_YSA'] %>% 
                                as_tibble() %>% 
                                mutate(Slope = Estimate,
                                       Slope_lower = Q2.5,
                                       Slope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values for plotting slopes
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

c.alpha.rich_fitted$YSA<- as.numeric(c.alpha.rich_fitted$YSA)
c.alpha.rich_fitted$Field<-as.character(c.alpha.rich_fitted$Field)
c.alpha.rich_coef2$Field<-as.character(c.alpha.rich_coef2$Field)


# plot 
c.alpha.rich.fig<-ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = c.alpha.rich_fitted,
             aes(x = YSA, y = alpha_rich_p,
                 colour = Field),
             size = 1.2, shape=1,position = position_jitter(width = 0.95, height = 0.95)) +
  geom_line(data = alpha_dat_sum,aes(x = YSA, y= alpha_rich_p,
                     group = Field,
                     colour = Field),
                     size = 0.75)+
  geom_segment(data = c.alpha.rich_coef2,
               aes(x = xmin,
                   xend = xmax,
                   y = exp(Intercept + Slope * lxmin),
                   yend = exp(Intercept + Slope * lxmax ),
                   group = Field,
                   colour = Field),
               size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = c.alpha.rich_fitted,
              aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = c.alpha.rich_fitted,
            aes(x = YSA, y = exp(Estimate)),
            size = 1.5) +
  # scale_y_continuous(trans = 'log2', breaks = c(25,50,100,150)) +
  #scale_colour_manual(values = c( "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", "#0C5BB0FF", "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#FA6B09FF","#A1C720FF","#9A703EFF" ))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.position="none") +
  labs(title = (expression(paste(italic(alpha), '-scale', sep = ''))) ) +
 ylab("log(Species Richness) ")  + xlab("")

c.alpha.rich.fig



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
gamma_dat_of$log_gamma_rich <- log(gamma_dat_of$gamma_rich)
gamma_dat_of$log_YSA <- log(gamma_dat_of$YSA)
gamma_dat_of$c.YSA<-gamma_dat_of$YSA-mean(gamma_dat_of$YSA)
gamma_dat_of$Field<-as.character(gamma_dat_of$Field)

# ar(time = Year, gr = Field, p = 1),

c.gamma.rich <-  brm(log_gamma_rich ~  log_YSA + (log_YSA | Field/Year), 
                  data = gamma_dat_of,cores = 4, iter=2000, chains = 4)

save(c.gamma.rich, file = '~/Dropbox/Projects/CCRScale/data/model_fits/count/c.gamma.rich.Rdata')
load("~/Dropbox/Projects/CCRScale/data/model_fits/count/c.gamma.rich.Rdata") 

summary(c.gamma.rich)

color_scheme_set("darkgray")
pp_check(c.gamma.rich)+ theme_classic() # predicted vs. observed values

# models residuals
m1<-residuals(c.gamma.rich)
m1<-as.data.frame(m1)
rr.plot<-cbind(gamma_dat_of,m1$Estimate)

par(mfrow=c(1,2))
with(rr.plot, plot(Field, m1$Estimate))
with(rr.plot, plot(Year, m1$Estimate))




# for plotting fixed effects
c.gamma.rich_fitted <- cbind(c.gamma.rich$data,
                       fitted(c.gamma.rich, re_formula = NA)) %>% 
  as_tibble() %>% 
  # join with plot data for figures
  inner_join(gamma_dat_of %>% distinct(Field, Year, log_YSA, YSA, log_gamma_rich, gamma_rich_p, gamma_rich),
  by= c("Field", "Year", "log_YSA", "log_gamma_rich"))


View(c.gamma.rich_fitted)

# fixed effect coefficients
c.gamma.rich_fixef <- fixef(c.gamma.rich)


c.gamma.rich_coef <- coef(c.gamma.rich)
c.gamma.rich_coef 

c.gamma.rich_coef2 <-  bind_cols(c.gamma.rich_coef$Field[,,'Intercept'] %>% 
                                 as_tibble() %>% 
                                 mutate(Intercept = Estimate,
                                        Intercept_lower = Q2.5,
                                        Intercept_upper = Q97.5,
                                        Field = rownames(c.gamma.rich_coef$Field[,,'Intercept'])) %>% 
                                 select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              c.gamma.rich_coef$Field[,,'log_YSA'] %>% 
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

c.gamma.rich_fitted$YSA<- as.numeric(c.gamma.rich_fitted$YSA)
c.gamma.rich_fitted$Field<-as.character(c.gamma.rich_fitted$Field)
c.gamma.rich_coef2$Field<-as.character(c.gamma.rich_coef2$Field)


View(c.gamma.rich_coef2)

c.gamma.rich.fig<-ggplot() +
  #geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = c.gamma.rich_fitted,
             aes(x = YSA, y = gamma_rich,
                 colour = Field),
             size = 1.2, shape=1,position = position_jitter(width = 0.95, height = 0.95)) +
  geom_line(data = c.gamma.rich_fitted,aes(x = YSA, y= gamma_rich,
                                     group = Field,
                                     colour = Field),
            size = 0.75)+
  geom_segment(data = c.gamma.rich_coef2,
               aes(x = xmin,
                   xend = xmax,
                   y = exp(Intercept + Slope * lxmin),
                   yend = exp(Intercept + Slope * lxmax),
                   group = Field,
                   colour = Field),
               size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = c.gamma.rich_fitted,
              aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = c.gamma.rich_fitted,
            aes(x = YSA, y = exp(Estimate)),
            size = 1.5) +
  #scale_y_continuous(trans = 'log2', breaks = c(25,50,100,150)) +
  #scale_colour_manual(values = c( "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", "#0C5BB0FF", "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#FA6B09FF","#A1C720FF","#9A703EFF" ))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.position="none")  +
  labs(title = (expression(paste(italic(gamma), '-scale', sep = '')))
                     ) + ylab("log(Species Richness)")  + xlab("")


c.gamma.rich.fig




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
gamma_dat_of$log_beta_rich <- log(gamma_dat_of$beta_rich)
gamma_dat_of$log_YSA <- log(gamma_dat_of$YSA)
gamma_dat_of$c.YSA<-gamma_dat_of$YSA-mean(gamma_dat_of$YSA)


c.beta.div <-  brm(log_beta_rich ~  log_YSA + (log_YSA | Field/Year), 
                  data = gamma_dat_of,cores = 4, iter=2000, chains = 4)

save(c.beta.div, file = '~/Dropbox/Projects/CCRScale/data/model_fits/count/c.beta.div.Rdata')
load("~/Dropbox/Projects/CCRScale/data/model_fits/count/c.beta.div.Rdata") 


summary(beta.rich)




color_scheme_set("darkgray")
pp_check(c.beta.div)+ theme_classic() # predicted vs. observed values

# models residuals
m1<-residuals(c.beta.div)
m1<-as.data.frame(m1)
rr.plot<-cbind(gamma_dat_of,m1$Estimate)

par(mfrow=c(1,2))
with(rr.plot, plot(Field, m1$Estimate))
with(rr.plot, plot(Year, m1$Estimate))




# for plotting fixed effects
c.beta.div_fitted <- cbind(c.beta.div$data,
                          fitted(c.beta.div, re_formula = NA)) %>% 
  as_tibble() %>% 
  # join with plot data for figures
  inner_join(gamma_dat_of %>% distinct(Field, Year, log_YSA, YSA, log_beta_rich, beta_rich),
             by= c("Field", "Year", "log_YSA", "log_beta_rich"))


View(c.beta.div_fitted)

# fixed effect coefficients
c.beta.div_fixef <- fixef(c.beta.div)


c.beta.div_coef <- coef(c.beta.div)
c.beta.div_coef 

gamma_dat_of$Field<-as.character(gamma_dat_of$Field)


c.beta.div_coef2 <-  bind_cols(c.beta.div_coef$Field[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       Field = rownames(c.beta.div_coef$Field[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             c.beta.div_coef$Field[,,'log_YSA'] %>% 
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

c.beta.div_fitted$YSA<- as.numeric(c.beta.div_fitted$YSA)
c.beta.div_fitted$Field<-as.character(c.beta.div_fitted$Field)
c.beta.div_coef2$Field<-as.character(c.beta.div_coef2$Field)


View(beta.rich_coef2)

View(beta.rich_fitted)

c.beta.div.fig<-ggplot() +
 # geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = c.beta.div_fitted,
             aes(x = YSA, y = beta_rich,
                 colour = Field),
             size = 1.2, shape=1,position = position_jitter(width = 0.95, height = 0.95)) +
  geom_line(data = c.beta.div_fitted,aes(x = YSA, y= beta_rich,
                                        group = Field,
                                        colour = Field),
             size = 0.75)+
  geom_segment(data = c.beta.div_coef2,
               aes(x = xmin,
                   xend = xmax,
                   y = exp(Intercept + Slope * lxmin),
                   yend = exp(Intercept + Slope * lxmax),
                   group = Field,
                   colour = Field),
               size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = c.beta.div_fitted,
              aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = c.beta.div_fitted,
            aes(x = YSA, y = exp(Estimate)),
            size = 1.5) +
  #scale_y_continuous(trans = 'log', breaks = c(25,50,100,150)) +
  #scale_colour_manual(values = c( "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", "#0C5BB0FF", "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#FA6B09FF","#A1C720FF","#9A703EFF" ))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                    legend.direction = "horizontal", legend.position="bottom")  +
  labs(title = (expression(paste('', italic(beta), '-scale', sep = '')))) +
  ylab((expression('log('~paste(italic(beta), '-Diversity)', sep = '')))) +  xlab("Years since agricultural abandonment") + guides(col = guide_legend(ncol = 13))



c.beta.div.fig


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

ysa.legend<-g_legend(c.beta.div.fig)

(c.plot.rich.fig |  c.beta.div.fig + theme(legend.position="none") | c.site.rich.fig )/(ysa.legend) + plot_layout(heights = c(10,1)) 
