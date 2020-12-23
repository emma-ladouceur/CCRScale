

library(tidyverse)
library(ggplot2)
library(brms)
library(bayesplot)
library(patchwork)
library(RColorBrewer)
library(tidybayes)
library(ggdist)
library(modelr)

ccr_dat <- read.csv("~/Dropbox/Projects/CCRScale/E14 _133/e014_e133_cleaned_1983-2016_EL.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
alpha_dat <- read.csv("~/Dropbox/Projects/CCRScale/E14 _133/alpha_div.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
gamma_dat <- read.csv("~/Dropbox/Projects/CCRScale/E14 _133/gamma_div.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

# SPIE = mobr
# ENSPIE = vegan - inverse Simpson's

View(alpha_dat)
View(gamma_dat)
View(ccr_dat)

# check what the mean richness is for each field type
# turn the grouping on or off to see details
alpha_dat_np <- alpha_dat %>% filter(site_status == "never-plowed") %>% #group_by(Field,YSA) %>%
  summarise(alpha_rich_p_np = mean(alpha_rich))

alpha_dat_np

alpha_dat_of <- alpha_dat %>% filter(site_status == "old field") %>%  #group_by(Field,YSA) %>%
  summarise(alpha_rich_p = mean(alpha_rich))

alpha_dat_of

alpha_dat_of <- alpha_dat %>% filter(site_status == "old field") 

alpha_dat_of$alpha_rich_p <- (alpha_dat_of$alpha_rich/9.21 *100)
  
View(alpha_dat_of)

alpha_dat_of$YSA <- as.numeric(alpha_dat_of$YSA)
alpha_dat_of$log_alpha_rich_p <- log(alpha_dat_of$alpha_rich_p)
alpha_dat_of$log_YSA <- log(alpha_dat_of$YSA)
alpha_dat_of$c.YSA<-alpha_dat_of$YSA-mean(alpha_dat_of$YSA)
alpha_dat_of$Field<-as.factor(as.character(alpha_dat_of$Field))
alpha_dat_of$Year<-as.factor(as.character(alpha_dat_of$Year))
alpha_dat_of$Field<-as.factor(as.character(alpha_dat_of$Field))

# This model takes some time to run so it is recommended to run on a  server or cluster 
# possible to run on local machine but it takes some time
# see alpha.rich.sh for cluster submit script and paired R script alpha.rich.R 

# p.alpha.rich.s <-  brm(log_alpha_rich_p ~  log_YSA + ( 1 + log_YSA  | Field/Transect/Plot) + (1 | Year), 
#                        data = alpha_dat_of, family=student(), cores = 4, iter=10000,warmup = 1000, control =
#                          list(adapt_delta = 0.99), chains = 4)
# 
# save(p.alpha.rich, file = '~/Dropbox/Projects/CCRScale/data/model_fits/percent/p.alpha.rich.Rdata')
load("~/Dropbox/Projects/CCRScale/data/model_fits/percent/p.alpha.rich.Rdata") 
load("~/Desktop/p.alpha.rich.Rdata") 

summary(p.alpha.rich.s)


color_scheme_set("darkgray")
pp_check(p.alpha.rich.s)+ theme_classic() # predicted vs. observed values

alpha_dat_of$Field<-as.factor(as.character(alpha_dat_of$Field))
alpha_dat_of$Year<-as.factor(as.character(alpha_dat_of$Year))

# models residuals
ma<-residuals(p.alpha.rich.s)
ma<-as.data.frame(ma)
ar.plot<-cbind(alpha_dat_of,ma$Estimate)

par(mfrow=c(1,2))
with(ar.plot, plot(Field, ma$Estimate))
with(ar.plot, plot(Year, ma$Estimate))

alpha_dat_of$Field<-as.numeric(alpha_dat_of$Field)

# for plotting fixed effects
p.alpha.rich_fitted <- cbind(p.alpha.rich.s$data,
                          fitted(p.alpha.rich.s, re_formula = NA)) %>% 
  as_tibble()


inner_join(alpha_dat_of %>% distinct(Field, Year, log_YSA, YSA, log_alpha_rich_p, alpha_rich_p, alpha_rich),
             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
             )


View(p.alpha.rich_fitted)


# fixed effect coefficients
p.alpha.rich_fixef <- fixef(p.alpha.rich.s)


p.alpha.rich_coef <- coef(p.alpha.rich.s)
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

View(p.alpha.rich_fitted)
View(p.alpha.rich_coef2)


# Define the number of colors you want
nb.cols <- 17
mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)


p.alpha.rich.fig<-ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = p.alpha.rich_fitted,
             aes(x = YSA, y = alpha_rich_p,
                 colour = Field),
             size = 1.2, shape=1, position = position_jitter(width = 0.95, height=0.95)) +
  # geom_line(data = alpha_dat_sum,aes(x = YSA, y= alpha_rich_p,
  #                    group = Field,
  #                    colour = Field),
  #                    size = 0.55)+
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
 #  scale_y_continuous( limits=c(10,210),breaks = c(25,50,100,150,200)) +
  scale_color_manual(values = mycolors) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.position="none") +
  labs(title = (expression(paste(italic(alpha), '-scale', sep = ''))) ) +
 ylab("Species Richness (%)")  + xlab("")

p.alpha.rich.fig



#----------------------------------------------------------------------------------------------
gamma_dat_np <- gamma_dat %>% filter(site_status == "never-plowed") %>% 
  summarise(gamma_rich_p_np = mean(gamma_rich))

gamma_dat_np

gamma_dat_of <- gamma_dat %>% filter(site_status == "old field") %>% 
  summarise(gamma_rich_p = mean(gamma_rich))

gamma_dat_of

gamma_dat_of <- gamma_dat %>% filter(site_status == "old field") 

gamma_dat_of$gamma_rich_p<-(gamma_dat_of$gamma_rich/43.33 *100)


View(gamma_dat_of)

gamma_dat_of$YSA <- as.numeric(gamma_dat_of$YSA)
gamma_dat_of$log_gamma_rich_p <- log(gamma_dat_of$gamma_rich_p)
gamma_dat_of$log_YSA <- log(gamma_dat_of$YSA)
gamma_dat_of$c.YSA<-gamma_dat_of$YSA-mean(gamma_dat_of$YSA)
gamma_dat_of$Field<-as.character(gamma_dat_of$Field)
gamma_dat_of$Year<-as.factor(as.character(gamma_dat_of$Year))


# ar(time = Year, gr = Field, p = 1),


p.gamma.rich<-  brm(log_gamma_rich_p ~  log_YSA  +  ( 1 + log_YSA  | Field) + (1 | Year), 
                     data = gamma_dat_of,family=student(),cores = 4, iter=3000, warmup=1000, chains = 4)


save(p.gamma.rich, file = '~/Dropbox/Projects/CCRScale/data/model_fits/percent/p.gamma.rich.Rdata')
load("~/Dropbox/Projects/CCRScale/data/model_fits/percent/p.gamma.rich.Rdata") 

summary(p.gamma.rich)



color_scheme_set("darkgray")
pp_check(p.gamma.rich)+ theme_classic() # predicted vs. observed values

gamma_dat_of$Field<-as.factor(as.character(gamma_dat_of$Field))
gamma_dat_of$Year<-as.factor(as.character(gamma_dat_of$Year))

# models residuals
mg<-residuals(p.gamma.rich)
mg<-as.data.frame(mg)
gr.plot<-cbind(gamma_dat_of,mg$Estimate)

par(mfrow=c(1,2))
with(gr.plot, plot(Field, mg$Estimate))
with(gr.plot, plot(Year, mg$Estimate))



# for plotting fixed effects
p.gamma.rich_fitted <- cbind(p.gamma.rich$data,
                       fitted(p.gamma.rich, re_formula = NA
                              )) %>% 
   as_tibble() %>%
  # join with plot data for figures
  inner_join(gamma_dat_of %>% distinct(Field, Year, log_YSA, YSA, log_gamma_rich_p, gamma_rich_p, gamma_rich),
  #by= c("Field", "Year", "log_YSA", "log_gamma_rich_p")
  )



View(p.gamma.rich_fitted)

# fixed effect coefficients
p.gamma.rich_fixef <- fixef(p.gamma.rich)



View(p.gamma.rich_fixef)

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



p.gamma.rich.fig<-ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = p.gamma.rich_fitted,
             aes(x = YSA, y = gamma_rich_p,
                 colour = Field),
             size = 1.2, shape=1) +
  # geom_line(data = p.gamma.rich_fitted,aes(x = YSA, y= gamma_rich_p,
  #                                    group = Field,
  #                                    colour = Field),
  #           size = 0.55)+
  geom_segment(data = p.gamma.rich_coef2,
               aes(x = xmin,
                   xend = xmax,
                   y = exp(Intercept + Slope * lxmin),
                   yend = exp(Intercept + Slope * lxmax),
                   group = Field,
                   colour = Field),
               size = 1.2) +
  #uncertainy in fixed effect
  geom_ribbon(data = p.gamma.rich_fitted,
              aes(x=YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = p.gamma.rich_fitted,
            aes(x=YSA, y = exp(Estimate)),
            size = 1.5) +
  # geom_ribbon(data = gamma.predict,
  #             aes(x=YSA, ymin = (pred_low), ymax = (pred_high)),
  #             alpha = 0.3) +
  # # fixed effect
  # geom_line(data = gamma.predict,
  #           aes(x=YSA, y = (pred_m)),
  #           size = 1.5) +
  scale_y_continuous( limits=c(10,105),breaks = c(25,50,100)) +
  #scale_x_continuous(trans = 'log2') +
  scale_color_manual(values = mycolors) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.position="none")  +
  labs(title = (expression(paste(italic(gamma), '-scale', sep = '')))
                     ) + ylab("Species Richness (%)")  + xlab("")


p.gamma.rich.fig




#----------------------------------------------------------------------------------------------


gamma_dat_np <- gamma_dat %>% filter(site_status == "never-plowed") %>% 
  summarise(beta_rich_p_np = mean(beta_rich))

gamma_dat_np

gamma_dat_of <- gamma_dat %>% filter(site_status == "old field") %>% 
  summarise(beta_rich_p = mean(beta_rich))

gamma_dat_of

gamma_dat_of <- gamma_dat %>% filter(site_status == "old field") 

gamma_dat_of$beta_rich_p<-(gamma_dat_of$beta_rich/4.79 *100)

View(gamma_dat_of)

gamma_dat_of$YSA <- as.numeric(gamma_dat_of$YSA)
gamma_dat_of$log_beta_rich_p <- log(gamma_dat_of$beta_rich_p)
gamma_dat_of$log_YSA <- log(gamma_dat_of$YSA)
gamma_dat_of$c.YSA<-gamma_dat_of$YSA-mean(gamma_dat_of$YSA)
gamma_dat_of$Field<-as.character(gamma_dat_of$Field)
gamma_dat_of$Year<-as.factor(as.character(gamma_dat_of$Year))


p.beta.div <-  brm(log_beta_rich_p ~  log_YSA + (1 + log_YSA | Field) + (1 | Year), 
                  data = gamma_dat_of, family=student(), cores = 4, iter=6000, warmup=1000, control =
                    list(adapt_delta = 0.99), chains = 4)

save(p.beta.div, file = '~/Dropbox/Projects/CCRScale/data/model_fits/percent/p.beta.div.Rdata')
load("~/Dropbox/Projects/CCRScale/data/model_fits/percent/p.beta.div.Rdata") 

summary(p.beta.div)




color_scheme_set("darkgray")
pp_check(p.beta.div)+ theme_classic() # predicted vs. observed values


gamma_dat_of$Field<-as.character(gamma_dat_of$Field)
gamma_dat_of$Year<-as.factor(as.character(gamma_dat_of$Year))

# models residuals
mb<-residuals(p.beta.div)
mb<-as.data.frame(mb)
br.plot<-cbind(gamma_dat_of,mb$Estimate)

par(mfrow=c(1,2))
with(br.plot, plot(Field, mb$Estimate))
with(br.plot, plot(Year, mb$Estimate))




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
             size = 1.2, shape=1) +
  # geom_line(data = p.beta.div_fitted,aes(x = YSA, y= beta_rich_p,
  #                                       group = Field,
  #                                       colour = Field),
  #            size = 0.55)+
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
  scale_y_continuous( limits=c(25,125),breaks = c(25,50,100,125)) +
  scale_color_manual(values = mycolors) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                    legend.direction = "horizontal", legend.position="bottom")  +
  labs(title = (expression(paste('', italic(beta), '-scale', sep = '')))) +
  ylab((expression(''~paste(italic(beta), '-Diversity (%)', sep = '')))) +  xlab("Years since agricultural abandonment") + guides(col = guide_legend(ncol = 13))



p.beta.div.fig


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

ysa.legend<-g_legend(p.beta.div.fig)

(p.alpha.rich.fig | p.gamma.rich.fig   | p.beta.div.fig + theme(legend.position="none"))/(ysa.legend) + plot_layout(heights = c(10,1)) 


#7.5 X 12



#SPIE
# ALPHA

head(alpha_dat)

alpha_dat_np <- alpha_dat %>% filter(site_status == "never-plowed") %>% #group_by(Field,YSA) %>%
  summarise(alpha_rich_p_np = mean(alpha_ENSPIE))

alpha_dat_np

alpha_dat_of <- alpha_dat %>% filter(site_status == "old field") %>% # group_by(Field,YSA) %>%
  summarise(alpha_rich_p = mean(alpha_ENSPIE))

alpha_dat_of

alpha_dat_of <- alpha_dat %>% filter(site_status == "old field") 

alpha_dat_of$alpha_ENSPIE_p<-(alpha_dat_of$alpha_ENSPIE/6.33 *100)

View(alpha_dat_of)

alpha_dat_of$YSA <- as.numeric(alpha_dat_of$YSA)
alpha_dat_of$log_alpha_ENSPIE_p <- log(alpha_dat_of$alpha_ENSPIE_p)
alpha_dat_of$log_YSA <- log(alpha_dat_of$YSA)
alpha_dat_of$c.YSA<-alpha_dat_of$YSA-mean(alpha_dat_of$YSA)
alpha_dat_of$Field<-as.factor(as.character(alpha_dat_of$Field))
alpha_dat_of$Year<-as.factor(as.character(alpha_dat_of$Year))



p.alpha.spie <-  brm(log_alpha_ENSPIE_p ~  log_YSA + ( 1 + log_YSA  | Field/Transect/Plot) + (1 | Year), 
                     data = alpha_dat_of, family=student(), cores = 4, iter=7000,warmup = 1000, chains = 4)



save(p.alpha.spie, file = '~/Dropbox/Projects/CCRScale/data/model_fits/percent/p.alpha.spie.Rdata')
load("~/Dropbox/Projects/CCRScale/data/model_fits/percent/p.alpha.spie.Rdata") 

summary(p.alpha.spie)


color_scheme_set("darkgray")
pp_check(p.alpha.spie)+ theme_classic() # predicted vs. observed values


# models residuals
ma<-residuals(p.alpha.spie)
ma<-as.data.frame(m1)
ar.plot<-cbind(alpha_dat_of,ma$Estimate)

par(mfrow=c(1,2))
with(ar.plot, plot(Field, ma$Estimate))
with(ar.plot, plot(Year, ma$Estimate))


alpha_dat_of$Field<-as.factor(as.character(alpha_dat_of$Field))
alpha_dat_of$Year<-as.factor(as.character(alpha_dat_of$Year))


# for plotting fixed effects
p.alpha.spie_fitted <- cbind(p.alpha.spie$data,
                             fitted(p.alpha.spie, re_formula = NA)) %>% 
  as_tibble() %>% 
  # join with plot data for figures
  inner_join(alpha_dat_of %>% distinct(Field, Year, log_YSA, YSA, log_alpha_ENSPIE_p, alpha_ENSPIE_p,alpha_ENSPIE),
             by= c("Field", "Year", "log_YSA", "log_alpha_ENSPIE_p"))


View(p.alpha.spie_fitted)

# fixed effect coefficients
p.alpha.spie_fixef <- fixef(p.alpha.spie)


p.alpha.spie_coef <- coef(p.alpha.spie)
p.alpha.spie_coef 

alpha_dat_of$Field<-as.character(alpha_dat_of$Field)


p.alpha.spie_coef2 <-  bind_cols(p.alpha.spie_coef$Field[,,'Intercept'] %>% 
                                   as_tibble() %>% 
                                   mutate(Intercept = Estimate,
                                          Intercept_lower = Q2.5,
                                          Intercept_upper = Q97.5,
                                          Field = rownames(p.alpha.spie_coef$Field[,,'Intercept'])) %>% 
                                   select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                                 p.alpha.spie_coef$Field[,,'log_YSA'] %>% 
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

p.alpha.spie_fitted$YSA<- as.numeric(p.alpha.spie_fitted$YSA)
p.alpha.spie_fitted$Field<-as.character(p.alpha.spie_fitted$Field)
p.alpha.spie_coef2$Field<-as.character(p.alpha.spie_coef2$Field)

View(alpha_dat_of)
View(p.alpha.spie_coef2)


# Define the number of colors you want
nb.cols <- 17
mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)


p.alpha.spie.fig<-ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = p.alpha.spie_fitted,
             aes(x = YSA, y = alpha_ENSPIE_p,
                 colour = Field),
             size = 1.2, shape=1, position = position_jitter(width = 0.95, height=0.95)) +
  # geom_line(data = alpha_dat_sum,aes(x = YSA, y= alpha_ENSPIE_p,
  #                                    group = Field,
  #                                    colour = Field),
  #           size = 0.55)+
  geom_segment(data = p.alpha.spie_coef2,
               aes(x = xmin,
                   xend = xmax,
                   y = exp(Intercept + Slope * lxmin),
                   yend = exp(Intercept + Slope * lxmax ),
                   group = Field,
                   colour = Field),
            size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = p.alpha.spie_fitted,
              aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = p.alpha.spie_fitted,
            aes(x = YSA, y = exp(Estimate)),
            size = 1.5) +
  scale_y_continuous( limits=c(10,210),breaks = c(25,50,100,150,200)) +
  scale_color_manual(values = mycolors) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.position="none") +
  #labs(title = (expression(paste(italic(alpha), '-scale', sep = ''))) ) +
  ylab( expression(paste(ENS[PIE])) )  + xlab("")

p.alpha.spie.fig


#gamma


#----------------------------------------------------------------------------------------------

head(gamma_dat)

gamma_dat_np <- gamma_dat %>% filter(site_status == "never-plowed") %>% 
  summarise(gamma_rspie_p_np = mean(gamma_ENSPIE))

gamma_dat_np

gamma_dat_of <- gamma_dat %>% filter(site_status == "old field") %>% 
  summarise(gamma_spie_p = mean(gamma_ENSPIE))

gamma_dat_of

gamma_dat_of <- gamma_dat %>% filter(site_status == "old field") 

gamma_dat_of$gamma_ENSPIE_p<-(gamma_dat_of$gamma_ENSPIE/121.8052 *100)


View(gamma_dat_of)

gamma_dat_of$YSA <- as.numeric(gamma_dat_of$YSA)
gamma_dat_of$log_gamma_ENSPIE_p <- log(gamma_dat_of$gamma_ENSPIE_p)
gamma_dat_of$log_YSA <- log(gamma_dat_of$YSA)
gamma_dat_of$c.YSA<-gamma_dat_of$YSA-mean(gamma_dat_of$YSA)
gamma_dat_of$Field<-as.character(gamma_dat_of$Field)
gamma_dat_of$Year<-as.factor(as.character(gamma_dat_of$Year))


# ar(time = Year, gr = Field, p = 1),


p.gamma.spie<-  brm(log_gamma_ENSPIE_p ~  log_YSA  +  ( 1 + log_YSA  | Field) + (1 | Year), 
                    data = gamma_dat_of,family=student(), cores = 4, iter=10000, warmup=1000, chains = 4)


save(p.gamma.spie, file = '~/Dropbox/Projects/CCRScale/data/model_fits/percent/p.gamma.spie.Rdata')
load("~/Dropbox/Projects/CCRScale/data/model_fits/percent/p.gamma.spie.Rdata") 

summary(p.gamma.spie)



color_scheme_set("darkgray")
pp_check(p.gamma.spie)+ theme_classic() # predicted vs. observed values

gamma_dat_of$Field<-as.factor(as.character(gamma_dat_of$Field))
gamma_dat_of$Year<-as.factor(as.character(gamma_dat_of$Year))

# models residuals
mg<-residuals(p.gamma.spie)
mg<-as.data.frame(mg)
gr.plot<-cbind(gamma_dat_of,mg$Estimate)

par(mfrow=c(1,2))
with(gr.plot, plot(Field, mg$Estimate))
with(gr.plot, plot(Year, mg$Estimate))



# for plotting fixed effects
p.gamma.spie_fitted <- cbind(p.gamma.spie$data,
                             fitted(p.gamma.spie, re_formula = NA
                             )) %>% 
  as_tibble() %>%
  # join with plot data for figures
  inner_join(gamma_dat_of %>% distinct(Field, Year, log_YSA, YSA, log_gamma_ENSPIE_p, gamma_ENSPIE_p, gamma_ENSPIE),
             #by= c("Field", "Year", "log_YSA", "log_gamma_rich_p")
  )



View(p.gamma.spie_fitted)

# fixed effect coefficients
p.gamma.spie_fixef <- fixef(p.gamma.spie)



View(p.gamma.spie_fixef)

p.gamma.spie_coef <- coef(p.gamma.spie)

p.gamma.spie_coef 


p.gamma.spie_coef2 <-  bind_cols(p.gamma.spie_coef$Field[,,'Intercept'] %>% 
                                   as_tibble() %>% 
                                   mutate(Intercept = Estimate,
                                          Intercept_lower = Q2.5,
                                          Intercept_upper = Q97.5,
                                          Field = rownames(p.gamma.spie_coef$Field[,,'Intercept'])) %>% 
                                   select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                                 p.gamma.spie_coef$Field[,,'log_YSA'] %>% 
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
  summarise(gamma_ENSPIE_p = mean(gamma_ENSPIE_p),
            gamma_ENSPIE = mean(gamma_ENSPIE),
            xmin = min(YSA),
            xmax = max(YSA))

View(gamma_dat_sum)

p.gamma.spie_fitted$YSA<- as.numeric(p.gamma.spie_fitted$YSA)
p.gamma.spie_fitted$Field<-as.character(p.gamma.spie_fitted$Field)
p.gamma.spie_coef2$Field<-as.character(p.gamma.spie_coef2$Field)



p.gamma.spie.fig<-ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = p.gamma.spie_fitted,
             aes(x = YSA, y = gamma_ENSPIE_p,
                 colour = Field),
             size = 1.2, shape=1) +
  # geom_line(data = p.gamma.spie_fitted,aes(x = YSA, y= gamma_ENSPIE_p,
  #                                    group = Field,
  #                                    colour = Field),
  #           size = 0.55)+
  geom_segment(data = p.gamma.spie_coef2,
               aes(x = xmin,
                   xend = xmax,
                   y = exp(Intercept + Slope * lxmin),
                   yend = exp(Intercept + Slope * lxmax),
                   group = Field,
                   colour = Field),
               size = 1.2) +
  #uncertainy in fixed effect
  geom_ribbon(data = p.gamma.spie_fitted,
              aes(x=YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = p.gamma.spie_fitted,
            aes(x=YSA, y = exp(Estimate)),
            size = 1.5) +
  # geom_ribbon(data = gamma.predict,
  #             aes(x=YSA, ymin = (pred_low), ymax = (pred_high)),
  #             alpha = 0.3) +
  # # fixed effect
  # geom_line(data = gamma.predict,
  #           aes(x=YSA, y = (pred_m)),
  #           size = 1.5) +
  scale_y_continuous( limits=c(10,105),breaks = c(25,50,100)) +
  #scale_x_continuous(trans = 'log2') +
  scale_color_manual(values = mycolors) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.position="none")  +
  #labs(title = (expression(paste(italic(gamma), '-scale', sep = '')))) +
  ylab(expression(ENS[PIE]))  + xlab("")


p.gamma.spie.fig



#----------------------------------------------------------------------------------------------

gamma_dat_np <- gamma_dat %>% filter(site_status == "never-plowed") %>% 
  summarise(beta_rich_p_np = mean(beta_ENSPIE))

gamma_dat_np

gamma_dat_of <- gamma_dat %>% filter(site_status == "old field") %>% 
  summarise(beta_rich_p = mean(beta_ENSPIE))

gamma_dat_of

gamma_dat_of <- gamma_dat %>% filter(site_status == "old field") 

gamma_dat_of$beta_ENSPIE_p<-(gamma_dat_of$beta_ENSPIE/19.24 *100)

View(gamma_dat_of)

gamma_dat_of$YSA <- as.numeric(gamma_dat_of$YSA)
gamma_dat_of$log_beta_ENSPIE_p <- log(gamma_dat_of$beta_ENSPIE_p)
gamma_dat_of$log_YSA <- log(gamma_dat_of$YSA)
gamma_dat_of$c.YSA<-gamma_dat_of$YSA-mean(gamma_dat_of$YSA)
gamma_dat_of$Field<-as.character(gamma_dat_of$Field)
gamma_dat_of$Year<-as.factor(as.character(gamma_dat_of$Year))


p.beta.spie <-  brm(log_beta_ENSPIE_p ~  log_YSA + (1 + log_YSA | Field) + (1 | Year), 
                   data = gamma_dat_of, family=student(),cores = 4, iter=6000,warmup=1000, chains = 4)

save(p.beta.spie, file = '~/Dropbox/Projects/CCRScale/data/model_fits/percent/p.beta.spie.Rdata')
load("~/Dropbox/Projects/CCRScale/data/model_fits/percent/p.beta.spie.Rdata") 

summary(p.beta.spie)




color_scheme_set("darkgray")
pp_check(p.beta.spie)+ theme_classic() # predicted vs. observed values


gamma_dat_of$Field<-as.character(gamma_dat_of$Field)
gamma_dat_of$Year<-as.factor(as.character(gamma_dat_of$Year))

# models residuals
mb<-residuals(p.beta.spie)
mb<-as.data.frame(mb)
br.plot<-cbind(gamma_dat_of,mb$Estimate)

par(mfrow=c(1,2))
with(br.plot, plot(Field, mb$Estimate))
with(br.plot, plot(Year, mb$Estimate))




# for plotting fixed effects
p.beta.spie_fitted <- cbind(p.beta.spie$data,
                           fitted(p.beta.spie, re_formula = NA)) %>% 
  as_tibble() %>% 
  # join with plot data for figures
  inner_join(gamma_dat_of %>% distinct(Field, Year, log_YSA, YSA, log_beta_ENSPIE_p, beta_ENSPIE_p, beta_ENSPIE),
             by= c("Field", "Year", "log_YSA", "log_beta_ENSPIE_p"))


View(p.beta.spie_fitted)

# fixed effect coefficients
p.beta.spie_fixef <- fixef(p.beta.spie)


p.beta.spie_coef <- coef(p.beta.spie)
p.beta.spie_coef 

gamma_dat_of$Field<-as.character(gamma_dat_of$Field)


p.beta.spie_coef2 <-  bind_cols(p.beta.spie_coef$Field[,,'Intercept'] %>% 
                                 as_tibble() %>% 
                                 mutate(Intercept = Estimate,
                                        Intercept_lower = Q2.5,
                                        Intercept_upper = Q97.5,
                                        Field = rownames(p.beta.spie_coef$Field[,,'Intercept'])) %>% 
                                 select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                               p.beta.spie_coef$Field[,,'log_YSA'] %>% 
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
  summarise(beta_ENSPIE_p = mean(beta_ENSPIE_p),
            beta_ENSPIE = mean(beta_ENSPIE),
            xmin = min(YSA),
            xmax = max(YSA),
            lxmin = min(log_YSA),
            lxmax = max(log_YSA))

View(beta_dat_sum)

p.beta.spie_fitted$YSA<- as.numeric(p.beta.spie_fitted$YSA)
p.beta.spie_fitted$Field<-as.character(p.beta.spie_fitted$Field)
p.beta.spie_coef2$Field<-as.character(p.beta.spie_coef2$Field)


View(p.beta.spie_coef2)

View(p.beta.spie_fitted)

p.beta.spie.fig<-ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = p.beta.spie_fitted,
             aes(x = YSA, y = beta_ENSPIE_p,
                 colour = Field),
             size = 1.2, shape=1) +
  # geom_line(data = p.beta.spie_fitted,aes(x = YSA, y= beta_ENSPIE_p,
  #                                        group = Field,
  #                                        colour = Field),
  #           size = 0.55)+
  geom_segment(data = p.beta.spie_coef2,
               aes(x = xmin,
                   xend = xmax,
                   y = exp(Intercept + Slope * lxmin),
                   yend = exp(Intercept + Slope * lxmax),
                   group = Field,
                   colour = Field),
               size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = p.beta.spie_fitted,
              aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = p.beta.spie_fitted,
            aes(x = YSA, y = exp(Estimate)),
            size = 1.5) +
  scale_y_continuous( limits=c(25,125),breaks = c(25,50,100,125)) +
  scale_color_manual(values = mycolors) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.direction = "horizontal", legend.position="bottom")  +
  #labs(title = (expression(paste('', italic(beta), '-scale', sep = '')))) +
  ylab((expression(paste(italic(beta), -ENS[PIE], sep = ' '))))  +  xlab("Years since agricultural abandonment") + guides(col = guide_legend(ncol = 13))



p.beta.spie.fig


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

ysa.legend<-g_legend(p.beta.spie.fig)

(p.alpha.spie.fig | p.gamma.spie.fig   | p.beta.spie.fig + theme(legend.position="none"))/(ysa.legend) + plot_layout(heights = c(10,1)) 

(p.alpha.rich.fig| p.alpha.spie.fig)/ (p.gamma.rich.fig | p.gamma.spie.fig)/( p.beta.div.fig + theme(legend.position="none") | p.beta.spie.fig + theme(legend.position="none"))/(ysa.legend) + plot_layout(heights = c(10,10,10,2)) 





