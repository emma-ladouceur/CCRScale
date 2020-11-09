

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
alpha_dat_of$log_alpha_rich_p <- log(alpha_dat_of$alpha_rich_p)
alpha_dat_of$log_YSA <- log(alpha_dat_of$YSA)
alpha_dat_of$c.YSA<-alpha_dat_of$YSA-mean(alpha_dat_of$YSA)



plot.rich <-  brm(log_alpha_rich_p ~  log_YSA + ( log_YSA | Field/Transect/Plot/Year), 
                    data = alpha_dat_of, cores = 4, iter=2000, chains = 4)


summary(plot.rich)


color_scheme_set("darkgray")
pp_check(plot.rich)+ theme_classic() # predicted vs. observed values



alpha_dat_of$Field<-as.numeric(alpha_dat_of$Field)


# for plotting fixed effects
plot.rich_fitted <- cbind(plot.rich$data,
                          fitted(plot.rich, re_formula = NA)) %>% 
  as_tibble() %>% 
  # join with plot data for figures
  inner_join(alpha_dat_of %>% distinct(Field, Year, log_YSA, YSA, log_alpha_rich_p, alpha_rich_p,alpha_rich),
             by= c("Field", "Year", "log_YSA", "log_alpha_rich_p"))


View(plot.rich_fitted)

# fixed effect coefficients
plot.rich_fixef <- fixef(plot.rich)


plot.rich_coef <- coef(plot.rich)
plot.rich_coef 

alpha_dat_of$Field<-as.character(alpha_dat_of$Field)


plot.rich_coef2 <-  bind_cols(plot.rich_coef$Field[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       Field = rownames(plot.rich_coef$Field[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              plot.rich_coef$Field[,,'log_YSA'] %>% 
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
                         #cxmin = min(c.YSA),
                         # cxmax = max(c.YSA)
               ),
             by = 'Field')


 alpha_dat_sum   <- alpha_dat_of %>% 
                      group_by(Field,YSA) %>% 
                        summarise(alpha_rich_p = mean(alpha_rich_p),
                          alpha_rich = mean(alpha_rich),
                            xmin = min(YSA),
                                xmax = max(YSA))

View(alpha_dat_sum)

plot.rich_fitted$YSA<- as.numeric(plot.rich_fitted$YSA)
plot.rich_fitted$Field<-as.character(plot.rich_fitted$Field)
plot.rich_coef2$Field<-as.character(plot.rich_coef2$Field)


View(plot.rich_coef2)

plot.rich.fig<-ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = plot.rich_fitted,
             aes(x = YSA, y = alpha_rich_p,
                 colour = Field),
             size = 1.2, shape=1,position = position_jitter(width = 0.95, height = 0.95)) +
  geom_line(data = alpha_dat_sum,aes(x = YSA, y= alpha_rich_p,
                     group = Field,
                     colour = Field),
                     size = 0.75)+
  # geom_segment(data = plot.rich_coef2,
  #              aes(x = xmin, 
  #                  xend = xmax,
  #                  y = exp(Intercept + Slope),
  #                  yend = exp(Intercept + Slope ),
  #                  group = Field,
  #                  colour = Field),
  #              size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = plot.rich_fitted,
              aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = plot.rich_fitted,
            aes(x = YSA, y = exp(Estimate)),
            size = 1.5) +
   scale_y_continuous(trans = 'log2', breaks = c(25,50,100,150)) +
  #scale_colour_manual(values = c( "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", "#0C5BB0FF", "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#FA6B09FF","#A1C720FF","#9A703EFF" ))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.position="none") +
  labs(title = (expression(paste(italic(alpha), '-scale', sep = ''))) ) +
 ylab("log(Species Richness) (%)")  + xlab("")

plot.rich.fig



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

site.rich <-  brm(log_gamma_rich_p ~  log_YSA + (log_YSA | Field/Year), 
                  data = gamma_dat_of,cores = 4, iter=2000, chains = 4)


summary(site.rich)

color_scheme_set("darkgray")
pp_check(site.rich)+ theme_classic() # predicted vs. observed values

# models residuals
m1<-residuals(site.rich)
m1<-as.data.frame(m1)
rr.plot<-cbind(gamma_dat_of,m1$Estimate)

par(mfrow=c(1,2))
with(rr.plot, plot(Field, m1$Estimate))
with(rr.plot, plot(Year, m1$Estimate))




# for plotting fixed effects
site.rich_fitted <- cbind(site.rich$data,
                       fitted(site.rich, re_formula = NA)) %>% 
  as_tibble() %>% 
  # join with plot data for figures
  inner_join(gamma_dat_of %>% distinct(Field, Year, log_YSA, YSA, log_gamma_rich_p, gamma_rich_p, gamma_rich),
  by= c("Field", "Year", "log_YSA", "log_gamma_rich_p"))


View(site.rich_fitted)

# fixed effect coefficients
site.rich_fixef <- fixef(site.rich)


site.rich_coef <- coef(site.rich)
site.rich_coef 

site.rich_coef2 <-  bind_cols(site.rich_coef$Field[,,'Intercept'] %>% 
                                 as_tibble() %>% 
                                 mutate(Intercept = Estimate,
                                        Intercept_lower = Q2.5,
                                        Intercept_upper = Q97.5,
                                        Field = rownames(site.rich_coef$Field[,,'Intercept'])) %>% 
                                 select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              site.rich_coef$Field[,,'log_YSA'] %>% 
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
                         #cxmin = min(c.YSA),
                        # cxmax = max(c.YSA)
                        ),
             by = 'Field')


gamma_dat_sum   <- gamma_dat_of %>% 
  group_by(Field,YSA) %>% 
  summarise(gamma_rich_p = mean(gamma_rich_p),
            gamma_rich = mean(gamma_rich),
            xmin = min(YSA),
            xmax = max(YSA))

View(gamma_dat_sum)

site.rich_fitted$YSA<- as.numeric(site.rich_fitted$YSA)
site.rich_fitted$Field<-as.character(site.rich_fitted$Field)
site.rich_coef2$Field<-as.character(site.rich_coef2$Field)


View(site.rich_coef2)

site.rich.fig<-ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = site.rich_fitted,
             aes(x = YSA, y = gamma_rich_p,
                 colour = Field),
             size = 1.2, shape=1,position = position_jitter(width = 0.95, height = 0.95)) +
  geom_line(data = site.rich_fitted,aes(x = YSA, y= gamma_rich_p,
                                     group = Field,
                                     colour = Field),
            size = 0.75)+
  # geom_segment(data = plot.rich_coef2,
  #              aes(x = xmin, 
  #                  xend = xmax,
  #                  y = exp(Intercept + Slope),
  #                  yend = exp(Intercept + Slope ),
  #                  group = Field,
  #                  colour = Field),
  #              size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = site.rich_fitted,
              aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = site.rich_fitted,
            aes(x = YSA, y = exp(Estimate)),
            size = 1.5) +
  scale_y_continuous(trans = 'log2', breaks = c(25,50,100,150)) +
  #scale_colour_manual(values = c( "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", "#0C5BB0FF", "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#FA6B09FF","#A1C720FF","#9A703EFF" ))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.position="none")  +
  labs(title = (expression(paste(italic(gamma), '-scale', sep = '')))
                     ) + ylab("log(Species Richness) (%)")  + xlab("")


site.rich.fig




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


beta.rich <-  brm(log_beta_rich_p ~  log_YSA + (log_YSA | Field/Year), 
                  data = gamma_dat_of,cores = 4, iter=2000, chains = 4)


summary(beta.rich)




color_scheme_set("darkgray")
pp_check(site.rich)+ theme_classic() # predicted vs. observed values

# models residuals
m1<-residuals(beta.rich)
m1<-as.data.frame(m1)
rr.plot<-cbind(gamma_dat_of,m1$Estimate)

par(mfrow=c(1,2))
with(rr.plot, plot(Field, m1$Estimate))
with(rr.plot, plot(Year, m1$Estimate))




# for plotting fixed effects
beta.rich_fitted <- cbind(beta.rich$data,
                          fitted(beta.rich, re_formula = NA)) %>% 
  as_tibble() %>% 
  # join with plot data for figures
  inner_join(gamma_dat_of %>% distinct(Field, Year, log_YSA, YSA, log_beta_rich_p, beta_rich_p, beta_rich),
             by= c("Field", "Year", "log_YSA", "log_beta_rich_p"))


View(beta.rich_fitted)

# fixed effect coefficients
beta.rich_fixef <- fixef(beta.rich)


beta.rich_coef <- coef(beta.rich)
beta.rich_coef 

gamma_dat_of$Field<-as.character(gamma_dat_of$Field)


beta.rich_coef2 <-  bind_cols(beta.rich_coef$Field[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       Field = rownames(beta.rich_coef$Field[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             beta.rich_coef$Field[,,'YSA'] %>% 
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
                         #cxmin = min(c.YSA),
                         # cxmax = max(c.YSA)
               ),
             by = 'Field')


beta_dat_sum   <- gamma_dat_of %>% 
  group_by(Field,YSA) %>% 
  summarise(betaa_rich_p = mean(beta_rich_p),
            beta_rich = mean(gamma_rich),
            xmin = min(YSA),
            xmax = max(YSA))

View(beta_dat_sum)

beta.rich_fitted$YSA<- as.numeric(beta.rich_fitted$YSA)
beta.rich_fitted$Field<-as.character(beta.rich_fitted$Field)
beta.rich_coef2$Field<-as.character(beta.rich_coef2$Field)


View(beta.rich_coef2)

View(beta.rich_fitted)

beta.rich.fig<-ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = beta.rich_fitted,
             aes(x = YSA, y = beta_rich_p,
                 colour = Field),
             size = 1.2, shape=1,position = position_jitter(width = 0.95, height = 0.95)) +
  geom_line(data = beta.rich_fitted,aes(x = YSA, y= beta_rich_p,
                                        group = Field,
                                        colour = Field),
             size = 0.75)+
  # geom_segment(data = beta.rich_coef2,
  #              aes(x = xmin,
  #                  xend = xmax,
  #                  y = (Intercept + Slope * xmin),
  #                  yend = (Intercept + Slope * xmax),
  #                  group = Field,
  #                  colour = Field),
  #              size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = beta.rich_fitted,
              aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = beta.rich_fitted,
            aes(x = YSA, y = exp(Estimate)),
            size = 1.5) +
  scale_y_continuous(trans = 'log2', breaks = c(25,50,100,150)) +
  #scale_colour_manual(values = c( "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", "#0C5BB0FF", "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#FA6B09FF","#A1C720FF","#9A703EFF" ))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                    legend.direction = "horizontal", legend.position="bottom")  +
  labs(title = (expression(paste('', italic(beta), '-scale', sep = '')))) +
  ylab((expression('log('~paste(italic(beta), '-Diversity) (%)', sep = '')))) +  xlab("Years since agricultural abandonment") + guides(col = guide_legend(ncol = 13))



beta.rich.fig


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

ysa.legend<-g_legend(beta.rich.fig)

(plot.rich.fig |  beta.rich.fig + theme(legend.position="none") | site.rich.fig )/(ysa.legend) + plot_layout(heights = c(10,1)) 
