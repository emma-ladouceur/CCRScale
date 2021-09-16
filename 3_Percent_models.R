

library(tidyverse)
library(ggplot2)
library(brms)
library(bayesplot)
library(patchwork)
library(RColorBrewer)
library(tidybayes)
library(ggdist)
library(modelr)
library(viridis)

alpha_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/alpha_div_percent.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
gamma_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/gamma_div_percent.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


head(alpha_dat)

# SPIE = mobr
# ENSPIE = vegan - inverse Simpson's

head(alpha_dat)
head(gamma_dat)
head(ccr_dat)


# p.alpha.rich.s <-  brm(log_alpha_rich_p ~  log_YSA + ( 1 + log_YSA  | Field/Transect/Plot) + (1 | Year),
#                        data = alpha_dat_of, family=student(), cores = 4, iter = 10000, warmup = 1000, control =
#                          list(adapt_delta = 0.99), chains = 4)

#save(p.alpha.rich.s, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/p.alpha.rich.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/alpha_rich_c.Rdata") 


summary(p.alpha.rich)


color_scheme_set("darkgray")
fig_s3c <- pp_check(p.alpha.rich)+ xlab( "Species richness (%)") + ylab("Density") +
  labs(title= "c)")+
  theme_classic()+  theme(legend.position= "bottom") # predicted vs. observed values

fig_s3c

alpha_dat_of$Field<-as.factor(as.character(alpha_dat_of$Field))
alpha_dat_of$Year<-as.factor(as.character(alpha_dat_of$Year))

# models residuals
ma<-residuals(p.alpha.rich.s)
ma<-as.data.frame(ma)
ar.plot<-cbind(alpha_dat_of,ma$Estimate)

par(mfrow=c(1,2))
with(ar.plot, plot(Field, ma$Estimate))
with(ar.plot, plot(Year, ma$Estimate))

alpha_dat_of$Field<-as.character(as.factor(alpha_dat_of$Field))

# for plotting fixed effects
p.alpha.rich_fitted <- cbind(p.alpha.rich$data,
                          fitted(p.alpha.rich, re_formula = NA
                                 )) %>% 
  as_tibble() %>% inner_join(alpha_dat %>% distinct(Field, Year, log_YSA, YSA, log_alpha_rich_p, alpha_rich_p, alpha_rich),
             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
             )


head(p.alpha.rich_fitted)


# fixed effect coefficients
p.alpha.rich_fixef <- fixef(p.alpha.rich)


head(p.alpha.rich_fixef)

p.alpha.rich_coef <- coef(p.alpha.rich)
p.alpha.rich_coef 

alpha_dat$Field<-as.character(alpha_dat$Field)

# predict estimates for each field across a sequence of log_YSA's and YSA's
obs_nest.alpha <- alpha_dat %>% 
  mutate(Field_group = Field) %>%
  group_by(Field_group, Field) %>% 
  summarise(log_YSA = seq(min(log_YSA), max(log_YSA), length.out = 6 ),
         YSA = seq(min(YSA), max(YSA), length.out = 6)) %>%
  nest(data = c(Field,YSA,log_YSA)) %>%
  mutate(predicted = map(data, ~predict(p.alpha.rich, newdata= .x, re_formula = ~(1 + log_YSA | Field) ))) 

alpha_dat$Field<- as.factor(as.character(alpha_dat$Field))

# linear/lazy version version of site-level coefs
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
  inner_join(alpha_dat %>% 
               group_by(Field) %>% 
               summarise(xmin = min(YSA),
                         xmax = max(YSA),
                         lxmin = min(log_YSA),
                         lxmax = max(log_YSA)
               ),
             by = 'Field')




p.alpha.rich_fitted$YSA<- as.numeric(p.alpha.rich_fitted$YSA)
p.alpha.rich_fitted$Field<-as.character(p.alpha.rich_fitted$Field)
p.alpha.rich_coef2$Field<-as.character(p.alpha.rich_coef2$Field)

head(p.alpha.rich_fitted)
head(p.alpha.rich_coef2)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/')
# save data objects to avoid time of compiling every time
save(p.alpha.rich_fitted,p.alpha.rich_fixef,p.alpha.rich_coef,p.alpha.rich_coef2, obs_nest.alpha, file = 'a.rich.mod_dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/a.rich.mod_dat.Rdata')


fig_1c <-ggplot() + 
 # facet_grid(~Field, scales="free") +
  geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = p.alpha.rich_fitted %>% mutate( `Old field` = fct_recode( Field,  "A" = "601",
                                                                           "B" = "600",
                                                                           "C" = "10",
                                                                           "D" = "28",
                                                                           "E" = "41",
                                                                           "F" = "39",
                                                                           "G" = "40",
                                                                           "H" = "4",
                                                                           "I" = "44",
                                                                           "J" = "53",
                                                                           "K" = "47",
                                                                           "L" = "21",
                                                                           "M" = "70",
                                                                           "N" = "5",
                                                                           "O" = "27",
                                                                           "P" = "45",
                                                                           "Q" = "32",
                                                                           "R" = "35",
                                                                           "S" = "72"
  )) %>% 
    mutate( `Old field` = as.character(`Old field`)) %>%
    arrange(`Old field`)
  ,
             aes(x = YSA, y = alpha_rich_p,
                 colour = `Old field`),
             size = 1.2, shape=1, position = position_jitter(width = 2, height=2.5)) +
  geom_line(data = obs_nest.alpha  %>% unnest(cols = c(data, predicted)) %>% mutate( `Old field` = fct_recode( Field,   "A" = "601",
                                                                                                               "B" = "600",
                                                                                                               "C" = "10",
                                                                                                               "D" = "28",
                                                                                                               "E" = "41",
                                                                                                               "F" = "39",
                                                                                                               "G" = "40",
                                                                                                               "H" = "4",
                                                                                                               "I" = "44",
                                                                                                               "J" = "53",
                                                                                                               "K" = "47",
                                                                                                               "L" = "21",
                                                                                                               "M" = "70",
                                                                                                               "N" = "5",
                                                                                                               "O" = "27",
                                                                                                               "P" = "45",
                                                                                                               "Q" = "32",
                                                                                                               "R" = "35",
                                                                                                               "S" = "72")) %>%  
              mutate( `Old field` = as.character(`Old field`)) %>%
              arrange(`Old field`), aes(x = YSA, y= exp(predicted[,1]) ,
                     group = Field,
                     colour = `Old field`),
                     size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = p.alpha.rich_fitted,
              aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = p.alpha.rich_fitted,
            aes(x = YSA, y = exp(Estimate)),
            size = 1.5) +
   scale_y_continuous( limits=c(10,210),breaks = c(25,50,75,100,150,200)) +
  scale_color_viridis(discrete = T, option="D")  + 
    theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.position="none") +
  labs(subtitle= 'c)'
  ) +
 ylab("Species Richness (%)")  + xlab("Years since agricultural abandonment")

fig_1c



#----------------------------------------------------------------------------------------------


# p.gamma.rich<-  brm(log_gamma_rich_p ~  log_YSA  +  ( 1 + log_YSA  | Field) + (1 | Year), 
#                      data = gamma_dat_of,family=student(), cores = 4, iter=3000, warmup=1000, chains = 4)
# 
# 
# save(p.gamma.rich, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/p.gamma.rich.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/gamma_rich_c.Rdata") 


summary(p.gamma.rich)



color_scheme_set("darkgray")
fig_s3d <- pp_check(p.gamma.rich)+ xlab( "Species richness (%)") + ylab("") +
  labs(title= "d)")+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values
fig_s3d


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
  inner_join(gamma_dat %>% distinct(Field, Year, log_YSA, YSA, log_gamma_rich_p, gamma_rich_p, gamma_rich),
  #by= c("Field", "Year", "log_YSA", "log_gamma_rich_p")
  )



head(p.gamma.rich_fitted)

# fixed effect coefficients
p.gamma.rich_fixef <- fixef(p.gamma.rich)



head(p.gamma.rich_fixef)

p.gamma.rich_coef <- coef(p.gamma.rich)

p.gamma.rich_coef 

gamma_dat$Field<- as.character(gamma_dat$Field)
# predict estimates for each field across a sequence of log_YSA's and YSA's
obs_nest.gamma <- gamma_dat %>% 
  mutate(Field_group = Field) %>%
  group_by(Field_group, Field) %>% 
  summarise(log_YSA = seq(min(log_YSA), max(log_YSA), length.out = 6 ),
            YSA = seq(min(YSA), max(YSA), length.out = 6)) %>%
  nest(data = c(Field,YSA,log_YSA)) %>%
  mutate(predicted = map(data, ~predict(p.gamma.rich, newdata= .x, re_formula = ~(1 + log_YSA | Field) ))) 


# coefs
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
  inner_join(gamma_dat %>% 
               group_by(Field) %>% 
               summarise(xmin = min(YSA),
                         xmax = max(YSA),
                         lxmin = min(log_YSA),
                         lxmax = max(log_YSA)
               ),
             by = 'Field')




setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/')
save(p.gamma.rich_fitted,p.gamma.rich_fixef,p.gamma.rich_coef,p.gamma.rich_coef2,obs_nest.gamma, file = 'g.rich.mod_dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/g.rich.mod_dat.Rdata')

p.gamma.rich_fitted$YSA<- as.numeric(p.gamma.rich_fitted$YSA)
p.gamma.rich_fitted$Field<-as.character(p.gamma.rich_fitted$Field)
p.gamma.rich_coef2$Field<-as.character(p.gamma.rich_coef2$Field)



fig_1d <- ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = p.gamma.rich_fitted %>%  mutate( `Old field` = fct_recode( Field,  "A" = "601",
                                                                               "B" = "600",
                                                                               "C" = "10",
                                                                               "D" = "28",
                                                                               "E" = "41",
                                                                               "F" = "39",
                                                                               "G" = "40",
                                                                               "H" = "4",
                                                                               "I" = "44",
                                                                               "J" = "53",
                                                                               "K" = "47",
                                                                               "L" = "21",
                                                                               "M" = "70",
                                                                               "N" = "5",
                                                                               "O" = "27",
                                                                               "P" = "45",
                                                                               "Q" = "32",
                                                                               "R" = "35",
                                                                               "S" = "72"
  )) %>% 
    mutate( `Old field` = as.character(`Old field`)) %>%
    arrange(`Old field`)
  ,
             aes(x = YSA, y = gamma_rich_p,
                 colour = `Old field`),
             size = 1.2, shape=1) +
  geom_line(data = obs_nest.gamma %>% unnest(cols = c(data, predicted)) %>% mutate( `Old field` = fct_recode( Field,   "A" = "601",
                                                                                                                                                   "B" = "600",
                                                                                                                                                   "C" = "10",
                                                                                                                                                   "D" = "28",
                                                                                                                                                   "E" = "41",
                                                                                                                                                   "F" = "39",
                                                                                                                                                   "G" = "40",
                                                                                                                                                   "H" = "4",
                                                                                                                                                   "I" = "44",
                                                                                                                                                   "J" = "53",
                                                                                                                                                   "K" = "47",
                                                                                                                                                   "L" = "21",
                                                                                                                                                   "M" = "70",
                                                                                                                                                   "N" = "5",
                                                                                                                                                   "O" = "27",
                                                                                                                                                   "P" = "45",
                                                                                                                                                   "Q" = "32",
                                                                                                                                                   "R" = "35",
                                                                                                                                                   "S" = "72")) %>%  
              mutate( `Old field` = as.character(`Old field`)) %>%
              arrange(`Old field`), aes(x = YSA, y= exp(predicted[,1]) ,
                 group = Field,
                 colour = `Old field`),
            size = 1.2) +
  #uncertainy in fixed effect
  geom_ribbon(data = p.gamma.rich_fitted,
              aes(x=YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = p.gamma.rich_fitted,
            aes(x=YSA, y = exp(Estimate)),
            size = 1.5) +
  scale_y_continuous( limits=c(10,105),breaks = c(25,50,65,75,100)) +
  scale_color_viridis(discrete = T, option="D")  + 
    theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.position="none")  +
  labs(#title = (expression(paste(italic(gamma), '-scale', sep = '')))
       subtitle= 'd)' ) + ylab("Species Richness (%)")  + xlab("Years since agricultural abandonment")



fig_1d




 #----------------------------------------------------------------------------------------------

# 

# p.beta.div <-  brm(log_beta_rich_p ~  log_YSA + (1 + log_YSA | Field) + (1 | Year), 
#                   data = gamma_dat_of, family=student(), cores = 4, iter=6000, warmup=1000, control =
#                     list(adapt_delta = 0.99), chains = 4)

#save(p.beta.div, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/p.beta.div.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/beta_div_c.Rdata") 


summary(p.beta.div)

color_scheme_set("darkgray")
fig_s5b <- pp_check(p.beta.div)+  xlab((expression(paste(italic(beta), '-Diversity (%)', sep = '')))) +  ylab("") +
  labs(title= "b)")+
  theme_classic() +  theme(legend.position= "bottom")# predicted vs. observed values
fig_s5b

gamma_dat_of$Field<-as.character(gamma_dat_of$Field)
gamma_dat_of$Year<-as.factor(as.character(gamma_dat_of$Year))

# models residuals
mb<-residuals(p.beta.div)
mb<-as.data.frame(mb)
br.plot<-cbind(gamma_dat_of,mb$Estimate)

par(mfrow=c(1,2))
with(br.plot, plot(Field, mb$Estimate))
with(br.plot, plot(Year, mb$Estimate))



colnames(gamma_dat)

gamma_dat$Field<-as.numeric(gamma_dat$Field)
# for plotting fixed effects
p.beta.div_fitted <- cbind(p.beta.div$data,
                          fitted(p.beta.div, re_formula = NA)) %>% 
  as_tibble() %>% 
  # join with plot data for figures
  inner_join(gamma_dat %>% distinct(Field, Year, log_YSA, YSA, log_beta_div_p, beta_div_p, beta_div),
             by= c("Field", "Year", "log_YSA", "log_beta_div_p"))


head(p.beta.div_fitted)

# fixed effect coefficients
p.beta.div_fixef <- fixef(p.beta.div)


p.beta.div_coef <- coef(p.beta.div)
p.beta.div_coef 

gamma_dat$Field<-as.character(gamma_dat$Field)

# predict estimates for each field across a sequence of log_YSA's and YSA's
obs_nest.beta.div <- gamma_dat %>% 
  mutate(Field_group = Field) %>%
  group_by(Field_group, Field) %>% 
  summarise(log_YSA = seq(min(log_YSA), max(log_YSA), length.out = 6 ),
            YSA = seq(min(YSA), max(YSA), length.out = 6)) %>%
  nest(data = c(Field,YSA,log_YSA)) %>%
  mutate(predicted = map(data, ~predict(p.beta.div, newdata= .x, re_formula = ~(1 + log_YSA | Field) ))) 


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
  inner_join(gamma_dat %>% 
               group_by(Field) %>% 
               summarise(xmin = min(YSA),
                         xmax = max(YSA),
                         lxmin = min(log_YSA),
                         lxmax = max(log_YSA)
               ),
             by = 'Field')



p.beta.div_fitted$YSA<- as.numeric(p.beta.div_fitted$YSA)
p.beta.div_fitted$Field<-as.character(p.beta.div_fitted$Field)
p.beta.div_coef2$Field<-as.character(p.beta.div_coef2$Field)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/')
# avoid running above code everytime
save(p.beta.div_fitted,p.beta.div_fixef,p.beta.div_coef,p.beta.div_coef2,obs_nest.beta.div, file = 'b.div.mod_dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/b.div.mod_dat.Rdata')



head(p.beta.div_coef2)

head(p.beta.div_fitted)

p.beta.div_fitted$Field<-as.factor(p.beta.div_fitted$Field)
levels(p.beta.div_fitted$Field)

head(p.beta.div_fitted)

p.beta.div_fitted %>% distinct(Field, YSA)

ysa_range <- p.beta.div_fitted %>% group_by(Field) %>%
  summarise(min_YSA = min(YSA),
            max_YSA = max(YSA)) %>%
  unite("range_YSA",min_YSA:max_YSA, sep="-", remove = F ) %>%
  mutate(min_YSA = as.numeric(min_YSA),
         max_YSA = as.numeric(max_YSA))


head(ysa_range)

field_ysa <- p.beta.div_fitted %>% left_join(ysa_range) %>% arrange(min_YSA, max_YSA) %>%
  mutate(Field =  as.factor(Field)) %>% distinct(Field,min_YSA, max_YSA)

levels(p.beta.div_fitted_t$Field)

write_csv(field_ysa, "~/Desktop/field_ysa.csv")

# rename fields to be letters instead of numbers- because numbers get confusing when you're looking at YSA & Field #'s
p.beta.div_fitted2<-p.beta.div_fitted %>% left_join(ysa_range) %>%
  mutate( `Old field` = fct_recode( Field,  "A" = "601",
                                                      "B" = "600",
                                                      "C" = "10",
                                                      "D" = "28",
                                                      "E" = "41",
                                                      "F" = "39",
                                                      "G" = "40",
                                                      "H" = "4",
                                                      "I" = "44",
                                                      "J" = "53",
                                                      "K" = "47",
                                                      "L" = "21",
                                                      "M" = "70",
                                                      "N" = "5",
                                                      "O" = "27",
                                                      "P" = "45",
                                                      "Q" = "32",
                                                      "R" = "35",
                                                      "S" = "72"
                                                      )) %>% 
  mutate( `Old field` = as.character(`Old field`)) %>%
  arrange(`Old field`)


fig_3b <- ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = p.beta.div_fitted2,
             aes(x = YSA, y = beta_div_p,
                 colour = `Old field`),
             size = 1.2, shape=1) +
  geom_line(data = obs_nest.beta.div %>% unnest(cols = c(data, predicted))%>% mutate( `Old field` = fct_recode( Field,   "A" = "601",
                                                                                                                "B" = "600",
                                                                                                                "C" = "10",
                                                                                                                "D" = "28",
                                                                                                                "E" = "41",
                                                                                                                "F" = "39",
                                                                                                                "G" = "40",
                                                                                                                "H" = "4",
                                                                                                                "I" = "44",
                                                                                                                "J" = "53",
                                                                                                                "K" = "47",
                                                                                                                "L" = "21",
                                                                                                                "M" = "70",
                                                                                                                "N" = "5",
                                                                                                                "O" = "27",
                                                                                                                "P" = "45",
                                                                                                                "Q" = "32",
                                                                                                                "R" = "35",
                                                                                                                "S" = "72")) %>%  
              mutate( `Old field` = as.character(`Old field`)) %>%
              arrange(`Old field`),
            aes(x = YSA, y= exp(predicted[,1]) ,
                   group = `Old field`,
                    colour = `Old field`),
            size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = p.beta.div_fitted,
              aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = p.beta.div_fitted,
            aes(x = YSA, y = exp(Estimate)),
            size = 1.5) +
  scale_y_continuous( limits=c(25,125),breaks = c(25,50,80,90,100,125)) +
  #scale_color_manual(values = mycolors) +
  scale_color_viridis(discrete = T, option="D")  + 
    theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                    legend.direction = "horizontal", legend.position="bottom")  +
  labs(color = "Old field", subtitle= 'b)') +
  ylab((expression(''~paste(italic(beta), '-Diversity (%)', sep = '')))) +  xlab("Years since agricultural abandonment") +
  guides(col = guide_legend(ncol = 9))



fig_3b


#SPIE
# ALPHA
# 
# head(alpha_dat)
# 


# p.alpha.spie <-  brm(log_alpha_ENSPIE_p ~  log_YSA + ( 1 + log_YSA  | Field/Transect/Plot) + (1 | Year), 
#                      data = alpha_dat_of, family=student(), cores = 4, iter=7000,warmup = 1000, chains = 4)
# 
# 
# 
#save(p.alpha.spie, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/p.alpha.spie.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/alpha_pie_c.Rdata") 


summary(p.alpha.spie)


color_scheme_set("darkgray")
fig_s4c <- pp_check(p.alpha.spie)+  xlab((expression(paste(italic(alpha), -ENS[PIE], " (%)", sep = ' ')))) + ylab("Density") +
  labs(title= "c)")+
  theme_classic() +  theme(legend.position= "bottom")# predicted vs. observed values
fig_s4c

# models residuals
ma<-residuals(p.alpha.spie)
ma<-as.data.frame(m1)
ar.plot<-cbind(alpha_dat_of,ma$Estimate)

par(mfrow=c(1,2))
with(ar.plot, plot(Field, ma$Estimate))
with(ar.plot, plot(Year, ma$Estimate))


alpha_dat$Field<-as.factor(as.character(alpha_dat$Field))
alpha_dat$Year<-as.factor(as.character(alpha_dat$Year))

alpha_dat$Field<-as.numeric(alpha_dat$Field)

# for plotting fixed effects
p.alpha.spie_fitted <- cbind(p.alpha.spie$data,
                             fitted(p.alpha.spie, re_formula = NA)) %>% 
  as_tibble() %>% 
  # join with plot data for figures
  inner_join(alpha_dat %>% distinct(Field, Year, log_YSA, YSA, log_alpha_ENSPIE_p, alpha_ENSPIE_p,alpha_ENSPIE),
             by= c("Field", "Year", "log_YSA", "log_alpha_ENSPIE_p"))


head(p.alpha.spie_fitted)

# fixed effect coefficients
p.alpha.spie_fixef <- fixef(p.alpha.spie)


p.alpha.spie_coef <- coef(p.alpha.spie)
p.alpha.spie_coef 

alpha_dat$Field<-as.character(alpha_dat$Field)

# predict estimates for each field across a sequence of log_YSA's and YSA's
obs_nest.alpha.pie <- alpha_dat %>% 
  mutate(Field_group = Field) %>%
  group_by(Field_group, Field) %>% 
  summarise(log_YSA = seq(min(log_YSA), max(log_YSA), length.out = 6 ),
            YSA = seq(min(YSA), max(YSA), length.out = 6)) %>%
  nest(data = c(Field,YSA,log_YSA)) %>%
  mutate(predicted = map(data, ~predict(p.alpha.spie, newdata= .x, re_formula = ~(1 + log_YSA | Field) ))) 


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
  inner_join(alpha_dat %>% 
               group_by(Field) %>% 
               summarise(xmin = min(YSA),
                         xmax = max(YSA),
                         lxmin = min(log_YSA),
                         lxmax = max(log_YSA)
               ),
             by = 'Field')



p.alpha.spie_fitted$YSA<- as.numeric(p.alpha.spie_fitted$YSA)
p.alpha.spie_fitted$Field<-as.character(p.alpha.spie_fitted$Field)
p.alpha.spie_coef2$Field<-as.character(p.alpha.spie_coef2$Field)

head(alpha_dat_of)
head(p.alpha.spie_coef2)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/')
# avoid running above code everytime
save(p.alpha.spie_fitted,p.alpha.spie_fixef,p.alpha.spie_coef,p.alpha.spie_coef2,obs_nest.alpha.pie, file = 'alpha.spie.mod_dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/alpha.spie.mod_dat.Rdata')



fig_2c <- ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = p.alpha.spie_fitted %>%  mutate( `Old field` = fct_recode( Field,  "A" = "601",
                                                                               "B" = "600",
                                                                               "C" = "10",
                                                                               "D" = "28",
                                                                               "E" = "41",
                                                                               "F" = "39",
                                                                               "G" = "40",
                                                                               "H" = "4",
                                                                               "I" = "44",
                                                                               "J" = "53",
                                                                               "K" = "47",
                                                                               "L" = "21",
                                                                               "M" = "70",
                                                                               "N" = "5",
                                                                               "O" = "27",
                                                                               "P" = "45",
                                                                               "Q" = "32",
                                                                               "R" = "35",
                                                                               "S" = "72"
  )) %>% 
    mutate( `Old field` = as.character(`Old field`)) %>%
    arrange(`Old field`)
  ,
             aes(x = YSA, y = alpha_ENSPIE_p,
                 colour = `Old field`),
             size = 1.2, shape=1, position = position_jitter(width = 0.95, height=0.95)) +
  geom_line(data = obs_nest.alpha.pie %>% unnest(cols = c(data, predicted))%>% mutate( `Old field` = fct_recode( Field,   "A" = "601",
                                                                                                                 "B" = "600",
                                                                                                                 "C" = "10",
                                                                                                                 "D" = "28",
                                                                                                                 "E" = "41",
                                                                                                                 "F" = "39",
                                                                                                                 "G" = "40",
                                                                                                                 "H" = "4",
                                                                                                                 "I" = "44",
                                                                                                                 "J" = "53",
                                                                                                                 "K" = "47",
                                                                                                                 "L" = "21",
                                                                                                                 "M" = "70",
                                                                                                                 "N" = "5",
                                                                                                                 "O" = "27",
                                                                                                                 "P" = "45",
                                                                                                                 "Q" = "32",
                                                                                                                 "R" = "35",
                                                                                                                 "S" = "72")) %>%  
              mutate( `Old field` = as.character(`Old field`)) %>%
              arrange(`Old field`),
            aes(x = YSA, y= exp(predicted[,1]) ,
                group = Field,
                colour = `Old field`),
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
  scale_color_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.position="none") +
  labs(subtitle = 'c)') +
  xlab("Years since agricultural abandonment") +  ylab((expression(paste(italic(alpha), -ENS[PIE], " (%)", sep = ' '))))

fig_2c


#gamma


#----------------------------------------------------------------------------------------------


# p.gamma.spie<-  brm(log_gamma_ENSPIE_p ~  log_YSA  +  ( 1 + log_YSA  | Field) + (1 | Year), 
#                     data = gamma_dat_of, control = list(adapt_delta = 0.99), cores = 4, iter=12000, warmup=1000, chains = 4)

# student() does not converge
# went with gaussian

#save(p.gamma.spie, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/p.gamma.spie.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/gamma_pie_c.Rdata") 


summary(p.gamma.spie)


color_scheme_set("darkgray")
fig_s4d <- pp_check(p.gamma.spie) + xlab((expression(paste(italic(gamma), -ENS[PIE], " (%)", sep = ' '))))  + ylab("") + 
  labs(title= "d)")+
  theme_classic() +  theme(legend.position= "none")# predicted vs. observed values
fig_s4d


gamma_dat_of$Field<-as.factor(as.character(gamma_dat_of$Field))
gamma_dat_of$Year<-as.factor(as.character(gamma_dat_of$Year))

# models residuals
mg<-residuals(p.gamma.spie)
mg<-as.data.frame(mg)
gr.plot<-cbind(gamma_dat_of,mg$Estimate)

par(mfrow=c(1,2))
with(gr.plot, plot(Field, mg$Estimate))
with(gr.plot, plot(Year, mg$Estimate))


gamma_dat_of <- gamma_dat #%>% mutate(log_gamma_ENSPIE_p = round(log_gamma_ENSPIE_p ,2)) 

head(gamma_dat_of)

gamma_dat$Field<-as.numeric(gamma_dat$Field)

# for plotting fixed effects
p.gamma.spie_fitted <- cbind(p.gamma.spie$data,
                             fitted(p.gamma.spie, re_formula = NA
                             )) %>% 
  as_tibble() %>%
  # join with plot data for figures
  inner_join(gamma_dat %>% distinct(Field, Year, log_YSA, YSA, log_gamma_ENSPIE_p, gamma_ENSPIE_p, gamma_ENSPIE),
             #by= c("Field", "Year", "log_YSA", "log_gamma_rich_p")
  )



head(p.gamma.spie_fitted)

# fixed effect coefficients
p.gamma.spie_fixef <- fixef(p.gamma.spie)



head(p.gamma.spie_fixef)

p.gamma.spie_coef <- coef(p.gamma.spie)

p.gamma.spie_coef 


gamma_dat$Field<-as.factor(as.character(gamma_dat$Field))

# predict estimates for each field across a sequence of log_YSA's and YSA's
obs_nest.gamma.pie <- gamma_dat %>% 
  mutate(Field_group = Field) %>%
  group_by(Field_group, Field) %>% 
  summarise(log_YSA = seq(min(log_YSA), max(log_YSA), length.out = 6 ),
            YSA = seq(min(YSA), max(YSA), length.out = 6)) %>%
  nest(data = c(Field,YSA,log_YSA)) %>%
  mutate(predicted = map(data, ~predict(p.gamma.spie, newdata= .x, re_formula = ~(1 + log_YSA | Field) ))) 

head(obs_nest.gamma.pie)

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
  inner_join(gamma_dat %>% 
               group_by(Field) %>% 
               summarise(xmin = min(YSA),
                         xmax = max(YSA),
                         lxmin = min(log_YSA),
                         lxmax = max(log_YSA)
               ),
             by = 'Field')



head(gamma_dat_sum)

p.gamma.spie_fitted$YSA<- as.numeric(p.gamma.spie_fitted$YSA)
p.gamma.spie_fitted$Field<-as.character(p.gamma.spie_fitted$Field)
p.gamma.spie_coef2$Field<-as.character(p.gamma.spie_coef2$Field)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/')
# avoid running above code everytime
save(p.gamma.spie_fitted,p.gamma.spie_fixef,p.gamma.spie_coef,p.gamma.spie_coef2,obs_nest.gamma.pie, file = 'gamma.spie.mod_dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/gamma.spie.mod_dat.Rdata')


fig_2d <- ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = p.gamma.spie_fitted %>%  mutate( `Old field` = fct_recode( Field,  "A" = "601",
                                                                               "B" = "600",
                                                                               "C" = "10",
                                                                               "D" = "28",
                                                                               "E" = "41",
                                                                               "F" = "39",
                                                                               "G" = "40",
                                                                               "H" = "4",
                                                                               "I" = "44",
                                                                               "J" = "53",
                                                                               "K" = "47",
                                                                               "L" = "21",
                                                                               "M" = "70",
                                                                               "N" = "5",
                                                                               "O" = "27",
                                                                               "P" = "45",
                                                                               "Q" = "32",
                                                                               "R" = "35",
                                                                               "S" = "72"
  )) %>% 
    mutate( `Old field` = as.character(`Old field`)) %>%
    arrange(`Old field`)
  ,
             aes(x = YSA, y = gamma_ENSPIE_p,
                 colour = `Old field`),
             size = 1.2, shape=1) +
  geom_line(data = obs_nest.gamma.pie %>% unnest(cols = c(data, predicted))%>% mutate( `Old field` = fct_recode( Field,   "A" = "601",
                                                                                                                 "B" = "600",
                                                                                                                 "C" = "10",
                                                                                                                 "D" = "28",
                                                                                                                 "E" = "41",
                                                                                                                 "F" = "39",
                                                                                                                 "G" = "40",
                                                                                                                 "H" = "4",
                                                                                                                 "I" = "44",
                                                                                                                 "J" = "53",
                                                                                                                 "K" = "47",
                                                                                                                 "L" = "21",
                                                                                                                 "M" = "70",
                                                                                                                 "N" = "5",
                                                                                                                 "O" = "27",
                                                                                                                 "P" = "45",
                                                                                                                 "Q" = "32",
                                                                                                                 "R" = "35",
                                                                                                                 "S" = "72")) %>%  
              mutate( `Old field` = as.character(`Old field`)) %>%
              arrange(`Old field`),
            aes(x = YSA, y= exp(predicted[,1]) ,
                group = Field,
                colour = `Old field`),
            size = 1.2) +
  #uncertainy in fixed effect
  geom_ribbon(data = p.gamma.spie_fitted,
              aes(x=YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = p.gamma.spie_fitted,
            aes(x=YSA, y = exp(Estimate)),
            size = 1.5) +
  scale_color_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.position="none")  +
  labs(subtitle = 'd)') +
  xlab("Years since agricultural abandonment") +
  ylab((expression(paste(italic(gamma), -ENS[PIE], " (%)", sep = ' '))))  


fig_2d



#----------------------------------------------------------------------------------------------
head(gamma_dat)

# p.beta.spie <-  brm(log_beta_ENSPIE_p ~  log_YSA + (1 + log_YSA | Field) + (1 | Year), 
#                    data = gamma_dat_of, family=student(),cores = 4, iter=10000,warmup=1000, chains = 4)
# 
# save(p.beta.spie, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/p.beta.spie.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/beta_pie_c.Rdata") 

summary(p.beta.spie)


color_scheme_set("darkgray")
fig_s5d <- pp_check(p.beta.spie)+   xlab((expression(paste(italic(beta), -ENS[PIE], " (%)", sep = ' '))))  +  ylab("") +
  labs(title= "d)")+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values
fig_s5d

gamma_dat_of$Field<-as.character(gamma_dat_of$Field)
gamma_dat_of$Year<-as.factor(as.character(gamma_dat_of$Year))

# models residuals 
mb<-residuals(p.beta.spie)
mb<-as.data.frame(mb)
br.plot<-cbind(gamma_dat_of,mb$Estimate)

par(mfrow=c(1,2))
with(br.plot, plot(Field, mb$Estimate))
with(br.plot, plot(Year, mb$Estimate))


gamma_dat$Field <- as.numeric(gamma_dat$Field)

# for plotting fixed effects
p.beta.spie_fitted <- cbind(p.beta.spie$data,
                           fitted(p.beta.spie, re_formula = NA)) %>% 
  as_tibble() %>% 
  # join with plot data for figures
  inner_join(gamma_dat %>% distinct(Field, Year, log_YSA, YSA, log_beta_ENSPIE_p, beta_ENSPIE_p, beta_ENSPIE),
             by= c("Field", "Year", "log_YSA", "log_beta_ENSPIE_p"))

head(gamma_dat)
head(p.beta.spie_fitted)

# fixed effect coefficients
p.beta.spie_fixef <- fixef(p.beta.spie)


p.beta.spie_coef <- coef(p.beta.spie)
p.beta.spie_coef 


gamma_dat$Field<-as.factor(as.character(gamma_dat$Field))

# predict estimates for each field across a sequence of log_YSA's and YSA's
obs_nest.beta.pie <- gamma_dat %>% 
  mutate(Field_group = Field) %>%
  group_by(Field_group, Field) %>% 
  summarise(log_YSA = seq(min(log_YSA), max(log_YSA), length.out = 6 ),
            YSA = seq(min(YSA), max(YSA), length.out = 6)) %>%
  nest(data = c(Field,YSA,log_YSA)) %>%
  mutate(predicted = map(data, ~predict(p.beta.spie, newdata= .x, re_formula = ~(1 + log_YSA | Field) ))) 



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
  inner_join(gamma_dat %>% 
               group_by(Field) %>% 
               summarise(xmin = min(YSA),
                         xmax = max(YSA),
                         lxmin = min(log_YSA),
                         lxmax = max(log_YSA)
               ),
             by = 'Field')




p.beta.spie_fitted$YSA <- as.numeric(p.beta.spie_fitted$YSA)
p.beta.spie_fitted$Field<-as.character(p.beta.spie_fitted$Field)
p.beta.spie_coef2$Field<-as.character(p.beta.spie_coef2$Field)

head(p.beta.spie_coef2)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/')
# avoid running above code everytime
save(p.beta.spie_fitted,p.beta.spie_fixef,p.beta.spie_coef,p.beta.spie_coef2,obs_nest.beta.pie, file = 'beta.spie.mod_dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/beta.spie.mod_dat.Rdata')



head(p.beta.spie_coef2)

head(p.beta.spie_fitted)

fig_3d <-ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  geom_point(data = p.beta.spie_fitted %>%  mutate( `Old field` = fct_recode( Field,  "A" = "601",
                                                                              "B" = "600",
                                                                              "C" = "10",
                                                                              "D" = "28",
                                                                              "E" = "41",
                                                                              "F" = "39",
                                                                              "G" = "40",
                                                                              "H" = "4",
                                                                              "I" = "44",
                                                                              "J" = "53",
                                                                              "K" = "47",
                                                                              "L" = "21",
                                                                              "M" = "70",
                                                                              "N" = "5",
                                                                              "O" = "27",
                                                                              "P" = "45",
                                                                              "Q" = "32",
                                                                              "R" = "35",
                                                                              "S" = "72"
  )) %>% 
    mutate( `Old field` = as.character(`Old field`)) %>%
    arrange(`Old field`)
  ,
             aes(x = YSA, y = beta_ENSPIE_p,
                 colour = `Old field`),
             size = 1.2, shape=1) +
  geom_line(data = obs_nest.beta.pie %>% unnest(cols = c(data, predicted))%>% mutate( `Old field` = fct_recode( Field,   "A" = "601",
                                                                                                                "B" = "600",
                                                                                                                "C" = "10",
                                                                                                                "D" = "28",
                                                                                                                "E" = "41",
                                                                                                                "F" = "39",
                                                                                                                "G" = "40",
                                                                                                                "H" = "4",
                                                                                                                "I" = "44",
                                                                                                                "J" = "53",
                                                                                                                "K" = "47",
                                                                                                                "L" = "21",
                                                                                                                "M" = "70",
                                                                                                                "N" = "5",
                                                                                                                "O" = "27",
                                                                                                                "P" = "45",
                                                                                                                "Q" = "32",
                                                                                                                "R" = "35",
                                                                                                                "S" = "72")) %>%  
              mutate( `Old field` = as.character(`Old field`)) %>%
              arrange(`Old field`),
            aes(x = YSA, y= exp(predicted[,1]) ,
                group = Field,
                colour = `Old field`),
            size = 1.2) +
  geom_ribbon(data = p.beta.spie_fitted,
              aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = p.beta.spie_fitted,
            aes(x = YSA, y = exp(Estimate)),
            size = 1.5) +
  scale_y_continuous( limits=c(25,125),breaks = c(25,50,70,80,100,125)) +
  #scale_color_manual(values = mycolors) +
  scale_color_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.direction = "horizontal", legend.position="bottom")  +
  labs(subtitle= 'd)' ) +
  ylab((expression(paste(italic(beta), -ENS[PIE], " (%)", sep = ' '))))  +  xlab("Years since agricultural abandonment") + guides(col = guide_legend(ncol = 13))



fig_3d


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

ysa.legend<-g_legend(fig_3b)

# (p.alpha.spie.fig | p.gamma.spie.fig   | p.beta.spie.fig + theme(legend.position="none"))/(ysa.legend) + plot_layout(heights = c(10,1)) 
# 
# (p.alpha.rich.fig| p.alpha.spie.fig)/ (p.gamma.rich.fig | p.gamma.spie.fig)/( p.beta.div.fig + theme(legend.position="none") | p.beta.spie.fig + theme(legend.position="none"))/(ysa.legend) + plot_layout(heights = c(10,10,10,2)) 
# 
# (p.alpha.rich.fig|p.gamma.rich.fig  )/ ( p.alpha.spie.fig| p.gamma.spie.fig)/( p.beta.div.fig + theme(legend.position="none") | p.beta.spie.fig + theme(legend.position="none"))/(ysa.legend) + plot_layout(heights = c(10,10,10,2)) 
# 
# 
# (d.alpha.rich.eff | d.gamma.rich.eff  ) / (d.alpha.spie.eff | d.gamma.spie.eff) / (d.beta.div.eff | d.beta.spie.eff + theme(legend.position="none")) + plot_layout(heights = c(10,10,10)) 


# FOR PAPER
# LANDSCAPE 10X11

# FIG 1 ALPHA GAMMA
 fig_1 <- (fig_1a | fig_1b  )/ (fig_1c | fig_1d  )/ (ysa.legend) + plot_layout(heights = c(10,10,2)) 

fig_1


# FIG 2 EVENNESS
fig_2 <- (fig_2a | fig_2b)/( fig_2c | fig_2d)/(ysa.legend) + plot_layout(heights = c(10,10,2)) 

fig_2

# FIG 3 BETA
almost_fig_3 <- (fig_3a | fig_3b + theme(legend.position="none")) /(  fig_3c + theme(legend.position="none") | fig_3d + theme(legend.position="none"))/(ysa.legend) + plot_layout(heights = c(10,10,2)) 
almost_fig_3

fig_3 <- (almost_fig_3) + plot_annotation(title = (expression(paste('', italic(beta), '-scale', sep = ''))),
                                          theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) + plot_layout(ncol=1)

fig_3


# posterior predictive checks plots for supplementary info

#s3
fig_s3_legend <- g_legend(fig_s3c)

fig_s3 <- (fig_s3a | fig_s3b)/(fig_s3c +  theme(legend.position= "none") | fig_s3d)/(fig_s3_legend) + plot_layout(heights = c(10,10, 0.75))
fig_s3

#s4
fig_s4_legend <- g_legend(fig_s4c)

fig_s4 <- (fig_s4a | fig_s4b)/(fig_s4c +  theme(legend.position= "none") | fig_s4d)/(fig_s4_legend) + plot_layout(heights = c(10,10, 0.75))
fig_s4

#s5
fig_s5_legend <- g_legend(fig_s5b)

almost_fig_s5 <- (fig_s5a | fig_s5b +  theme(legend.position= "none") )/(fig_s5c | fig_s5d)/(fig_s5_legend) + plot_layout(heights = c(10,10, 0.75))

fig_s5 <- (almost_fig_s5) + plot_annotation(title = (expression(paste('', italic(beta), '-scale', sep = ''))),
                                            theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) + plot_layout(ncol=1)

fig_s5



# PRESENTATIONS


# (d.alpha.rich.eff | p.alpha.rich.fig  )/ (ysa.legend) + plot_layout(heights = c(10,2)) 
# 
# 
# (d.alpha.spie.eff | p.alpha.spie.fig  )/ (ysa.legend) + plot_layout(heights = c(10,2)) 
# 
# 
# (d.gamma.rich.eff  | p.gamma.rich.fig  )/ (ysa.legend) + plot_layout(heights = c(10,2)) 
# 
# (d.gamma.spie.eff  | p.gamma.spie.fig  )/ (ysa.legend) + plot_layout(heights = c(10,2)) 




b.fig <- (d.beta.div.eff | p.beta.div.fig + theme(legend.position="none")) /(  d.beta.spie.eff + theme(legend.position="none") | p.beta.spie.fig + theme(legend.position="none"))/(ysa.legend) + plot_layout(heights = c(10,10,2)) 
b.fig

(b.fig) + plot_annotation(title = (expression(paste('', italic(beta), '-scale', sep = ''))),
                          theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) + plot_layout(ncol=1)



