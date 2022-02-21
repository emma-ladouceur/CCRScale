



rm(list=ls())

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

load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/alpha_rich_c.Rdata") 


# p.alpha.rich_fitted <- cbind(p.alpha.rich$data,
#                              fitted(p.alpha.rich, re_formula = NA
#                              )) %>% 
#   as_tibble() %>% inner_join(alpha_dat %>% distinct(Field, Year, log_YSA, YSA, log_alpha_rich_p, alpha_rich_p, alpha_rich),
#                              #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
#   )


head(alpha_dat)
summary(p.alpha.rich)

obs_nest.alpha <- alpha_dat %>% 
  mutate(Field_group = Field) %>%
  group_by(Field_group, Field, Transect, Plot) %>% 
  summarise(log_YSA = seq(max(log_YSA), 6.907755, length.out = 920 ),
            YSA = seq(80, 1000, length.out = 920) ) %>%
  nest(data = c(Field,YSA, log_YSA, Transect, Plot)) %>%
  mutate(predicted = map(data, ~predict(p.alpha.rich, newdata= .x, re_formula = ~(1 + log_YSA | Field/Transect/Plot) ))) 

View(obs_nest.alpha)

obs_nest.alpha.df <- obs_nest.alpha  %>% unnest(cols = c(data, predicted))%>% 
  filter(YSA >= 80) %>% mutate(predicted_exp = exp(predicted))

View(obs_nest.alpha.df)                       

write.csv(obs_nest.alpha.df, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/predicted_alpha.csv")

#gamma
gamma_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/gamma_div_percent.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


# save(p.gamma.rich, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/p.gamma.rich.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/gamma_rich_c.Rdata") 

head(gamma_dat)
summary(p.gamma.rich)

max(gamma_dat$log_YSA)
log(1000)

obs_nest.gamma <- gamma_dat %>% 
  mutate(Field_group = Field) %>%
  group_by(Field_group, Field) %>% 
  summarise(log_YSA = seq(max(log_YSA), 6.907755, length.out = 920 ),
            YSA = seq(80, 1000, length.out = 920) ) %>%
  nest(data = c(Field, YSA, log_YSA)) %>%
  mutate(predicted = map(data, ~predict(p.gamma.rich, newdata= .x, re_formula = ~(1 + log_YSA | Field) ))) 

View(obs_nest.gamma)

obs_nest.gamma.df <- obs_nest.gamma  %>% unnest(cols = c(data, predicted)) %>% 
  filter(YSA >= 80) %>% mutate(predicted_exp = exp(predicted))

View(obs_nest.gamma.df)   

write.csv(obs_nest.gamma.df, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/predicted_gamma.csv")


head(obs_nest.alpha.df) 
head(obs_nest.gamma.df) 

mean_alpha <- obs_nest.alpha.df %>%
  group_by(Field, YSA) %>%
  summarise(mean_alpha = mean(predicted_exp))


head(mean_alpha)

mean_gamma <- obs_nest.gamma.df %>%
  group_by(Field, YSA) %>%
  summarise(mean_gamma = mean(predicted_exp))

head(mean_gamma)

mean_predict <- mean_alpha %>% left_join(mean_gamma)

head(mean_predict)

head(gamma_dat)

# gamma_rich_mean_np    beta_div_mean_np          
#   43.3                4.82              






fig_5b <- ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  #uncertainy in fixed effect
  geom_ribbon(data = p.gamma.rich_fitted,
              aes(x=YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = p.gamma.rich_fitted,
            aes(x=YSA, y = exp(Estimate), linetype = Method),
            size = 1.5) +
  scale_y_continuous( breaks = c(25, 50,60,70, 75, 80 , 90, 95,100, 125)) +
  scale_x_continuous( breaks = c(0, 80, 250, 500, 750, 1000)) +
  coord_cartesian( ylim = c(25,125)) +
  scale_color_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom")  +
  labs( subtitle= expression(paste('b) ',italic(gamma), '-scale', sep = ''))
  ) + ylab("Species Richness (%)")  + xlab("Years since agricultural \n abandonment") +
  theme(legend.key.width = unit(2,"cm"))



fig_5b

# beta

load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/beta_div_c.Rdata") 



p.beta.div_fitted <- cbind(p.beta.div$data,
                           fitted(p.beta.div, re_formula = NA)) %>% 
  as_tibble() %>% 
  # join with plot data for figures
  inner_join(gamma_dat %>% distinct(Field, Year, log_YSA, YSA, log_beta_div_p, beta_div_p, beta_div),
             by= c("Field", "Year", "log_YSA", "log_beta_div_p"))



betadata <- data.frame(YSA =  c(80,90,100,125,150,175,250,500,750,1000))

head(betadata)

betadata <- betadata %>%  mutate(
  log_YSA = log(YSA))

predicted.beta <-  predict(p.beta.div, newdata = betadata, re_formula = ~(1 + log_YSA) )

beta_predicts <-  mutate(as.data.frame(predicted.beta)) %>% 
  bind_cols(betadata) %>% mutate(Method = "Predicted")


p.beta.div_fitted <- p.beta.div_fitted %>% mutate(Method = "Estimated from observed values") %>%
  bind_rows(beta_predicts)



fig_5c <- ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  # uncertainy in fixed effect
  geom_ribbon(data = p.beta.div_fitted,
              aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = p.beta.div_fitted,
            aes(x = YSA, y = exp(Estimate), linetype=Method),
            size = 1.5) +
  scale_y_continuous( breaks = c(25, 50,60,70, 75, 80 , 90, 95,100, 125)) +
  scale_x_continuous( breaks = c(0, 80, 250, 500, 750, 1000)) +
  coord_cartesian( ylim = c(10,125)) +
  #scale_color_manual(values = mycolors) +
  scale_color_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.direction = "horizontal", legend.position="none")  +
  labs(color = "Old field", subtitle= expression(paste('c) ', italic(beta), '-scale', sep = ''))) +
  ylab((expression(''~paste(italic(beta), '-Diversity (%)', sep = '')))) +  xlab("") +
  guides(col = guide_legend(ncol = 9))



fig_5c


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

line.legend <- g_legend(fig_5b)

fig_5 <- ( (fig_5a | fig_5b + theme(legend.position="none") | fig_5c) / (line.legend) + plot_layout(heights = c(10,1)) )

fig_5

# Instead of Predicting Beta..... Lets' Calc from alpha gamma


head(p.alpha.rich_fitted)

head(p.gamma.rich_fitted)

alpha.rich.predict.est <- p.alpha.rich_fitted %>% filter(Method == "Predicted") %>% 
  select(log_YSA, YSA, Estimate, Q2.5, Q97.5) %>%
  mutate(alpha.Estimate = Estimate,
         alpha.lower = Q2.5,
         alpha.upper = Q97.5) %>%
  select(-c(Estimate, Q2.5, Q97.5))

head(alpha.rich.predict.est)


gamma.rich.predict.est <- p.gamma.rich_fitted %>% filter(Method == "Predicted") %>% 
  #select(log_YSA, YSA, Estimate, Q2.5, Q97.5)%>%
  mutate(gamma.Estimate = Estimate,
         gamma.lower = Q2.5,
         gamma.upper = Q97.5) %>%
  select(-c(Estimate, Q2.5, Q97.5))

head(gamma.rich.predict.est)

div.predict <- alpha.rich.predict.est %>% left_join(gamma.rich.predict.est) %>%
  mutate( beta.Estimate = (gamma.Estimate/alpha.Estimate),
          beta.lower = (gamma.lower/alpha.lower),
          beta.upper = (gamma.upper/alpha.upper)) %>% 
  select(log_YSA, YSA, beta.Estimate, beta.lower, beta.upper) %>% 
  filter(YSA >= 80) %>% mutate(Method = "Predicted")

head(div.predict)


beta.div <- p.beta.div_fitted %>% 
  filter(Method == "Estimated from observed values") %>%
  mutate( beta.Estimate = Estimate,
          beta.lower = Q2.5,
          beta.upper = Q97.5 ) %>%
  select(log_YSA, YSA, beta.Estimate, beta.lower, beta.upper , Method) %>%
  bind_rows(div.predict)


max(beta.div$YSA)

head(beta.div)


fig_5c_2 <- ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  # uncertainy in fixed effect
  geom_ribbon(data = div.predict %>% 
                filter(Method == "Estimated from observed values") ,
              aes(x = YSA, ymin = exp(beta.lower), ymax = exp(beta.upper)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = div.predict %>% 
              filter(Method == "Estimated from observed values") ,
            aes(x = YSA, y = exp(beta.Estimate), linetype = Method),
            size = 1.5) +
  # scale_y_continuous( breaks = c(25,50,75,95,100,125)) +
  # coord_cartesian( ylim = c(10,125)) +
  #scale_color_manual(values = mycolors) +
  scale_color_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.direction = "horizontal", legend.position="none")  +
  labs(color = "Old field", subtitle= expression(paste('c) ', italic(beta), '-scale', sep = ''))) +
  ylab((expression(''~paste(italic(beta), '-Diversity (%)', sep = '')))) +  xlab("") +
  guides(col = guide_legend(ncol = 9))



fig_5c_2
