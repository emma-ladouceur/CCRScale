



rm(list=ls())

library(tidyverse)
library(ggplot2)
library(brms)
library(bayesplot)
library(tidybayes)
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
  summarise(log_YSA = seq(max(log_YSA), 6.907755, length.out = 920 ),
            YSA = seq(80, 1000, length.out = 920) ) %>%
  mutate(YSA_group = YSA) %>%
  group_by(YSA_group, YSA) %>% 
  nest(data = c( log_YSA)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(p.alpha.rich, newdata= .x, re_formula = ~(1 + log_YSA ) ))) 

head(obs_nest.alpha)

obs_nest.alpha.df <- obs_nest.alpha  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  filter(YSA >= 80) %>% mutate( log_predicted = .prediction,
                                predicted = exp(.prediction)) %>%
  select(-.prediction)


head(obs_nest.alpha.df)                       

write.csv(obs_nest.alpha.df, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/predicted_alpha.csv")

alpha_pre <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/predicted_alpha.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


head(alpha_pre)


#gamma
gamma_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/gamma_div_percent.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/gamma_rich_c.Rdata") 

head(gamma_dat)
summary(p.gamma.rich)

max(gamma_dat$log_YSA)
log(1000)


obs_nest.gamma <- gamma_dat %>% 
  summarise(log_YSA = seq(max(log_YSA), 6.907755, length.out = 920 ),
            YSA = seq(80, 1000, length.out = 920) ) %>%
  mutate(YSA_group = YSA) %>%
  group_by(YSA_group, YSA) %>% 
  nest(data = c( log_YSA)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(p.gamma.rich, newdata= .x, re_formula = ~(1 + log_YSA ) ))) 

head(obs_nest.gamma)

obs_nest.gamma.df <- obs_nest.gamma  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  filter(YSA >= 80) %>% mutate( log_predicted = .prediction,
                                predicted = exp(.prediction)) %>%
  select(-.prediction)

head(obs_nest.gamma.df)


write.csv(obs_nest.gamma.df, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/predicted_gamma.csv")

gamma_pre <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/predicted_gamma.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


head(alpha_pre) 
head(obs_nest.gamma.df) 

alpha_predict <- alpha_pre %>%
  mutate(YSA = round(YSA),
         log_YSA = round(log_YSA, 2)) %>%
  select(YSA,log_YSA, log_predicted, predicted) %>%
  mutate(alpha_log_predicted = log_predicted,
        alpha_predicted = predicted) %>%
  select(-c(log_predicted, predicted)) %>%
  group_by(YSA) %>%
  sample_n(1000, replace = F)

write.csv(alpha_predict, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/smol_predicted_alpha.csv")


head(alpha_predict)
nrow(alpha_pre)
nrow(alpha_predict)

gamma_predict <- gamma_pre %>%
  mutate(YSA = round(YSA),
         log_YSA = round(log_YSA, 2),
         ) %>%
  select(YSA,log_YSA, log_predicted, predicted) %>%
  mutate(gamma_log_predicted = log_predicted,
         gamma_predicted = predicted) %>%
  select(-c(log_predicted, predicted)) %>%
  group_by(YSA) %>%
  sample_n(1000, replace = F)

head(gamma_predict)

write.csv(gamma_predict, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/smol_predicted_gamma.csv")


#smol version
alpha_predict <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/smol_predicted_alpha.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

gamma_predict <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/smol_predicted_gamma.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

np_means <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/np_means.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

head(np_means)

??roll_mean
library(RcppRoll)


beta_predict <- alpha_predict %>% left_join(gamma_predict) %>%
  # back transform alphaand gamma percentages to diversity
  mutate(alpha_predicted_div = ((alpha_predicted/100) *(np_means$alpha_rich_mean_np)),
         gamma_predicted_div = ((gamma_predicted/100) *(np_means$gamma_rich_mean_np)),
         ) %>%
  #then calculate predicted beta
mutate( beta_predicted_div = (gamma_predicted_div/alpha_predicted_div),
        beta_predicted = (beta_predicted_div/np_means$beta_div_mean_np) * 100) %>%
  # Take rolling means and probs for every year
  group_by(YSA, log_YSA) %>%
  mutate(
    #alpha 
    mean_alpha = roll_mean(alpha_predicted, n = 1000,  align = "right", fill = NA, na.rm = T ),
    alpha_lower = quantile(alpha_predicted, probs=0.025),
    alpha_upper = quantile(alpha_predicted, probs=0.975),
    #gamma
    mean_gamma = roll_mean(gamma_predicted,  n = 1000,  align = "right", fill = NA, na.rm = T),
            gamma_lower = quantile(gamma_predicted, probs=0.025),
            gamma_upper = quantile(gamma_predicted, probs=0.975),
       #beta
            mean_beta = roll_mean(beta_predicted, n = 1000, align = "right", fill = NA, na.rm = T),
beta_lower = quantile(beta_predicted, probs=0.025),
beta_upper = quantile(beta_predicted, probs=0.975)) %>%
  select(c(mean_alpha, alpha_lower, alpha_upper, 
           mean_gamma, gamma_lower, gamma_upper,
           mean_beta, beta_lower, beta_upper)) %>% distinct() %>%
  filter(!is.na(mean_alpha))

View(beta_predict)

write.csv(beta_predict, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/beta_predict.csv")

beta_predict <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/beta_predict.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
gamma_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/gamma_div_percent.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


# beta

load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/beta_div_c.Rdata") 

p.beta.div_fitted <- cbind(p.beta.div$data,
                           fitted(p.beta.div, re_formula = NA)) %>% 
  as_tibble() %>% 
  # join with plot data for figures
  inner_join(gamma_dat %>% distinct(Field, Year, log_YSA, YSA, log_beta_div_p, beta_div_p, beta_div),
             by= c("Field", "Year", "log_YSA", "log_beta_div_p"))

head(p.beta.div_fitted)

betadata <- data.frame(YSA =  c(80,90,100,125,150,175,250,500,750,1000))

head(betadata)

betadata <- betadata %>%  mutate(
  log_YSA = log(YSA))

predicted.beta <-  predict(p.beta.div, newdata = betadata, re_formula = ~(1 + log_YSA) )

beta_predicts <-  mutate(as.data.frame(predicted.beta)) %>%
  bind_cols(betadata) %>% mutate(Method = "Predicted from model")

head(p.beta.div_fitted)
head(beta_predict)

beta_predict_calc <- beta_predict %>%
  mutate( Estimate = log(mean_beta),
          `Q2.5` = log(beta_lower),
          `Q97.5` = log(beta_upper)
  ) %>% mutate(Method = "Predicted from gamma & alpha models") %>%
  select(YSA, `Q2.5`, `Q97.5`, Estimate, Method)
  
head(beta_predict_calc)

p.beta.div_fitted_bm <- p.beta.div_fitted %>% mutate(Method = "Estimated from observed values") %>%
  select(YSA, `Q2.5`, `Q97.5`, Estimate, Method) %>%
  bind_rows(beta_predicts) %>% select(-c(`Est.Error`, `log_YSA`))

head(p.beta.div_predicted_bm)

p.beta.div_predicted_om <- beta_predict_calc %>% select(-c( `log_YSA`)) 
  
head(p.beta.div_predicted_om)
  
fig_5c <- ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  # uncertainy in fixed effect
  geom_ribbon(data = p.beta.div_fitted_bm,
              aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  geom_ribbon(data = p.beta.div_predicted_om,
              aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.1) +
  # fixed effect
  # geom_line(data = p.beta.div_predicted_om,
  #           aes(x = YSA, y = exp(Estimate)),
  #           size = 0.75, alpha = 0.6) +
  geom_line(data = p.beta.div_fitted_bm,
            aes(x = YSA, y = exp(Estimate), linetype = Method, color = Method),
             size = 1.5) +
  geom_smooth(data = p.beta.div_predicted_om,
            aes(x = YSA, y = exp(Estimate), linetype = Method, color = Method),
             size = 1, method = "lm" ) +
  scale_y_continuous( breaks = c(25, 50,60,70, 75, 80 , 90, 95,100, 125)) +
  scale_x_continuous( breaks = c(0, 80, 250, 500, 750, 1000)) +
  coord_cartesian( ylim = c(10,125)) +
  #scale_color_manual(values = mycolors) +
  #scale_color_viridis(discrete = T, option="D")  + 
  scale_colour_manual(values = c("black", "black", "black"))+
  scale_linetype_manual(values = c("solid", "dotted", "dashed" )) +
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.direction = "horizontal", legend.position="none")  +
  labs(color = "Old field", subtitle= expression(paste('c) ', italic(beta), '-scale', sep = ''))) +
  ylab((expression(''~paste(italic(beta), '-Diversity (%)', sep = '')))) +  xlab("") +
  guides(col = guide_legend(ncol = 9))



fig_5c


line_types_dat <- p.beta.div_fitted_bm %>% bind_rows(p.beta.div_predicted_om) %>%
  mutate(Method = as.factor(Method)) %>%
  mutate(Method = fct_relevel(Method, "Estimated from observed values",
                              "Predicted from model",
                              "Predicted from gamma & alpha models"))

levels(line_types_dat$Method)

fig_5_legend <-  ggplot() +
  geom_hline(yintercept = 100, lty = 2) +
  #uncertainy in fixed effect
  geom_ribbon(data = line_types_dat,
              aes(x=YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = line_types_dat,
            aes(x=YSA, y = exp(Estimate), linetype = Method),
            size = 1.5) +
  scale_linetype_manual(values = c("solid",  "dashed", "dotted" )) +
  scale_y_continuous( breaks = c(25, 50,60,70, 75, 80 , 90, 95,100, 125)) +
  scale_x_continuous( breaks = c(0, 80, 250, 500, 750, 1000)) +
  coord_cartesian( ylim = c(25,125)) +
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom")  +
  labs( subtitle= expression(paste('b) ',italic(gamma), '-scale', sep = ''))
  ) + ylab("Species Richness (%)")  + xlab("Years since agricultural \n abandonment") +
  theme(legend.key.width = unit(2,"cm"))



g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

line.legend <- g_legend(fig_5_legend)

fig_5 <- ( (fig_5a | fig_5b + theme(legend.position="none") | fig_5c) / (line.legend) + plot_layout(heights = c(10,1)) )

fig_5
