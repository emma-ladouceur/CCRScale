
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


p.alpha.rich_fitted <- cbind(p.alpha.rich$data,
                             fitted(p.alpha.rich, re_formula = NA
                             )) %>% 
  as_tibble() %>% inner_join(alpha_dat %>% distinct(Field, Year, log_YSA, YSA, log_alpha_rich_p, alpha_rich_p, alpha_rich),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  )


head(gamma_dat)

ysa_summary <- alpha_dat %>% select(YSA, log_YSA) %>% distinct() %>%
  arrange(YSA)

View(ysa_summary)

alphdata <- data.frame(YSA =  c(80,90,100,125,150,175,200,225,250,275,300,325,350,375,400,425,450,475,500, 525,550,575,600,725,750,775,800,825,850,900,925,950,1000))

head(alphdata)

alphdata <- alphdata %>%  mutate(
  log_YSA = log(YSA))

predicted.alpha <- predict(p.alpha.rich, newdata = alphdata, re_formula = ~(1 + log_YSA) )

head(predicted.alpha)

alpha_predicts <-  mutate(as.data.frame(predicted.alpha)) %>% 
  bind_cols(alphdata) %>% mutate(Method = "Predicted")

head(alpha_predicts)

p.alpha.rich_fitted <- p.alpha.rich_fitted %>% mutate(Method = "Estimated from observed values") %>%
  bind_rows(alpha_predicts)


head(p.alpha.rich_fitted)

fig_5a <-ggplot() + 
  # facet_grid(~Field, scales="free") +
  geom_hline(yintercept = 100, lty = 2) +
  # uncertainy in fixed effect
  geom_ribbon(data = p.alpha.rich_fitted,
              aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = p.alpha.rich_fitted,
            aes(x = YSA, y = exp(Estimate), linetype= Method),
            size = 1.5) +
 scale_y_continuous( breaks = c(25,50,75,95,100, 125)) +
  coord_cartesian( ylim = c(25,125)) +
  scale_color_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none") +
  labs(subtitle = expression(paste('a) ',italic(alpha), '-scale', sep = ''))
  ) +
  ylab("Species Richness (%)")  + xlab("")

fig_5a



#gamma
gamma_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/gamma_div_percent.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


# save(p.gamma.rich, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/p.gamma.rich.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/gamma_rich_c.Rdata") 

# for plotting fixed effects
p.gamma.rich_fitted <- cbind(p.gamma.rich$data,
                             fitted(p.gamma.rich, re_formula = NA
                             )) %>% 
  as_tibble() %>%
  # join with plot data for figures
  inner_join(gamma_dat %>% distinct(Field, Year, log_YSA, YSA, log_gamma_rich_p, gamma_rich_p, gamma_rich),
             #by= c("Field", "Year", "log_YSA", "log_gamma_rich_p")
  )


# try predicting for longer time frames
## predict response for new data

head(gamma_dat)

gamdata <- data.frame(YSA =  c(80,90,100,125,150,175,200,225,250,275,300,325,350,375,400,425,450,475,500, 525,550,575,600,725,750,775,800,825,850,900, 1000))

gamdata <- gamdata %>%  mutate(
  log_YSA = log(YSA))

predicted.gamma <- predict(p.gamma.rich, newdata = gamdata, re_formula = ~(1 + log_YSA) )

head(predicted.gamma)

gamma_predicts <-  mutate(as.data.frame(predicted.gamma)) %>% 
  bind_cols(gamdata) %>% mutate(Method = "Predicted")


p.gamma.rich_fitted$Field <- as.character(p.gamma.rich_fitted$Field)


p.gamma.rich_fitted <- p.gamma.rich_fitted %>% mutate(Method = "Estimated from observed values") %>%
  bind_rows(gamma_predicts)

head(p.gamma.rich_fitted)


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
  scale_y_continuous( breaks = c(25,50,75,95,100, 125)) +
  coord_cartesian( ylim = c(25,125)) +
  scale_color_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom")  +
  labs( subtitle= expression(paste('b) ',italic(gamma), '-scale', sep = ''))
        ) + ylab("Species Richness (%)")  + xlab("Years since agricultural abandonment") +
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
  scale_y_continuous( breaks = c(25,50,75,95,100,125)) +
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

( (fig_5a | fig_5b + theme(legend.position="none") | fig_5c) / (line.legend) + plot_layout(heights = c(10,1)) )

