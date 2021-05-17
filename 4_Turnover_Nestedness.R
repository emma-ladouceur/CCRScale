


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
library(betapart)
library(purrr)


ccr_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/e014_e133_cleaned_1983-2016_EL.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


head(ccr_dat)


# how many of each?
ccr_dat %>% select(Field,site_status) %>% distinct() %>% arrange(site_status)


ccr_dat$pres<-1


head(ccr_dat)

ccr_dat$Year <- as.factor(as.character(ccr_dat$Year))

# regional gamma never-plowed field within calendar year
np_wide <-ccr_dat %>% filter(site_status == "never-plowed") %>% 
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', Species)) %>% 
  group_by(site_status,Year,YSA,species2) %>% 
  summarise(pres=n_distinct(pres)) %>%
  spread(species2,pres,fill = 0) %>%
  mutate(OYear = Year) %>%
  mutate(Year = fct_recode(Year,
                           "1983" ="1984", # here i rename the years never-plowed fields were surveyed to match the years
                           "1989" = "1990", # of old fields, so i can easily pair them together
                           "1994" = "1995",
                          "1997" = "2000",
                          "2002" = "2005",
                          "2006" = "2010")) 

View(np_wide)
colnames(np_wide)

# every old field by calendar year and YSA
of_wide <- ccr_dat %>% filter(site_status == "old field") %>% 
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', Species)) %>% 
  group_by(site_status,Year,Field,YSA,species2) %>% 
  summarise(pres=n_distinct(pres)) %>%
  spread(species2,pres,fill = 0) %>% mutate(OYear = Year) %>%
  arrange(Year, Field)


View(of_wide)
colnames(of_wide)

of_wide$Year <- as.factor(as.character(of_wide$Year))

ccr_wide <-  of_wide %>% bind_rows(np_wide) %>% 
  replace(is.na(.), 0) %>%
  select(-OYear) %>%
  arrange(Year, Field)

View(ccr_wide)

colnames(ccr_wide)


beta_pairs <- function(x){
  # function to return the dissimilarities (turnover and nestedness component)
  # for each control treatment comparison in x
  
  # return dataframe with dissimilarities and the treatment magnitude (seed.rich)
  
  # separate out the control and treatment plots
  np = x %>% 
    filter(site_status == 'never-plowed')
  
  # fix for treatment labels
  of = x %>% 
    filter(site_status == 'old field' )
  
  out <- tibble()
  if(nrow(np)>0){
    for(i in 1:nrow(np)){
      beta = beta.pair(bind_rows(np %>% 
                                   slice(i) %>% 
                                   select(-site_status, -Field, -YSA), 
                                 of %>% 
                                   select(-site_status, -Field, -YSA)),
                       index.family = 'jaccard')
      # buid the data we want for analysis
      out <- bind_rows(out,
                       tibble(
                         Field = of$Field,
                         YSA = of$YSA,
                         jtu = as.matrix(beta$beta.jtu)[-1,1],
                         jne = as.matrix(beta$beta.jne)[-1,1],
                         jac = as.matrix(beta$beta.jac)[-1,1],
                         group = i)
      )
    }
  }  
  # escape for no controls
  else{
    out = tibble(
      #seed.rich = NA,
      jtu = NA,
      jne = NA,
      jac = NA,
      group = NA)
  }
  return(out)
}

colnames(ccr_wide)

wide.df <- ccr_wide %>%
  group_by(Year) %>%
    nest_legacy(starts_with('sp_'), site_status, Field, YSA)
 
View(wide.df)

wide.df <- wide.df %>% 
  mutate(beta = purrr::map(data, ~ beta_pairs(.x)))


beta.df = wide.df %>% 
  unnest_legacy(beta) %>%
  unite(col = pw_beta_group,
        c(Year,Field,YSA, group), sep = '_', remove = F) %>% 
  select(-group)



View(beta.df)



write.csv(beta.df,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/beta.df.csv")

beta <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/beta.df.csv", header=TRUE) %>%
  as_tibble()


beta <- beta %>% arrange(Field,YSA)

head(beta)

beta$Field <- as.factor(as.character(beta$Field))

# ccr.turnover <- brm(jtu ~  YSA + (1 + YSA | Field) + (1 | Year),
#                          family=zero_one_inflated_beta(),
#                          data = beta,
#                          inits = '0',
#                          cores = 4, chains = 4)


summary(ccr.turnover)
pp_check(ccr.turnover) + theme_classic()

plot(ccr.turnover)

ccr.turnover$Field<-as.factor(as.character(ccr.turnover$Field))
ccr.turnover$Year<-as.factor(as.character(ccr.turnover$Year))

# models residuals
ma<-residuals(ccr.turnover)
ma<-as.data.frame(ma)
ar.plot<-cbind(ccr.turnover,ma$Estimate)


par(mfrow=c(1,2))
with(ar.plot, plot(YSA, ma$Estimate))
with(ar.plot, plot(Year, ma$Estimate))


#save(ccr.turnover, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/betat.Rdata')

# ccr.nest <- brm(jne ~  YSA + (1 + YSA | Field) + (1 | Year),
#                     family=zero_inflated_beta(),
#                     data = beta,
#                     inits = '0',
#                     cores = 4, chains = 4)


plot(ccr.nest)

#save(ccr.nest, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/betan.Rdata')


load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/betat.Rdata") # object name: ccr.turnover
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/betan.Rdata") # object name: ccr.nest

summary(ccr.nest)
summary(ccr.turnover)


color_scheme_set("darkgray")
pp_check(ccr.nest) + xlab("Nestedness") + ylab("Density")+ theme_classic()
pp_check(ccr.turnover) + xlab("Turnover") + ylab("Density")+ theme_classic()




betat_fitted <- cbind(ccr.turnover$data,
                      fitted(ccr.turnover, re_formula = NA)) %>% 
  as_tibble() 


betat_fixef <- fixef(ccr.turnover)

betat_coef <- coef(ccr.turnover)

summary(ccr.turnover)

# predict estimates for each field across a sequence of log_YSA's and YSA's
obs_nest.betat <- beta %>% 
  mutate(Field_group = Field) %>%
  group_by(Field_group, Field) %>% 
  summarise(YSA = seq(min(YSA), max(YSA), length.out = 6 ),
            YSA = seq(min(YSA), max(YSA), length.out = 6)) %>%
  nest(data = c(Field,YSA)) %>%
  mutate(predicted = map(data, ~predict(ccr.turnover, newdata= .x, re_formula = ~(1 + YSA | Field) ))) 


betat_coef2 <-  bind_cols(betat_coef$Field[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       Field = rownames(betat_coef$Field[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              betat_coef$Field[,,'YSA'] %>% 
                                as_tibble() %>% 
                                mutate(Slope = Estimate,
                                       Slope_lower = Q2.5,
                                       Slope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  inner_join(beta %>% 
               group_by(Field) %>% 
               summarise(xmin = min(YSA),
                         xmax = max(YSA)),
             by = 'Field')

View(betat_coef2)

 setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/')
 save(betat_fitted,betat_fixef,betat_coef,betat_coef2,obs_nest.betat, file = 'betat.mod_dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/betat.mod_dat.Rdata')


betan_fitted <- cbind(ccr.nest$data,
                      fitted(ccr.nest, re_formula = NA)) %>% 
  as_tibble() 

betan_fixef <- fixef(ccr.nest)

betan_coef <- coef(ccr.nest)

# predict estimates for each field across a sequence of log_YSA's and YSA's
obs_nest.betan <- beta %>% 
  mutate(Field_group = Field) %>%
  group_by(Field_group, Field) %>% 
  summarise(YSA = seq(min(YSA), max(YSA), length.out = 6 ),
            YSA = seq(min(YSA), max(YSA), length.out = 6)) %>%
  nest(data = c(Field,YSA)) %>%
  mutate(predicted = map(data, ~predict(ccr.nest, newdata= .x, re_formula = ~(1 + YSA | Field) ))) 


betan_coef2 <-  bind_cols(betan_coef$Field[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       Field = rownames(betan_coef$Field[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              betan_coef$Field[,,'YSA'] %>% 
                                as_tibble() %>% 
                                mutate(Slope = Estimate,
                                       Slope_lower = Q2.5,
                                       Slope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  inner_join(beta %>% 
               group_by(Field) %>% 
               summarise(xmin = min(YSA),
                         xmax = max(YSA)),
             by = 'Field')


setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/')
 save(betan_fitted,betan_fixef,betan_coef,betan_coef2,obs_nest.betan, file = 'betan.mod_dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/betan.mod_dat.Rdata')

colnames(betat_fitted)

betat_fitted2<-betat_fitted %>% mutate( `Old field` = fct_recode( Field,  "A" = "10",
                                                                     "B" = "21",
                                                                     "C" = "27",
                                                                     "D" = "28",
                                                                     "E" = "32",
                                                                     "F" = "35",
                                                                     "G" = "39",
                                                                     "H" = "4",
                                                                     "I" = "40",
                                                                     "J" = "41",
                                                                     "K" = "44",
                                                                     "L" = "45",
                                                                     "M" = "47",
                                                                     "N" = "47",
                                                                     "O" = "5",
                                                                     "P" = "53",
                                                                     "Q" = "70",
                                                                     "R" = "72"))

betat_coef3<-betat_coef2 %>% mutate( `Old field` = fct_recode( Field,  "A" = "10",
                                                                  "B" = "21",
                                                                  "C" = "27",
                                                                  "D" = "28",
                                                                  "E" = "32",
                                                                  "F" = "35",
                                                                  "G" = "39",
                                                                  "H" = "4",
                                                                  "I" = "40",
                                                                  "J" = "41",
                                                                  "K" = "44",
                                                                  "L" = "45",
                                                                  "M" = "47",
                                                                  "N" = "47",
                                                                  "O" = "5",
                                                                  "P" = "53",
                                                                  "Q" = "70",
                                                                  "R" = "72"))

turn.fig <- ggplot() +
  #facet_grid(~Site) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = betat_fitted2,
             aes(x = YSA, y = jtu,
                 colour = `Old field`),
             size = 1.2, shape=1) +
  # geom_line(data = p.beta.div_fitted,aes(x = YSA, y= beta_rich_p,
  #                                       group = Field,
  #                                       colour = Field),
  #            size = 0.55)+
  geom_segment(data = betat_coef3,
               aes(x = xmin,
                   xend = xmax,
                   y = plogis(Intercept + Slope * xmin),
                   yend = plogis(Intercept + Slope * xmax),
                   group = `Old field`,
                   colour = `Old field`),
               size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = betat_fitted,
              aes(x = YSA, ymin = (Q2.5), ymax = (Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = betat_fitted,
            aes(x = YSA, y = (Estimate)),
            size = 1.5) +
 # scale_y_continuous( limits=c(25,125),breaks = c(25,50,100,125)) +
  #scale_color_manual(values = mycolors) +
  scale_color_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.direction = "horizontal", legend.position="bottom")  +
  labs(subtitle= 'a)', color = "Old field") +
  ylab("Turnover")+  xlab("Years since agricultural abandonment") +
  guides(col = guide_legend(ncol = 9))

turn.fig

betan_fitted2<-betan_fitted %>% mutate( `Old field` = fct_recode( Field,  "A" = "10",
                                                           "B" = "21",
                                                           "C" = "27",
                                                           "D" = "28",
                                                           "E" = "32",
                                                           "F" = "35",
                                                           "G" = "39",
                                                           "H" = "4",
                                                           "I" = "40",
                                                           "J" = "41",
                                                           "K" = "44",
                                                           "L" = "45",
                                                           "M" = "47",
                                                           "N" = "47",
                                                           "O" = "5",
                                                           "P" = "53",
                                                           "Q" = "70",
                                                           "R" = "72"))

betan_coef3<-betan_coef2 %>% mutate( `Old field` = fct_recode( Field,  "A" = "10",
                                                        "B" = "21",
                                                        "C" = "27",
                                                        "D" = "28",
                                                        "E" = "32",
                                                        "F" = "35",
                                                        "G" = "39",
                                                        "H" = "4",
                                                        "I" = "40",
                                                        "J" = "41",
                                                        "K" = "44",
                                                        "L" = "45",
                                                        "M" = "47",
                                                        "N" = "47",
                                                        "O" = "5",
                                                        "P" = "53",
                                                        "Q" = "70",
                                                        "R" = "72"))

nest.fig <- ggplot() +
  #facet_grid(~Site) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = betan_fitted2,
             aes(x = YSA, y = jne,
                 colour = `Old field`),
             size = 1.2, shape=1) +
  # geom_line(data = p.beta.div_fitted,aes(x = YSA, y= beta_rich_p,
  #                                       group = Field,
  #                                       colour = Field),
  #            size = 0.55)+
  geom_segment(data = betan_coef3,
               aes(x = xmin,
                   xend = xmax,
                   y = plogis(Intercept + Slope * xmin),
                   yend = plogis(Intercept + Slope * xmax),
                   group = `Old field`,
                   colour = `Old field`),
               size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = betan_fitted,
              aes(x = YSA, ymin = (Q2.5), ymax = (Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = betan_fitted,
            aes(x = YSA, y = (Estimate)),
            size = 1.5) +
  # scale_y_continuous( limits=c(25,125),breaks = c(25,50,100,125)) +
  #scale_color_manual(values = mycolors) +
  scale_color_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.direction = "horizontal", legend.position="bottom") +
labs( subtitle= 'b)', color = "Old field") +
ylab("Nestedness") +  xlab("Years since agricultural abandonment") +
guides(col = guide_legend(ncol = 9))

nest.fig




g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

ysa.legend<-g_legend(nest.fig)

(turn.fig + theme(legend.position="none") | nest.fig+ theme(legend.position="none"))/(ysa.legend) + plot_layout(heights = c(10,1)) 

# REARRANGE STACK PLOTS


turn.fig <- ggplot() +
  #facet_grid(~Site) +
  #geom_hline(yintercept = 0, lty = 2) +
  annotate("text", x = -15, y = 0.7, label =  c( "a)"), size= 6) +
  coord_cartesian(xlim = c(0, 80), ylim = c(0.15,0.7), clip = "off")+
  geom_point(data = betat_fitted2,
             aes(x = YSA, y = jtu,
                 colour = `Old field`),
             size = 1.2, shape=1) +
  geom_line(data = obs_nest.betat %>% unnest(cols = c(data, predicted))%>% mutate( `Old field` = fct_recode( Field,  "A" = "10",
                                                                                                             "B" = "21",
                                                                                                             "C" = "27",
                                                                                                             "D" = "28",
                                                                                                             "E" = "32",
                                                                                                             "F" = "35",
                                                                                                             "G" = "39",
                                                                                                             "H" = "4",
                                                                                                             "I" = "40",
                                                                                                             "J" = "41",
                                                                                                             "K" = "44",
                                                                                                             "L" = "45",
                                                                                                             "M" = "47",
                                                                                                             "N" = "47",
                                                                                                             "O" = "5",
                                                                                                             "P" = "53",
                                                                                                             "Q" = "70",
                                                                                                             "R" = "72")),
            aes(x = YSA, y= (predicted[,1]) ,
                group = `Old field`,
                colour = `Old field`),
            size = 1.2) +
  # geom_segment(data = betat_coef3,
  #              aes(x = xmin,
  #                  xend = xmax,
  #                  y = plogis(Intercept + Slope * xmin),
  #                  yend = plogis(Intercept + Slope * xmax),
  #                  group = `Old field`,
  #                  colour = `Old field`),
  #              size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = betat_fitted,
              aes(x = YSA, ymin = (Q2.5), ymax = (Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = betat_fitted,
            aes(x = YSA, y = (Estimate)),
            size = 1.5) +
  #ylim(0.15,0.7)+
  # scale_y_continuous( limits=c(25,125),breaks = c(25,50,100,125)) +
  #scale_color_manual(values = mycolors) +
  scale_color_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.border = element_blank(),
        axis.line = element_line(),
                                 plot.margin = unit(c(1,1,-0.5,7), "lines"),
                                 strip.background = element_rect(colour="black", fill="white"),
                     axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     legend.direction = "horizontal", legend.position="bottom",
                     axis.line.x.bottom=element_line(colour = NA) )  +
  annotate(
    geom = 'segment',
    y = Inf,
    yend = Inf,
    x = -Inf,
    xend = Inf, size=0.75
  )+
  annotate(
    geom = 'segment',
    y = -Inf,
    yend = Inf,
    x = Inf,
    xend = Inf, size=0.75
  )+
  labs( color = "Old field") +
  ylab("Turnover")+  xlab("") +
  guides(col = guide_legend(ncol = 9))

turn.fig

head(betan_fitted2)
head(betan_coef3)
head(betan_fitted)

nest.fig <- ggplot() +
  #facet_grid(~Site) +
  #geom_hline(yintercept = 0, lty = 2) +
  #geom_hline(yintercept = 1, lty = 2) +
  annotate("text", x = -15, y = 0.7, label =  c( "b)"), size= 6) +
  coord_cartesian(xlim = c(0, 80), ylim = c(0.2,0.7), clip = "off")+
  geom_point(data = betan_fitted2,
             aes(x = YSA, y = jne,
                 colour = `Old field`),
             size = 1.2, shape=1) +
  geom_line(data = obs_nest.betan %>% unnest(cols = c(data, predicted))%>% mutate( `Old field` = fct_recode( Field,  "A" = "10",
                                                                                                             "B" = "21",
                                                                                                             "C" = "27",
                                                                                                             "D" = "28",
                                                                                                             "E" = "32",
                                                                                                             "F" = "35",
                                                                                                             "G" = "39",
                                                                                                             "H" = "4",
                                                                                                             "I" = "40",
                                                                                                             "J" = "41",
                                                                                                             "K" = "44",
                                                                                                             "L" = "45",
                                                                                                             "M" = "47",
                                                                                                             "N" = "47",
                                                                                                             "O" = "5",
                                                                                                             "P" = "53",
                                                                                                             "Q" = "70",
                                                                                                             "R" = "72")),
            aes(x = YSA, y= (predicted[,1]) ,
                group = `Old field`,
                colour = `Old field`),
            size = 1.2) +
  # geom_segment(data = betan_coef3,
  #              aes(x = xmin,
  #                  xend = xmax,
  #                  y = plogis(Intercept + Slope * xmin),
  #                  yend = plogis(Intercept + Slope * xmax),
  #                  group = `Old field`,
  #                  colour = `Old field`),
  #              size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = betan_fitted,
              aes(x = YSA, ymin = (Q2.5), ymax = (Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = betan_fitted,
            aes(x = YSA, y = (Estimate)),
            size = 1.5) +
#  ylim(0.2,0.7)+
  # scale_y_continuous( limits=c(25,125),breaks = c(25,50,100,125)) +
  #scale_color_manual(values = mycolors) +
  scale_color_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                 panel.border = element_blank(),
                                 axis.line = element_line(),
                                 plot.margin = unit(c(-0.5,1,1,7), "lines"),
                     strip.background = element_rect(colour="black", fill="white"),
                     legend.direction = "vertical", legend.position="right",
                     axis.line.x.top = element_line(colour = NA)) +
  annotate(
    geom = 'segment',
    y = -Inf,
    yend = Inf,
    x = Inf,
    xend = Inf, size=1.5
  )+
  labs(  color = "Old field") +
  ylab("Nestedness") +  xlab("Years since agricultural abandonment") +
  guides(col = guide_legend(ncol = 2))

nest.fig

ysa.legend<-g_legend(nest.fig)




# DRAW THE CENTRE BIT
Model <-c((expression(paste(italic(gamma), -ENS[PIE], " (%) Recovery", sep = ' '))))
Model <- c('Never-Plowed Regional Species Pool')
conceptdat<- data.frame(Model)
conceptdat  


conceptual <- ggplot(data = conceptdat) +
  #facet_grid(~Model) +
  #labs(title = (expression(paste( 'Never-Plowed Regional ', italic(gamma), '-scale', sep = ''))) ) +
 # annotate("text", x = -1, y = 0, label = "  paste( 'Never-Plowed Regional ',italic(gamma), '-scale')", size= 6, parse=TRUE ) +
  annotate("text", x = -1.7:-1.7, y = 0.5:-0.5, label =  c( "Never-Plowed" , "paste( 'Regional ',italic(gamma), '-scale')" ), size= 6, parse=TRUE ) +
  coord_cartesian(xlim = c(0, 8), ylim = c(-1,1), clip = "off")+
   geom_hline( yintercept = 0, linetype="longdash") + theme_classic() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.margin = unit(c(0,1,0,7), "lines"),
        strip.background = element_rect(colour=NA, fill=NA),
        legend.position="bottom",  axis.title.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),  axis.title.x = element_blank(),
        panel.background = element_rect(color = NA),
        axis.line = element_line(colour = NA),
        # text = element_text(size=rel(14)),
        # plot.title=element_text(size=18, hjust=0.5)
         )
  
conceptual

#  PORTRAIT 11X14
p.lot <- (turn.fig + theme(legend.position="none") ) / (conceptual) / ( nest.fig + theme(legend.position="none"))  + plot_layout(heights = c(10,1.5,10)) 

(p.lot | ysa.legend) + plot_layout(widths = c(30,5) )


