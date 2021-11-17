


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
levels(ccr_dat$Year)

ccr_dat %>% distinct(Exp, site_status ,  Year)


fix_dat <- ccr_dat %>% filter(site_status == "never-plowed") %>% 
  filter(Year == "2010") %>% # seperate out 2010 data and change to 2016 for comparison with 2016 old fields
  mutate(Year = fct_recode(Year,
                           "2016" = "2010",
  )) 


# regional gamma never-plowed field within calendar year
np_wide <- ccr_dat %>% filter(site_status == "never-plowed") %>% 
  bind_rows(fix_dat) %>% # add in 2016 comparison dat
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
                          "2006" = "2010", # 2010 data is used for 2006 and 2016 comparison
                          )) 


head(np_wide)
colnames(np_wide)

# every old field by calendar year and YSA
of_wide <- ccr_dat %>% filter(site_status == "old field") %>% 
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', Species)) %>% 
  group_by(site_status,Year,Field,YSA,species2) %>% 
  summarise(pres=n_distinct(pres)) %>%
  spread(species2,pres,fill = 0) %>% mutate(OYear = Year) %>%
  arrange(Year, Field)


head(of_wide)
colnames(of_wide)

of_wide$Year <- as.factor(as.character(of_wide$Year))

ccr_wide <-  of_wide %>% bind_rows(np_wide) %>% 
  replace(is.na(.), 0) %>%
  select(-OYear) %>%
  arrange(Year, Field)

head(ccr_wide)

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
 
head(wide.df)

wide.df <- wide.df %>% 
  mutate(beta = purrr::map(data, ~ beta_pairs(.x)))


beta.df = wide.df %>% 
  unnest_legacy(beta) %>%
  unite(col = pw_beta_group,
        c(Year,Field,YSA, group), sep = '_', remove = F) %>% 
  select(-group)



head(beta.df)



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


load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/turnover.Rdata") # object name: ccr.turnover
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/nest.Rdata") # object name: ccr.nest

summary(ccr.nest)
summary(ccr.turnover)



fig_s7a <- pp_check(ccr.turnover) + 
  labs(title= "a)")+
  xlab("Turnover") + ylab("Density")+ theme_classic()+  theme(legend.position= "none")
fig_s7a

fig_s7b <- pp_check(ccr.nest) + 
  labs(title= "b)")+
  xlab("Nestedness") + ylab("")+ theme_classic()+  theme(legend.position= "bottom")
fig_s7b


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

head(betat_coef2)

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

betat_fitted2<-betat_fitted %>% mutate( `Old field` = fct_recode( Field,  "A" = "601",
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


betat_coef3<-betat_coef2 %>% mutate( `Old field` = fct_recode( Field,  "A" = "601",
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



betan_fitted2<-betan_fitted %>% mutate( `Old field` = fct_recode( Field,  "A" = "601",
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


betan_coef3<-betan_coef2%>% mutate( `Old field` = fct_recode( Field,  "A" = "601",
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




fig_4a <- ggplot() +
  annotate("text", x = -15, y = 0.7, label =  c( "a)"), size= 6) +
  coord_cartesian(xlim = c(0, 80), ylim = c(0.15,0.7), clip = "off")+
  geom_point(data = betat_fitted2,
             aes(x = YSA, y = jtu,
                 colour = `Old field`),
             size = 1.2, shape=1) +
  geom_line(data = obs_nest.betat %>% unnest(cols = c(data, predicted)) %>% mutate( `Old field` = fct_recode( Field,   "A" = "601",
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
              arrange(`Old field`),  aes(x = YSA, y= (predicted[,1]) ,
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

fig_4a

head(betan_fitted2)
head(betan_coef3)
head(betan_fitted)

fig_4b <- ggplot() +
  annotate("text", x = -15, y = 0.7, label =  c( "b)"), size= 6) +
  coord_cartesian(xlim = c(0, 80), ylim = c(0.2,0.7), clip = "off")+
  geom_point(data = betan_fitted2,
             aes(x = YSA, y = jne,
                 colour = `Old field`),
             size = 1.2, shape=1) +
  geom_line(data = obs_nest.betan %>% unnest(cols = c(data, predicted)) %>% mutate( `Old field` = fct_recode( Field,   "A" = "601",
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
              arrange(`Old field`),   aes(x = YSA, y= (predicted[,1]) ,
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

fig_4b

ysa.legend<-g_legend(fig_4b)


# DRAW THE CENTRE BIT
Label <-c((expression(paste(italic(gamma), -ENS[PIE], " (%) Recovery", sep = ' '))))
Label <- c('Never-Plowed Regional Species Pool')
label_dat<- data.frame(Label)
label_dat  


middle_bit <- ggplot(data = label_dat) +
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
         )
  
middle_bit

#  PORTRAIT 11X14
almost_fig_4 <- (fig_4a + theme(legend.position="none") ) / (middle_bit) / ( fig_4b + theme(legend.position="none"))  + plot_layout(heights = c(10,1.5,10)) 

fig_4 <- (almost_fig_4 | ysa.legend) + plot_layout(widths = c(30,5) )

fig_4

# posterior predictive check for turnover and nestedness supp figure

fig_s7_legend <- g_legend(fig_s7b)

fig_s7 <- (fig_s7a | fig_s7b +  theme(legend.position= "none") ) / (fig_s7_legend)+ plot_layout(heights = c(10,0.75))
fig_s7


