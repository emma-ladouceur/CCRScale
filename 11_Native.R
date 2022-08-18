
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

ccr_sp <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Clark_2018/species.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


head(ccr_dat)
head(ccr_sp)

ccr_sp$FunctionalGroup <- as.factor(as.character(ccr_sp$FunctionalGroup))
levels(ccr_sp$FunctionalGroup)

ccr_sp$Duration <- as.factor(as.character(ccr_sp$Duration))
levels(ccr_sp$Duration)

ccr_sp$Origin <- as.factor(as.character(ccr_sp$Origin))
levels(ccr_sp$Origin)

sp <- ccr_sp %>% select(-c(Species, Justification_1, Justification_2)) %>%
  mutate( Duration = case_when(Duration == "perennial" ~ "Perennial",  TRUE ~ as.character(Duration))) %>%
  mutate( FunctionalGroup = case_when(FunctionalGroup == "C3" ~ "G", 
                                      FunctionalGroup == "C4" ~ "G", 
                                      FunctionalGroup == "S" ~ "G", TRUE ~ as.character(FunctionalGroup))) %>%
  mutate(Species = LCD_Species) %>% select(-LCD_Species) %>% distinct()

sp$Duration <- as.factor(as.character(sp$Duration))
levels(sp$Duration)

sp$FunctionalGroup <- as.factor(as.character(sp$FunctionalGroup))
levels(sp$FunctionalGroup)

head(sp)
nrow(sp)

ccr_dat %>% select(Species) %>% distinct() %>% nrow()

# how many of each?
ccr_dat %>% select(Field,site_status) %>% distinct() %>% arrange(site_status)

ccr_dat$pres<-1

head(ccr_dat)

ccr_dat$Year <- as.factor(as.character(ccr_dat$Year))
levels(ccr_dat$Year)

ccr_dat %>% distinct(Exp, site_status ,  Year)

ccr_comp <- ccr_dat %>% left_join(sp)

head(ccr_comp)
View(ccr_comp)

ccr_comp %>% group_by(Exp, site_status, Field, Year, YSA, Transect, Plot) %>%
  mutate(rp_sum = sum(Relative_pCover),
         p_sum = sum(pCover),
        ) 

np_sp <- ccr_comp  %>% filter(site_status == "never-plowed") %>%
  select(Species) %>% distinct() %>% mutate(np_pres = 1)

head(np_sp)
nrow(np_sp)


ccr_site_cover <- ccr_comp %>% left_join(np_sp) %>%
  select(-c(X, Transect, Plot,  Relative_pCover, pCover_plot_sum, n, pres, FunctionalGroup, Duration)) %>%
               group_by(Exp, site_status, Field, Year, YSA) %>%
               summarise(pCover_site_sum = sum(pCover) )   %>% 
  distinct()

ccr_site_cover

ccr_o_groups <- ccr_comp %>% left_join(np_sp) %>%
  select(-c(X, Transect, Plot,  Relative_pCover, pCover_plot_sum, n, pres,  Duration)) %>%
  group_by(Exp, site_status, Field, Year, YSA,  FunctionalGroup, Origin, np_pres) %>%
  mutate(pCover_site_origin_sum = sum(pCover)) %>% 
  group_by(Exp, site_status, Field, Year, YSA) %>%
  mutate(pCover_site_sum = sum(pCover) )   %>% 
  mutate(P_origin = (pCover_site_origin_sum/pCover_site_sum) * 100) %>%
  select(-c(Species, pCover)) %>%
   distinct() 


View(ccr_o_groups)

# are native species characteristic of np sites increasing in cover as YSA goes on?
ccr_nat <- ccr_o_groups %>% filter(!site_status == "never-plowed") %>%
  filter(Origin == "Native") %>%
  filter(np_pres == "1") %>% 
  mutate( #P_origin = as.numeric(round(P_origin, 2)),
         YSA = as.numeric(YSA)
         ) %>%
  mutate( # prep for modeling
    log_P_origin  = log(P_origin),
    log_YSA = log(YSA) )  %>% arrange(YSA, Field, Year)
  
  View(ccr_nat)

  is.numeric(ccr_nat$P_origin)
  ccr_nat$Field <- as.factor(ccr_nat$Field)
  summary(ccr_nat)
  nrow(ccr_nat)
  
  
p_nat <-  brm(log_P_origin ~  log_YSA * FunctionalGroup + ( 1 + log_YSA * FunctionalGroup  | Field) + (1 | Year),
                     data = ccr_nat, family=student(), cores = 4, iter=2000, warmup=1000, chains = 4)


save(p_nat, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/p_o_func.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/p_nat.Rdata") 



summary(p_nat)


color_scheme_set("darkgray")
fig_s <- pp_check(p_nat)+ xlab( "Relative native cover (%)") + ylab("Density") +
  labs(title= "")+
  theme_classic()+  theme(legend.position= "bottom") # predicted vs. observed values

fig_s

ccr_nat$Field<-as.factor(as.character(ccr_nat$Field))
ccr_nat$Year<-as.factor(as.character(ccr_nat$Year))

# models residuals
ma<-residuals(p_nat)
ma<-as.data.frame(ma)
ar.plot<-cbind(ccr_nat,ma$Estimate)

par(mfrow=c(1,2))
with(ar.plot, plot(Field, ma$Estimate))
with(ar.plot, plot(Year, ma$Estimate))



ccr_nat$Field<-as.character(as.factor(ccr_nat$Field))

# for plotting fixed effects
p_nat_fitted <- cbind(p_nat$data,
                             fitted(p_nat, re_formula = NA
                             )) %>% 
  as_tibble() %>% inner_join(ccr_nat %>% distinct(Field, Year, YSA, P_origin, log_YSA, log_P_origin, FunctionalGroup),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  )


head(p_nat_fitted)


# fixed effect coefficients
p_nat_fixef <- fixef(p_nat)


head(p_nat_fixef)

p_nat_coef <- coef(p_nat)
p_nat_coef 

ccr_nat$Field<-as.character(ccr_nat$Field)

# predict estimates for each field across a sequence of log_YSA's and YSA's
obs_nest.native <- ccr_nat %>% 
  mutate(Field_group = Field, FunctionalGroup) %>%
  group_by(Field_group, Field, FunctionalGroup) %>% 
  summarise(log_YSA = seq(min(log_YSA), max(log_YSA), length.out = 6 ),
            YSA = seq(min(YSA), max(YSA), length.out = 6 )) %>%
  nest(data = c(FunctionalGroup, Field,YSA, log_YSA)) %>%
  mutate(predicted = map(data, ~predict(p_nat, newdata= .x, re_formula = ~(1 + log_YSA * FunctionalGroup | Field) ))) 


setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/')
# save data objects to avoid time of compiling every time
save(p_nat_fitted, obs_nest.native, file = 'p_nat_dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/p_nat_dat.Rdata')


fig_nat <-ggplot() + 
  facet_grid(~FunctionalGroup, scales="free") +
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(data = p_nat_fitted %>% mutate( `Old field` = fct_recode( Field,  "A" = "601",
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
  aes(x = YSA, y = P_origin,
      colour = `Old field`),
  size = 1.2, shape=1, position = position_jitter(width = 2, height=2.5)) +
  geom_line(data = obs_nest.native  %>% unnest(cols = c(data, predicted)) %>% mutate( `Old field` = fct_recode( Field,   "A" = "601",
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
  geom_ribbon(data = p_nat_fitted,
              aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = p_nat_fitted,
            aes(x = YSA, y = exp(Estimate)),
            size = 1.5) +
 # scale_y_continuous( breaks = c(0,10,25,50,75,100)) +
  #coord_cartesian( ylim = c(0,100)) +
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") +
  labs(subtitle= ''
  ) +
  ylab("Native cover (%)")  + xlab("Years since agricultural abandonment")

fig_nat


# func groups

ccr_groups <- ccr_comp %>% left_join(np_sp) %>%
  select(-c(X, Species, pCover,  pCover_plot_sum, n, pres, Duration)) %>%
  group_by(Exp, site_status, Field, Year, YSA, Transect, Plot, FunctionalGroup,  Origin) %>%
  mutate(P_origin = sum(Relative_pCover)) %>% 
  select(-c(Relative_pCover)) %>%
  distinct() %>% group_by(Exp, site_status, Field, Year, YSA, Transect, Plot) %>%
  mutate(rp_sum = sum(P_origin)
  ) 


head(ccr_groups)
summary(ccr_groups)

ccr_nat_groups <- ccr_groups %>% filter(!site_status == "never-plowed") %>%
  #filter(Origin == "Native") %>%
  filter(np_pres == "1") %>% 
  mutate(P_origin = as.numeric(round(P_origin, 2))) %>%
  mutate( # prep for modeling
    log_P_origin  = log(P_origin),
    YSA = as.numeric(YSA),
    log_YSA = log(YSA) ) 


head(ccr_nat_groups)

is.numeric(ccr_nat_groups$P_origin)
ccr_nat_groups$YSA  <- as.numeric(ccr_nat_groups$YSA)
ccr_nat_groups$Field <- as.factor(ccr_nat_groups$Field)
summary(ccr_nat_groups)
nrow(ccr_nat_groups)


p_o_func <-  brm(log_P_origin ~  log_YSA * Origin * FunctionalGroup + ( 1 + log_YSA * Origin * FunctionalGroup | Field ) + (1 | Year),
                 data = ccr_nat_groups, family=student(), cores = 4, iter=2000, warmup=1000, chains = 4)


save(p_o_func, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/p_o_func_np.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/p_o_func.Rdata") 



summary(p_o_func)


color_scheme_set("darkgray")
fig_s <- pp_check(p_o_func)+ xlab( "Relative cover (%)") + ylab("Density") +
  labs(title= "")+
  theme_classic()+  theme(legend.position= "bottom") # predicted vs. observed values

fig_s

ccr_nat$Field<-as.factor(as.character(ccr_nat$Field))
ccr_nat$Year<-as.factor(as.character(ccr_nat$Year))

# models residuals
ma<-residuals(p_o_func)
ma<-as.data.frame(ma)
ar.plot<-cbind(ccr_nat,ma$Estimate)

par(mfrow=c(1,2))
with(ar.plot, plot(Field, ma$Estimate))
with(ar.plot, plot(Year, ma$Estimate))

ccr_nat$Field<-as.character(as.factor(ccr_nat$Field))

# for plotting fixed effects
p_o_func_fitted <- cbind(p_o_func$data,
                         fitted(p_o_func, re_formula = NA
                         )) %>% 
  as_tibble() %>% inner_join(ccr_nat_groups %>% distinct(Field, Year, YSA,  P_origin, log_P_origin, log_YSA, Origin, FunctionalGroup),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate( `Old field` = fct_recode( Field,  "A" = "601", "B" = "600","C" = "10", "D" = "28", "E" = "41",
                                          "F" = "39", "G" = "40",   "H" = "4",    "I" = "44",  "J" = "53",  "K" = "47",  "L" = "21",   "M" = "70",  "N" = "5", "O" = "27",
                                          "P" = "45", "Q" = "32",  "R" = "35",  "S" = "72")) %>%
  mutate( FunctionalGroup = fct_recode( FunctionalGroup,  "Forb" = "F", "Graminoid" = "G","Legume" = "L")) 


head(p_o_func_fitted)

# fixed effect coefficients
p_o_fixef <- fixef(p_o_func)


head(p_o_fixef)


obs_nest_p_o_func <- ccr_nat_groups %>% 
  mutate(Field_group = Field) %>%
  group_by(Field_group, Field, Origin, FunctionalGroup) %>% 
  summarise(log_YSA = seq(min(log_YSA), max(log_YSA), length.out = 6 ),
            YSA = seq(min(YSA), max(YSA), length.out = 6 )) %>%
  nest(data = c(Field, YSA, log_YSA, Origin, FunctionalGroup)) %>%
  mutate(predicted = map(data, ~predict(p_o_func, newdata= .x, re_formula = ~(1 + log_YSA * Origin * FunctionalGroup | Field) ))) 


setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/')
# save data objects to avoid time of compiling every time
save(p_o_func_fitted, file = 'p_o_func_fitted_dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/p_o_func_fitted_dat.Rdata')


fig_o_func <- ggplot() + 
  facet_wrap(~  fct_relevel(Origin, "Native", "Introduced")  * fct_relevel(FunctionalGroup, "Graminoid", "Forb", "Legume"), scales = "free" ) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
geom_point(data = p_o_func_fitted %>% 
  mutate( `Old field` = as.character(`Old field`)) %>%
  arrange(`Old field`)
,
aes(x = YSA, y = P_origin,
    colour = `Old field`),
size = 1.2, shape=1, position = position_jitter(width = 2, height=2.5)) +
geom_line(data = obs_nest_p_o_func  %>% unnest(cols = c(data, predicted)) %>% mutate( `Old field` = fct_recode( Field,  "A" = "601", "B" = "600","C" = "10", "D" = "28", "E" = "41",
                                                                                                                "F" = "39", "G" = "40",   "H" = "4",    "I" = "44",  "J" = "53",  "K" = "47",  "L" = "21",   "M" = "70",  "N" = "5", "O" = "27",
                                                                                                                "P" = "45", "Q" = "32",  "R" = "35",  "S" = "72")) %>%
            mutate( FunctionalGroup = fct_recode( FunctionalGroup,  "Forb" = "F", "Graminoid" = "G","Legume" = "L")) %>%
            mutate( `Old field` = as.character(`Old field`)) %>%
            filter(!is.na(Origin)) %>%
            arrange(`Old field`), aes(x = YSA, y= exp(predicted[,1]) ,
                                      group = Field,
                                      colour = `Old field`),
          size = 1.2) +
# uncertainy in fixed effect
geom_ribbon(data = p_o_func_fitted,
            aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
            alpha = 0.3) +
  # fixed effect
  geom_line(data = p_o_func_fitted,
            aes(x = YSA, y = exp(Estimate)),
            size = 1.5) +
  scale_y_continuous( breaks = c(0,10,25,50,75,100)) +
  coord_cartesian( ylim = c(0,100)) +
  scale_color_viridis(discrete = T, option="D")  + 
  scale_fill_viridis(discrete = T, option="D")  + 
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") +   guides(col = guide_legend(ncol = 7)) +
  labs(subtitle= ''
  ) +
  ylab("Relative cover (%)")  + xlab("Years since agricultural abandonment")

# LANDSCAPE 10X11
fig_o_func


p_o_func.p <- as.data.frame(p_o_func,  variable = "^b_", regex = TRUE, draw = floor(runif(n = 1000, 1, max = 2000))) 


head(p_o_func.p)

p_o_func_posterior <-  p_o_func.p %>% dplyr::select(`b_log_YSA`,`b_log_YSA:OriginNative`,
                                                    `b_log_YSA:FunctionalGroupG`, `b_log_YSA:OriginNative:FunctionalGroupG`,
                                                    `b_log_YSA:FunctionalGroupL`, `b_log_YSA:OriginNative:FunctionalGroupL`) %>%
  mutate(i.forb =`b_log_YSA`,
         n.forb = (`b_log_YSA`+ `b_log_YSA:OriginNative`) ,
           i.grass = (`b_log_YSA`+`b_log_YSA:FunctionalGroupG` ),
           n.grass = (`b_log_YSA`+ `b_log_YSA:OriginNative:FunctionalGroupG`),
           i.legume = (`b_log_YSA`+ `b_log_YSA:FunctionalGroupL`),
           n.legume = (`b_log_YSA`+ `b_log_YSA:OriginNative:FunctionalGroupL`) ) %>%
  dplyr::select(c(i.forb, n.forb, i.grass, n.grass, i.legume, n.legume))

head(p_o_func_posterior)

n.forb.p <-  p_o_func_posterior %>% 
  mutate( response="Native Forb", eff = mean(n.forb),
          eff_lower = quantile(n.forb, probs=0.025),
          eff_upper = quantile(n.forb, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

head(n.forb.p)

i.forb.p <-  p_o_func_posterior %>% 
  mutate( response="Introduced Forb", eff = mean(i.forb),
          eff_lower = quantile(i.forb, probs=0.025),
          eff_upper = quantile(i.forb, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

head(i.forb.p)

n.grass.p <-  p_o_func_posterior %>% 
  mutate( response="Native Grass", eff = mean(n.grass),
          eff_lower = quantile(n.grass, probs=0.025),
          eff_upper = quantile(n.grass, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

head(n.grass.p)

i.grass.p <-  p_o_func_posterior %>% 
  mutate( response="Introduced Grass", eff = mean(i.grass),
          eff_lower = quantile(i.grass, probs=0.025),
          eff_upper = quantile(i.grass, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

head(i.grass.p)
n.legume.p <-  p_o_func_posterior %>% 
  mutate( response="Native Legume", eff = mean(n.legume),
          eff_lower = quantile(n.legume, probs=0.025),
          eff_upper = quantile(n.legume, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

head(n.legume.p)

i.legume.p <-  p_o_func_posterior %>% 
  mutate( response="Introduced Legume", eff = mean(i.legume),
          eff_lower = quantile(i.legume, probs=0.025),
          eff_upper = quantile(i.legume, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

head(i.legume.p)

global.p <- bind_rows(i.forb.p, n.forb.p, i.grass.p, n.grass.p, i.legume.p, n.legume.p) %>%
  mutate( eff = round(eff, 2),
          eff_upper = round(eff_upper, 2),
          eff_lower = round(eff_lower, 2))

global.p

 ggplot() + 
  geom_point(data = global.p, aes(x = response, y = eff),size = 2) +
  geom_errorbar(data = global.p, aes(x = response, ymin = eff_lower,
                                           ymax = eff_upper),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  #scale_y_continuous(breaks=c(-0.5,-0.2,0)) +
  #scale_color_manual(values = c("#000000","#B40F20")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")




