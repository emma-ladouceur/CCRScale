

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
summary(ccr_comp)
View(ccr_comp)

ccr_comp %>% group_by(Exp, site_status, Field, Year, YSA, Transect, Plot) %>%
  mutate(rp_sum = sum(Relative_pCover),
         p_sum = sum(pCover),
        ) 



# func groups
View(ccr_comp)
View(np_sp)

ccr_groups_prep <- ccr_comp %>% left_join(np_sp) %>%
  select(-c(X, Species, pCover,  pCover_plot_sum, n, pres, Duration)) %>%
  filter(np_pres == "1") %>%
  group_by(Exp, site_status, Field, Year, YSA, Transect, Plot, FunctionalGroup,  Origin) %>%
  mutate(P_origin = sum(Relative_pCover)) %>% 
  select(-c(Relative_pCover)) %>%
  distinct() %>% group_by(Exp, site_status, Field, Year, YSA, Transect, Plot) %>%
  mutate(rp_sum = sum(P_origin)
  ) %>% ungroup()


head(ccr_groups_prep)
summary(ccr_groups_prep)
View(ccr_groups_prep)

ccr_groups <- ccr_groups_prep %>% filter(site_status == "old field") %>%  # calculate percent recovery relative to mean of never plowed sites
  mutate(P_origin = as.numeric(round(P_origin, 2))) %>%
  mutate( # prep for modeling
    YSA = as.numeric(YSA),
    log_YSA = log(YSA),
    c.YSA = (YSA - mean(YSA)),
    log_P_origin  = log(P_origin),
    ) %>%
  mutate(Field = as.character(Field)) %>%
  mutate(Year = as.factor(as.character(Year))) 

head(ccr_groups)



p_o_func <-  brm(log_P_origin ~  log_YSA * Origin * FunctionalGroup + ( 1 + log_YSA * Origin * FunctionalGroup | Field ) + (1 | Year),
                 data = ccr_groups, family=student(), cores = 4, iter=2000, warmup=1000, chains = 4)


save(p_o_func, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/p_o_func.Rdata')
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/p_o_func.Rdata") 



summary(p_o_func)


color_scheme_set("darkgray")
fig_s7b <- pp_check(p_o_func)+ xlab( "Relative cover (Log %)") + ylab("Density") +
  labs(title= "", subtitle = 'b)')+
  theme_classic()+  theme(legend.position= "bottom") # predicted vs. observed values

fig_s7b

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
  as_tibble() %>% inner_join(ccr_groups %>% distinct(Field, Year, YSA,  P_origin, log_P_origin, log_YSA, Origin, FunctionalGroup),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate( `Old field` = fct_recode( Field,  "A" = "601", "B" = "600","C" = "10", "D" = "28", "E" = "41",
                                          "F" = "39", "G" = "40",   "H" = "4",    "I" = "44",  "J" = "53",  "K" = "47",  "L" = "21",   "M" = "70",  "N" = "5", "O" = "27",
                                          "P" = "45", "Q" = "32",  "R" = "35",  "S" = "72")) %>%
  mutate( FunctionalGroup = fct_recode( FunctionalGroup,  "Forb" = "F", "Graminoid" = "G","Legume" = "L")) 


head(p_o_func_fitted)

# fixed effect coefficients
p_o_fixef <- fixef(p_o_func)


head(p_o_fixef)


obs_nest_p_o_func <- ccr_groups %>% 
  mutate(Field_group = Field) %>%
  group_by(Field_group, Field, Origin, FunctionalGroup) %>% 
  summarise(log_YSA = seq(min(log_YSA), max(log_YSA), length.out = 6 ),
            YSA = seq(min(YSA), max(YSA), length.out = 6 )) %>%
  nest(data = c(Field, YSA, log_YSA, Origin, FunctionalGroup)) %>%
  mutate(predicted = map(data, ~predict(p_o_func, newdata= .x, re_formula = ~(1 + log_YSA * Origin * FunctionalGroup | Field) ))) 


setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/')
# save data objects to avoid time of compiling every time
save(p_o_func_fitted, obs_nest_p_o_func, file = 'p_o_func_fitted_dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/p_o_func_fitted_dat.Rdata')


fig_s10 <- ggplot() + 
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
fig_s10

 
 #__________________________________________________________
 
 # percentage of np fields
 ccr_groups_p_prep <- ccr_comp %>% left_join(np_sp) %>%
   select(-c(X, Species, pCover,  pCover_plot_sum, n, pres, Duration)) %>%
   select(-np_pres) %>%
   group_by(Exp, site_status, Field, Year, YSA, Transect, Plot, FunctionalGroup,  Origin) %>%
   mutate(P_origin = sum(Relative_pCover)) %>% 
   select(-c(Relative_pCover)) %>%
   distinct() %>% group_by(Exp, site_status, Field, Year, YSA, Transect, Plot) %>%
   mutate(rp_sum = sum(P_origin)
   ) %>% ungroup()
 
 summary(ccr_groups_p_prep)
 
 np_origin_means <- ccr_groups_p_prep %>% filter(site_status == "never-plowed") %>% 
   group_by(FunctionalGroup, Origin) %>%
   summarise(origin_mean_np = mean(P_origin),
   ) %>% filter(!is.na(Origin))
 
 np_origin_means
 
 
 
 ccr_groups_p <- ccr_groups_p_prep %>% filter(site_status == "old field") %>%  # calculate percent recovery relative to mean of never plowed sites
   left_join(np_origin_means) %>%
   mutate(origin_p = ((P_origin/origin_mean_np) * 100)
   ) %>%
   mutate(P_origin = as.numeric(round(P_origin, 2))) %>%
   mutate( # prep for modeling
     YSA = as.numeric(YSA),
     log_YSA = log(YSA),
     c.YSA = (YSA - mean(YSA)),
     log_origin_p  = log(origin_p),
   ) %>%
   mutate(Field = as.character(Field)) %>%
   mutate(Year = as.factor(as.character(Year))) 
 
 View(ccr_groups_p)
 summary(ccr_groups_p)
 head(ccr_groups_p)
 
 
 p_o_func_np <-  brm(log_origin_p ~  log_YSA * Origin * FunctionalGroup + ( 1 + log_YSA * Origin * FunctionalGroup | Field ) + (1 | Year),
                  data = ccr_groups_p, family=student(), cores = 4, iter=2000, warmup=1000, chains = 4)
 
 
 save(p_o_func_np, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/p_o_func_np.Rdata')
 load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/p_o_func_np.Rdata") 
 
 
 
 summary(p_o_func_np)
 
 
 
 color_scheme_set("darkgray")
 fig_s7a <- pp_check(p_o_func_np)+ xlab( "Relative cover (log % never-plowed)") + ylab("Density") +
   labs(title= "", subtitle = 'a)')+
   theme_classic()+  theme(legend.position= "bottom") # predicted vs. observed values
 
 fig_s7a
 

 fig_s7_legend <- g_legend(fig_s7b)
 
 fig_s7 <- (fig_s7a  +  theme(legend.position= "none") | fig_s7b +  theme(legend.position= "none") ) / (fig_s7_legend)+ plot_layout(heights = c(10,0.75))
 fig_s7
 
 
 ccr_nat$Field<-as.factor(as.character(ccr_nat$Field))
 ccr_nat$Year<-as.factor(as.character(ccr_nat$Year))
 
 # models residuals
 ma<-residuals(p_o_func_np)
 ma<-as.data.frame(ma)
 ar.plot<-cbind(ccr_nat,ma$Estimate)
 
 par(mfrow=c(1,2))
 with(ar.plot, plot(Field, ma$Estimate))
 with(ar.plot, plot(Year, ma$Estimate))
 
 ccr_nat$Field<-as.character(as.factor(ccr_nat$Field))
 
 # for plotting fixed effects
 p_o_func_np_fitted <- cbind(p_o_func_np$data,
                          fitted(p_o_func_np, re_formula = NA
                          )) %>% 
   as_tibble() %>% inner_join(ccr_groups_p %>% distinct(Field, Year, YSA,  origin_p, log_origin_p, log_YSA, Origin, FunctionalGroup),
                              #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
   ) %>% mutate( `Old field` = fct_recode( Field,  "A" = "601", "B" = "600","C" = "10", "D" = "28", "E" = "41",
                                           "F" = "39", "G" = "40",   "H" = "4",    "I" = "44",  "J" = "53",  "K" = "47",  "L" = "21",   "M" = "70",  "N" = "5", "O" = "27",
                                           "P" = "45", "Q" = "32",  "R" = "35",  "S" = "72")) %>%
   mutate( FunctionalGroup = fct_recode( FunctionalGroup,  "Forb" = "F", "Graminoid" = "G","Legume" = "L")) 
 
 
 head(p_o_func_np_fitted)
 
 # fixed effect coefficients
 p_o_np_fixef <- fixef(p_o_func_np)
 
 
 head(p_o_fixef)
 
 
 obs_nest_p_o_func_np <- ccr_groups_p %>% 
   mutate(Field_group = Field) %>%
   group_by(Field_group, Field, Origin, FunctionalGroup) %>% 
   summarise(log_YSA = seq(min(log_YSA), max(log_YSA), length.out = 6 ),
             YSA = seq(min(YSA), max(YSA), length.out = 6 )) %>%
   nest(data = c(Field, YSA, log_YSA, Origin, FunctionalGroup)) %>%
   mutate(predicted = map(data, ~predict(p_o_func_np, newdata= .x, re_formula = ~(1 + log_YSA * Origin * FunctionalGroup | Field) ))) 
 
 
 setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/')
 # save data objects to avoid time of compiling every time
 save(p_o_func_np_fitted, obs_nest_p_o_func_np, file = 'p_o_func_np_fitted_dat.Rdata')
 load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Data/p_o_func_np_fitted_dat.Rdata')
 
 
 fig_6 <- ggplot() + 
   facet_wrap(~  fct_relevel(Origin, "Native", "Introduced")  * fct_relevel(FunctionalGroup, "Graminoid", "Forb", "Legume"), scales = "free" ) +
   geom_hline(yintercept = 100, lty = 2) +
   geom_hline(yintercept = 100, lty = 2) +
   geom_point(data = p_o_func_np_fitted %>% 
                mutate( `Old field` = as.character(`Old field`)) %>%
                arrange(`Old field`)
              ,
              aes(x = YSA, y = origin_p,
                  colour = `Old field`),
              size = 1.2, shape=1, position = position_jitter(width = 2, height=2.5)) +
   geom_line(data = obs_nest_p_o_func_np  %>% unnest(cols = c(data, predicted)) %>% mutate( `Old field` = fct_recode( Field,  "A" = "601", "B" = "600","C" = "10", "D" = "28", "E" = "41",
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
   geom_ribbon(data = p_o_func_np_fitted,
               aes(x = YSA, ymin = exp(Q2.5), ymax = exp(Q97.5)),
               alpha = 0.3) +
   # fixed effect
   geom_line(data = p_o_func_np_fitted,
             aes(x = YSA, y = exp(Estimate)),
             size = 1.5) +
   scale_y_continuous( breaks = c(0,100,200,500)) +
   coord_cartesian( ylim = c(0,500)) +
   scale_color_viridis(discrete = T, option="D")  + 
   scale_fill_viridis(discrete = T, option="D")  + 
   theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                   legend.position="bottom") +   guides(col = guide_legend(ncol = 7)) +
   labs(subtitle= ''
   ) +
   ylab("Relative cover (%)")  + xlab("Years since agricultural abandonment")
 
 # LANDSCAPE 10X11
 fig_6
 
 