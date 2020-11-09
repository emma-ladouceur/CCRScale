
# Cedar Creek Beta Diversity

# Libraries
library(tidyverse)
library(betapart)
library(bayesplot)
library(patchwork)
library(ggplot2)
library(purrr)
library(vegan)

# species data
splong <- read.csv("~/Dropbox/Projects/CCRScale/E_001/e001.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

splong <- splong %>%  filter(!Species %in% c( "Miscellaneous litter","Mosses & lichens","Mosses & lichens 2") ) %>%
  droplevels()


colnames(e1_biomass)
splong$NTrt<-as.factor(as.character(splong$NTrt))
levels(splong$NAdd)

splong$NAdd<-as.factor(as.character(splong$NAdd))
levels(splong$NAdd)

splong$NitrAdd<-as.factor(as.character(splong$NitrAdd))
levels(splong$NitrAdd)


splong <- splong %>% 
  filter(NAdd %in% c("0"),
         NitrAdd %in% c("0"),) %>%
  droplevels() %>%
  mutate(
    YearAbandoned = case_when(
      Field == "A" ~ "1968",
      Field == "B" ~ "1957",
      Field == "C" ~ "1934",
      Field == "D" ~ "never-plowed"),
    site_status = case_when(
      Field == "A" ~ "old field",
      Field == "B" ~ "old field",
      Field == "C" ~ "old field",
      Field == "D" ~ "never-plowed"
    )) %>% select(-NTrt,-NAdd,-NitrAdd,-NAtm.NAdd)



View(splong)


# create a presence count column
splong$pres<-1
splong<- splong %>% unite(alpha.id,Field,Plot,Year,sep="_",remove=F) %>%
  unite(gamma.id,Field,Year,sep="_",remove=F)
View(splong)

#split the dataset into a list of datasets based on the value of experiment
sp.long <- split(splong, splong$Field) 
list2env(sp.long, envir= .GlobalEnv) #split the list into separate dataframes


levels(splong$Field)
colnames(splong)

#create wide subsets of every experiment level
A.alpha.w<- A %>% 
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', Species)) %>% 
  group_by(alpha.id,species2) %>% 
  summarise(pres=n_distinct(pres)) %>%
  spread(species2,pres,fill = 0) %>%
  left_join(A) %>%
  select(-Exp,-Species,-Biomass,-YearAbandoned,-site_status,-pres) %>%
  mutate(scale="alpha") %>%
  mutate(id=alpha.id, .keep=c("all") )  %>% distinct()

A.w<- A %>% 
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', Species)) %>% 
  group_by(gamma.id,species2) %>% 
  summarise(pres=n_distinct(pres)) %>%
  spread(species2,pres,fill = 0) %>%
  left_join(A) %>%
  select(-Exp,-Species,-Biomass,-YearAbandoned,-site_status,-pres) %>%
  mutate(scale="gamma") %>%
  mutate(id=gamma.id, .keep=c("all") ) %>%
  distinct() %>%
  bind_rows(A.alpha.w) 
 

write.csv(A.w,"~/Desktop/a.w.csv")


B.alpha.w<- B %>% 
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', Species)) %>% 
  group_by(alpha.id,species2) %>% 
  summarise(pres=n_distinct(pres)) %>%
  spread(species2,pres,fill = 0) %>%
  left_join(B) %>%
  select(-Exp,-Species,-Biomass,-YearAbandoned,-site_status,-pres) %>%
  mutate(scale="alpha") %>%
  mutate(id=alpha.id, .keep=c("all") ) %>%
  distinct()

B.w<- B %>% 
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', Species)) %>% 
  group_by(gamma.id,species2) %>% 
  summarise(pres=n_distinct(pres)) %>%
  spread(species2,pres,fill = 0) %>%
  left_join(B) %>%
  select(-Exp,-Species,-Biomass,-YearAbandoned,-site_status,-pres) %>%
  mutate(id=gamma.id, .keep=c("all") ) %>%
  mutate(scale="gamma") %>%
  distinct() %>%
  bind_rows(B.alpha.w)


write.csv(B.w,"~/Desktop/b.w.csv")

C.alpha.w<- C %>% 
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', Species)) %>% 
  group_by(alpha.id,species2) %>% 
  summarise(pres=n_distinct(pres)) %>%
  spread(species2,pres,fill = 0) %>%
  left_join(C) %>%
  select(-Exp,-Species,-Biomass,-YearAbandoned,-site_status,-pres)%>%
  mutate(scale="alpha")%>%
  mutate(id=gamma.id, .keep=c("all") ) %>%
  distinct() 

C.w<- C %>% 
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', Species)) %>% 
  group_by(gamma.id,species2) %>% 
  summarise(pres=n_distinct(pres)) %>%
  spread(species2,pres,fill = 0) %>%
  left_join(C) %>%
  select(-Exp,-Species,-Biomass,-YearAbandoned,-site_status,-pres) %>%
  mutate(id=gamma.id, .keep=c("all") ) %>%
  mutate(scale="gamma")%>%
  distinct() %>%
  bind_rows(C.alpha.w)

D.alpha.w<- D %>% 
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', Species)) %>% 
  group_by(alpha.id,species2) %>% 
  summarise(pres=n_distinct(pres)) %>%
  spread(species2,pres,fill = 0) %>%
  left_join(D) %>%
  select(-Exp,-Species,-Biomass,-YearAbandoned,-site_status,-pres)%>%
  mutate(scale="alpha") %>%
  mutate(id=gamma.id, .keep=c("all") ) %>%
  distinct() 

D.w <- D %>% 
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', Species)) %>% 
  group_by(gamma.id,species2) %>% 
  summarise(pres=n_distinct(pres)) %>%
  spread(species2,pres,fill = 0) %>%
  left_join(D) %>%
  select(-Exp,-Species,-Biomass,-YearAbandoned,-site_status,-pres) %>%
  mutate(id=gamma.id, .keep=c("all") ) %>%
  mutate(scale="gamma") %>%
  distinct() %>%
  bind_rows(D.alpha.w)


View(D.w)

plot<-read.csv("~/Dropbox/Projects/CCRScale/data/e1_alpha_summary.csv", header=TRUE) %>%
  as_tibble()

alpha_dat<- plot %>% filter(Field == "A" | Field == "B" | Field == "C" | Field == "D") %>%
  unite(alpha.id,Field,Plot,Year,sep="_",remove=F) %>%
  unite(gamma.id,Field,Year,sep="_",remove=F)

field<-read.csv("~/Dropbox/Projects/CCRScale/data/e1_gamma_summary.csv", header=TRUE) %>%
  as_tibble()

View(field)

gamma_dat<- field %>% filter(Field == "A" | Field == "B" | Field == "C" | Field == "D") %>%
  unite(gamma.id,Field,Year,sep="_",remove=F)

View(gamma_dat)

dat <- alpha_dat %>% left_join(gamma_dat)%>%
  gather(S_measure,S_biomass,alpha_S_biomass,gamma_S_biomass ) %>%
  arrange(Field,Year,Plot) %>%
  select(Field,Year,Plot)

dat$Year<-as.character(as.factor(dat$Year))
dat$Plot<-as.character(as.factor(dat$Plot))
View(dat)

plot2 %>% 
  distinct(trt)

beta_pairs <- function(x){
  # function to return the dissimilarities (turnover and nestedness component)
  # for each control treatment comparison in x
  
  # return dataframe with dissimilarities and the treatment magnitude (seed.rich)
  
  # separate out the control and treatment plots
  gamma.s = x %>% 
    filter(scale=='gamma')
  
  # fix for treatment labels
  alpha.s = x %>% 
    filter(scale=='alpha')
  
  out <- tibble()
  if(nrow(alpha.s)>0){
    for(i in 1:nrow(alpha.s)){
      beta = beta.pair(bind_rows(gamma.s %>% 
                                   slice(i) %>% 
                                   select(-scale), 
                                 alpha.s %>% 
                                   select(-scale)),
                       index.family = 'jaccard')
      # build the data we want for analysis
      out <- bind_rows(out,
                       tibble(
                         plot = alpha.s$Plot,
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
     # plot = NA,
      #jtu = NA,
      #jne = NA,
      jac = NA,
      group = NA)
  }
  return(out)
}


View(dat )
View(A.w )

wide.df <- bind_rows(
  # 1) Field A 
  left_join(A.w, dat)  %>% 
    distinct() %>%
    group_by(Field,Year,Plot) %>% 
    nest_legacy(starts_with('sp_'),scale),
  # 2) Field B
  left_join(B.w, dat) %>% 
    distinct() %>%
    group_by(Field,Year,Plot) %>% 
    nest_legacy(starts_with('sp_'),scale),
  # 3) Field C
  left_join(C.w, dat) %>% 
    distinct() %>%
    group_by(Field,Year,Plot) %>% 
    nest_legacy(starts_with('sp_'),scale),
  # 4) Field D
  left_join(D.w, dat) %>% 
    distinct() %>%
    group_by(Field,Year,Plot) %>% 
    nest_legacy(starts_with('sp_'),scale)
) %>% tibble()

View(wide.df)

colnames(wide.df)


# calculate the beta components
wide.df <- wide.df %>% 
  mutate(beta = purrr::map(data, ~ beta_pairs(.x)))

View(wide.df)


beta.df = wide.df %>% 
  unnest_legacy(beta) %>%
  unite(col = pw_beta_group,
        c(Field,Year,Plot ,group), sep = '_', remove = F) %>% 
  select(-group,-data)

View(beta.df)

write.csv(beta.df,"~/Dropbox/Projects/CCRScale/data/beta.df.csv")

beta<-read.csv("~/Dropbox/Projects/CCRScale/data/beta.df.csv", header=TRUE) %>%
  as_tibble()

beta$Experiment<-beta$Experiment_
beta$fyr.trt<-as.factor(beta$yr.trt)
beta$seed.rich<-as.numeric(as.character(beta$seed.rich))
beta$site<-as.factor(beta$site)
beta$block<-as.factor(beta$block)
beta<-beta %>%
  drop_na() 

nrow(beta)
nrow(beta2)
View(beta2)


# Load model objects
load("./Model Fits/betat.Rdata") # object name: turnover.zoib
load("./Model Fits/betan.Rdata") # object name: nested.zib

# turnover.zoib <- brm(jtu ~  seed.rich + (seed.rich | Experiment/site/block/fyr.trt),
#                          family=zero_one_inflated_beta(),
#                          data = beta,
#                          inits = '0',
#                          cores = 4, chains = 4)
# 

# save(turnover.zoib, file = './Model_fits/betat.Rdata')

# Turnover model
summary(turnover.zoib)

plot(turnover.zoib) 

# Figure S1 d
pp_check(turnover.zoib) + theme_classic()

betat_fitted <- cbind(turnover.zoib$data,
                      fitted(turnover.zoib, re_formula = NA)) %>% 
  as_tibble() 


betat_fixef <- fixef(turnover.zoib)

betat_exp_coef <- coef(turnover.zoib)

betat_exp_coef2 <-  bind_cols(betat_exp_coef$Experiment[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       Experiment = rownames(betat_exp_coef$Experiment[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              betat_exp_coef$Experiment[,,'seed.rich'] %>% 
                                as_tibble() %>% 
                                mutate(Slope = Estimate,
                                       Slope_lower = Q2.5,
                                       Slope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  inner_join(beta %>% 
               group_by(Experiment) %>% 
               summarise(xmin = min(seed.rich),
                         xmax = max(seed.rich)),
             by = 'Experiment')


# Nestedness model

# nested.zib <- brm(jne ~  seed.rich + (seed.rich | Experiment/site/block/fyr.trt),
#                       family=zero_inflated_beta(),
#                       data = beta,
#                       inits = '0',
#                       cores = 4, chains = 4)
# 

# save(nested.zib, file = './Model_fits/betan.Rdata')

summary(nested.zib)
plot(nested.zib) 

# Figure S1 e
pp_check(nested.zib)+ theme_classic()

# residuals
n1<-residuals(nested.zib)
n1<-as.data.frame(n1)

rn.plot<-cbind(beta,n1$Estimate)

par(mfrow=c(2,3))
with(rn.plot, plot(Experiment, n1$Estimate))
with(rn.plot, plot(site, n1$Estimate))
with(rn.plot, plot(block, n1$Estimate))
with(rn.plot, plot(fyr.trt, n1$Estimate))
with(rn.plot, plot(pw_beta_group, n1$Estimate))


# make sure to detach plyr at top

# fixed effects
betan_fitted <- cbind(nested.zib$data,
                      fitted(nested.zib, re_formula = NA)) %>% 
  as_tibble() 

betan_fixef <- fixef(nested.zib)

betan_exp_coef <- coef(nested.zib)
betan_exp_coef 

betad<-beta%>%distinct(Experiment,seed.rich)
View(betad)
# this gets us the coefficients for the varying intercepts and slopes
betan_exp_coef2 <-  bind_cols(betan_exp_coef$Experiment[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       Experiment = rownames(betan_exp_coef$Experiment[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              betan_exp_coef$Experiment[,,'seed.rich'] %>% 
                                as_tibble() %>% 
                                mutate(Slope = Estimate,
                                       Slope_lower = Q2.5,
                                       Slope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  inner_join(beta %>% 
               group_by(Experiment) %>% 
               summarise(xmin = min(seed.rich),
                         xmax = max(seed.rich)),
             by = 'Experiment')


# use grid_arrange_shared_legend function at the beginning of Main_Analysis.R
# to create figures


# beta plots

colnames(betat_exp_coef2)
colnames(betan_exp_coef2)
betat_exp_coef2$Model<-'Turnover'
betan_exp_coef2$Model<-'Nestedness'

#fixed
betat_fixef_df<-as.data.frame(betat_fixef)
betan_fixef_df<-as.data.frame((betan_fixef))
betat_fixef_df$Model<-'Turnover'
betan_fixef_df$Model<-'Nestedness'
fixef.all<-bind_rows(betat_fixef_df,betan_fixef_df)
fixef.all


betat_exp_coef2$Experiment<-plyr::revalue(betat_exp_coef2$Experiment, c("ASGA_Michigan"="Michigan", "California_Invade"="California.1","California_Prop_Limi"="California.2","CCR_04"="Cedar.Creek.4","CCR_093"="Cedar.Creek.93","Germany_Montane"="Montane","Halle"="Halle","Jena"="Jena","Jena2"="Jena.2","Kansas_KUFS_LTER_Hay_Meadow_Exp_2"="Kansas.Hay.Meadow","Kansas_KUFS_LTER_Old_Field_Exp_1"="Kansas.Old.Field","Texas_Temple_Prarie"="Texas.Temple.Prairie"))
betan_exp_coef2$Experiment<-plyr::revalue(betan_exp_coef2$Experiment, c("ASGA_Michigan"="Michigan", "California_Invade"="California.1","California_Prop_Limi"="California.2","CCR_04"="Cedar.Creek.4","CCR_093"="Cedar.Creek.93","Germany_Montane"="Montane","Halle"="Halle","Jena"="Jena","Jena2"="Jena.2","Kansas_KUFS_LTER_Hay_Meadow_Exp_2"="Kansas.Hay.Meadow","Kansas_KUFS_LTER_Old_Field_Exp_1"="Kansas.Old.Field","Texas_Temple_Prarie"="Texas.Temple.Prairie"))

betat_exp_coef2$Experiment<-factor(as.character(betat_exp_coef2$Experiment))
betan_exp_coef2$Experiment<-factor(as.character(betan_exp_coef2$Experiment))

betat_fitted$Experiment<-plyr::revalue(betat_fitted$Experiment, c("ASGA_Michigan"="Michigan", "California_Invade"="California.1","California_Prop_Limi"="California.2","CCR_04"="Cedar.Creek.4","CCR_093"="Cedar.Creek.93","Germany_Montane"="Montane","Halle"="Halle","Jena"="Jena","Jena2"="Jena.2","Kansas_KUFS_LTER_Hay_Meadow_Exp_2"="Kansas.Hay.Meadow","Kansas_KUFS_LTER_Old_Field_Exp_1"="Kansas.Old.Field","Texas_Temple_Prarie"="Texas.Temple.Prairie"))
betat_fitted$Experiment<-factor(as.character(betat_fitted$Experiment))
betan_fitted$Experiment<-plyr::revalue(betan_fitted$Experiment, c("ASGA_Michigan"="Michigan", "California_Invade"="California.1","California_Prop_Limi"="California.2","CCR_04"="Cedar.Creek.4","CCR_093"="Cedar.Creek.93","Germany_Montane"="Montane","Halle"="Halle","Jena"="Jena","Jena2"="Jena.2","Kansas_KUFS_LTER_Hay_Meadow_Exp_2"="Kansas.Hay.Meadow","Kansas_KUFS_LTER_Old_Field_Exp_1"="Kansas.Old.Field","Texas_Temple_Prarie"="Texas.Temple.Prairie"))
betan_fitted$Experiment<-factor(as.character(betan_fitted$Experiment))


# Figure S4 b) : Turnover coefficients
tc<-ggplot() + 
  geom_point(data = betat_exp_coef2, aes(x = Experiment, y = Slope,colour = Experiment),size = 2) +
  geom_errorbar(data = betat_exp_coef2, aes(x = Experiment,ymin = Slope_lower,
                                            ymax = Slope_upper,colour = Experiment),
                width = 0, size = 1) + 
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = filter(fixef.all, Model=='Turnover'),
             aes(yintercept = Estimate[2]), size = 1.2) +
  geom_rect(data = filter(fixef.all, Model=='Turnover'),
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[2], ymax = Q97.5[2]),
            alpha = 0.3) +
  labs(x = 'Experiment',
       y = 'Change in Turnover / species of seed added', subtitle= "b) ") +
  scale_colour_manual(values = c( "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", "#0C5BB0FF", "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#FA6B09FF","#A1C720FF","#9A703EFF" ))+
  scale_x_discrete(limits = rev(levels(betat_exp_coef2$Experiment)))+
  coord_flip() + 
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom",axis.title.y=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks.y=element_blank())

# Figure S 4 d) : Nestedness Coefficients
nc<-ggplot() + 
  geom_point(data = betan_exp_coef2, aes(x = Experiment, y = Slope,colour = Experiment),size = 2) +
  geom_errorbar(data = betan_exp_coef2, aes(x = Experiment,ymin = Slope_lower,
                                            ymax = Slope_upper,colour = Experiment),
                width = 0, size = 1) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = filter(fixef.all, Model=='Nestedness'),
             aes(yintercept = Estimate[2]), size = 1.2) +
  geom_rect(data = filter(fixef.all, Model=='Nestedness'),
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[2], ymax = Q97.5[2]),
            alpha = 0.3) +
  labs(x = 'Experiment',
       y = 'Change in Nestedness / species of seed added', subtitle = "d) ") +
  scale_colour_manual(values = c( "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", "#0C5BB0FF", "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#FA6B09FF","#A1C720FF","#9A703EFF" ))+
  scale_x_discrete(limits = rev(levels(betan_exp_coef2$Experiment)))+coord_flip() + 
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom",#axis.title.y=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks.y=element_blank())

nc
colnames(betat_fitted)

betat_exp_coef2
betat.line<-betat_exp_coef2 %>% filter(xmin < xmax)
betat.point<-betat_exp_coef2 %>% filter(xmin==xmax)

betan.line<-betan_exp_coef2 %>% filter(xmin < xmax)
betan.point<-betan_exp_coef2 %>% filter(xmin==xmax)

# assign colors for each line and point
color_line <- c("#EC579AFF", "#149BEDFF","#8F2F8BFF")
color_point <- c("#FA6B09FF","#EE0011FF" , "#15983DFF","#A1C720FF","#0C5BB0FF","#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#9A703EFF" )


betat.point

# Figure S4 a) : Turnover Regression
btr <-
  ggplot() +
  geom_point(data = betat_fitted,
             aes(x = seed.rich, y = jtu,
                 colour = Experiment), #alpha=0.4,
             size = 1, position = position_jitter(width = 0.1)) +
  geom_segment(data = betat.line,
               aes(x = xmin,
                   xend = xmax,
                   y = plogis(Intercept + Slope * xmin),
                   yend = plogis(Intercept + Slope * xmax),
                   colour = Experiment),
               #group = Experiment,
               colour = color_line,
               size = 1.2) +
  geom_point(data = betat.point,
             aes(x = xmax, y = plogis(Intercept + Slope)), 
             fill=color_point,shape=21, size=3.5,stroke=1,
             color="black") +
  geom_ribbon(data = betat_fitted,
              aes(x = seed.rich, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  geom_line(data = betat_fitted,
            aes(x = seed.rich, y = Estimate),
            size = 1.5) +
  labs(x = 'Number of species of seed added',
       y = 'Turnover', title= 'Turnover', subtitle="a)") +
  scale_colour_manual(values = c( "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", "#0C5BB0FF", "#8F2F8BFF","#F9B90AFF" , "#16A08CFF" ,"#6A7F93FF","#FA6B09FF","#A1C720FF","#9A703EFF" ))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.position="bottom")
btr

summary(turnover.zoib)
summary(nested.zib)

View(betat_exp_coef2)

# Figure S4 c) : Nestedness Regression
bnr <- ggplot() +
  geom_point(data = betan_fitted,
             aes(x = seed.rich, y = jne,
                 colour = Experiment),
             size = 1.2, position = position_jitter(width = 0.1)) +
  geom_segment(data = betan.line,
               aes(x = xmin,
                   xend = xmax,
                   y = plogis(Intercept + Slope * xmin),
                   yend = plogis(Intercept + Slope * xmax)),
               #group = Experiment,
               colour = color_line,
               size = 1.2) +
  geom_point(data = betan.point,
             aes(x = xmax, y = plogis(Intercept + Slope)), 
             fill=color_point,shape=21, size=3.5,stroke=1,
             color="black") +
  geom_ribbon(data = betan_fitted,
              aes(x = seed.rich, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  geom_line(data = betan_fitted,
            aes(x = seed.rich, y = Estimate),
            size = 1.5) +
  labs(x = 'Number of species of seed added',
       y = 'Nestedness', title= ' Nestedness', subtitle="c)") +
  scale_colour_manual(values = c( "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", "#0C5BB0FF", "#8F2F8BFF","#F9B90AFF" , "#16A08CFF" ,"#6A7F93FF","#FA6B09FF","#A1C720FF","#9A703EFF" ))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.position="bottom")
bnr



# extract legend
# https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
# make legend an object
b.legend<-g_legend(nc)

# use patchwork to arrange figures with single legend
( btr+ theme(legend.position="none") | tc+ theme(legend.position="none")  ) / (bnr+ theme(legend.position="none")| nc+ theme(legend.position="none") ) /(b.legend)  +
  plot_layout(heights = c(10,10,2.3))



