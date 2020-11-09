



# Cedar Creek Nestedness

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
splong<- splong %>% 
  unite(gamma.id,Field,Year,sep="_",remove=F)
View(splong)

splong$site_status<-as.factor(as.character(splong$site_status))


#split the dataset into a list of datasets based on the value of experiment
sp.long <- split(splong, splong$Field) 
list2env(sp.long, envir= .GlobalEnv) #split the list into separate dataframes


levels(splong$Field)
colnames(splong)

#create wide subsets of every experiment level

A.D<- A %>% bind_rows(D) 
  
A.w<- A.D %>%
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', Species)) %>% 
  group_by(gamma.id,species2) %>% 
  summarise(pres=n_distinct(pres)) %>%
  spread(species2,pres,fill = 0) %>%
  left_join(A.D) %>%
  select(-Exp,-Species,-Biomass,-YearAbandoned,-pres,-Plot) %>%
  mutate(scale="gamma") %>%
  mutate(id=gamma.id, .keep=c("all") ) %>%
  distinct() 

colnames(A.w)
levels(A.w$site_status)
#write.csv(A.w,"~/Desktop/a.w.csv")

B.D<- B %>% bind_rows(D) 

B.w<- B.D %>%
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', Species)) %>% 
  group_by(gamma.id,species2) %>% 
  summarise(pres=n_distinct(pres)) %>%
  spread(species2,pres,fill = 0) %>%
  left_join(B.D) %>%
  select(-Exp,-Species,-Biomass,-YearAbandoned,-pres,-Plot) %>%
  mutate(id=gamma.id, .keep=c("all") ) %>%
  mutate(scale="gamma") %>%
  distinct() 

levels(B.w$site_status)

#write.csv(B.w,"~/Desktop/b.w.csv")

C.D<- C %>% bind_rows(D) 


C.w<- C.D %>%
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', Species)) %>% 
  group_by(gamma.id,species2) %>% 
  summarise(pres=n_distinct(pres)) %>%
  spread(species2,pres,fill = 0) %>%
  left_join(C.D) %>%
  select(-Exp,-Species,-Biomass,-YearAbandoned,-pres,-Plot) %>%
  mutate(id=gamma.id, .keep=c("all") ) %>%
  mutate(scale="gamma")%>%
  distinct() 

View(C.w)

# D.w <- D %>% 
#   as_tibble() %>% 
#   mutate(species2 = paste0('sp_', Species)) %>% 
#   group_by(gamma.id,species2) %>% 
#   summarise(pres=n_distinct(pres)) %>%
#   spread(species2,pres,fill = 0) %>%
#   left_join(D) %>%
#   select(-Exp,-Species,-Biomass,-YearAbandoned,-pres) %>%
#   mutate(id=gamma.id, .keep=c("all") ) %>%
#   mutate(scale="gamma")%>%
#   distinct() 



field<-read.csv("~/Dropbox/Projects/CCRScale/data/e1_gamma_summary.csv", header=TRUE) %>%
  as_tibble()

View(field)

gamma_dat<- field %>% filter(Field == "A" | Field == "B" | Field == "C" | Field == "D") %>%
  unite(gamma.id,Field,Year,sep="_",remove=F)

View(gamma_dat)

dat <- gamma_dat %>% 
  gather(S_measure,S_biomass,gamma_S_biomass) %>%
  arrange(Field,Year) %>%
  select(Field,Year,gamma.id)

dat$Year<-as.character(as.factor(dat$Year))

View(dat)

beta_pairs <- function(x){
  # function to return the dissimilarities (turnover and nestedness component)
  # for each control treatment comparison in x
  
  # return dataframe with dissimilarities and the treatment magnitude (seed.rich)
  
  # separate out the control and treatment plots
  never.plowed = x %>% 
    filter(site_status == 'never-plowed')
  
  # fix for treatment labels
  old.field = x %>% 
    filter(site_status == 'old field')
  
  out <- tibble()
  if(nrow(old.field)>0){
    for(i in 1:nrow(old.field)){
      beta = beta.pair(bind_rows(never.plowed %>% 
                                   slice(i) %>% 
                                   select(-site_status), 
                                 old.field %>% 
                                   select(-site_status)),
                       index.family = 'jaccard')
      # build the data we want for analysis
      out <- bind_rows(out,
                       tibble(
                         #year = old.field$Year,
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
       #year = NA,
      jtu = NA,
      jne = NA,
      jac = NA,
      group = NA)
  }
  return(out)
}


View(dat )
View(A.w )
colnames(A.w)

A.wide.df <- A.w %>%
  left_join(dat) %>% 
     group_by(Year) %>% 
     nest_legacy(starts_with('sp_'),site_status) %>% tibble()


B.wide.df <- B.w %>%
  left_join(dat) %>% 
  group_by(Year) %>% 
  nest_legacy(starts_with('sp_'),site_status) %>% tibble()

C.wide.df <- C.w %>%
  left_join(dat) %>% 
  group_by(Year) %>% 
  nest_legacy(starts_with('sp_'),site_status) %>% tibble()




# calculate the beta components
A.df <- A.wide.df %>% 
  mutate(beta = purrr::map(data, ~ beta_pairs(.x)))

View(A.df)

B.df <- B.wide.df %>% 
  mutate(beta = purrr::map(data, ~ beta_pairs(.x)))

C.df <- C.wide.df %>% 
  mutate(beta = purrr::map(data, ~ beta_pairs(.x)))


A.beta.df = A.df %>% 
  unnest_legacy(beta) %>%
  mutate(Field = "A") %>%
  unite(col = pw_beta_group,
        c(Field, Year), sep = '_', remove = F) %>% 
  select(-group)

B.beta.df = B.df %>% 
  unnest_legacy(beta) %>%
  mutate(Field = "B") %>%
  unite(col = pw_beta_group,
        c(Field, Year), sep = '_', remove = F) %>% 
  select(-group)

C.beta.df = C.df %>% 
  unnest_legacy(beta) %>%
  mutate(Field = "C") %>%
  unite(col = pw_beta_group,
        c(Field, Year), sep = '_', remove = F) %>% 
  select(-group)


View(gamma_dat)

gamma_dat$Year<-as.character(as.factor(gamma_dat$Year))

beta.df <- bind_rows(A.beta.df,B.beta.df,C.beta.df) %>% left_join(gamma_dat)


View(beta.df)

write.csv(beta.df,"~/Dropbox/Projects/CCRScale/data/beta.df.csv")







# old field nested within never plowed and vice versa

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
splong<- splong %>% 
  #unite(gamma.id,Field,Year,sep="_",remove=F)
unite(status.id,site_status,Year,sep="_",remove=F)
View(splong)

splong$site_status<-as.factor(as.character(splong$site_status))


View(splong )

colnames(splong)

sp_wide<- splong %>%
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', Species)) %>% 
  group_by(status.id,species2) %>% 
  summarise(pres=n_distinct(pres)) %>%
  spread(species2,pres,fill = 0) %>%
  mutate(id=status.id, .keep=c("all") ) %>%
  distinct() 

colnames(sp_wide)

field<-read.csv("~/Dropbox/Projects/CCRScale/data/e1_gamma_summary.csv", header=TRUE) %>%
  as_tibble()

View(field)

gamma_dat<- field %>% filter(Field == "A" | Field == "B" | Field == "C" | Field == "D") %>%
  unite(status.id,site_status,Year,sep="_",remove=F)

View(gamma_dat)

dat <- gamma_dat %>% 
  gather(S_measure,S_biomass,gamma_S_biomass) %>%
  arrange(Year) %>%
  select(Year,status.id,site_status) %>% distinct()

dat$Year<-as.character(as.factor(dat$Year))

View(dat)

beta_pairs <- function(x){
  # function to return the dissimilarities (turnover and nestedness component)
  # for each control treatment comparison in x
  
  # return dataframe with dissimilarities and the treatment magnitude (seed.rich)
  
  # separate out the control and treatment plots
  never.plowed = x %>% 
    filter(site_status == 'never-plowed')
  
  # fix for treatment labels
  old.field = x %>% 
    filter(site_status == 'old field')
  
  out <- tibble()
  if(nrow(old.field)>0){
    for(i in 1:nrow(old.field)){
      beta = beta.pair(bind_rows(never.plowed %>% 
                                   slice(i) %>% 
                                   select(-site_status), 
                                 old.field %>% 
                                   select(-site_status)),
                       index.family = 'jaccard')
      # build the data we want for analysis
      out <- bind_rows(out,
                       tibble(
                         #year = old.field$Year,
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
      #year = NA,
      jtu = NA,
      jne = NA,
      jac = NA,
      group = NA)
  }
  return(out)
}


colnames(sp_wide)
View(sp_wide)
View(dat)

sp.wide.df <- sp_wide %>%
  left_join(dat) %>% 
  group_by(Year) %>% 
  nest_legacy(starts_with('sp_'),site_status) %>% tibble()


View(sp.wide.df)

# calculate the beta components
sp.df <- sp.wide.df %>% 
  mutate(beta = purrr::map(data, ~ beta_pairs(.x)))


View(sp.df)
sp.beta.df = sp.df %>% 
  unnest_legacy(beta) %>%
  #mutate(Field = "A") %>%
  unite(col = pw_beta_group,
        c(Year), sep = '_', remove = F) %>% 
  select(-group)


View(sp.beta.df)

sp.beta.df$Year<- as.numeric(sp.beta.df$Year)

beta.nest.status<-ggplot() +
  #facet_grid(.~Field)+
  geom_point(data = sp.beta.df,
             aes(x = Year, y = jne), alpha=0.6,
             size = 1.3)+
  geom_line(data = sp.beta.df,
               aes(x = Year, y = jne)) +
  #scale_color_manual(values =  c("#A1C720FF","#15983DFF","#00AFBB", "#E7B800", "#FC4E07"))  + 
  #scale_x_continuous(breaks=c(0,25,50,75,89)) +
 # scale_x_continuous(sec.axis = sec_axis("Year"))+
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), 
                        legend.direction = "horizontal", legend.position = "none" )+
  labs(title = "Nestedness") + ylab("Nestedness") + xlab("Year") 

beta.nest.status
