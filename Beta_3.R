
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
splong<- splong %>% 
  unite(gamma.id,Field,Year,sep="_",remove=F)
View(splong)

#split the dataset into a list of datasets based on the value of experiment
sp.long <- split(splong, splong$Field) 
list2env(sp.long, envir= .GlobalEnv) #split the list into separate dataframes


levels(splong$Field)
colnames(splong)

#create wide subsets of every experiment level


A.w<- A %>% 
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', Species)) %>% 
  group_by(gamma.id,species2) %>% 
  summarise(pres=n_distinct(pres)) %>%
  spread(species2,pres,fill = 0) %>%
  left_join(A) %>%
  select(-Exp,-Species,-Biomass,-YearAbandoned,-site_status,-pres,-Plot) %>%
  mutate(scale="gamma") %>%
  mutate(id=gamma.id, .keep=c("all") ) %>%
  distinct() 
 
View(A.w)
colnames(A.w)


B.w<- B %>% 
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', Species)) %>% 
  group_by(gamma.id,species2) %>% 
  summarise(pres=n_distinct(pres)) %>%
  spread(species2,pres,fill = 0) %>%
  left_join(B) %>%
  select(-Exp,-Species,-Biomass,-YearAbandoned,-site_status,-pres,-Plot) %>%
  mutate(id=gamma.id, .keep=c("all") ) %>%
  mutate(scale="gamma") %>%
  distinct() 




C.w<- C %>% 
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', Species)) %>% 
  group_by(gamma.id,species2) %>% 
  summarise(pres=n_distinct(pres)) %>%
  spread(species2,pres,fill = 0) %>%
  left_join(C) %>%
  select(-Exp,-Species,-Biomass,-YearAbandoned,-site_status,-pres,-Plot) %>%
  mutate(id=gamma.id, .keep=c("all") ) %>%
  mutate(scale="gamma")%>%
  distinct() 



D.w <- D %>% 
  as_tibble() %>% 
  mutate(species2 = paste0('sp_', Species)) %>% 
  group_by(gamma.id,species2) %>% 
  summarise(pres=n_distinct(pres)) %>%
  spread(species2,pres,fill = 0) %>%
  left_join(D) %>%
  select(-Exp,-Species,-Biomass,-YearAbandoned,-site_status,-pres,-Plot) %>%
  mutate(id=gamma.id, .keep=c("all") ) %>%
  mutate(scale="gamma")%>%
  distinct() 


View(D.w)


field<-read.csv("~/Dropbox/Projects/CCRScale/data/e1_gamma_summary.csv", header=TRUE) %>%
  as_tibble()

View(field)

dat<- field %>% filter(Field == "A" | Field == "B" | Field == "C" | Field == "D") %>%
  unite(gamma.id,Field,Year,sep="_",remove=F) %>%
  arrange(Field,Year) %>%
  select(Field,Year)

View(dat)


dat$Year<-as.character(as.factor(dat$Year))

View(dat)

plot2 %>% 
  distinct(trt)

beta_pairs <- function(x){
  # function to return the dissimilarities (turnover and nestedness component)
  # for each control treatment comparison in x
  
  # return dataframe with dissimilarities and the treatment magnitude 
  
  # compare field D to never plowed sites
  never.plowed = x %>% 
    filter(Field=='D')
  
  # fix for treatment labels
  old.field = x %>% 
    filter(Field=='C')
  
  out <- tibble()
  if(nrow(old.field)>0){
    for(i in 1:nrow(old.field)){
      beta = beta.pair(bind_rows(never.plowed %>% 
                                   slice(i) %>% 
                                   select(-Field), 
                                 old.field %>% 
                                   select(-Field)),
                       index.family = 'jaccard')
      # build the data we want for analysis
      out <- bind_rows(out,
                       tibble(
                         Field = old.field$Field,
                         #jtu = as.matrix(beta$beta.jtu)[-1,1],
                         #jne = as.matrix(beta$beta.jne)[-1,1],
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
colnames(A.w )

gamma_fields<- bind_rows(A.w,B.w,C.w,D.w) %>%
  replace(is.na(.), 0)

View(gamma_fields)

View(dat)



wide.df <- 
  left_join(gamma_fields, dat)  %>% 
    distinct() %>%
    group_by(Year) %>% 
    nest_legacy(starts_with('sp_'),Field) %>% 
  tibble()



View(wide.df)

colnames(wide.df)


# calculate the beta components
beta.wide.df.C <- wide.df %>% 
  mutate(beta = purrr::map(data, ~ beta_pairs(.x)))


beta.df.C = beta.wide.df.C %>% 
  unnest_legacy(beta)  %>%
   unite(col = pw_beta_group,
         c(Year,Field), sep = '_', remove = F) %>% 
   select(-group)


beta.df <- bind_rows(beta.df.A,beta.df.B,beta.df.C) %>%
  drop_na()
  
View(beta.df)
View(dat)

splong$Year<-as.numeric(splong$Year)
splong$YearAbandoned<-as.numeric(splong$YearAbandoned)
splong$YSA<-splong$Year-splong$YearAbandoned

field.info<-splong %>% select(Field,Year,YearAbandoned,YSA) %>%
  distinct() 
  
beta.df$Year<-as.numeric(beta.df$Year)
beta.df<-beta.df %>% left_join(field.info)

View(beta.df)

write.csv(beta.df,"~/Dropbox/Projects/CCRScale/data/beta.df.csv")

