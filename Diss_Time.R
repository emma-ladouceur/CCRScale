


library(gridExtra)
library(ggplot2)
library(reshape2) 
library(MCMCglmm)
library(tidyr)

library(priceTools)
library(tidyverse)
library(betapart)

# Dissimilarity in time




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
  



e1_wide<-splong %>% group_by(Exp,Year,Field,Plot,site_status,Species) %>%
  summarise(Biomass=sum(Biomass)) %>%
  ungroup()%>%
  group_by(Exp,Year,Field,Plot,site_status) %>%
  spread(Species,Biomass) %>%
  ungroup()



e1_select<- e1_wide %>% group_by(Exp,Year,Field,site_status) %>%
  sample_n(10) 

colnames(e1_select)

e1_long<-e1_select %>% gather(Species,Biomass, "Achillea millefolium(lanulosa)":"Viola sp.") %>%
  filter(!is.na(Biomass)) %>% droplevels() %>%
  arrange(Year,Field,Plot)

e1_long$pres<-1


e1_long$YSA<-as.numeric(e1_long$YSA)

plot_prep<-e1_long %>% distinct(Year,Field, Plot,site_status)%>%
  group_by(Year,Field) %>% mutate(y = 1:n()) %>%
  arrange(Year,Field,Plot) %>%
  ungroup()

plot_attr<- plot_prep %>% distinct(Year,Field) %>%  mutate(x = 1:n()) %>%
  left_join(plot_prep) %>%
  rename(group=site_status) %>%
  arrange(Year,Field,Plot) 

View(plot_attr)


ccr_prep <- plot_attr %>% left_join(e1_long) %>%
  mutate(Plotn = y) 


View(ccr_prep)


splong<-unite_(ccr_prep, "plot.site.year.id", c("Field","Year"), remove=FALSE)
splong<-unite_(splong, "sample.id", c("Field","Year","Plotn"), remove=FALSE)

View(splong)


splong$Year<-as.numeric(as.character(splong$Year))
splong$plot.site.year.id<-as.character(splong$plot.site.year.id)

splong$pres<-as.numeric(splong$pres)

#create index's
index<-paste(splong$Field, splong$plot.site.year.id)
sindex<-as.character(splong$Field)
yindex<-as.character(splong$Year)
uindex<-sort(unique(index))
usindex<-sort(unique(sindex))
uyindex<-sort(unique(yindex))


View(uyindex)


#progressive
all_lst<-NULL
n<-1

for (i in 1:length(usindex)){
  #select all instances from a site
  subs<-which(sindex==usindex[i])
  uindex_small<-sort(unique(splong[subs,]$Year))

  for(j in 2:length(uindex_small)) {
    subs2<-which(as.numeric(as.character(yindex[subs]))%in%
                   (uindex_small[j]-c(0,1)))

    #table(splong[subs[subs2],]$plot.site.year.id)
    if(length(unique(splong[subs[subs2],]$plot.site.year.id))==2) {
      all_lst[[n]]<-splong[subs[subs2],]
      names(all_lst)[n]<-paste(usindex[i], uindex_small[j], sep="_")
      n<-n+1
    }
  }
  print(i/length(usindex))
}



View(all_lst)

df.list<-bind_rows(all_lst, .id = "column_label")


spr <- splong %>% select(sample.id,Year) %>% distinct()


spr$Year<-as.numeric(spr$Year)


summary(splong)

ccr.wide <- df.list %>%
                      select(-plot.site.year.id,-Year,-x,-Exp,-group,-y,-site_status,-Biomass,-Plotn) %>%
                      mutate(species2 = paste0('sp_', Species)) %>% 
                      select(-Species) %>%  
                      spread(key = species2, value = pres) %>%
                      replace(is.na(.), 0) %>%
                      left_join(spr)# %>%
                     #column_to_rownames(var = "sample.id") 
                  

View(ccr.wide)
colnames(ccr.wide)


wide.df <- ccr.wide %>%
  group_by(column_label,Plot) %>% 
  nest_legacy(starts_with('sp_'),Year) %>% 
  tibble()


View(wide.df)


beta_pairs <- function(x){
  # function to return the dissimilarities (turnover and nestedness component)
  # for each control treatment comparison in x
  
  # return dataframe with dissimilarities and the treatment magnitude 
  
  # compare min year within subset to max year within subset
  min.year = x %>% 
    filter(Year == min(Year))
  
  # identify max year
  max.year = x %>% 
    filter(Year == max(Year) )
  
  out <- tibble()
  if(nrow(max.year)>0){
    for(i in 1:nrow(max.year)){
      beta = beta.pair(bind_rows(min.year %>% 
                                   slice(i) %>% 
                                   select(-Year), 
                                 max.year %>% 
                                   select(-Year)),
                       index.family = 'jaccard')
      # build the data we want for analysis
      out <- bind_rows(out,
                       tibble(
                         MaxYear = max.year$Year,
                         MinYear = min.year$Year,
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


beta.wide.df <- wide.df %>% 
  mutate(beta = purrr::map(data, ~ beta_pairs(.x)))



beta.df = beta.wide.df %>% 
  unnest_legacy(beta)  %>%
  unite(col = pw_beta_group,
        c(MinYear,MaxYear), sep = '_', remove = F) %>% 
  select(-group,-data) %>%
  separate(column_label, c("Field", "Group"), sep="_") %>%
  separate(pw_beta_group, c("MinGrp", "MxGrp"), sep="_", remove=FALSE) %>%
  filter(!MinGrp == MxGrp)
  

View(beta.df)



beta.a<-beta.df %>% filter(Field=="A")
beta.b<-beta.df %>% filter(Field=="B")
beta.c<-beta.df %>% filter(Field=="C")
beta.d<-beta.df %>% filter(Field=="D")

View(beta.a)

beta.plot<-ggplot() +
  #facet_grid(.~Field)+
  geom_point(data = beta.df,
             aes(x = MaxYear, y = jtu,
                 colour = `Field`, shape=`Field`), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_smooth(data = beta.df,
              aes(x = MaxYear, y = jtu,
                  colour = `Field`,group=`Field`))+
  stat_summary(data = beta.a,
               aes(x = MaxYear, y = jtu,color=Field,shape=Field),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,size=0.25)+
  stat_summary(data = beta.a,
               aes(x = MaxYear, y = jtu,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  stat_summary(data = beta.b,
               aes(x = MaxYear, y = jtu,color=Field,shape=Field),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,size=0.25)+
  stat_summary(data = beta.c,
               aes(x = MaxYear, y = jtu,color=Field,shape=Field),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,size=0.25)+
  stat_summary(data = beta.c,
               aes(x = MaxYear, y = jtu,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  stat_summary(data = beta.b,
               aes(x = MaxYear, y = jtu,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  stat_summary(data = beta.d,
               aes(x = MaxYear, y = jtu,color=Field,shape=Field),fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="pointrange", shape=16,size=0.25)+
  stat_summary(data = beta.d,
               aes(x = MaxYear, y = jtu,color=Field,group=Field),fun = mean, geom="line",size=0.55) +
  scale_color_manual(values =  c("#E7B800", "#972C8DFF" ,"#00AFBB","#15983DFF", "#FC4E07"))  + 
  #scale_x_continuous(breaks=c(0,25,50,75,89)) +
  #scale_x_continuous(sec.axis = sec_axis("Year"))+
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), 
                        legend.direction = "horizontal", legend.position = "bottom" )+
  labs(title = "Species Turnover in Time") + ylab("Turnover") + xlab("Year") 

beta.plot



