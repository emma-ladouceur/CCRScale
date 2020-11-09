

library(tidyverse)
library(patchwork)
library(iNEXT)
library(ggplot2)


e1_biomass <- read.csv("~/Dropbox/Projects/CCRScale/E_001/e001.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


# Exp e1


distinct(e1_biomass,Species) %>% arrange(Species)

oldfields_biomass <- e1_biomass %>% filter(!Species %in% c( "Miscellaneous litter", "Mosses & lichens","Mosses & lichens 2") ) %>%
  droplevels()


View(e1_biomass)
colnames(e1_biomass)
e1_biomass$NTrt<-as.factor(as.character(e1_biomass$NTrt))
levels(e1_biomass$NAdd)

e1_biomass$NAdd<-as.factor(as.character(e1_biomass$NAdd))
levels(e1_biomass$NAdd)

e1_biomass$NitrAdd<-as.factor(as.character(e1_biomass$NitrAdd))
levels(e1_biomass$NitrAdd)


e1_summary <- e1_biomass %>% 
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

e1_summary$Year<-as.numeric(e1_summary$Year)
e1_summary$YearAbandoned<-as.numeric(e1_summary$YearAbandoned)

e1<-e1_summary %>% 
  mutate(YSA = (Year-YearAbandoned)) %>%
  mutate( YSA= ifelse(is.na(YSA) & site_status=="never-plowed","never-plowed",YSA)) %>%
  select(Exp,Year,YSA,Field,Plot,Species,Biomass,site_status) %>%
  rename(Transect=Plot)%>%
  arrange(YSA,Field,Transect)


distinct(e1,YSA,Field,Transect)
distinct(oldfields_summary,YSA,Field,Transect)
View(e1)
View(oldfields_summary)

e1_wide<-e1 %>% group_by(Exp,Year,YSA,Field,Transect,site_status,Species) %>%
  summarise(Biomass=sum(Biomass)) %>%
  ungroup()%>%
  group_by(Exp,Year,YSA,Field,Transect,site_status) %>%
  spread(Species,Biomass) %>%
  ungroup()



e1_select<- e1_wide %>% group_by(Exp,Year,YSA,Field,site_status) %>%
  sample_n(10) 


e1_long<-e1_select %>% gather(Species,Biomass, "Achillea millefolium(lanulosa)":"Viola sp.") %>%
  filter(!is.na(Biomass)) %>% droplevels() %>%
  arrange(YSA,Field,Transect)

e1_long$pres<-1

e1_long$YSA<-as.numeric(e1_long$YSA)

plot_prep<-e1_long %>% distinct(Year,YSA,Field, Transect,site_status)%>%
  group_by(YSA,Field) %>% mutate(y = 1:n()) %>%
  arrange(YSA,Field,Transect) %>%
  ungroup()

plot_attr<- plot_prep %>% distinct(Year,Field) %>%  mutate(x = 1:n()) %>%
  left_join(plot_prep) %>%
  rename(group=site_status) %>%
  arrange(YSA,Field,Transect) 

View(plot_attr)


ccr_prep <- plot_attr %>% left_join(e1_long) 


View(ccr_prep)




  
ccr_comm_prep <- ccr_prep %>%  group_by(group,x,y,Year,YSA,Field,Transect,Species) %>%
  summarise(pres=sum(pres))%>%
  ungroup() %>%
  unite(sample_id,Year,Field,Transect, sep="_",remove=F) %>%
  # spread(sample_id,pres) %>%
  # replace(is.na(.), 0) %>%
  unite(group,group,Field,sep="_",remove=F) %>%
  select(-group,-x,-y) %>%
  unite(site_id,Year,Field, sep="_",remove=F) %>%
  arrange(Year,YSA,Field,Transect) %>%
  #select(-Year,-Field,-YSA,-Transect)  %>%
  distinct() 
  
View(ccr_comm_prep)




# AVERAGE
ccr.list.all <- ccr_comm_prep %>%
  split(.$site_id)

ccr.list.all <- map(ccr.list.all, ~ .x %>% 
                     select(-site_id,-Year,-Field,-YSA,-Transect) %>%
                     spread(key = sample_id, value = pres) %>%
                     replace(is.na(.), 0) %>%
                     column_to_rownames(var = "Species") )


ccr.all.out <- iNEXT(ccr.list.all, q=0, datatype="incidence_raw")


ccr.all.df <- fortify(ccr.all.out, type=1)

View(ccr.all.df)

ccr.all.df.avg <- ccr.all.df %>% filter(method == "interpolated" | method == "observed") %>%
  mutate(method= recode( method, "interpolated" = "observed")) %>%
  separate(site, c("Year","Field")) %>%
  group_by(Field,method,x) %>%
  summarise(y=mean(y),y.lwr=mean(y.lwr),y.upr=mean(y.upr)) %>%
  select(-method)

View(ccr.all.df.avg)


df.point.all <- ccr.all.df.avg[which(ccr.all.df.avg$method=="observed"),]
df.line.all <- ccr.all.df.avg[which(ccr.all.df.avg$method!="observed"),]
df.line.all$method <- factor(df.line.all$method,
                            c("interpolated"),
                            c("interpolation"))

View(df.point.all)


View(ccr.all.df.avg)


sa.all<-ggplot(ccr.all.df.avg, aes(x=x, y=y, color=Field)) +
  #geom_point(aes(shape=Field), size=4, data=df.point.all) +
  geom_line(aes(), lwd=1, data=ccr.all.df.avg) +
  geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
                  fill=Field, colour=NULL), alpha=0.2) +
  labs(x="Number of sampling units", y="Species richness",title="") +
  scale_color_manual(values =  c("#E7B800", "#972C8DFF" ,"#00AFBB","#15983DFF", "#FC4E07"))  + 
  scale_fill_manual(values =  c("#E7B800", "#972C8DFF" ,"#00AFBB", "#15983DFF","#E7B800", "#FC4E07"))  + 
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        text=element_text(size=18)) + 
  labs(title='a) Species accumulation')+
  xlim(0,10)+ theme_classic()+
  theme(legend.position="right")


sa.all

#"#D86C4FFF","#972C8DFF" ,"#00AFBB","#15983DFF", "#E7B800", "#FC4E07"

# TIME SLICES
ccr.list.82 <- ccr_comm_prep %>%
  filter(Year == "1982") %>%
  #select(-Year,-Field,-YSA,-Transect)  %>%
  split(.$site_id)



ccr.list.82 <- map(ccr.list.82, ~ .x %>% 
                    select(-site_id,-Year,-Field,-YSA,-Transect) %>%
                spread(key = sample_id, value = pres) %>%
                  replace(is.na(.), 0) %>%
                  column_to_rownames(var = "Species") )


ccr.82.out <- iNEXT(ccr.list.82, q=0, datatype="incidence_raw")


ccr.82.df <- fortify(ccr.82.out, type=1)

df.point.82 <- ccr.82.df[which(ccr.82.df$method=="observed"),]
df.line.82 <- ccr.82.df[which(ccr.82.df$method!="observed"),]
df.line.82$method <- factor(df.line.82$method,
                         c("interpolated"),
                         c("interpolation"))

sa.82<-ggplot(ccr.82.df, aes(x=x, y=y, colour=site)) +
  geom_point(aes(shape=site), size=4, data=df.point.82) +
  geom_line(aes(linetype=method), lwd=1, data=df.line.82) +
  geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
                  fill=site, colour=NULL), alpha=0.2) +
  labs(x="Number of sampling units", y="Species diversity",title="1982") +
  scale_color_manual(values =  c("#A1C720FF","#15983DFF","#00AFBB", "#E7B800", "#FC4E07"))  + 
  scale_fill_manual(values =  c("#A1C720FF","#15983DFF","#00AFBB", "#E7B800", "#FC4E07"))  + 
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        text=element_text(size=18)) + xlim(0,10)+ theme_classic()+
  theme(legend.position="none")


# Sample‐size‐based R/E curves
# sa.82<-ggiNEXT(ccr.82.out, type=1, color.var="site",endpoint=10) +
#   theme_classic()+
#   theme(legend.position="none")


sa.82


ccr.list.92 <- ccr_comm_prep %>%
  filter(Year == "1992") %>%
  #select(-Year,-Field,-YSA,-Transect)  %>%
  split(.$site_id)



ccr.list.92 <- map(ccr.list.92, ~ .x %>% 
                     select(-site_id,-Year,-Field,-YSA,-Transect) %>%
                     spread(key = sample_id, value = pres) %>%
                     replace(is.na(.), 0) %>%
                     column_to_rownames(var = "Species") )


ccr.92.out <- iNEXT(ccr.list.92, q=0, datatype="incidence_raw")

ccr.92.df <- fortify(ccr.92.out, type=1)

df.point.92 <- ccr.92.df[which(ccr.92.df$method=="observed"),]
df.line.92 <- ccr.92.df[which(ccr.92.df$method!="observed"),]
df.line.92$method <- factor(df.line.92$method,
                            c("interpolated"),
                            c("interpolation"))

sa.92<-ggplot(ccr.92.df, aes(x=x, y=y, colour=site)) +
  geom_point(aes(shape=site), size=4, data=df.point.92) +
  geom_line(aes(linetype=method), lwd=1, data=df.line.92) +
  geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
                  fill=site, colour=NULL), alpha=0.2) +
  labs(x="Number of sampling units", y="Species diversity",title=1992) +
  scale_color_manual(values =  c("#A1C720FF","#15983DFF","#00AFBB", "#E7B800", "#FC4E07"))  + 
  scale_fill_manual(values =  c("#A1C720FF","#15983DFF","#00AFBB", "#E7B800", "#FC4E07"))  + 
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        text=element_text(size=18)) + xlim(0,10)+ theme_classic()+
  theme(legend.position="none")

# Sample‐size‐based R/E curves
# sa.92<-ggiNEXT(ccr.92.out, type=1, color.var="site") +
#   theme_classic()+
#   theme(legend.position="none")




ccr.list.02 <- ccr_comm_prep %>%
  filter(Year == "2002") %>%
  #select(-Year,-Field,-YSA,-Transect)  %>%
  split(.$site_id)



ccr.list.02 <- map(ccr.list.02, ~ .x %>% 
                     select(-site_id,-Year,-Field,-YSA,-Transect) %>%
                     spread(key = sample_id, value = pres) %>%
                     replace(is.na(.), 0) %>%
                     column_to_rownames(var = "Species") )


ccr.02.out <- iNEXT(ccr.list.02, q=0, datatype="incidence_raw")

ccr.02.out

ccr.02.df <- fortify(ccr.02.out, type=1)

df.point.02 <- ccr.02.df[which(ccr.02.df$method=="observed"),]
df.line.02 <- ccr.02.df[which(ccr.02.df$method!="observed"),]
df.line.02$method <- factor(df.line.02$method,
                            c("interpolated"),
                            c("interpolation"))

sa.02<-ggplot(ccr.02.df, aes(x=x, y=y, colour=site)) +
  geom_point(aes(shape=site), size=4, data=df.point.02) +
  geom_line(aes(linetype=method), lwd=1, data=df.line.02) +
  geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
                  fill=site, colour=NULL), alpha=0.2) +
  labs(x="Number of sampling units", y="Species diversity",title=2002) +
  scale_color_manual(values =  c("#A1C720FF","#15983DFF","#00AFBB", "#E7B800", "#FC4E07"))  + 
  scale_fill_manual(values =  c("#A1C720FF","#15983DFF","#00AFBB", "#E7B800", "#FC4E07"))  + 
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        text=element_text(size=18)) + xlim(0,10)+ theme_classic()+
  theme(legend.position="none")

sa.02

# Sample‐size‐based R/E curves
# sa.02<-ggiNEXT(ccr.02.out, type=1, color.var="site") +
#   theme_classic() +
#   theme(legend.position="none")




ccr.list.08 <- ccr_comm_prep %>%
  filter(Year == "2008") %>%
  #select(-Year,-Field,-YSA,-Transect)  %>%
  split(.$site_id)



ccr.list.08 <- map(ccr.list.08, ~ .x %>% 
                     select(-site_id,-Year,-Field,-YSA,-Transect) %>%
                     spread(key = sample_id, value = pres) %>%
                     replace(is.na(.), 0) %>%
                     column_to_rownames(var = "Species") )


ccr.08.out <- iNEXT(ccr.list.08, q=0, datatype="incidence_raw")

ccr.08.df <- fortify(ccr.08.out, type=1)

df.point.08 <- ccr.08.df[which(ccr.08.df$method=="observed"),]
df.line.08 <- ccr.08.df[which(ccr.08.df$method!="observed"),]
df.line.08$method <- factor(df.line.08$method,
                            c("interpolated"),
                            c("interpolation"))

sa.08<-ggplot(ccr.08.df, aes(x=x, y=y, colour=site)) +
  geom_point(aes(shape=site), size=4, data=df.point.08) +
  geom_line(aes(), lwd=1, data=df.line.08) +
  geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
                  fill=site, colour=NULL), alpha=0.2) +
  labs(x="Number of sampling units", y="Species diversity",title=2008) +
  scale_color_manual(labels = c("A", "B","C","D"),values =  c("#A1C720FF","#15983DFF","#00AFBB", "#E7B800", "#FC4E07"))  + 
  scale_fill_manual(labels = c("A", "B","C","D"),values =  c("#A1C720FF","#15983DFF","#00AFBB", "#E7B800", "#FC4E07"))  + 
  scale_shape_manual(labels = c("A", "B","C","D"),values=c(16,17,15,3))+
  labs(color="Field",shape="Field",fill="Field")+
 xlim(0,10)+ theme_classic() +
  theme(legend.position = "bottom") 

sa.08

# Sample‐size‐based R/E curves
# sa.08<-ggiNEXT(ccr.08.out, type=1, color.var="site") +
#   theme_classic()


#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

rlegend<-g_legend(sa.08)




(sa.82 | sa.92)/(sa.02 | sa.08+ theme(legend.position="none"))/(rlegend) +
  plot_layout(heights = c(10,10,2.5))


