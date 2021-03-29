

# Multiscale Analysis

library(tidyverse)
library(patchwork)
library(iNEXT)
library(ggplot2)
library(viridis)


clean_cover <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/e014_e133_cleaned_1983-2016_EL.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


head(clean_cover)

Fields_check<-distinct(clean_cover,Field)
View(Fields)
clean_cover$pres<-1

cover_wide<- clean_cover %>% select(-c(pCover_plot_sum,pCover,Relative_pCover,X)) %>% # remove riff raff
  distinct() %>%
  group_by(Exp,Year,YSA,Field,Transect,Plot,site_status) %>%
  spread(Species,pres) %>% # spread to wide format
  ungroup()  %>%
  arrange(Exp,Year,YSA,Field,Transect,Plot) 


View(cover_wide)

cover_select<- cover_wide %>% group_by(Exp,Year,YSA,Field,site_status) %>%
  sample_n(20) %>% ungroup() # select only 20 random plots

colnames(cover_select)

cover_long <- cover_select %>% gather(Species,Pres, "Achillea millefolium":"Vitis riparia") %>%
  filter(!is.na(Pres)) %>% droplevels() %>%
  arrange(YSA,Field,Transect,Plot,n) # back to long


cover_long$site_status<- as.factor(as.character(cover_long$site_status))

plot_prep <- cover_long %>% distinct(Year,YSA,Field, Transect, Plot,site_status)%>%
  group_by(YSA,Field) %>% mutate(y = 1:n()) %>% # number each plot
  arrange(YSA,Field,Transect,Plot) %>%
  ungroup()

plot_attr <- plot_prep %>% distinct(Year,Field) %>%  mutate(x = 1:n()) %>%
  left_join(plot_prep) %>%
  rename(group=site_status) %>%
  arrange(YSA,Field,Transect,Plot) 

ccr_prep <- plot_attr %>% left_join(cover_long) 

ccr_prep$YSA<- as.factor(as.character(ccr_prep$YSA))
levels(ccr_prep$YSA)

ccr_comm_prep <- ccr_prep %>%  select(group,x,y,Year,YSA,Field,Transect,Plot,n,Species,Pres) %>%  
  group_by(group,x,y,Year,YSA,Field,Transect,Plot,Species) %>%
  distinct() %>%
  ungroup() %>%
  unite(sample_id,Year,Field,Transect,Plot, sep="_",remove=F) %>%
  # spread(sample_id,Pres) %>%
  # replace(is.na(.), 0) %>%
  unite(group,group,Field,sep="_",remove=F) %>%
  select(-group,-x,-y) %>%
  unite(site_id,Year,Field, sep="_",remove=F) %>%
  arrange(Field,Year,YSA,Transect,Plot) %>%
  #select(-Year,-Field,-YSA,-Transect)  %>%
  distinct() 

View(ccr_comm_prep)

write.csv(ccr_comm_prep, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/star_prep.csv")

# ~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/
ccr_comm_prep <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/star_prep.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


summary(ccr_comm_prep)

Fields <- distinct(ccr_comm_prep, Field, YSA)
View(Fields)


# all sites raw
ccr.list.all <- ccr_comm_prep %>%
  split(.$site_id)

ccr.list.all <- map(ccr.list.all, ~ .x %>% 
                      select(Species,sample_id,Pres) %>%
                      distinct() %>%
                      spread(key = sample_id, value = Pres) %>%
                      replace(is.na(.), 0) %>%
                      column_to_rownames(var = "Species") )


View(ccr.list.all)


ccr.all.out <- iNEXT(ccr.list.all, q=0, datatype="incidence_raw")


ccr.all.df <- fortify(ccr.all.out, type=1)


colnames(ccr.all.df)
colnames(ccr_comm_prep)

ccr_dat <- ccr_comm_prep %>% distinct(site_id,Year, YSA, Field) %>%
  mutate(site = site_id) %>%
  left_join(ccr.all.df)


ccr_deets <-ccr_prep %>% distinct(Field,YSA, site_status)


ccr.all.df <- ccr_dat %>% filter(method == "interpolated" | method == "observed") %>%
  mutate(method= recode( method, "interpolated" = "observed")) %>%
  #separate(site, c("Year","Field")) %>%
  group_by(site_id,Year, YSA, Field,method,x) %>%
  summarise(y=mean(y),y.lwr=mean(y.lwr),y.upr=mean(y.upr)) %>%
  select(-method) %>%
  left_join(ccr_deets) %>%
  unite(Field_stat,Field,site_status,sep="_",remove=F) 

View(ccr.all.df)


ccr.all.df.np <- ccr.all.df %>% filter(YSA == "never-plowed") %>%
  mutate( State = YSA)

ccr.all.df.of <- ccr.all.df %>% filter(!YSA == "never-plowed")

levels(ccr.all.df$YSA)
ccr.all.df.of$YSA<-factor(ccr.all.df.of$YSA,  levels=c("1" , "3" , "5" , "6" , "7" , "8" , "9" , "11" , "12" , "14" , "15" , "17" , "18" , "19" , "20" , "21" , "22" , "23" , "24" , "25" , "26" , "27" , "28" , "29" , "30" , "31" , "32" , "33" , "34" , "35" , "36" , "37" ,"38" , "39" , "40" , "41" , "42" , "43" , "45" , "46" , "47", "48" , "49" , "50" , "51" , "53" , "54" , "55" , "56" ,"59" ,  "61" , "62" , "63" , "65" , "67" , "70",  "75" , "79" , "never-plowed"))


sa.all.of<-ggplot(ccr.all.df.of, aes(x=x, y=y,  group=site_id, color=YSA)) +
 #facet_wrap(~site_status)+
  #facet_wrap(~Field)+
  #facet_wrap(~YSA)+
  #geom_point(aes(shape=Field), size=4, data=df.point.all) +
  geom_line(aes(), lwd=1, data=ccr.all.df.of) +
  # geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
  #                 fill=YSA, colour=NULL), alpha=0.2) +
  labs(x="Number of sampling units", y="Species richness",title="") +
  scale_color_viridis(discrete = T, option="C")  + 
  # scale_fill_manual(values =  c("#E7B800", "#972C8DFF" ,"#00AFBB", "#15983DFF","#E7B800", "#FC4E07"))  + 
  theme_classic()+
  theme(legend.position = "none",
        legend.title=element_blank(),
        text=element_text(size=10)) + 
  labs(subtitle='c) Old field')
#xlim(0,20)+ 

sa.all.of


sa.all.np<-ggplot(ccr.all.df.np, aes(x=x, y=y,  group=site_id )) +
  #facet_wrap(~site_status)+
  #facet_wrap(~Field)+
  #facet_wrap(~YSA)+
  #geom_point(aes(shape=Field), size=4, data=df.point.all) +
  geom_line(aes(), lwd=1, data=ccr.all.df.np,color= "#5DC863FF") +
  # geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
  #                 fill=YSA, colour=NULL), alpha=0.2) +
  labs(x="Number of sampling units", y="Species richness",title="") +
  #scale_color_viridis(discrete = T, option="C")  + 
  # scale_fill_manual(values =  c("#E7B800", "#972C8DFF" ,"#00AFBB", "#15983DFF","#E7B800", "#FC4E07"))  + 
  theme_classic()+
  theme(legend.position = "none",
        legend.title=element_blank(),
        text=element_text(size=10)) + 
  labs(title='Accumulation for every field at every time point', subtitle="b) Never-plowed")
#xlim(0,20)+ 

sa.all.np

all.sites  <- (sa.all.np) + (sa.all.of) 


# CALCULATED AS  AVERAGE ACROSS YSA'S

ccr.all.df.avg <- ccr_dat %>% filter(method == "interpolated" | method == "observed") %>%
  mutate(method= recode( method, "interpolated" = "observed")) %>%
  group_by(YSA,x) %>%
  summarise(y=mean(y),y.lwr=mean(y.lwr),y.upr=mean(y.upr)) 


View(ccr.all.df.avg)
levels(ccr.all.df.avg$YSA)
ccr.all.df.avg$YSA<-factor(ccr.all.df.avg$YSA,  levels=c("1" , "3" , "5" , "6" , "7" , "8" , "9" , "11" , "12" , "14" , "15" , "17" , "18" , "19" , "20" , "21" , "22" , "23" , "24" , "25" , "26" , "27" , "28" , "29" , "30" , "31" , "32" , "33" , "34" , "35" , "36" , "37" ,"38" , "39" , "40" , "41" , "42" , "43" , "45" , "46" , "47", "48" , "49" , "50" , "51" , "53" , "54" , "55" , "56" ,"59" ,  "61" , "62" , "63" , "65" , "67" , "70",  "75" , "79" , "never-plowed"))


sa.avg <- ggplot(ccr.all.df.avg, aes(x=x, y=y,   color=YSA)) +
  # facet_wrap(~YSA)+
  #geom_point(aes(shape=Field), size=4, data=df.point.all) +
  geom_line(aes(), lwd=1, data=ccr.all.df.avg) +
  # geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
  #                 fill=YSA, colour=NULL), alpha=0.2) +
  labs(x="Number of sampling units", y="Species richness",title="") +
  scale_color_viridis(discrete = T)  + 
  # scale_fill_manual(values =  c("#E7B800", "#972C8DFF" ,"#00AFBB", "#15983DFF","#E7B800", "#FC4E07"))  + 
  labs(title='a) Accumulation for every site at every time point', color="Years Since Abandonment")+
  #xlim(0,20)+ 
  theme_classic() +   theme(legend.direction = "horizontal",legend.position = "bottom") +
  guides(col = guide_legend(ncol = 15))


sa.avg

(sa.avg)/(sa.all)


# fix the color problem
colnames(ccr.all.df.avg)
 
 
ccr.all.df.avg.np <- ccr.all.df.avg %>% filter(YSA == "never-plowed") %>%
  mutate( State = YSA)

ccr.all.df.avg.of <- ccr.all.df.avg %>% filter(!YSA == "never-plowed")

View(ccr.all.df.avg.of)

ccr.all.df.avg.of$YSA<-factor(ccr.all.df.avg.of$YSA,  levels=c("1" , "3" , "5" , "6" , "7" , "8" , "9" , "11" , "12" , "14" , "15" , "17" , "18" , "19" , "20" , "21" , "22" , "23" , "24" , "25" , "26" , "27" , "28" , "29" , "30" , "31" , "32" , "33" , "34" , "35" , "36" , "37" ,"38" , "39" , "40" , "41" , "42" , "43" , "45" , "46" , "47", "48" , "49" , "50" , "51" , "53" , "54" , "55" , "56" ,"59" ,  "61" , "62" , "63" , "65" , "67" , "70",  "75" , "79" ))




sa.avg.leg <- ggplot() +
  # facet_wrap(~YSA)+
  #geom_point(aes(shape=Field), size=4, data=df.point.all) +
  #geom_line(data=ccr.all.df.avg.of, aes(x=x, y=y,   color=YSA), lwd=1) +
  geom_line(data=ccr.all.df.avg.np, aes(x=x, y=y, color = State),   lwd=1 ) +
  # geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
  #                 fill=YSA, colour=NULL), alpha=0.2) +
  labs(x="Number of sampling units", y="Species richness",title="") +
  #scale_color_viridis(discrete = T, option="A")  + 
  scale_color_manual(values =  c("#5DC863FF"))  + 
  labs(title='Species accumulation across scales', color="Reference Habitat")+
  #xlim(0,20)+ 
  theme_classic(base_size=18) +   theme(legend.direction = "horizontal",legend.position = "bottom") 


sa.avg.leg


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

np.legend<-g_legend(sa.avg.leg)

sa.avg <- ggplot() +
  # facet_wrap(~YSA)+
  #geom_point(aes(shape=Field), size=4, data=df.point.all) +
  geom_line(data=ccr.all.df.avg.of, aes(x=x, y=y,   color=YSA), lwd=1) +
  geom_line(data=ccr.all.df.avg.np, aes(x=x, y=y), color= "#5DC863FF" ,  lwd=1 ) +
  # geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
  #                 fill=YSA, colour=NULL), alpha=0.2) +
  labs(x="Number of sampling units", y="Species richness",title="") +
  scale_color_viridis(discrete = T, option="C")  + 
 #scale_color_manual(values =  c("#E7B800"))  + 
  labs(title='Multi-scale',subtitle="Average Accumulation for Year Since Abandonment", color="Old fields\nYears Since Abandonment")+
  #xlim(0,20)+ 
  theme_classic(base_size=18) +   theme(legend.direction = "horizontal",legend.position = "bottom") +
  guides(col = guide_legend(ncol = 15))


sa.avg

of.legend <- g_legend(sa.avg)

# multi.scale<-(sa.avg + theme(legend.position="none")) / (np.legend) / (of.legend)/ (all.sites)  + plot_layout(heights = c(6,0.75,1.5,6)) 
# 
# multi.scale


multi.scale<-(sa.avg + theme(legend.position="none")) / (np.legend) / (of.legend)  + plot_layout(heights = c(10,0.75,1)) 

multi.scale


# scales.fig <- (p.alpha.rich.fig | p.gamma.rich.fig   | p.beta.div.fig + theme(legend.position="none"))/(ysa.legend)/
#   (sa.avg + theme(legend.position="none")) / (np.legend) / (of.legend) + plot_layout(heights = c(6,1,8,0.75,1)) 
# 
# # PORTRAIT 11 X 15
# scales.fig


# MODEL THE SP ACCUM CURVES BY YSA ?

library(brms)

View(ccr.all.df)

ccr.all.df$YSA <- as.factor(ccr.all.df$YSA)
ccr.all.df$log_x <- log(ccr.all.df$x)
ccr.all.df$log_y <- log(ccr.all.df$y)




multi_scale_mod <-  brm(log_y ~  YSA * log_x + ( 1 + YSA * log_x | Field ) + (1 | Year),
                        data = ccr.all.df, family=student(), cores = 4, iter=2000, chains = 4)


#cores = 4, iter=10000,warmup = 1000, control =list(adapt_delta = 0.99), chains = 4)




