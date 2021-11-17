

# Multi-scale Analysis, just some sample-based species accumulation curves etc

library(tidyverse)
library(patchwork)
library(iNEXT)
library(ggplot2)
library(viridis)
library(brms)
library(bayesplot)

citation("iNEXT")


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


ccr.all.out <- iNEXT(ccr.list.all, q = 0, endpoint = 50, datatype = "incidence_raw")

head(ccr.all.out)

ccr.all.df <- fortify(ccr.all.out, type = 1) # type 1 = sample-size-based rarefaction/extrapolation curve


colnames(ccr.all.df)
colnames(ccr_comm_prep)

ccr_dat <- ccr_comm_prep %>% distinct(site_id,Year, YSA, Field) %>%
  mutate(site = site_id) %>%
  left_join(ccr.all.df)


View(ccr_dat)

ccr_deets <- ccr_prep %>% distinct(Field, YSA, site_status) %>%
  mutate(Field = as.character(Field))


# incidence-based species accumulation curves with only interpolated and observed data

ccr.all.df <- ccr_dat %>% filter(method == "interpolated" | method == "observed") %>%
  mutate(method= recode( method, "interpolated" = "observed")) %>%
  separate(site, c("Year","Field")) %>%
  group_by(site_id,Year, YSA, Field, method, x) %>%
  summarise(y = mean(y), y.lwr = mean(y.lwr), y.upr = mean(y.upr)) %>%
  select(-method) %>%
  left_join(ccr_deets) %>%
  unite(Field_stat, Field, site_status, sep="_",remove=F) 

head(ccr.all.df)


ccr.all.df.np <- ccr.all.df %>% filter(YSA == "never-plowed") %>%
  mutate( State = YSA)

ccr.all.df.of <- ccr.all.df %>% filter(!YSA == "never-plowed")

levels(ccr.all.df$YSA)
ccr.all.df.of$YSA<-factor(ccr.all.df.of$YSA,  levels=c("1" , "3" , "5" , "6" , "7" , "8" , "9" , "11" , "12" , "14" , "15" , "17" , "18" , "19" , "20" , "21" , "22" , "23" , "24" , "25" , "26" , "27" , "28" , "29" , "30" , "31" , "32" , "33" , "34" , "35" , "36" , "37" ,"38" , "39" , "40" , "41" , "42" , "43" , "45" , "46" , "47", "48" , "49" , "50" , "51" , "53" , "54" , "55" , "56" ,"59" ,  "61" , "62" , "63" , "65" , "67" , "70",  "75" , "79" , "never-plowed"))



head(ccr.all.df.of)

sa.all.of <- ggplot(ccr.all.df.of, aes(x = x, y = y,  group = site_id, color = YSA)) +
 #facet_wrap(~site_status)+
  #facet_wrap(~Field)+
  #facet_wrap(~YSA)+
  #geom_point(aes(shape=Field), size=4, data=df.point.all) +
  geom_line(aes(), lwd=1, data = ccr.all.df.of) +
  # geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
  #                 fill=YSA, colour=NULL), alpha=0.2) +
  labs(x="Number of sampling units", y = "Species richness",title="") +
  scale_color_viridis(discrete = T, option="C")  + 
  # scale_fill_manual(values =  c("#E7B800", "#972C8DFF" ,"#00AFBB", "#15983DFF","#E7B800", "#FC4E07"))  + 
  theme_classic()+
  theme(legend.position = "none",
        legend.title=element_blank(),
        text=element_text(size=10)) + 
  labs(subtitle='c) Old field')
#xlim(0,20)+ 

sa.all.of


sa.all.np <- ggplot(ccr.all.df.np, aes(x=x, y=y,  group=site_id )) +
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
head(ccr_dat)

ccr.all.df.avg <- ccr_dat %>% #filter(method == "interpolated" | method == "observed") %>%
  #mutate(method= recode( method, "interpolated" = "observed")) %>%
  group_by(YSA,x, method) %>%
  summarise(y=mean(y),y.lwr=mean(y.lwr),y.upr=mean(y.upr))  %>%
  left_join(ccr_deets) 


head(ccr.all.df.avg)
levels(ccr.all.df.avg$YSA)
ccr.all.df.avg$YSA<-factor(ccr.all.df.avg$YSA,  levels=c("1" , "3" , "5" , "6" , "7" , "8" , "9" , "11" , "12" , "14" , "15" , "17" , "18" , "19" , "20" , "21" , "22" , "23" , "24" , "25" , "26" , "27" , "28" , "29" , "30" , "31" , "32" , "33" , "34" , "35" , "36" , "37" ,"38" , "39" , "40" , "41" , "42" , "43" , "45" , "46" , "47", "48" , "49" , "50" , "51" , "53" , "54" , "55" , "56" ,"59" ,  "61" , "62" , "63" , "65" , "67" , "70",  "75" , "79" , "never-plowed"))

# deliminate points and lines
df.point <- ccr.all.df.avg[which(ccr.all.df.avg$method=="observed"),]
df.line <- ccr.all.df.avg[which(ccr.all.df.avg$method!="observed"),]
df.line$method <- factor(df.line$method, 
                         c("interpolated", "extrapolated"),
                         c("interpolation", "extrapolation"))

# make an iNext style plot
sa.avg <- ggplot(ccr.all.df.avg, aes(x = x, y = y,   color = YSA)) +
  geom_point(aes(shape = site_status), size=5, data = df.point) +
  geom_line(aes(linetype = method), lwd=1.5, data = df.line) +
  # geom_ribbon(aes(ymin = y.lwr, ymax = y.upr,
  #                 fill = site, colour=NULL), alpha=0.2) +
  # facet_wrap(~YSA)+
  #geom_point(aes(shape=Field), size=4, data=df.point.all) +
  #geom_line(aes(), lwd=1, data=ccr.all.df.avg) +
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




# fix the color problem and make a plot for publishing
ccr.all.df.avg <- ccr_dat %>% #filter(method == "interpolated" | method == "observed") %>%
 # mutate(method= recode( method, "observed" = "interpolated")) %>%
  group_by(YSA,x, method) %>%
  summarise(y=mean(y),y.lwr=mean(y.lwr),y.upr=mean(y.upr))  %>%
  left_join(ccr_deets) 


head(ccr.all.df.avg)
 
 
ccr.all.df.avg.np <- ccr.all.df.avg %>% filter(YSA == "never-plowed") %>%
  mutate( State = YSA)

df.point.np <- ccr.all.df.avg.np[which(ccr.all.df.avg.np$method=="observed"),]
ccr.all.df.avg.np$Method <- factor(ccr.all.df.avg.np$method, 
                                   c("interpolated", "extrapolated"),
                                   c("interpolation", "extrapolation"))

ccr.all.df.avg.of <- ccr.all.df.avg %>% filter(!YSA == "never-plowed")

head(ccr.all.df.avg.of)

head(ccr.all.df.avg.of %>% filter(Field == 600))

ccr.all.df.avg.of$YSA<-factor(ccr.all.df.avg.of$YSA,  levels=c("1" , "2" ,"3" , "5" , "6" , "7" , "8" , "9" , "11" , "12" , "14" , "15" , "17" , "18" , "19" , "20" , "21" , "22" , "23" , "24" , "25" , "26" , "27" , "28" , "29" , "30" , "31" , "32" , "33" , "34" , "35" , "36" , "37" ,"38" , "39" , "40" , "41" , "42" , "43" , "45" , "46" , "47", "48" , "49" , "50" , "51" , "53" , "54" , "55" , "56" ,"59" ,  "61" , "62" , "63" , "65" , "67" , "70",  "75" , "79" ))

df.point.of <- ccr.all.df.avg.of[which(ccr.all.df.avg.of$method=="observed"),]
ccr.all.df.avg.of$Method <- factor(ccr.all.df.avg.of$method, 
                                   c("interpolated", "extrapolated"),
                                   c("interpolation", "extrapolation"))

# create custom legends

# never plowed line
np.leg <- ggplot() +
  geom_line(data=ccr.all.df.avg.np %>% filter(Method == c("interpolation", "extrapolation")), aes(x=x, y=y, color=State), lwd=1) +
  labs(x="Number of sampling units", y="Species richness",title="") +
  scale_color_manual(values =  c("#5DC863FF"))  + 
  labs(title='Species accumulation across scales', color="Reference Habitat")+
  theme_classic(base_size=18) +   theme(legend.direction = "horizontal", legend.position = "bottom") 


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

np.legend <- g_legend(np.leg)

# line types / method
line.leg <- ggplot() +
  geom_line(data=ccr.all.df.avg.np %>% filter(Method == c("interpolation", "extrapolation")), aes(x=x, y=y,  linetype = Method), lwd=1) +
  labs(x="Number of sampling units", y="Species richness",title="") +
  #scale_color_viridis(discrete = T, option="A")  + 
  scale_color_manual(values =  c("#5DC863FF"))  + 
  labs(title='Species accumulation across scales', color="Reference Habitat")+
  theme_classic(base_size=18) +   theme(legend.direction = "horizontal", legend.position = "bottom") 

line.legend <- g_legend(line.leg)

# olf field colors
of.leg <- ggplot() +
  geom_line(data = ccr.all.df.avg.of, aes(x = x, y = y,   color = YSA), lwd = 1) +
  labs(x="Number of sampling units", y="Species richness",title="") +
  scale_color_viridis(discrete = T, option="C")  + 
  labs(title='Multi-scale',subtitle="Average Accumulation for Year Since Abandonment", color="Old fields\nYears Since Abandonment")+
  theme_classic(base_size=18) +   theme(legend.direction = "horizontal",legend.position = "bottom") +
  guides(col = guide_legend(ncol = 15))

of.legend <- g_legend(of.leg)

head(ccr.all.df.avg.of)

sa.avg <- ggplot(data = ccr.all.df.avg.of, aes(x=x, y=y)) +
  # facet_wrap(~YSA)+
  geom_point(data = df.point.of, aes( color = YSA), shape =16) +
  geom_line(data = ccr.all.df.avg.of, aes(x = x, y = y,   color = YSA, linetype = Method), lwd = 1) +
  geom_point(data = df.point.np,  color= "#5DC863FF" , shape =15) +
  geom_line(data=ccr.all.df.avg.np, aes(x=x, y=y,  linetype = Method), color= "#5DC863FF" ,  lwd = 1) +
  labs(x="Number of sampling units", y="Species richness",title="") +
  scale_color_viridis(discrete = T, option="C")  + 
  labs(title='Multi-scale',subtitle="Average Accumulation for Year Since Abandonment", color="Old fields\nYears Since Abandonment")+
  theme_classic(base_size=18) +   theme(legend.position = "none") +
  guides(col = guide_legend(ncol = 15))


sa.avg



fig_s2 <- (sa.avg + theme(legend.position="none")) / (line.legend) / (np.legend) / (of.legend)  + plot_layout(heights = c(10,0.5,0.5,1.5)) 

fig_s2

# # PORTRAIT 11 X 15



# take only extrapolated values at 50 samps and have a look at what we might expect overall

ccr.extrap.df <- ccr_dat %>% filter(method == "extrapolated" ,
                                    x == 50) %>%
  separate(site, c("Year","Field")) %>%
  # group_by(site_id,Year, YSA, Field, method, x) %>%
  # summarise(y = mean(y), y.lwr = mean(y.lwr), y.upr = mean(y.upr)) %>%
  #select(-method) %>%
  left_join(ccr_deets) %>%
  unite(Field_stat, Field, site_status, sep="_",remove=F) 

head(ccr.extrap.df)

write.csv(ccr.extrap.df, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/extrap.df.csv")

ccr.extrap.df <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/extrap.df.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


# d.extrap.rich <-  brm(y ~  site_status +  ( 1 | Field) + (1 | Year),
#                       data = ccr.extrap.df, family = student(), cores = 4, iter = 3000, warmup = 1000, chains = 4)
# 
# save(d.extrap.rich, file = '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/d.extrap.rich.Rdata')
load( '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/discrete/d.extrap.rich.Rdata')

summary(d.extrap.rich) # model summary


color_scheme_set("darkgray")
fig_s8 <- pp_check(d.extrap.rich) +   
  xlab( "Extrapolated species richness (50 samples)") + ylab("Density") + 
  ggtitle((expression(paste(italic(gamma), '-scale', sep = ''))))+
  labs(subtitle= "")+
  theme_classic() +  theme( plot.title=element_text(size=18, hjust=0.5), legend.position= "bottom")# predicted vs. observed values

fig_s8

extrap_c <- conditional_effects(d.extrap.rich, effects = 'site_status', re_formula = NA, method = 'fitted')  # conditional effects

head(alpha_c)

ccr.extrap.df$site_status <- factor(ccr.extrap.df$site_status  , levels=c("old field","never-plowed"))


fig_s3 <- ggplot() + 
  geom_point(data = ccr.extrap.df,
             aes(x = site_status, y = y, colour = 	"#C0C0C0"), 
             size = 0.25, gamma = 0.2, position = position_jitter(width = 0.02, height=0.05)) +
  geom_point(data = extrap_c$site_status,
             aes(x = site_status, y = estimate__, colour = site_status), size = 3) +
  geom_errorbar(data = extrap_c$site_status,
                aes(x = site_status, ymin = lower__, ymax = upper__, colour = site_status),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  +
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                  plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                  strip.background = element_blank(),legend.position="none") +
  labs(title =  '',
       subtitle = (expression(paste('Exrapolated ', italic(gamma), '-scale at 50 samples', sep = '')))
  )+
  ylab('Extrapolated richness') + xlab('')


fig_s3

