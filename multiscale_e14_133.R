

library(tidyverse)
library(patchwork)
library(iNEXT)
library(ggplot2)
library(viridis)


cover <- read.csv("~/Dropbox/Projects/CCRScale/E14 _133/e014_e133_cleaned_1983-2016.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

colnames(cover)
head(cover)
# Exp E14 & E133

distinct(cover,LCD_species) %>% arrange(LCD_species)

savanna_cover <- cover %>% filter(!Count_Species %in% c( "0") ) %>%  # remove not a species
  filter(HasBeenPlowed %in% c("0"))  # remnant savannas only

savanna_cover$site_status<- "never-plowed"

oldfield_cover <- cover %>% filter(!Count_Species %in% c( "0") ) %>%  # remove not a species
  filter(!HasBeenPlowed %in% c("0")) %>% # only old field
  filter(!forest_status %in% c( "HF") ) %>%  # remove heavily forested
  filter(!Year > first_year_burned) %>%  # if Year is greater than first year burned, filter out
  droplevels()

oldfield_cover$site_status<- "old field"
  
clean_cover <- savanna_cover %>% bind_rows(oldfield_cover)  %>% # bind cleaned old field and remnants data
  mutate(YSA = (Year-YearAb)) %>% # calculate year since abandoned
  mutate( YSA = ifelse(YearAb =="0" & site_status=="never-plowed","never-plowed",YSA))  %>% # if year ab = 0, then its never plowed
  filter(!LCD_species %in% c("Miscellaneous woody plants","Canopy Cover", "Miscellaneous species",
                             "Mosses & lichens")) %>% droplevels()

View(clean_cover)

clean_cover$pres<-1

plot_count <- clean_cover %>% distinct(Exp,Year,YSA,Field,Transect,Plot) %>%
   group_by(Exp,Year,YSA,Field) %>%
  count() %>%
  arrange(n) %>% ungroup() %>% left_join(clean_cover) %>% 
  filter(n >= 20) # select sites with 20 samps or greater

View(plot_count)

cover_wide<- plot_count %>% select(-Species,-pCover_class,-pCover,-Count_Species,-X) %>%
  distinct() %>%
  group_by(Exp,Year,YSA,Field,Transect,Plot,site_status) %>%
  spread(LCD_species,pres) %>% # spread to wide format
  ungroup()  %>%
  arrange(Exp,Year,YSA,Field,Transect,Plot) 


View(cover_wide)

cover_select<- cover_wide %>% group_by(Exp,Year,YSA,Field,site_status) %>%
  sample_n(20) %>% ungroup() # select only 20 random plots


cover_long <- cover_select %>% gather(Species,Pres, "Achillea millefolium":"Vitis riparia") %>%
 filter(!is.na(Pres)) %>% droplevels() %>%
  arrange(YSA,Field,Transect,Plot,n) # back to long


cover_long$site_status<- as.factor(as.character(cover_long$site_status))



plot_prep <- cover_long %>% distinct(Year,YSA,Field, Transect, Plot,site_status)%>%
  group_by(YSA,Field) %>% mutate(y = 1:n()) %>%
  arrange(YSA,Field,Transect,Plot) %>%
  ungroup()

plot_attr<- plot_prep %>% distinct(Year,Field) %>%  mutate(x = 1:n()) %>%
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

write.csv(ccr_comm_prep, "~/Dropbox/Projects/CCRScale/E14 _133/star_prep.csv")


summary(ccr_comm_prep)

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



levels(ccr.all.df$YSA)
ccr.all.df$YSA<-factor(ccr.all.df$YSA,  levels=c("1" , "3" , "5" , "6" , "7" , "8" , "9" , "11" , "12" , "14" , "15" , "17" , "18" , "19" , "20" , "21" , "22" , "23" , "24" , "25" , "26" , "27" , "28" , "29" , "30" , "31" , "32" , "33" , "34" , "35" , "36" , "37" ,"38" , "39" , "40" , "41" , "42" , "43" , "45" , "46" , "47", "48" , "49" , "50" , "51" , "53" , "54" , "55" , "56" ,"59" ,  "61" , "62" , "63" , "65" , "67" , "70",  "75" , "79" , "never-plowed"))


sa.all<-ggplot(ccr.all.df, aes(x=x, y=y,  group=site_id, color=YSA)) +
 # facet_wrap(~site_status)+
  #facet_wrap(~Field)+
  facet_wrap(~YSA)+
  #geom_point(aes(shape=Field), size=4, data=df.point.all) +
  geom_line(aes(), lwd=1, data=ccr.all.df) +
  # geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
  #                 fill=YSA, colour=NULL), alpha=0.2) +
  labs(x="Number of sampling units", y="Species richness",title="") +
   scale_color_viridis(discrete = T)  + 
  # scale_fill_manual(values =  c("#E7B800", "#972C8DFF" ,"#00AFBB", "#15983DFF","#E7B800", "#FC4E07"))  + 
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        text=element_text(size=18)) + 
  labs(title='Species accumulation')+
  #xlim(0,20)+ 
  theme_classic()+
  theme(legend.position="right")


sa.all


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
  labs(title='', color="Years Since Abandonment")+
  #xlim(0,20)+ 
  theme_classic() +   theme(legend.direction = "horizontal",legend.position = "bottom") +
  guides(col = guide_legend(ncol = 15))


sa.avg




# MODEL THE SP ACCUM CURVES BY YSA ?

library(brms)

View(ccr.all.df)

ccr.all.df$YSA <- as.factor(ccr.all.df$YSA)
ccr.all.df$log_x <- log(ccr.all.df$x)
ccr.all.df$log_y <- log(ccr.all.df$y)




multi_scale_mod <-  brm(log_y ~  YSA * log_x + ( YSA * log_x| Field/Year), 
                     data = ccr.all.df, cores = 4, iter=2000, chains = 4)



