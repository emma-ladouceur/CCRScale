



library(tidyverse)


splong <- read.csv("~/Dropbox/Projects/CCRScale/E_001/e001.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

splong<- splong %>%  filter(!Species %in% c( "Miscellaneous litter","Mosses & lichens","Mosses & lichens 2") ) %>%
  droplevels()


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
  arrange(Year,Field,Plot) %>% ungroup()


e1_long$pres<-1

splong$pres<-1

View(e1_long)


Field.ABCD <- splong %>% filter(Field == "A" | Field =="B" | Field =="C" | Field == "D") %>%
  select(Field,Species,pres) %>%
  distinct() %>% spread(Field,pres) %>% arrange(Species)

View(Field.ABCD)


write.csv(Field.ABCD, "~/Dropbox/Projects/CCRScale/data/species.csv")


View(splong)
View(e1_long)

Field.Year.ABCD <- e1_long  %>% filter(Field == "A" | Field =="B" | Field =="C" | Field == "D") %>%
  select(Field,Year,Species,pres) %>%
  distinct() %>% spread(Field,pres) %>% 
  replace(is.na(.), 0) %>%
  arrange(Year,Species)

View(Field.Year.ABCD)



Field.Year.ABCD$`Species Occurance` <-  ifelse(Field.Year.ABCD$A == 1 & Field.Year.ABCD$D==1, 'shared',
             ifelse(Field.Year.ABCD$B == 1 & Field.Year.ABCD$D==1, 'shared',
                    ifelse(Field.Year.ABCD$C == 1 & Field.Year.ABCD$D==1, 'shared',
                           ifelse(Field.Year.ABCD$D == 1,  'never-plowed', 'old field'))))



View(Field.Year.ABCD)

Field.Year.ABCD$`Species Occurance`<- as.factor(as.character(Field.Year.ABCD$`Species Occurance`))
levels(Field.Year.ABCD$`Species Occurance`)

Field.Year.ABCD$pres<-1


ss_sum <- Field.Year.ABCD %>% group_by(Year,`Species Occurance`) %>%
  summarise(ss_sum= sum(pres))


Field.ABCD <- Field.Year.ABCD %>% select(-`Species Occurance`) %>%
  group_by(Species) %>% summarise( A= sum(A), 
                                   B= sum(B), 
                                   C= sum(C), 
                                   D= sum(D), 
                                   )
  

View(Field.ABCD)

Field.ABCD$`Species Occurance` <-  ifelse(Field.ABCD$A >= 1 & Field.ABCD$D>=1, 'shared',
                                          ifelse(Field.ABCD$B >= 1 & Field.ABCD$D>=1, 'shared',
                                                 ifelse(Field.ABCD$C >= 1 & Field.ABCD$D>=1, 'shared',
                                                        ifelse(Field.ABCD$D >= 1,  'never-plowed', 'old field'))))


Field.ABCD$pres<-1



field.sp <- Field.Year.ABCD %>%
  group_by(Year) %>%
  summarise(
    SS_A = sum(A),
    SS_B = sum(B),
    SS_C = sum(C),
    SS_D = sum(D))


View(field.sp)


 field.s <- field.sp %>% left_join(ss_sum)

 
 View(field.s)

g_sum <- Field.Year.ABCD %>% group_by(Year) %>%
   summarise(gamma_field= sum(pres))
 
fields <- field.s %>% left_join(g_sum) %>% 
  filter(!Year %in% c("2005","2006","2007","2009","2010","2011","2012","2013","2014","2015") )


View(fields) 


fields$p_sp <- (fields$ss_sum/fields$gamma_field)*100

fields$Year<- as.factor(as.character(fields$Year))

fields2<-fields %>% select(Year, `Species Occurance`,ss_sum,p_sp,gamma_field)

colnames(fields)
colnames(g_sum)

g_sum <- Field.ABCD %>%  select(Species,pres,`Species Occurance`) %>%
  distinct() %>% group_by(`Species Occurance`) %>%
  summarise(ss_sum= sum(pres))

gg_sum <- Field.Year.ABCD %>% select(Species,pres) %>%
  distinct() %>%
  summarise(g_gamma_field = sum(pres))

View(gg_sum)

g_sum$gamma_field <- 190

g_sum$p_sp <- (g_sum$ss_sum/g_sum$gamma_field)*100

g_sum$Year <- "Total"



g_sum2<-g_sum %>% select(Year, `Species Occurance`,ss_sum,p_sp,gamma_field)


fields3 <- fields2 %>% rbind(g_sum2)


View(fields)

library(yarrr)
piratepal(palette = "all")

piratepal(palette = "southpark")

of_alpha <- fields3 %>% filter(`Species Occurance` == "old field" ) %>%
filter(!Year == "Total") 
np_alpha <- fields3 %>% filter(`Species Occurance` == "never-plowed" ) %>%
  filter(!Year == "Total") 
s_alpha <- fields3 %>% filter(`Species Occurance` == "shared" ) %>%
  filter(!Year == "Total") 


of_st <- fields3 %>% filter(Year == "Total") %>%
  filter(`Species Occurance` == "old field" ) 
np_st <- fields3 %>% filter(Year == "Total") %>%
  filter(`Species Occurance` == "never-plowed"  ) 
s_st <- fields3 %>% filter(Year == "Total") %>%
  filter(`Species Occurance` == "shared"  ) 


g_f <- fields3 %>% select(Year,gamma_field) %>% distinct() %>% filter(!Year == "Total") 

g_fst <- fields3 %>% select(Year,gamma_field) %>% distinct() %>% filter(Year == "Total") 

View(g_f)

sp_bar<-fields3 %>%
  mutate(`Species Occurance` = factor( `Species Occurance`, levels=c("old field","shared","never-plowed"))) %>%
ggplot( aes(color=`Species Occurance`,  y=ss_sum, x=Year)) + 
  geom_bar(position="stack", stat="identity", aes(fill=`Species Occurance`) , alpha=0.5) +
  scale_color_manual(values =  c("#A1C720FF", "#2F86FFFF" ,"#15983DFF"))  + 
  scale_fill_manual(values =  c("#A1C720FF", "#2F86FFFF" ,"#15983DFF"))  + 
  geom_text(data= g_f, aes(x= Year, y=92, label=paste(gamma_field)), color="black",size= 3, vjust=1.6) +
  geom_text(data= g_fst, aes(x= Year, y=197, label=paste(gamma_field)), color="black",size= 3, vjust=1.6) +
  geom_text(data=np_alpha , aes(x= Year, y=10, label=paste(ss_sum)), color="black", size=3,vjust=1.6) +
  geom_text(data=s_alpha ,aes(x= Year, y=35, label=paste(ss_sum)), color="black", size=3,vjust=1.6) +
  geom_text(data=of_alpha ,aes(x= Year, y=60, label=paste(ss_sum)), color="black", size=3, vjust=1.6) +
  geom_text(data=np_st , aes(x= Year, y=10, label=paste(ss_sum)), color="black", size=3,vjust=1.6) +
  geom_text(data=s_st ,aes(x= Year, y=100, label=paste(ss_sum)), color="black",size=3, vjust=1.6) +
  geom_text(data=of_st ,aes(x= Year, y=170, label=paste(ss_sum)), color="black",size=3, vjust=1.6) +
  theme_classic()+ #labs(color="Species Occurance")+
  theme(axis.text.x = element_text(size=9, angle=14),legend.direction = "horizontal", legend.position = "bottom") + ylab("Species Richness") 
  
sp_bar


View(fields)

of_alpha <- fields %>% filter(`Species Occurance` == "old field" ) 
np_alpha <- fields %>% filter(`Species Occurance` == "never-plowed" ) 
s_alpha <- fields %>% filter(`Species Occurance` == "shared" ) 

sp_line<-fields %>%
  mutate(`Species Occurance` = factor( `Species Occurance`, levels=c("old field","shared","never-plowed"))) %>% ggplot() +
  geom_point(#data = fields,
             aes(x = Year, y = ss_sum,
                 colour = `Species Occurance`, shape=`Species Occurance`), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_smooth(#data = fields,
              aes(x = Year, y = ss_sum,
                  colour = `Species Occurance`,group=`Species Occurance`))+
  scale_color_manual(values =  c("#A1C720FF", "#8F2F8BFF","#15983DFF"))  + 
  #scale_x_continuous(breaks=c(0,25,50,75,89)) +
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), plot.margin=margin(t=2,1,1,1, "lines"),
                        legend.direction = "vertical", legend.position = "right" )+
  labs(title = "Species occurance in fields")+
    ylab("Species Richness") + xlab("Year") 



sp_line<- fields %>%
  mutate(`Species Occurance` = factor( `Species Occurance`, levels=c("old field","shared","never-plowed"))) %>% 
  ggplot() +
  geom_point(#data = fields,
             aes(x = Year, y = p_sp,
                 colour = `Species Occurance`, shape=`Species Occurance`), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.15 ))+
  geom_smooth(#data = fields,
              aes(x = Year, y = p_sp,
                  colour = `Species Occurance`,group=`Species Occurance`))+
  scale_color_manual(values =  c("#A1C720FF", "#8F2F8BFF","#15983DFF"))  + 
  #scale_x_continuous(breaks=c(0,25,50,75,89)) +
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), plot.margin=margin(t=2,1,1,1, "lines"),
                        legend.direction = "vertical", legend.position = "right" )+
  labs(title = "Species occurance in fields")+
 ylab("Percentage of species across all fields ") + xlab("Year") 



(sp_bar)/(sp_line)

