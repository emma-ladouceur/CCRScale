
library(tidyverse)


# investigate species present


cover_long <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/e014_e133_cleaned_1983-2016_EL.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
sp <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/Clark_2018/species.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


head(cover_long)
head(sp)


 sp <- sp %>%  mutate(Species = LCD_Species)  %>% 
   select(-c(LCD_Species, Justification_1, Justification_2)) 
  
 
sp_deets <- cover_long %>% full_join(sp)
  
  
head(sp_deets)



View(sp_deets)  


sp_list_np <- sp_deets %>% select(site_status, Species, Duration, Origin) %>%
  filter(site_status %in% "never-plowed") %>% distinct() %>%
  mutate( np = "1") %>% select(-site_status)

head(sp_list_np)


sp_list_of <- sp_deets %>% select(site_status, Species, Duration, Origin) %>%
  filter(site_status %in% "old field") %>% distinct()%>%
  mutate( of = "1") %>% select(-site_status)



sp_list <- sp_list_np %>% full_join(sp_list_of)


View(sp_list)


np_only <- sp_list %>% filter(is.na(of)) %>% arrange(Species)

View(np_only)

of_only <- sp_list %>% filter(is.na(np)) %>% arrange(Species)

View(of_only)
