



# libraries
library(tidyverse)
library(vegan)
library(mobr)

#data
cover <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/e014_e133_cleaned_1983-2016.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

colnames(cover)
head(cover)
# Exp E14 & E133

cover %>% distinct(LCD_species, Count_Species) %>% arrange(LCD_species, Count_Species)

live_cover <- cover %>% filter(!Count_Species %in% c( "0") ) # remove not a species

`905_cover` <- live_cover %>% filter(Field %in% c( "905") ) # keep site '905' (says zero burn_freq but not)

head(`905_cover`)

savanna_cover <- live_cover %>% 
  filter(HasBeenPlowed %in% c("0")) %>% # remnant savannas only
  filter(!burn_freq %in% c( "0") ) %>% # remove sites that have never been burned
  filter(!is.na(burn_freq)) %>% # remove sites that have 'NA" for burn frequency 
  bind_rows(`905_cover`) # add 905_cover back

savanna_cover$site_status<- "never-plowed" # informative categorical label

distinct(savanna_cover,Field) %>% arrange(Field) # Matches Isbell et al. NEE 2019? Yep!

View(savanna_cover)


savanna_cover$pCover_class<-as.factor(as.character(savanna_cover$pCover_class))
levels(savanna_cover$pCover_class)


savanna_clean <- savanna_cover %>% # convert cover classes into the midpoint between classes
        mutate( pCover = case_when(pCover_class == "[0,0.2)" ~ "0.1",
                                   pCover_class == "[0.2,0.26)" ~ "0.23",
                                   pCover_class == "[0.26,0.51)" ~ "0.38",
                                   pCover_class == "[0.51,0.6)" ~ "0.55",
                                   pCover_class == "[0.6,0.76)" ~ "0.68",
                                     pCover_class == "[0.76,1.1)" ~ "0.93" ))

savanna_clean$pCover<- as.numeric(savanna_clean$pCover)


# clean the old field data
oldfield_cover <- live_cover %>% 
  filter(!HasBeenPlowed %in% c("0")) %>% # only old fields
  filter(!forest_status %in% c( "HF") ) %>%  # remove heavily forested
  filter(!Year > first_year_burned) %>%  # if Year is greater than first year burned, filter out
  droplevels()

oldfield_cover$site_status<- "old field"

clean_cover <- savanna_clean %>% bind_rows(oldfield_cover)  %>% # bind cleaned old field and remnants data
  mutate(YSA = (Year-YearAb)) %>% # calculate year since agricultural abandonment
  mutate( YSA = ifelse(YearAb =="0" & site_status=="never-plowed","never-plowed",YSA))  %>% # if year ab = 0, then its never plowed
  filter(!LCD_species %in% c("Miscellaneous woody plants","Canopy Cover", "Miscellaneous species",
                             "Mosses & lichens")) %>% droplevels() # remove some other riff raff


plot_count <- clean_cover %>% distinct(Exp,Year,YSA,Field,Transect,Plot) %>%
  group_by(Exp,Year,YSA,Field) %>%
  count() %>% # count number of samps
  arrange(n) %>% ungroup() %>% left_join(clean_cover) %>% 
  filter(n >= 20)  %>% # select sites with 20 samps or greater 
  select(Exp,Year,YSA,Field,Transect,Plot,n,site_status,LCD_species,pCover) %>%
  arrange(Field,Year,Transect,Plot) %>%
  distinct()

View(plot_count)

cover_wide<- plot_count %>% 
  group_by(Exp,Year,YSA,Field,Transect,Plot,n,site_status,LCD_species) %>%
  summarise(pCover = sum(pCover)) %>%
  spread(LCD_species,pCover) %>% # spread to wide format
  ungroup()  %>%
  arrange(Exp,Year,YSA,Field,Transect,Plot) 


View(cover_wide)


cover_select<- cover_wide %>% group_by(Exp,Year,YSA,Field) %>%
  sample_n(20) %>% ungroup() # select only 20 random plots to keep it even

colnames(cover_select)



View(cover_rel)

cover_long <- cover_select %>% gather(Species,pCover, "Achillea millefolium":"Vitis riparia") %>%
  filter(!is.na(pCover)) %>% droplevels() %>%
  arrange(YSA,Field,Transect,Plot,n) # back to long


cover_long$site_status<- as.factor(as.character(cover_long$site_status))

View(cover_long)


cover_rel <- cover_long %>% group_by(site_status, Field, Year, Transect, Plot) %>%
  summarise(pCover_plot_sum = sum(pCover) ) %>%
   left_join(cover_long) %>%
   mutate( Relative_pCover = (pCover/pCover_plot_sum) * 100 ) %>%
  arrange(Field,Year,Transect,Plot) %>% ungroup()

View(cover_rel)





site_check <- distinct(cover_rel, Field)
View(site_check)

write.csv(cover_rel, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/e014_e133_cleaned_1983-2016_EL.csv")

cover_long <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/e014_e133_cleaned_1983-2016_EL.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

head(cover_long)
is.numeric(cover_long$Relative_pCover)


# calculate alpha scale metrics
alpha_dat <- cover_long %>%
  group_by(Exp,site_status,YSA,Field,Year,Transect,Plot) %>%
  summarise(
    alpha_rich = n_distinct(Species),
    alpha_ENSPIE = vegan::diversity(Relative_pCover, index='invsimpson')) %>%
  ungroup()  %>% arrange(Field, Transect, Plot, Year)


head(alpha_dat)


np_alpha_means <- alpha_dat %>% filter(site_status == "never-plowed") %>% 
  summarise(alpha_rich_mean_np = mean(alpha_rich),
            alpha_spie_mean_np = mean(alpha_ENSPIE)
  )

View(np_alpha_means)

colnames(np_alpha_means)

alpha_p <- alpha_dat %>% filter(site_status == "old field") %>% 
  mutate(alpha_rich_p = ((alpha_rich/np_alpha_means$alpha_rich_mean_np) * 100),
         alpha_ENSPIE_p = (( alpha_ENSPIE/np_alpha_means$alpha_spie_mean_np ) * 100),
         YSA = as.numeric(YSA)
  ) %>%
  mutate( 
    log_alpha_rich_p  = log(alpha_rich_p),
    log_alpha_ENSPIE_p = log(alpha_ENSPIE_p),
    log_YSA = log(YSA),
    c.YSA = (YSA-mean(YSA)) ) 


alpha_p$Field<-as.character(alpha_p$Field)
alpha_p$Year<-as.factor(as.character(alpha_p$Year))


View(alpha_dat)
View(alpha_p)

# alpha diversity
write.csv(alpha_dat, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/alpha_div.csv")
# percentage of recovery of old fields compared to never plowed sites
write.csv(alpha_div_p, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/alpha_div_percent.csv")


# gamma scale
cover_long <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/e014_e133_cleaned_1983-2016_EL.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

gamma_mean <- cover_long %>%
  group_by(Exp,site_status,YSA,Field,Year, Species) %>%
  summarise( 
    species_sum = sum(Relative_pCover))   %>% # sum species cover
  mutate(species_av = (species_sum/20), # divide by the number of plots
         species_av_p = (species_av/100), # make a proportion
         species_av_p = round(species_av_p,2)) %>% # round to 2 decimal places
   select(-c(species_sum, species_av)) %>%
  ungroup() %>% arrange(Exp,site_status,Field, Year,YSA) 


gamma_ccr <- gamma_mean %>%
  group_by(Exp,site_status,YSA,Field,Year) %>%
  summarise( 
   gamma_rich = n_distinct(Species),
    gamma_ENSPIE = vegan::diversity(species_av_p, index='invsimpson')) %>%
  ungroup()  %>% 
  arrange(Exp,site_status,Field, Year,YSA)


View(gamma_ccr)

View(gamma_mean)

gamma_ccr2 <- gamma_ccr %>% left_join(alpha_mean)

head(gamma_ccr2)

gamma_div <- gamma_ccr2 %>% 
mutate( beta_div = (gamma_rich/mean_alpha_rich),
        beta_ENSPIE = (gamma_ENSPIE/mean_alpha_ENSPIE))

head(gamma_div)


np_means <- gamma_div %>% filter(site_status == "never-plowed") %>% 
   summarise(gamma_rich_mean_np = mean(gamma_rich),
             beta_div_mean_np = mean(beta_div),
     gamma_spie_mean_np = mean(gamma_ENSPIE),
     beta_spie_mean_np = mean(beta_ENSPIE)
             )

View(np_means)

colnames(gamma_div)

gamma_p <- gamma_div %>% filter(site_status == "old field") %>% 
  mutate(gamma_rich_p = ((gamma_rich/np_means$gamma_rich_mean_np) * 100),
     beta_rich_p = (( beta_div/np_means$beta_div_mean_np ) * 100),
     gamma_ENSPIE_p = (( gamma_ENSPIE/np_means$gamma_spie_mean_np) * 100 ),
     beta_ENSPIE_p = (( beta_ENSPIE/np_means$beta_spie_mean_np) * 100 ),
     YSA = as.numeric(YSA)
    ) %>%
mutate( 
  log_gamma_rich_p  = log(gamma_rich_p),
  log_beta_rich_p = log(beta_rich_p),
  log_gamma_ENSPIE_p = log(gamma_ENSPIE_p),
  log_beta_ENSPIE_p = log(beta_ENSPIE_p),
  log_YSA = log(YSA),
  c.YSA = (YSA-mean(YSA)) ) %>% 
  select(-c(mean_alpha_rich, mean_alpha_ENSPIE))
  
  
gamma_p$Field<-as.character(gamma_p$Field)
gamma_p$Year<-as.factor(as.character(gamma_p$Year))


View(gamma_p)

# mean species cover at the gamma scale
write.csv(gamma_mean, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/gamma_species_mean.csv")
# gamma and beta diversity metrics (richness and spie) for all sites
write.csv(gamma_div, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/gamma_div.csv")
# percentage of recovery of old fields compared to never plowed sites
write.csv(gamma_p, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/gamma_div_percent.csv")