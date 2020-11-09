


library(tidyverse)
library(patchwork)
library(iNEXT)
library(ggplot2)
library(viridis)


ccr_comm_prep <- read.csv("~/Dropbox/Projects/CCRScale/E14 _133/star_prep.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


time.yr <- ccr_comm_prep %>% 
  select(Field,Year) %>% distinct() %>% arrange(Field,Year) %>% group_by(Field) %>%
  mutate(Time = 1:n()) %>% arrange(Field,Year,Time)

View(time.yr)

plot.n <- ccr_comm_prep %>% 
  select(Field,Year,Transect,Plot) %>% distinct() %>% arrange(Field,Year,Transect,Plot) %>% group_by(Field,Year) %>%
  mutate(Plot.n = 1:n()) %>% arrange(Field,Year,Transect,Plot.n)

View(plot.n)

ccr_prep <- ccr_comm_prep %>% left_join(time.yr) %>% left_join(plot.n)  %>%
  arrange(Field,Year,Transect,Plot.n)


View(ccr_prep)

colnames(ccr_prep)

ccr_prep$Time<-as.factor(as.character(ccr_prep$Time))
levels(ccr_prep$Time)



ccr.list.1 <- ccr_prep %>% filter( Time == "1") %>%
  split(.$Field)


ccr.list.1 <- map(ccr.list.1, ~ .x %>% 
                    select(Plot.n,Species,Pres) %>%
                    spread(key = Plot.n, value = Pres) %>%
                    replace(is.na(.), 0) %>%
                    column_to_rownames(var = "Species") )

View(ccr.list.1)

ccr.all.out.time.1 <- iNEXT(ccr.list.1, q=0, datatype="incidence_raw")

View(ccr.all.out.time)


ccr.all.time.df.1 <- fortify(ccr.all.out.time.1, type=1)

View(ccr.all.time.df.1)

ccr.star.time.1 <- ccr.all.time.df.1 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "1")

View(ccr.star.time.1)


# 2
ccr.list.2 <- ccr_prep %>% filter( Time == "1" | Time =="2") %>%
  split(.$Field)

ccr.list.2 <- map(ccr.list.2, ~ .x %>% 
                    select(Plot.n,Species,Pres) %>%
                    distinct() %>%
                    spread(key = Plot.n, value = Pres) %>%
                    replace(is.na(.), 0) %>%
                    column_to_rownames(var = "Species") )

ccr.all.out.time.2 <- iNEXT(ccr.list.2, q=0, datatype="incidence_raw")

ccr.all.time.df.2 <- fortify(ccr.all.out.time.2, type=1)


ccr.star.time.2 <- ccr.all.time.df.2 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "2")


# 3
ccr.list.3 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3") %>%
  split(.$Field)

ccr.list.3 <- map(ccr.list.3, ~ .x %>% 
                    select(Plot.n,Species,Pres) %>%
                    distinct() %>%
                    spread(key = Plot.n, value = Pres) %>%
                    replace(is.na(.), 0) %>%
                    column_to_rownames(var = "Species") )

ccr.all.out.time.3 <- iNEXT(ccr.list.3, q=0, datatype="incidence_raw")

ccr.all.time.df.3 <- fortify(ccr.all.out.time.3, type=1)


ccr.star.time.3 <- ccr.all.time.df.3 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "3")



# 4
ccr.list.4 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4") %>%
  split(.$Field)

ccr.list.4 <- map(ccr.list.4, ~ .x %>% 
                    select(Plot.n,Species,Pres) %>%
                    distinct() %>%
                    spread(key = Plot.n, value = Pres) %>%
                    replace(is.na(.), 0) %>%
                    column_to_rownames(var = "Species") )

ccr.all.out.time.4 <- iNEXT(ccr.list.4, q=0, datatype="incidence_raw")

ccr.all.time.df.4 <- fortify(ccr.all.out.time.4, type=1)


ccr.star.time.4 <- ccr.all.time.df.4 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "4")


# 5
ccr.list.5 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5") %>%
  split(.$Field)

ccr.list.5 <- map(ccr.list.5, ~ .x %>% 
                    select(Plot.n,Species,Pres) %>%
                    distinct() %>%
                    spread(key = Plot.n, value = Pres) %>%
                    replace(is.na(.), 0) %>%
                    column_to_rownames(var = "Species") )

ccr.all.out.time.5 <- iNEXT(ccr.list.5, q=0, datatype="incidence_raw")

ccr.all.time.df.5 <- fortify(ccr.all.out.time.5, type=1)


ccr.star.time.5 <- ccr.all.time.df.5 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "5")



# 6
ccr.list.6 <- ccr_prep %>% filter( Time == "1" | Time =="2"| Time =="3" | Time =="4"| Time =="5"| Time =="6") %>%
  split(.$Field)

ccr.list.6 <- map(ccr.list.6, ~ .x %>% 
                    select(Plot.n,Species,Pres) %>%
                    distinct() %>%
                    spread(key = Plot.n, value = Pres) %>%
                    replace(is.na(.), 0) %>%
                    column_to_rownames(var = "Species") )

ccr.all.out.time.6 <- iNEXT(ccr.list.6, q=0, datatype="incidence_raw")

ccr.all.time.df.6 <- fortify(ccr.all.out.time.6, type=1)


ccr.star.time.6 <- ccr.all.time.df.6 %>% filter(method == "interpolated" | method == "observed") %>% distinct () %>%
  rename(plot = x) %>% rename(richness = y) %>% mutate(time = "6")


ccr.star <- ccr.star.time.1 %>% bind_rows(ccr.star.time.2) %>% bind_rows(ccr.star.time.3)%>% bind_rows(ccr.star.time.4)%>% 
  bind_rows(ccr.star.time.5) %>% bind_rows(ccr.star.time.6)

head(ccr.star)  

ccr.star$log10.plot<-log10(ccr.star$plot)
ccr.star$log10.rich<-log10(ccr.star$richness)
ccr.star$time.f<-as.factor(ccr.star$time)

ccr.star$time.f<-factor(ccr.star$time.f,  levels=c("1","2","3","4","5","6","7","8","9","10","11","12" ))

ccr.star <- ccr.star %>% filter(site %in% c( "101", "102","103", "21", "27", "28",
                                 "32", "35","39", "40",  "41",  "44",   "45","47",  "53",  "70") )

View(ccr.star)


star.species.area<-ggplot() +
  facet_grid(.~site)+
  geom_point(data = ccr.star,
             aes(x = log10.plot, y = log10.rich,
                 colour = time.f), alpha=0.6,
             size = 1.3)+
  geom_line(data = ccr.star,
            aes(x = log10.plot, y = log10.rich,
                colour = time.f))+
  scale_color_viridis(discrete=TRUE,option = "plasma") +
  theme_classic()+theme(axis.text.x = element_text(size=6, angle=7), 
                        legend.direction = "horizontal", legend.position = "bottom")+
  guides(col = guide_legend(ncol = 13))+
  labs(title = "Species-Area",color="Year Sampled (Time)") + ylab("Log10 Species Richness") + xlab("Log10 Area") 

star.species.area


head(ccr.star)

ccr.star$time.n<-as.numeric(ccr.star$time)
ccr.star$log10.time<-log10(ccr.star$time.n)
ccr.star$plot.f<-as.factor(ccr.star$plot)

star.species.time<-ggplot() +
  facet_grid(.~site)+
  geom_point(data = ccr.star,
             aes(x = log10.time, y = log10.rich,
                 colour = `plot.f`), alpha=0.6,
             size = 1.3)+
  geom_line(data = ccr.star,
            aes(x = log10.time, y = log10.rich,
                colour = `plot.f`))+
  scale_color_viridis(discrete=TRUE) +
  theme_classic()+theme(axis.text.x = element_text(size=6, angle=7), 
                        legend.direction = "horizontal", legend.position = "bottom" )+
  guides(col = guide_legend(ncol = 10))+
  labs(title = "Species-Time",color= "Number of Samples (Area)") + ylab("Log10 Species Richness") + xlab("Log10 Time") 

star.species.time


(star.species.area )/ (star.species.time)




View(ccr.star)
colnames(ccr.star)


ccr.star.area<- ccr.star %>% filter(time.f == "6")

ccr.star.time<- ccr.star %>% filter(plot.f == "20")

View(ccr.star.area)

star.species.area.mx <-ggplot() +
  #facet_grid(.~site)+
  geom_point(data = ccr.star.area,
             aes(x = log10.plot, y = log10.rich,
                 group=Field,colour = YSA), alpha=0.6,
             size = 1.3)+
  geom_line(data = ccr.star.area,
            aes(x = log10.plot, y = log10.rich,
                group=Field,colour = YSA))+
  scale_color_viridis(discrete=TRUE,option = "plasma") +
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), 
                        legend.direction = "horizontal", legend.position = "bottom")+
  guides(col = guide_legend(ncol = 13))+
  labs(title = "Species-Area",color="Site") + ylab("Log10 Species Richness") + xlab("Log10 Area") 

star.species.area.mx




star.species.time.mx<-ggplot() +
  #facet_grid(.~site)+
  geom_point(data = ccr.star.time,
             aes(x = log10.time, y = log10.rich,
                 group=Field,colour = YSA), alpha=0.6,
             size = 1.3)+
  geom_line(data = ccr.star.time,
            aes(x = log10.time, y = log10.rich,
                group=Field,colour = YSA))+
  scale_color_viridis(discrete=TRUE) +
  theme_classic()+theme(axis.text.x = element_text(size=9, angle=7), 
                        legend.direction = "horizontal", legend.position = "bottom" )+
  guides(col = guide_legend(ncol = 10))+
  labs(title = "Species-Time",color= "Site") + ylab("Log10 Species Richness") + xlab("Log10 Time") 

star.species.time.mx


