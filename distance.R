
rm(list = ls())

# load packagaes
library(tidyverse)
library(ggplot2)
library(brms)
library(bayesplot)
library(patchwork)
library(raster)
library(geosphere)


locations <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/E14_location_matrix.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

head(locations)

mean_locations <- locations %>% dplyr::select(xloc, yloc, field, exp) %>%
  group_by(field, exp) %>% summarise(mean_xloc = mean(xloc),
                                mean_yloc = mean(yloc)) %>% ungroup()


head(mean_locations)
View(mean_locations)

fields <- mean_locations %>% dplyr::select(field, exp, mean_xloc, mean_yloc) %>%
  unite(loc, field, exp, remove = T ) %>%
  mutate(loc = as.factor(loc)) %>%
 # column_to_rownames( var = "field_exp") %>%
  mutate(lon = mean_xloc,
         lat = mean_yloc) %>% dplyr::select(-c(mean_xloc, mean_yloc)) %>%
  group_by(loc) %>%
  nest(lon, lat, .key = coords)

head(fields)

pw_coords <- fields %>%
  ungroup() %>%
  mutate(loc1 = loc) %>%
  dplyr::select(starts_with("loc")) %>%
  complete(loc, loc1) %>%
  filter(loc != loc1) %>%
  left_join(fields, by = "loc") %>%
  left_join(fields, by = c("loc1" = "loc")) %>%
  # Grid completed. Calcualte the distance by distHaversine
  mutate(distance = map2_dbl(coords.x, coords.y, distHaversine))

View(pw_coords)

field_dist <- pw_coords %>% dplyr::select(loc, loc1, distance) %>%
  filter(loc1 == "npc_133") %>% arrange(distance) %>%
  separate(loc, c("Field", "Exp"), sep = "_") %>%
  filter(Exp == 14) %>% 
  mutate( `Old field` = fct_recode( Field,  "A" = "601",
                                                                                 "B" = "600",
                                                                                 "C" = "10",
                                                                                 "D" = "28",
                                                                                 "E" = "41",
                                                                                 "F" = "39",
                                                                                 "G" = "40",
                                                                                 "H" = "4",
                                                                                 "I" = "44",
                                                                                 "J" = "53",
                                                                                 "K" = "47",
                                                                                 "L" = "21",
                                                                                 "M" = "70",
                                                                                 "N" = "5",
                                                                                 "O" = "27",
                                                                                 "P" = "45",
                                                                                 "Q" = "32",
                                                                                 "R" = "35",
                                                                                 "S" = "72"
  )) 

field_dist

View(field_dist)

write.csv(field_dist, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/field_dist.csv")

# datasets
field_dist <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/field_dist.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

alpha_dat_p <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/alpha_div_percent.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
gamma_dat_p <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/E14 _133/gamma_div_percent.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

beta <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/beta.df.csv", header=TRUE) %>%
  as_tibble()

# percent models
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/alpha_rich_c.Rdata") 
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/gamma_rich_c.Rdata") 
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/beta_div_c.Rdata") 

load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/alpha_pie_c.Rdata") 
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/gamma_pie_c.Rdata") 
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/percent/beta_pie_c.Rdata") 

load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/turnover.Rdata") # object name: ccr.turnover
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/CCRScale/data/model_fits/nest.Rdata") # object name: ccr.nest

field_dist <- field_dist %>%  mutate(`distance from never-plowed centroid (m)` = distance) %>% dplyr::select(-c(X, distance))
  
alpha_dat_p_d <- alpha_dat_p %>%  dplyr::select(-X) %>% left_join(field_dist) 
  
gamma_dat_p_d <- gamma_dat_p %>%  dplyr::select(-X) %>% left_join(field_dist) 
gamma_dat_p_d

beta_d <- beta %>%  dplyr::select(-X) %>% mutate( Exp = 14) %>% left_join(field_dist) 


r.ar <- residuals(p.alpha.rich)
r.ar <- as.data.frame(r.ar)
ar.plot <- cbind(alpha_dat_p_d, r.ar$Estimate)

r.gr <- residuals(p.gamma.rich)
r.gr <- as.data.frame(r.gr)
gr.plot <- cbind(gamma_dat_p_d, r.gr$Estimate)

r.bd <- residuals(p.beta.div)
r.bd <- as.data.frame(r.bd)
bd.plot <- cbind(gamma_dat_p_d, r.bd$Estimate)


r.as <- residuals(p.alpha.spie)
r.as <- as.data.frame(r.as)
as.plot <- cbind(alpha_dat_p_d, r.as$Estimate)

r.gs <- residuals(p.gamma.spie)
r.gs <- as.data.frame(r.gs)
gs.plot <- cbind(gamma_dat_p_d, r.gs$Estimate)

r.bs <- residuals(p.beta.spie)
r.bs <- as.data.frame(r.bs)
bs.plot <- cbind(gamma_dat_p_d, r.bs$Estimate)


r.jtu <- residuals(ccr.turnover)
r.jtu <- as.data.frame(r.jtu)
jtu.plot <- cbind(beta_d, r.jtu$Estimate)

r.jne <- residuals(ccr.nest)
r.jne <- as.data.frame(r.jne)
jne.plot <- cbind(beta_d, r.jne$Estimate)


par(mfrow=c(3,3))
with(ar.plot, plot(`distance from never-plowed centroid (m)`, r.ar$Estimate))
with(gr.plot, plot(`distance from never-plowed centroid (m)`, r.gr$Estimate))
with(bd.plot, plot(`distance from never-plowed centroid (m)`, r.bd$Estimate))
with(as.plot, plot(`distance from never-plowed centroid (m)`, r.as$Estimate))
with(gs.plot, plot(`distance from never-plowed centroid (m)`, r.gs$Estimate))
with(bs.plot, plot(`distance from never-plowed centroid (m)`, r.bs$Estimate))
with(jtu.plot, plot(`distance from never-plowed centroid (m)`, r.jtu$Estimate))
with(jne.plot, plot(`distance from never-plowed centroid (m)`, r.jne$Estimate))

