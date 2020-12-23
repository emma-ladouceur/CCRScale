

# Multiscale Analysis

library(tidyverse)
library(patchwork)
library(iNEXT)
library(ggplot2)
library(viridis)


cover <- read.csv("~/Dropbox/Projects/CCRScale/E14 _133/e014_e133_cleaned_1983-2016.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
