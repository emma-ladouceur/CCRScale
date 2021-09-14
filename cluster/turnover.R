
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/CCRScale'
beta <- read.csv(paste0(path, '/beta.df.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


beta$Field <- as.factor(as.character(beta$Field))

ccr.turnover <- brm(jtu ~  YSA + (1 + YSA | Field) + (1 | Year),
                         family = zero_one_inflated_beta(),
                         data = beta,
                         inits = '0',
                         cores = 4, chains = 4,
                    control = list(adapt_delta = 0.99))

save(ccr.turnover,
     file=Sys.getenv('OFILE'))



