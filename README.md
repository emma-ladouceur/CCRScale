# CCRScale

The recovery of plant community composition following passive restoration across spatial scales: At [Cedar Creek Ecosystem Science Reserve](https://www.cedarcreek.umn.edu/)

*Emma Ladouceur, Forest Isbell, Adam T. Clark, W. Stanley Harpole, Peter B. Reich, G. David Tilman, & Jonathan M. Chase (2022) The recovery of plant community composition following passive restoration across spatial scales. Journal of Ecology. Article DOI: [doi.org/]() & Data DOI: [Coming soon]()*
 
This work is an expansion on [Isbell et al. 2019](https://www.nature.com/articles/s41559-019-1012-1).

### Data
Species-level data is available at [Cedar Creek Data Catalog](https://www.cedarcreek.umn.edu/research/data). Data openly available to reproduce results includes;

**alpha_div.csv** Alpha diversity

**alpha_div_percent.csv** Alpha diversity

**gamma_div.csv** Gamma and Whittaker's Beta diversity

**gamma_div_percent.csv** Gamma and Whittaker's Beta diversity

**beta.df.csv** Turnover and Nestedness components of Jaccard's index

**multi_scale**

**cover_groups**

### R Scripts
This code is designed to be a pedagogic example of examining biodiversity change across scales.

**1_Data_wrangle.R** Wrangling data and quantifying metrics across scales.

**2_Discrete_models.R** Models, data extraction from models and figures. Figure 2a) & b), 3a) & b), 4a) & c). Supplementary Figures S3a) & b), S4a) & b), S5a) & c),

**3_Percent_models.R** Models, data extraction from models and figures. Figure 2c) & d), 3c) & d), 4b) & d). Supplementary Figures S3c) & d), S4c) & d), S5b) & d),

**4_Turnover_Nestedness.R** Quantifying metrics, models, data extraction from models and figures. Figure 5a) & b). Supplementary Figure S6a) & b)

**4_Functional_Groups.R** Models, data extraction from models and figures. Figure 6. Supplementary Figure S7a), b), & S10.

**6_Multiscale.R** Sample-Based species accumulation, rarefaction, extrapolation, Hill numbers. Figure S1, S2, & S8.

**7_Figure_S9.R** Checking model residuals for auto-correlation. Figure S9.

**8_Figure_S11.R** Predicting time to 100% recovery at different scales. Figure S11.

**9_Table_S1.R** Table S1.

