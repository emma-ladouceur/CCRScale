# CCRScale

The recovery of plant community composition following passive restoration across spatial scales: At [Cedar Creek Ecosystem Science Reserve](https://www.cedarcreek.umn.edu/)

*Emma Ladouceur, Forest Isbell, Adam T. Clark, W. Stanley Harpole, Peter B. Reich, G. David Tilman, & Jonathan M. Chase (2022) The recovery of plant community composition following passive restoration across spatial scales. Journal of Ecology. Article DOI: [doi.org/]() & Data DOI: [Coming soon]()*
 
This work builds on [Isbell et al. 2019](https://www.nature.com/articles/s41559-019-1012-1), using a scale-explicit approach to quantifying [Metrics of Biodiversity](https://doi.org/10.1111/2041-210X.13102).

### Data
Species-level data is available at [Cedar Creek Data Catalog](https://www.cedarcreek.umn.edu/research/data). Data openly available to reproduce results includes;

**alpha_div.csv** Alpha diversity (plot = 0.5m2)

**alpha_div_percent.csv** Alpha diversity as a percentage of the average diversity of all never-ploughed plots.

**gamma_div.csv** Gamma (Field = 20 plots) and Whittaker's Beta diversity (gamma/alpha)

**gamma_div_percent.csv** Gamma and Whittaker's Beta diversity (gamma/alpha) as a percentage of the average diversity of never-ploughed fields at each scale and for each metric.

**beta.df.csv** Turnover and Nestedness components of Jaccard's index, comparing a checklist of each field (gamma-scale) at each time point to the nearest measured time point for the checklist for all never-ploughed fields (regional-gamma-scale).

**multi_scale** Sample-based rarefied and extrapolated species richness (q = 0) for each field.

**cover_groups** Relative cover (plots = 0.5m2) of different functional groups (graminoid, forb, legume) and their origin (native, exotic) and as a percentage of that found on average in never-ploughed fields.

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

