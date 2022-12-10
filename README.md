

# CCRScale

The recovery of plant community composition following passive restoration across spatial scales: At [Cedar Creek Ecosystem Science Reserve](https://www.cedarcreek.umn.edu/)

*Emma Ladouceur, Forest Isbell, Adam T. Clark, W. Stanley Harpole, Peter B. Reich, G. David Tilman, & Jonathan M. Chase (2022) The recovery of plant community composition following passive restoration across spatial scales. Journal of Ecology. Article DOI: [doi.org/]() & Data DOI: [Coming soon]()*
 
This work builds on [Isbell et al. 2019](https://www.nature.com/articles/s41559-019-1012-1), using a scale-explicit approach to quantifying [Measurements of Biodiversity](https://doi.org/10.1111/2041-210X.13102). See another pedagogic example employing this framework [here](https://doi.org/10.1111/1365-2664.13549).

### Data
Species-level data is available at [Cedar Creek Data Catalog](https://www.cedarcreek.umn.edu/research/data). Aggregate-level diversity metrics openly available to reproduce results includes;

<code>**alpha_div.csv** α-diversity (plot = 0.5m<sup>2</sup>)

**alpha_div_percent.csv** α-diversity as a percentage of the average diversity of all never-ploughed plots.

**gamma_div.csv** γ (Field = 20 plots = 10m^2^) and Whittaker's β-diversity (γ/α)

**gamma_div_percent.csv** γ and Whittaker's β-diversity (γ/α) as a percentage of the average diversity of never-ploughed fields at each scale and for each metric.

**checklist.csv** Species checklist for every old field, and across all never-ploughed fields for each year, used to calculate pairwise turnover and nestedness components of Jaccard's index.

**beta.df.csv** Turnover and Nestedness components of Jaccard's index, comparing a checklist of each field (γ-scale) at each time point to the nearest measured time point for the checklist for all never-ploughed fields (regional-γ-scale).

**func_groups_percent** Relative cover (α-scale - plot = 0.5m^2^) of different functional groups (graminoid, forb, legume) and their origin (native, exotic) and as a percentage of that found on average in never-ploughed plots.

**multi_scale** Sample-based rarefied (1-19 samples), observed (20 samples) and extrapolated (21-50 samples) diversity (Hill numbers, q = 0), across multiple scales of sampling effort for each field.

**np_means** Mean diversity of never-ploughed fields at each scale and for each metric. Used to calculate relative percentages of old field recovery.

OTHER ENTITIES: **model objects** Each linear mixed effects model used in this analyses is saved as a model object so you can just load them to recreate figures, rather than run them on your local machine.

### R Scripts
This code is designed to be a pedagogic example of examining biodiversity change across scales.

**1_Data_wrangle.R** Wrangling data and quantifying metrics across scales.

**2_Discrete_models.R** Models, data extraction from models and figures for discrete analysis. Figure 2a) & b), 3a) & b), 4a) & c). Supplementary Figures S3a) & b), S4a) & b), S5a) & c),

**3_Percent_models.R** Models, data extraction from models and figures for continuous analysis. Figure 2c) & d), 3c) & d), 4b) & d). Supplementary Figures S3c) & d), S4c) & d), S5b) & d),

**4_Turnover_Nestedness.R** Quantifying metrics, models, data extraction from models and figures. Figure 5a) & b). Supplementary Figure S6a) & b)

**4_Functional_Groups.R** Models, data extraction from models and figures. Figure 6. Supplementary Figure S7a), b), & S10.

**6_Multiscale.R** Sample-based rarefied, observed, and extrapolated diversity (Hill numbers, q = 0), across multiple scales of sampling effort for each field. Figure S1, S2, & S8.

**7_Figure_S9.R** Checking model residuals for auto-correlation. Figure S9.

**8_Figure_S11.R** Predicting time to 100% recovery at different scales. Figure S11 a)-c).

**9_Table_S1.R** Table S1.

