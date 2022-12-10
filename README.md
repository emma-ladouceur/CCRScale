

# CCRScale

The recovery of plant community composition following passive restoration across spatial scales: At [Cedar Creek Ecosystem Science Reserve](https://www.cedarcreek.umn.edu/)

*Emma Ladouceur, Forest Isbell, Adam T. Clark, W. Stanley Harpole, Peter B. Reich, G. David Tilman, & Jonathan M. Chase (2022) The recovery of plant community composition following passive restoration across spatial scales. Journal of Ecology. Article DOI: [doi.org/]() & Data DOI: [Coming soon]()*
 
This work builds on [Isbell et al. 2019](https://www.nature.com/articles/s41559-019-1012-1), using a scale-explicit approach to quantifying [Measurements of Biodiversity](https://doi.org/10.1111/2041-210X.13102). 

### Data
Species-level data is available at [Cedar Creek Data Catalog](https://www.cedarcreek.umn.edu/research/data). Aggregate-level diversity metrics openly available to reproduce results includes;

* **alpha_div.csv** <code>α-diversity (species richness & ENS<sub>PIE</sub>) (plot = 0.5m<sup>2</sup>)</code>

* **alpha_div_percent.csv** <code>α-diversity (species richness & ENS<sub>PIE</sub>) as a percentage of the average diversity of all never-ploughed plots.</code>

* **gamma_div.csv** <code>γ-diversity (species richness & ENS<sub>PIE</sub>) (Field = 20 plots = 10m<sup>2</sup>) & Whittaker's β-diversity, β-ENS<sub>PIE</sub> (γ/α).</code>

* **gamma_div_percent.csv** <code>γ-diversity (species richness & ENS<sub>PIE</sub>) & Whittaker's β-diversity, β-ENS<sub>PIE</sub> (γ/α) as a percentage of the average diversity of never-ploughed fields at each scale and for each metric.</code>

* **checklist.csv** <code>Species checklist for every old field, and across all never-ploughed fields for each year, used to calculate pairwise turnover and nestedness components of Jaccard's index.</code>

* **beta.df.csv** <code>Turnover and Nestedness components of Jaccard's index, comparing a checklist of each field (γ-scale) at each time point to the nearest measured time point for the checklist for all never-ploughed fields (regional-γ-scale).</code>

* **func_groups_percent** <code>Relative cover (α-scale = plot = 0.5m<sup>2</sup>) of different functional groups (graminoid, forb, legume) and their origin (native, exotic) and as a percentage of that found on average in never-ploughed plots.</code>

* **multi_scale** <code>Sample-based rarefied (1-19 samples), observed (20 samples) and extrapolated (21-50 samples) diversity (Hill numbers, q = 0, equivelent to species richness), across multiple scales of sampling effort for each field.</code>

* **np_means** <code>Mean diversity of never-ploughed fields at each scale and for each metric. Used to calculate relative percentages of old field recovery.</code>

* OTHER ENTITIES: **model objects** <code>Each linear mixed effects model used in this analyses is saved as a model object *(.RData)* so you can just load them to recreate figures, rather than run them on your local machine.</code>

### R Scripts
This code is designed to be a pedagogic example of examining biodiversity change across scales.

* **1_Data_wrangle.R** <code>Wrangling data and quantifying metrics across scales.</code>

* **2_Discrete_models.R** <code>Models, data extraction from models and figures for discrete analysis. Figure 2a) & b), 3a) & b), 4a) & c). Supplementary Figures S3a) & b), S4a) & b), S5a) & c),</code>

* **3_Percent_models.R** <code>Models, data extraction from models and figures for continuous analysis. Figure 2c) & d), 3c) & d), 4b) & d). Supplementary Figures S3c) & d), S4c) & d), S5b) & d),</code>

* **4_Turnover_Nestedness.R** <code>Quantifying metrics, models, data extraction from models and figures. Figure 5a) & b). Supplementary Figure S6a) & b).</code>

* **4_Functional_Groups.R** <code>Models, data extraction from models and figures. Figure 6. Supplementary Figure S7a), b), & S10.</code>

* **6_Multiscale.R** <code>Sample-based rarefied, observed, and extrapolated diversity (Hill numbers, q = 0), across multiple scales of sampling effort for each field. Figure S1, S2, & S8.</code>

* **7_Figure_S9.R** <code>Checking model residuals for auto-correlation. Figure S9.</code>

* **8_Figure_S11.R** <code>Predicting time to 100% recovery at different scales. Figure S11 a)-c).</code>

* **9_Table_S1.R** <code>Table S1.</code>

