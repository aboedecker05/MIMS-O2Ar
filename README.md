# MIMS-O2Ar
Data and code to reproduce analyses in "Evaluating O2:Ar, N2:Ar, and 29,30N2 
using Membrane Inlet Mass Spectrometry configured to minimize oxygen 
interference" by Boedecker et al.

More detailed descriptions are in the R script "MIMS Paper Analyses.R"

Libraries needed:
lmodel2 - for reduced major axis regression
broom - for interpreting model output
rstatix - for stats interpretations
tidyverse and ggpubr - for data visualization

Data files:
Compiled_Subcategories.csv
Isotopes.csv
Isotopes_long.csv

Reduced Major Axis Regression (Figures 1 and 2)
We used RMA regression because gas measurements from the MIMS both with and without the 
furnace have error associated with the terms. They are also autocorrelated because they
come from the same sample and there is no defined dependent-independent relationship.

Citation: 
Friedman, J., Bohonak, A. J., & Levine, R. A. (2013). When are two pieces better than one: 
fitting and testing OLS and RMA regressions. Environmetrics, 24(5), 306-316.

For Figure 3, we calcuated the N2:Ar effect size (delta N2:Ar) for each experiment and 
graphed the results over the range of O2:Ar. 

Figure 4:
Paired t test assumes 3 things:
  1. Two groups are paired. -- Came from the same bottle
  2. No significant outliers -- no extreme outliers, see below
  3. Normality - difference of pairs follow a normal distribution
Computed difference between groups
30N2:28N2 were normally distributed, but 29N2:28N2 were not
Used a paired t test on 30N2:28N2 and a sign test on 29N2:28N2
