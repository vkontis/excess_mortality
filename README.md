# Excess mortality estimation

This repository contains the code used in [V. Kontis, J.E. Bennett, T. Rashid, R. M. Parks, J. Pearson-Stuttard, M. Guillot, P. Asaria, B. Zhou, M. Battaglini, G. Corsetti, M. McKee, M. Di Cesare, C. D. Mathers and M. Ezzati, Magnitude, demographics and dynamics of the effect of the first wave of the COVID-19 pandemic on all-cause mortality in 21 industrialized countries. Nature Medicine (2020). doi: 0.1038/s41591-020-1112-0](https://doi.org/10.1038/s41591-020-1112-0.).


## Prerequisites 

The code requires `R` version 3.6 or higher and  the following packages: `INLA` (>= 20.03), `tidyverse` (>= 1.3), `glue` (>= 1.4), `logging` (>= 0.10), `matrixStats` (>= 0.56).

`INLA` can be obtained from http://www.r-inla.org/download. The remaining packages are available from CRAN and can be installed using `install.packages()`.


## Data 

Weekly deaths data, for all countries until the end of May are available in `data/data.csv`. The data sources are detailed in the published paper. The column `week` contains the date indicating the start of the week. The columns `t2m` and `weekly_t2m_anomaly` contain (weekly average) temperature and deviation of temperature from the long-term average in each week. 


## How to run models

To run the models, modify `run.R` by setting the variables `OUTPUT_DIR`, `COUNTRIES`, `SEXES` and `AGES` to the desired values, and then run the script. It is recommended to run countries in different R sessions, by using a different output folder for each country. 


## Detailed results

The file `output/result_summaries.csv` contains results by country, sex, week and age group, as well as totals across sexes and age groups. The columns `q025`, `q050`, ..., `q975` contain the quantiles of interest. For example, `q025` contains the 2.5th percentile, `q050` the 5th percentile, and so on. 


