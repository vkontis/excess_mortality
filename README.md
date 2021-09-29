# Excess mortality estimation

This repository contains the code used in [V. Kontis, J.E. Bennett, R.M. Parks, T. Rashid, J. Pearson-Stuttard, P. Asaria, B. Zhou, M. Guillot, C. D. Mathers Y.-H. Khang, M. McKee and M. Ezzati, Lessons learned and lessons missed: impact of the coronavirus disease 2019 (COVID-19) pandemic on all-cause mortality in 40 industrialised countries prior to mass vaccination.]()


## Dependencies

The code requires `R` version 3.6 or higher and  the following packages: `INLA` (>= 20.03), `tidyverse` (>= 1.3), `glue` (>= 1.4), `logging` (>= 0.10), `matrixStats` (>= 0.56).

`INLA` can be obtained from http://www.r-inla.org/download. The remaining packages are available from CRAN and can be installed using `install.packages()`:

```r
install.packages('INLA', repos = c(getOption('repos'), INLA = 'https://inla.r-inla-download.org/R/stable'), dependencies = TRUE)
install.packages(c('tidyverse', 'glue', 'logging', 'matrixStats'))
```

## Data

Weekly deaths data, for all countries are available in `data/data.csv`. The data sources are detailed in the published paper. The column `week` contains the date indicating the start of the week. The columns `t2m` and `weekly_t2m_anomaly` contain (weekly average) temperature and deviation of temperature from the long-term average in each week.


## How to run models

The countries, ages, and sexes for which the analysis is to be run are specified in a csv file (see `data/analysis_units.csv` for an example). Create a csv file with only the countries, ages and sexes to be analysed (the country, age and sex values must match those in `data/data.csv`). Then modify `run.R` so that the file you have created is loaded in the `ANALYSIS_UNITS` variable. Finally, run `run.R`.


## Detailed results

The file `output/result_summaries.csv` contains results by country, sex, week and age group, as well as totals across sexes and age groups. The columns between `q025` and `q975` contain the quantiles of interest. For example, `q025` contains the 2.5th percentile, `q050` the 5th percentile, and so on.
