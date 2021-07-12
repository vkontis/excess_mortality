# Load libraries and local scripts
library(tidyverse)
library(INLA)

inla.setOption(scale.model.default = TRUE)

source('R/inla_formula.R')
source('R/train.R')
source('R/models.R')
source('R/model_average.R')
source('R/postprocessing.R')

# Set the following variables to the desired values:
# 
# OUTPUT_DIR is the folder where results should be saved (relative to the 
#   current working directory). It will be created if it doesn't already exist.
#   Note: Existing results in this folder will be overwritten! 
OUTPUT_DIR <- 'results'  

# Choose countries (or US states) and sex/age groups
ANALYSIS_UNITS <- readr::read_csv('data/analysis_units.csv') %>% 
    filter(
        country %in% c('chile', 'france', 'usa'), 
        age == 'all_ages', 
        sex == 'both_sexes'
    )

# Load data and keep only countries, ages and sexes of interest
deaths <- readr::read_csv('data/data.csv') %>%
    inner_join(ANALYSIS_UNITS) %>%
    prep_train_data


# Set up a data frame with models to fit. The bank holiday intercepts are 
# inferred by which columns contain missing values for each country (all 
# countries assigned)
train_grid <- deaths %>% 
    group_by(country, sex, age) %>% 
    group_modify(function(d, g) {
        bank_holiday_intercepts <- d %>% 
            select(starts_with('bank_holiday')) %>% 
            discard(~all(is.na(.x))) %>% 
            names
        model_fmls(bh_intercepts = bank_holiday_intercepts)
    }) %>% 
    ungroup %>% 
    arrange(country, sex, age, model_name)

# At this point train_grid contains a row for each country, sex, age and model
# to be trained. Next, train the models. 
fits <- train_grid %>%
    train_many(data = deaths, num_posterior_draws = 1000)

# Combine posterior draws to obtain draws from the model average
results <- combine_draws(
    fits, num_posterior_draws = 16000, with_replacement = FALSE)

excess_results <- calculate_excess_mortality(results)

# Summarise results
excess_result_summaries <- summarise_results(excess_results)

# Save results
dir.create(OUTPUT_DIR, showWarnings = FALSE)
write_csv(result_summaries, file.path(OUTPUT_DIR, 'result_summaries.csv'))

