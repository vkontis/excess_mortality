# Set the following variables to the desired values:
# 
# OUTPUT_DIR is the folder where results should be saved (relative to the 
#   current working directory). It will be created if it doesn't already exist.
#   Note: Existing results in this folder will be overwritten! 
# COUNTRIES defines the countries for which models are run. It is allowed to 
#   contain one or more values from the following list:
#   'australia', 'austria', 'belgium', 'bulgaria', 'czechia', 'denmark', 
#   'england_wales', 'finland', 'france', 'hungary', 'italy', 'netherlands', 
#   'new_zealand', 'norway', 'poland', 'portugal', 'scotland', 'slovakia', 
#   'spain', 'sweden', 'switzerland'
# AGES defines the age groups. It can be '0-64', '65+', or c('0-64', '65+') (to
#   run both).
# SEXES is one of 'men', 'women', or c('men', 'women').
OUTPUT_DIR <- 'results'  
COUNTRIES <- c(
    'australia', 'austria', 'belgium', 'bulgaria', 'czechia', 'denmark', 
    'england_wales', 'finland', 'france', 'hungary', 'italy', 'netherlands', 
    'new_zealand', 'norway', 'poland', 'portugal', 'scotland', 'slovakia', 
    'spain', 'sweden', 'switzerland'
)
AGES <- c('0-64', '65+')
SEXES <- c('men', 'women')

# Load libraries and local scripts
library(tidyverse)
library(INLA)

inla.setOption(scale.model.default = TRUE)

source('R/inla_formula.R')
source('R/train.R')
source('R/models.R')
source('R/model_average.R')
source('R/postprocessing.R')

# Load data and keep only countries, ages and sexes of interest
deaths <- read_csv('data/data.csv') %>%
    filter(country %in% COUNTRIES, age %in% AGES, sex %in% SEXES) %>% 
    prep_train_data

# Set up a data frame with models to fit. England and Wales and Scotland 
# use different bank holiday intercepts, so they are treated separately.
train_grid <- list(NULL, NULL)
ew_scot <- intersect(c('england_wales', 'scotland'), deaths$country)
if (length(ew_scot) > 0) {
    ew_scot_models <- model_fmls(bh_intercepts = c(
        'bank_holiday_christmas', 'bank_holiday_new_year',
        'bank_holiday_early_may', 'bank_holiday_easter_monday', 
        'bank_holiday_good_friday', 'bank_holiday_spring', 
        'bank_holiday_summer'
    ))
    train_grid[[1]] <- expand_grid(country = ew_scot, ew_scot_models)
}
other_countries <- setdiff(deaths$country, c('england_wales', 'scotland'))
if (length(other_countries) > 0) {
    other_country_models <- model_fmls(bh_intercepts = c(
        'bank_holiday_christmas', 'bank_holiday_new_year'
    ))
    train_grid[[2]] <- expand_grid(
        country = other_countries, other_country_models)
}
train_grid <- bind_rows(train_grid) %>% 
    expand_grid(sex = unique(deaths$sex), age = unique(deaths$age)) %>%
    select(country, sex, age, model_name, formula) %>% 
    arrange(country, sex, age, model_name)

# At this point train_grid contains a row for each country, sex, age and model
# to be trained. Next, train the models. 
fits <- train_grid %>%
    train_many(data = deaths, num_posterior_draws = 1000)

# Combine posterior draws to obtain draws from the model average
results <- combine_draws(
    fits, num_posterior_draws = 16000, with_replacement = FALSE)

results <- results %>% 
    calculate_age_sex_totals %>% 
    calculate_excess_mortality
    

# Summarise results
result_summaries <- summarise_results(results)

# Save results
dir.create(OUTPUT_DIR, showWarnings = FALSE)
write_csv(result_summaries, file.path(OUTPUT_DIR, 'result_summaries.csv'))
# Individual model summaries may result in a very large file, if running many 
# models. Uncomment the lines below to save them (results will be saved in a 
# different file for each country). Not recommended if running many models! 
# fits %>% 
#     split(.$country) %>% 
#     imap(~saveRDS(.x, file.path(OUTPUT_DIR, paste0(.y, '_models.rds'))))
# Uncomment the lines below to save detailed model average results (results 
# will be saved in a different file for each country).
# results %>%
#     split(.$country) %>%
#     imap(~saveRDS(.x, file.path(OUTPUT_DIR, paste0(.y, '_model_average.rds'))))




