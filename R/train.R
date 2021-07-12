# Create copies of a data frame column. Can be called with either a positive 
# integer `n` (the number of copies) or a character vector `names` of new 
# column names.
make_copies <- function(d, var, n = 0, names = character(0)) {
    if (length(names) > 0) {
        for (x in names) {
            d[x] <- d[[var]]
        }
    }
    if (n < 1) {
        return(d)
    }
    for (i in seq(n)) {
        new_col <- paste0(var, '_', i)
        stopifnot(!new_col %in% names(d))
        d[new_col] <- d[var]
    }
    d
}

# Prepare the training data to be used with INLA, by replacing the response 
# variable with NAs in the "out-of-sample" period and creating copies of the 
# indicators used for random effects.`prediction_period` is the number of 
# weeks at the end of the time series which are used for prediction.
prep_train_data <- function(data, prediction_period = 52) {
    data %>%
        group_by(country, sex, age) %>% 
        mutate(
            out_of_sample = row_number() > n() - prediction_period,
            y = if_else(out_of_sample, NA_real_, deaths),
            year = lubridate::year(week),
            weeks_since_start = week_id,
            global_slope = weeks_since_start
        ) %>%
        mutate(year_id = year - min(year) + 1) %>%
        make_copies('month_of_year', names = c('month_id')) %>%
        make_copies('year_id', names = c('month_year_ar1')) %>%
        make_copies('weeks_since_start', names = c(
            'remainder_iid',
            'weeks_since_start_rw1',
            'weeks_since_start_ar1',
            'weeks_since_start_ar2',
            'weeks_since_start_ar4',
            'weeks_since_start_ar8',
            'weeks_since_start_seas',
            'weeks_since_start_temperature_seas',
            'weeks_since_start_anomaly_seas'
        )) %>%
        make_copies('week_of_year', names = c(
            'week_of_year_iid',
            'week_of_year_rw1',
            'week_of_year_rw2',
            'week_of_year_temperature_rw1',
            'week_of_year_temperature_rw2',
            'week_of_year_temperature_iid',
            'week_of_year_temperature_ar1',
            'week_of_year_anomaly_rw1',
            'week_of_year_anomaly_rw2',
            'week_of_year_anomaly_iid',
            'week_of_year_anomaly_ar1'
        )) %>%
        ungroup
}

# Train a single model. 
# Arguments:
#   data: The training data as a data frame 
#   formula: A formula to be used with inla()
#   return_summary_only: If TRUE, only prediction summaries are returned (saves
#     disk space when running multiple models). Otherwise the full inla object  
#     is returned. 
#   num_postrior_draws: The number of posterior draws to produce.
#   refit_on_error: A boolean indicating whether a re-fit attempt should be 
#     made if convergence fails. If the initial model run fails, a re-run is 
#     attempted with a different initial configuration, and that intial run is  
#     then used as a starting point to help the main run converge.
# Returns:
#   If return_summary_only is FALSE, then the result of the call to inla() (
#   see ?inla for more), with two additional list elements: 
#       posterior_draws: A matrix of posterior draws for the linear predictor
#       time_taken: The time taken to fit the model.
#   If return_summary_only is TRUE, then a list with the following entries:
#       fixed: The fixed effects summary from the inla() call.
#       fitted_values: A data frame with fitted values.
#       random: Random effect estimates from the inla() call.
#       hyper: Hyperparameter estimates from the inla() call.
#       posterior_draws, time_taken: As above. 
train_one <- function(
    data, formula, ..., return_summary_only = FALSE, num_posterior_draws = 0,
    refit_on_error = TRUE
) {
    t0 <- Sys.time()
    
    m <- tryCatch(
        expr = {
            inla(
                formula = formula, family = 'poisson', data = data,
                E = population, control.predictor = list(link = 1),
                control.compute = list(dic = TRUE, config = TRUE)
            )
        },
        error = function(e) {
            logging::logerror(e)
            return(e)
        }
    )
    if ('error' %in% class(m) && refit_on_error) {
        logging::loginfo(paste0(
            'Attempting to re-fit, using diagonal offset for initial ',
            'configuration.'
        ))
        m <- tryCatch(
            expr = {
                m_init <- inla(
                    formula = formula, family = 'poisson', data = data,
                    E = population,
                    control.predictor = list(link = 1),
                    control.compute = list(dic = TRUE, config = TRUE),
                    control.inla = list(
                        int.strategy = 'eb', strategy = 'gaussian', diagonal = 10000)
                )
                inla(
                    formula = formula, family = 'poisson', data = data,
                    E = population,
                    control.predictor = list(link = 1),
                    control.compute = list(dic = TRUE, config = TRUE),
                    control.inla = list(diagonal = 0),
                    control.mode = list(result = m_init, restart = TRUE)
                )
            },
            error = function(e) {
                logging::logerror(paste0('Failed to converge: ', e))
                return(e)
            }
        )
    }
    if ('error' %in% class(m) && refit_on_error) {
        logging::loginfo('Attempting to re-fit, using non-scaled model for initial configuration.')
        m <- tryCatch(
            expr = {
                inla.setOption(scale.model.default = FALSE)
                m_init <- inla(
                    formula = formula, family = 'poisson', data = data,
                    E = population,
                    control.predictor = list(link = 1),
                    control.compute = list(dic = TRUE, config = TRUE)
                )
                inla(
                    formula = formula, family = 'poisson', data = data,
                    E = population,
                    control.predictor = list(link = 1),
                    control.compute = list(dic = TRUE, config = TRUE),
                    control.mode = list(result = m_init, restart = TRUE)
                )
            },
            error = function(e) {
                logging::logerror(paste0('Failed to converge: ', e))
                return(e)
            },
            finally = {
                inla.setOption(scale.model.default = TRUE)
            }
        )
    }
    if ('error' %in% class(m)) {
        logging::logerror('Failed to run model, exiting')
        return(m)
    } else {
        logging::loginfo('Model ran successfully.')
    }
    
    time_taken <- as.numeric(difftime(Sys.time(), t0, units = 'secs'))
    
    forecast_ids <- which(is.na(data[as.character(formula)[2]]))
    
    posterior_draws <- NULL
    if (num_posterior_draws > 0) {
        posterior_draws <- inla.posterior.sample(num_posterior_draws, m) %>%
            map(function(draw) {
                draw$latent[grep("Predictor", rownames(draw$latent))]
            })
        posterior_draws <- do.call(cbind, posterior_draws)
    }
    
    if (return_summary_only) {
        pred <- m$summary.fitted.values %>%
            slice(forecast_ids) %>%
            select(
                pred_med = `0.5quant`, pred_lb = `0.025quant`,
                pred_ub = `0.975quant`
            )
        fitted <- m$.args$data %>%
            select(week, week_id, population, deaths) %>%
            mutate(out_of_sample = row_number() %in% forecast_ids) %>%
            bind_cols(m$summary.fitted.values %>% select(-sd, -mode))
        m <- list(
            fixed = m$summary.fixed,
            fitted_values = fitted,
            random = m$summary.random,
            hyper = m$summary.hyperpar
        )
    }
    m$posterior_draws <- posterior_draws
    m$time_taken <- time_taken
    m
}

# Train a number of models. 
# Arguments:
#   data: Training data 
#   train_grid: A data frame with one row for each model run (e.g. one row for 
#     each country, sex, age, model, etc.)
#   append_to_grid: If TRUE, results are returned as a new column in train_grid. 
#     Otherwise a list of results is returned 
#   return_summary_only, refit_on_error, num_posterior_draws: See train_one.
# Returns: 
#   Either the train_grid with an additional column for each model run (if 
#   append_to_grid) otherwise a list of fitted models. See train_one for the 
#   structure of each entry of the list.
train_many <- function(
    data, train_grid, append_to_grid = TRUE, return_summary_only = TRUE,
    refit_on_error = TRUE, num_posterior_draws = 0
) {
    models <- map(seq(nrow(train_grid)), function(i) {
        logging::loginfo(paste0(
            'Fitting model ', train_grid$model_name[i], ' (', i, ' of ',
            nrow(train_grid), ')'
        ))
        train_args <- as.list(train_grid[i, ])
        train_args$data <- filter(
            data, country == train_args$country, sex == train_args$sex,
            age == train_args$age
        )
        train_args$formula <- train_args$formula[[1]]
        train_args$return_summary_only <- return_summary_only
        train_args$refit_on_error <- as.logical(refit_on_error)
        train_args$num_posterior_draws <- num_posterior_draws
        purrr::lift_dl(train_one)(train_args)
    })
    if (append_to_grid) {
        train_grid$model <- models
        return(train_grid)
    }
    models
}

