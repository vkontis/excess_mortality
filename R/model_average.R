# Combine model posterior draws
combine_draws <- function(
    model_grid, num_posterior_draws, with_replacement = FALSE
) {
    # model_grid is a data frame with columns 'sex', 'age', 'train_start',
    # 'train_end', 'forecast_horizon', and 'model_name', and a column
    # 'model' which contains a list with model outputs
    group_vars <- c('country', 'sex', 'age')
    stopifnot(group_vars %in% names(model_grid))
    num_models <- length(unique(model_grid$model_name))
    # take an equal number of draws from each model
    num_draws_per_model <- round(num_posterior_draws / num_models)
    
    model_grid %>%
        group_by_at(group_vars) %>%
        group_map(function(d, g) {
            # for each group check there is precisely one row for each model
            stopifnot(nrow(d) == num_models)
            # check all models use same out-of-sample period
            stopifnot(map_lgl(
                model_grid$model, ~all.equal(
                    .x$out_of_sample_error$week_id,
                    model_grid$model[[1]]$out_of_sample_error$week_id)
            ))
            # take draws of death rates, by sampling from each individual
            # model posterior draws, the appropriate number of times
            rate_draws <- map(d$model, function(m) {
                stopifnot(nrow(m$fitted_values) ==
                              nrow(m$posterior_draws[[1]]))
                draw_ids <- sample(
                    ncol(m$posterior_draws), size = num_draws_per_model,
                    replace = with_replacement
                )
                m$posterior_draws[, draw_ids]
            }) %>%
                do.call(what = cbind) %>%
                exp
            
            # get weeks and population from the data to generate
            # Poisson draws (we also pull deaths to return them with the
            # function output)
            model_data <- d$model[[1]]$fitted_values %>%
                select(week, week_id, population, deaths, out_of_sample)
            num_data_points <- nrow(model_data)
            # the Poisson lambdas become rate draws multiplied by
            # population
            lambdas <- rate_draws * model_data$population
            lambdas_vec <- as.vector(lambdas)
            stopifnot(
                length(lambdas_vec) == num_data_points * ncol(rate_draws))
            # take Poisson draws
            deaths_draws <- matrix(
                rpois(length(lambdas_vec), lambdas_vec),
                nrow = num_data_points
            )
            # return the grouping variables with bma draws of deaths, rates
            # and the data
            g %>%
                mutate(
                    death_draws = list(deaths_draws),
                    rate_draws = list(rate_draws),
                    data = list(model_data)
                    # fitted = map(model_grid$model, ~.x$fitted) %>%
                    #     set_names(model_grid$model_name)
                )
        }) %>%
        bind_rows
}
