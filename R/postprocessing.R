calculate_age_sex_totals <- function(result_grid) {

    agg_results <- function(d, g) {
        # Ensure same weeks and prediction periods
        stopifnot(map_lgl(d$data, ~all(.x$week == d$data[[1]]$week)))
        stopifnot(map_lgl(
            d$data, ~all(.x$out_of_sample == d$data[[1]]$out_of_sample)))

        dat <- d$data[[1]]
        dat$deaths <- reduce(map(d$data, 'deaths'), `+`)
        dat$population <- reduce(map(d$data, 'population'), `+`)

        death_draws <- reduce(d$death_draws, `+`)
        rate_draws <- death_draws / dat$population

        tibble(
            death_draws = list(death_draws),
            rate_draws = list(rate_draws),
            data = list(dat)
        )
    }

    all_age <- result_grid %>%
        group_by(country, sex) %>%
        group_modify(agg_results) %>%
        ungroup %>%
        mutate(age = 'all')
    all_sex <- result_grid %>%
        group_by(country, age) %>%
        group_modify(agg_results) %>%
        ungroup %>%
        mutate(sex = 'both')
    all_age_sex <- result_grid %>%
        group_by(country) %>%
        group_modify(agg_results) %>%
        ungroup %>%
        mutate(sex = 'both', age = 'all')

    bind_rows(result_grid, all_age, all_sex, all_age_sex)

}

calculate_excess_mortality <- function(result_grid) {
    weekly_excess <- result_grid %>%
        group_by(country, sex, age) %>%
        group_modify(function(d, g) {
            oos_mask <- d$data[[1]]$out_of_sample
            excess_death_draws <-
                d$data[[1]][oos_mask, ]$deaths - d$death_draws[[1]][oos_mask, ]
            excess_rate_draws <-
                excess_death_draws / d$data[[1]][oos_mask, ]$population
            relative_increase_draws <-
                excess_death_draws / d$death_draws[[1]][oos_mask, ]
            bind_cols(d, tibble(
                excess_death_draws = list(excess_death_draws),
                excess_rate_draws = list(excess_rate_draws),
                relative_increase_draws = list(relative_increase_draws)
            ))
        }) %>%
        ungroup
    all_week_excess <- weekly_excess %>%
        group_by(country, sex, age) %>%
        group_modify(function(d, g) {
            oos_mask <- d$data[[1]]$out_of_sample
            total_excess_deaths <- colSums(d$excess_death_draws[[1]])
            total_excess_rates <-
                total_excess_deaths / mean(d$data[[1]][oos_mask, ]$population)
            total_relative_increase <-
                total_excess_deaths / colSums(d$death_draws[[1]][oos_mask, ])
            bind_cols(d, tibble(
                all_week_excess_death_draws = list(total_excess_deaths),
                all_week_excess_rate_draws = list(total_excess_rates),
                all_week_relative_increase_draws = list(total_relative_increase)
            ))
        }) %>%
        ungroup
    all_week_excess
}


summarise_results <- function(
    result_grid, quantiles = seq(0.025, 0.975, .025)
) {
    result_grid %>%
        group_by(country, sex, age) %>%
        group_modify(function(d, g) {
            oos_mask <- d$data[[1]]$out_of_sample
            weeks <- as.character(d$data[[1]][oos_mask, ]$week)

            draws_summary <- function(draws) {
                if (is.matrix(draws)) {
                    qs <- matrixStats::rowQuantiles(draws, probs = quantiles)
                } else {
                    stopifnot(is.vector(draws))
                    qs <- quantile(draws, probs = quantiles) %>% t
                }
                qs %>%
                    as_tibble %>%
                    set_names(paste0('q', sprintf(
                        '%03.0f', parse_number(sub('%$', '', names(.))) * 10)))
            }

            deaths_summary <- bind_rows(
                d$death_draws[[1]][oos_mask, ] %>%
                    draws_summary %>%
                    mutate(week = weeks),
                colSums(d$death_draws[[1]][oos_mask, ]) %>%
                    draws_summary %>%
                    mutate(week = 'all')
            ) %>% mutate(quantity = 'Predicted deaths')

            excess_deaths_summary <- bind_rows(
                d$excess_death_draws[[1]] %>%
                    draws_summary %>%
                    mutate(week = weeks),
                d$all_week_excess_death_draws[[1]] %>%
                    draws_summary %>%
                    mutate(week = 'all')
            ) %>% mutate(quantity = 'Excess deaths')

            excess_rates_summary <- bind_rows(
                d$excess_rate_draws[[1]] %>%
                    draws_summary %>%
                    mutate(week = weeks),
                d$all_week_excess_rate_draws[[1]] %>%
                    draws_summary %>%
                    mutate(week = 'all')
            ) %>%
                mutate_at(vars(matches('^q\\d{3}$')), ~.x * 1e5) %>%
                mutate(quantity = 'Excess deaths per 100,000')

            relative_increase_summary <- bind_rows(
                d$relative_increase_draws[[1]] %>%
                    draws_summary %>%
                    mutate(week = weeks),
                d$all_week_relative_increase_draws[[1]] %>%
                    draws_summary %>%
                    mutate(week = 'all')
            ) %>% mutate(quantity = 'Relative change in deaths')

            bind_rows(
                deaths_summary, excess_deaths_summary, excess_rates_summary,
                relative_increase_summary
            )
        }) %>%
        ungroup %>%
        select(quantity, country, sex, age, week, week, matches('^q\\d{3}$'))
}
