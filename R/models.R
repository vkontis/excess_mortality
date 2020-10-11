# The functions in this file are used to set up the model formulas to pass to 
# inla(). See model_fmls() for slopes, seasonal effects, etc that are used in 
# the formulas.

make_col_from_list_names <- function(d, col, names_col = NULL) {
    if (!is.list(d[[col]])) {
        return(d)
    }
    if (is.null(names(d[[col]]))) {
        return(d)
    }
    if (is.null(names_col)) {
        names_col <- paste0(col, '_name')
        stopifnot(!names_col %in% names(d))
        d[names_col] <- names(d[[col]])
    }
    d
}

make_cols_from_list_names <- function(d, cols = NULL, drop = FALSE) {
    orig_names <- names(d)
    if (is.null(cols)) {
        cols <- names(d)
    }
    for (col in cols) {
        if (is.list(d[[col]])) {
            d <- make_col_from_list_names(d, col)
        }
    }
    if (drop) {
        d <- select(d, -all_of(orig_names))
    }
    d
}


model_fmls <- function(bh_intercepts, lg_shape = .001, lg_rate = .001) {
    
    lg_prior <- loggamma_prior(lg_shape, lg_rate)
    
    slopes <- list(
        nslp = list(intercept = TRUE, fixed = bh_intercepts),
        gslp = list(intercept = TRUE, fixed = c('global_slope', bh_intercepts))
    )
    
    week_components <- list(
        ar1 = ar1('weeks_since_start_ar1', hyper = lg_prior),
        ar2 = arp('weeks_since_start_ar2', p = 2, hyper = lg_prior),
        ar4 = arp('weeks_since_start_ar4', p = 4, hyper = lg_prior),
        ar8 = arp('weeks_since_start_ar8', p = 8, hyper = lg_prior)
    )
    
    seasonal_components <- list(
        wseas = seasonal('weeks_since_start_seas', n = 52, hyper = lg_prior)
    )
    
    remainder_components <- list(
        iid('remainder_iid', hyper = lg_prior)
    )
    
    temperature_components <- list(
        nt = '',
        awrw1 = list(
            fixed = 'weekly_t2m_anomaly',
            rw1(
                'week_of_year_anomaly_rw1', 'weekly_t2m_anomaly',
                cyclic = TRUE, hyper = lg_prior
            )
        )
    )
    
    fml_grid <- expand_grid(
        slopes,
        week_components,
        seasonal_components,
        remainder_components,
        temperature_components
    )
    
    model_names <- fml_grid %>%
        make_cols_from_list_names %>%
        select(ends_with('name')) %>%
        group_by(seq(n())) %>%
        group_map(function(row, ...) {
            row %>%
                select(ends_with('_name')) %>%
                as_vector %>%
                paste(collapse = '_')
        }) %>%
        as_vector
    
    fmls <- fml_grid %>%
        group_by(seq(n())) %>%
        group_map(function(row, ...) {
            fixed <- c()
            intercept <- NULL
            rest <- list()
            for (col in row) {
                if (is.list(col[[1]])) {
                    if ('intercept' %in% names(col[[1]])) {
                        intercept <- as.logical(col[[1]]$intercept)
                    }
                    if ('fixed' %in% names(col[[1]])) {
                        fixed <- c(fixed, col[[1]]$fixed)
                    }
                    rest <- c(
                        rest,
                        col[[1]][!names(col[[1]]) %in% c('intercept', 'fixed')]
                    )
                }
                else {
                    rest <- c(rest, col[[1]])
                }
            }
            fml_args <- c(
                list(y = 'y', fixed = fixed, intercept = intercept),
                rest
            )
            do.call(inla_formula, fml_args)
        })
    
    fmls_df <- tibble(
        formula_id = seq_along(fmls),
        formula = fmls,
        model_name = model_names
    )
    fmls_df
}
