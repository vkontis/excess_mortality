# This file contains functions to help create formulas to pass to inla. This is
# helpful to programmatically generate a large number of model formulae, 
# without having to type them all one by one (see models.R for an example).


# Priors: The following functions take the parameters for each prior and
# generate a string that can be later parsed as a inla formula
loggamma_prior <- function(gamma.shape = 0.001, gamma.rate = 0.001) {
    as.character(glue::glue(
        'list(prec = list(prior = "loggamma", param = ',
        'c({gamma.shape}, {gamma.rate})))'
    ))
}
pc_prior <- function(u = 1) {
    as.character(glue::glue(
        'list(theta = list(prior="pc.prec", param=c({u}, 0.01)))'
    ))
}

# Random effects: The following functions are used to create random effects.

# random_effect is the main function used to create a random effect.
# It takes a variable name an an optional weights vector and returns
# a string with an inla::f function that can be part of an inla formula
random_effect <- function(var, w = NULL, ...) {
    args <- list(var)
    if (!is.null(w)) {
        args <- list(var, w)
    }
    args <- c(args, list(...))
    args.str <- imap(args, function(arg, nm) {
        ifelse(
            is.null(nm) | is.numeric(nm) | nchar(nm) == 0 ,
            arg, paste(nm, arg, sep = '=')
        )
    })
    paste0('f(', paste(args.str, collapse = ', '), ')')
}

iid <- function(var, w = NULL, ...) {
    random_effect(var, w, model = '"iid"', ...)
}

rw1 <- function(var, w = NULL, ...) {
    random_effect(var, w, model = '"rw1"', ...)
}

ar1 <- function(var, w = NULL, ...) {
    random_effect(var, w, model = '"ar1"', ...)
}

arp <- function(var, w = NULL, p = 1, ...) {
    random_effect(var, w, model = '"ar"', order = p, ...)
}

seasonal <- function(var, w = NULL, n = 1, ...) {
    random_effect(var, w, model = '"seasonal"', season.length = n, ...)
}

# Generate an inla formula. 
# Arguments: 
#   y: The response variable, 
#   intercept: If TRUE, add a constant term to the formula. 
#   fixed: A vector of fixed effects.
#   ...: Random effects passed to the formula. 
# Returns: 
#   A formula that can be used in inla().
#
# For example:
# inla_formula(
#     y = 'response_var', intercept = TRUE, fixed = 'slope_variable_name',
#     rw1('ids_of_rw1_effect', hyper = loggamma_prior()),
#     seasonal('ids_of_seasonal_effect', n = 52),
#     <other random effects, etc...>
# )
inla_formula <- function(y, ..., intercept = TRUE, fixed = NULL) {
    rest <- list(...)
    rest <- keep(rest, ~nchar(.) > 0)
    left <- y
    right_components <- list()
    if (isTRUE(intercept)) {
        right_components <- c(right_components, list(1))
    }
    if (!is.null(fixed)) {
        fixed <- keep(fixed, ~nchar(.) > 0)
        if (length(fixed) > 0) {
            right_components <- c(right_components, as.list(fixed))
        }
    }
    if (length(rest) > 0) {
        right_components <- c(right_components, rest)
    }
    right <- paste(right_components, collapse = ' + ')
    as.formula(paste(left, right, sep = '~'))
}


