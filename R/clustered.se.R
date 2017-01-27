#' This function allows for calculation of clustered standard errors after running a standard linear regression with lm.
#'
#' @param model_result Model object returned by lm
#' @param data The data set used in the original regression
#' @param cluster The name of the clustering variable
#' @param level The confidence level at which the p-value should be calculated
#' @param Tvar The name of the treatment variable from the original regression
#'
#' @return A list containing a variance-covariance matrix, clustered standard errors and p-values for each coefficient in the original model, and lower and upper bounds for confidence intervals at each specified confidence level.
#'
#' @importFrom stats complete.cases qt
#' @importFrom sandwich estfun sandwich
#' @importFrom lmtest coeftest
clustered.se <- function(model_result, data, cluster, level = 0.95, Tvar = "treatment") {
    model_variables <- intersect(colnames(data), c(colnames(model_result$model), cluster))
    data <- data[, model_variables]
    data <- data[complete.cases(data), ]
    cl <- data[[cluster]]
    M <- length(unique(cl))
    N <- length(model_result$residuals)
    K <- model_result$rank
    dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
    uj <- apply(estfun(model_result), 2, function(x) tapply(x, cl, sum))
    vcovCL <- dfc * sandwich(model_result, meat. = crossprod(uj)/N)
    standard.errors <- coeftest(model_result, vcov. = vcovCL)[, 2]
    p.values <- coeftest(model_result, vcov. = vcovCL)[, 4]
    # a <- 1-(1 - level)/2
    df <- M - 1
    coeff <- model_result$coefficients
    na_coeff <- is.na(coeff)
    if (any(na_coeff))
        coeff <- coeff[!na_coeff]

    lb <- ub <- numeric(length(level))
    for (i in 1:length(level)) {
        a <- 1 - (1 - level[i])/2
        lb[i] <- coeff[[Tvar]] - qt(p = a, df = df) * standard.errors[[Tvar]]
        ub[i] <- coeff[[Tvar]] + qt(p = a, df = df) * standard.errors[[Tvar]]
    }

    clustered.se <- list(vcovCL = vcovCL, standard.errors = standard.errors, p.values = p.values, lb = lb, ub = ub)
    return(clustered.se)
}
