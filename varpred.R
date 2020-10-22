#' Compute marginal means
#'
#' @param mod
#' @param focal is of the form ~predictors
#' @param isolate logical
#' @param isoValue default NULL
#' @param level default 0.05
#' @param steps default 101
#' @param dfspec default 100
#' @param vv default NULL
#'
#' @importFrom stats model.frame model.matrix vcov
#'
#' @export
#'
varpred <- function(mod, focal, isolate = FALSE
	, isoValue = NULL, level = 0.05, steps = 101, vv = NULL) {
	beta_hat <- extractcoef(mod)
	rframe <- model.frame(mod)
	checkframe <- model.frame(focal, rframe)
	modTerms <- delete.response(terms(mod))
	mm <- model.matrix(modTerms, rframe)
}

#' Extract model coefficients
#'
#' @param mod model object
#' @noRd
#' @keywords internal
#'

extractcoef <- function(mod){
	if (inherits(mod, "lm")) return (coef(mod))
	if (inherits(mod, "mer")) return (fixef(mod))
	if (inherits(mod, "glmerMod")) return (fixef(mod))
	if (inherits(mod, "clmm")) {
		ef <- c(0, mod$beta)
		names(ef) <- c("(Intercept)", names(mod$beta))
		return (ef)
	} else {
		stop("Don't recognize model type")
	}
}

#' Variable levels or sequence
#'
#' @param varcol focal variable or column
#' @param steps
#' @noRd
#' @keywords internal

varfun <- function(vcol, steps){
	if(is.numeric(vcol)){
		if(is.null(steps)){return(sort(unique(vcol)))}
		return(seq(min(vcol), max(vcol), length.out = steps))
	}
	return(unique(vcol))
}
