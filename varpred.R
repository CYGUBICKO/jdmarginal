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
	focalForm <- as.formula(paste0("~", focal))
	updateForm <- as.formula(paste0(".~. + ", focal))
	updateMod <- update(mod, updateForm)
	betahat <- extractcoef(updateMod)
	xlevels <- mod$xlevels
	modFrame <- model.frame(updateMod, xlev = xlevels, drop.unused.levels = TRUE)
	modTerms <- delete.response(terms(updateMod))
	modMat <- model.matrix(modTerms, modFrame)
	modLabels <- attr(modTerms, "term.labels")
	
	## Better way to do this?
	focalVar <- attr(terms(focalForm), "term.labels")

	if(is.null(vv)) {vv <- varfun(modFrame[[focalVar]], steps)}
  	steps <- length(vv)
	
	rowMean <- matrix(apply(modMat, 2, mean), nrow = 1)
	modMean <- rowMean[rep(1, steps), ]

	# Model matrix with progression of focal variable
	varFrame <- modFrame[rep(1, steps), ]
	varFrame[focalVar] <- vv
	## FIXME: CHECK
	varFrame <- model.frame(modTerms, varFrame, xlev = xlevels, drop.unused.levels = TRUE)
	newMat <- model.matrix(modTerms, varFrame)

	## Better way to do this?
	colNames <- colnames(modMat)
	focalCols <- grepl(focalVar, colNames)
	modVar <- modMean
	modVar[, focalCols] <- newMat[, focalCols]
  
	vc <- vcov(updateMod)
	if (inherits(updateMod, "clmm")){
		f <- c(names(updateMod$alpha)[[1]], names(updateMod$beta))
		vc <- vc[f, f]
	}

	if(!identical(colNames, names(betahat))){
		print(setdiff(colNames, names(betahat)))
		print(setdiff(names(betahat), colNames))
		stop("Effect names do not match: check for empty factor levels?")
	}
  	pred <- modVar %*% betahat

	# (Centered) predictions for SEs
	if (isolate) {
		if(!is.null(isoValue)){
			modFrame[focalVar] <- 0*modFrame[focalVar]+isoValue
			modMat <- model.matrix(modTerms, modFrame)
			modMean <- matrix(apply(modMat, 2, mean), nrow=1)
			modMean <- modMean[rep(1, steps), ]
		}
		modVar <- modVar - modMean
	}

	pse_var <- sqrt(diag(modVar %*% tcrossprod(data.matrix(vc), modVar)))
	
	# Stats
	df <- ifelse(
		grepl("df.residual", paste(names(mod), collapse=""))
		, mod$df.residual, dfspec
	)
	mult <- qt(1-level/2, df)

	df <- data.frame(var = vv
		, fit = pred
		, lwr = pred - mult*pse_var
		, upr = pred + mult*pse_var
	)
  	names(df)[[1]] <- focal
	return(df)
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
