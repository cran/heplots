# partial eta^2 measures of association for multivariate tests
# (mod of car:::print.Anova.mlm, just for testing)

etasq <- function(x, ...){
	UseMethod("etasq", x)
}

etasq.mlm <- function(x, ...) {
	etasq(Anova(x, ...))
}

etasq.Anova.mlm <- function(x, anova=FALSE, ...){
	test <- x$test
	if (test == "Roy") warning("eta^2 not defined for Roy's test")
	repeated <- x$repeated
	ntests <- length(x$terms)
	tests <- matrix(NA, ntests, 4)
	assoc <- rep(NA, ntests)
	if (!repeated) SSPE.qr <- qr(x$SSPE) 
	for (term in 1:ntests){
		# some of the code here adapted from stats:::summary.manova
		eigs <- Re(eigen(qr.coef(if (repeated) qr(x$SSPE[[term]]) else SSPE.qr,
								x$SSP[[term]]), symmetric = FALSE)$values)
		tests[term, 1:4] <- switch(test,
				Pillai = stats:::Pillai(eigs, x$df[term], x$error.df),
				Wilks = stats:::Wilks(eigs, x$df[term], x$error.df),
				"Hotelling-Lawley" = stats:::HL(eigs, x$df[term], x$error.df),
				Roy = stats:::Roy(eigs, x$df[term], x$error.df))
		s <- min(length(eigs), x$df[term])
		assoc[term] <- switch(test,
			Pillai = tests[term,1] / s,
			Wilks = 1 - tests[term,1] ^(1/s),
			"Hotelling-Lawley" = tests[term,1] / (tests[term,1] + s),
			Roy = tests[term,1] / (tests[term,1] + 1))
			
	}
	ok <- tests[, 2] >= 0 & tests[, 3] > 0 & tests[, 4] > 0
	ok <- !is.na(ok) & ok
	if(anova) {
  	result <- cbind(assoc, x$df, tests, pf(tests[ok, 2], tests[ok, 3], tests[ok, 4], 
  					lower.tail = FALSE))
  	rownames(result) <- x$terms
  	colnames(result) <- c("eta^2", "Df", "test stat", "approx F", "num Df", "den Df", "Pr(>F)")
  	result <- structure(as.data.frame(result), 
  			heading = paste("\nType ", x$type, if (repeated) " Repeated Measures",
  					" MANOVA Tests: ", test, " test statistic", sep=""), 
  			class = c("anova", "data.frame"))
	}
	else {
		result <- data.frame(assoc)
  	rownames(result) <- x$terms
  	colnames(result) <- "eta^2"
		
	}
	
	result      
}

TESTME <- FALSE
if(TESTME) {
data(Soils) # from car package
soils.mod <- lm(cbind(pH,N,Dens,P,Ca,Mg,K,Na,Conduc) ~ Block + Contour*Depth, data=Soils)
#Anova(soils.mod)
etasq(Anova(soils.mod))
etasq(soils.mod) # same
etasq(Anova(soils.mod), anova=TRUE)

etasq(Anova(soils.mod, test="Wilks"))
etasq(Anova(soils.mod, test="Hotelling"))
}
