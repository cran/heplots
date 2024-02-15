## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  fig.height=5,
  fig.width=5,
  fig.align = "center",
  # results='hide',
  # fig.keep='none',
  fig.path='fig/mmra-',
  echo=TRUE,
  collapse = TRUE,
  comment = "#>"
  )


## ----setup, echo=FALSE--------------------------------------------------------
set.seed(1071)
options(width=80, digits=4, continue="  ")
library(heplots)
library(candisc)
library(car)
library(dplyr)
library(tidyr)
library(ggplot2)


## ----rohwer-some--------------------------------------------------------------
data(Rohwer)
Rohwer |> dplyr::sample_n(6)

## ----rohwer-long--------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)

yvars <- c("SAT", "PPVT", "Raven" )      # outcome variables
xvars <- c("n", "s", "ns", "na", "ss")   # predictors
xvars <- c("n", "s", "ns")               # make a smaller example

Rohwer_long <- Rohwer %>%
  dplyr::select(-group, -na, -ss) |>
  tidyr::pivot_longer(cols = all_of(xvars), 
                      names_to = "xvar", values_to = "x") |>
  tidyr::pivot_longer(cols = all_of(yvars), 
                      names_to = "yvar", values_to = "y") |>
  dplyr::mutate(xvar = factor(xvar, levels = xvars),
                yvar = factor(yvar, levels = yvars))
Rohwer_long

## ----rohwer-long-ggplot-------------------------------------------------------
ggplot(Rohwer_long, aes(x, y, color = SES, shape = SES)) +
  geom_jitter(size=1.5) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              formula = y ~ x, 
              size=1.5) +
  facet_grid(yvar ~ xvar,            # plot matrix of Y by X
             scales = "free") +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")


## ----rohwer-separate----------------------------------------------------------
rohwer.ses1 <- lm(cbind(SAT, PPVT, Raven) ~ n + s + ns + na + ss, data=Rohwer, 
                  subset=SES=="Hi")
Anova(rohwer.ses1)

rohwer.ses2 <- lm(cbind(SAT, PPVT, Raven) ~ n + s + ns + na + ss, data=Rohwer, 
                  subset=SES=="Lo")
Anova(rohwer.ses2)

## ----rohwer-coef--------------------------------------------------------------
coef(rohwer.ses1)
coef(rohwer.ses2)

## ----rohwer-coefplot----------------------------------------------------------
par(mar=c(4,4,1,1)+.1)
coefplot(rohwer.ses1, fill=TRUE, cex.label=1.5, cex.lab=1.5)
text(-10, 3, "High SES group", pos=4, cex=1.4)

coefplot(rohwer.ses2, fill=TRUE, cex.label=1.5, cex.lab=1.5)
text(-4.7, 2.5, "Low SES group", pos=4, cex=1.4)

## ----rohwer-HE1---------------------------------------------------------------
par(mar=c(4,4,1,1)+.1)
heplot(rohwer.ses1, 
       ylim=c(40,110),                        # allow more room for 2nd plot
       col=c("red", "black"), 
       fill = TRUE, fill.alpha = 0.1,
       lwd=2, cex=1.2)
heplot(rohwer.ses2, 
       add=TRUE, 
       col=c("brown", "black"), 
       grand.mean=TRUE, error.ellipse=TRUE,   # not shown by default when add=TRUE
       fill = TRUE, fill.alpha = 0.1,
       lwd=2, cex=1.2)
# label the groups at their centroid
means <- aggregate(cbind(SAT,PPVT)~SES, data=Rohwer,  mean)
text(means[,2], means[,3], labels=means[,1], pos=3, cex=2, col="black")

## ----rohwer-mod---------------------------------------------------------------
# MANCOVA, assuming equal slopes
rohwer.mod <- lm(cbind(SAT, PPVT, Raven) ~ SES + n + s + ns + na + ss, 
                 data=Rohwer)
Anova(rohwer.mod)

## ----rohwer-cov-names---------------------------------------------------------
covariates  <- c("n", "s", "ns", "na", "ss")
# or: covariates <- rownames(coef(rohwer.mod))[-(1:2)]

## ----rohwer-mod-test----------------------------------------------------------
Regr <- linearHypothesis(rohwer.mod, covariates)
print(Regr, digits=4, SSP=FALSE)

## ----rohwer-HE2---------------------------------------------------------------
par(mar=c(4,4,3,1)+.1)
colors <- c("red", "blue", rep("black",5), "#969696")
heplot(rohwer.mod, 
       col=colors, variables=c(1,2),
       hypotheses=list("Regr" = covariates),
       fill = TRUE, fill.alpha = 0.1,
       cex=1.5, lwd=c(2, rep(3,5), 4),
       main="(SAT, PPVT) in Rohwer MANCOVA model")

heplot(rohwer.mod, 
       col=colors,  variables=c(1,3),
       hypotheses=list("Regr" = covariates),
       fill = TRUE, fill.alpha = 0.1,
       cex=1.5, lwd=c(2, rep(3,5), 4),
       main="(SAT, Raven) in Rohwer MANCOVA model")

## ----rohwer-HE3---------------------------------------------------------------
pairs(rohwer.mod, col=colors,
      hypotheses=list("Regr" = c("n", "s", "ns", "na", "ss")),
      cex=1.3, lwd=c(2, rep(3,5), 4))

## ----rohwer-HE3D-code, eval=FALSE---------------------------------------------
#  colors <- c("pink", "blue", rep("black",5), "#969696")
#  heplot3d(rohwer.mod, col=colors,
#  	hypotheses=list("Regr" = c("n", "s", "ns", "na", "ss")))

## ----rohwer-HE3D--------------------------------------------------------------
knitr::include_graphics("fig/mmra-rohwer-HE3D.png")

## ----rohwer-mod2--------------------------------------------------------------
rohwer.mod2 <- lm(cbind(SAT, PPVT, Raven) ~ SES * (n + s + ns + na + ss),
                  data=Rohwer)
Anova(rohwer.mod2)

## ----rohwer-mod2-test---------------------------------------------------------
(coefs <- rownames(coef(rohwer.mod2)))
print(linearHypothesis(rohwer.mod2, coefs[grep(":", coefs)]), SSP=FALSE)

## ----rohwer-HE4---------------------------------------------------------------
par(mar=c(4,4,1,1)+.1)
colors <- c("red", "blue", rep("black",5), "#969696")
heplot(rohwer.mod2, col=c(colors, "brown"), 
      terms=c("SES", "n", "s", "ns", "na", "ss"), 
      hypotheses=list("Regr" = c("n", "s", "ns", "na", "ss"),
                      "Slopes" = coefs[grep(":", coefs)]))

## ----hern-str-----------------------------------------------------------------
data(Hernior)
str(Hernior)

## ----hern1--------------------------------------------------------------------
Hern.mod <- lm(cbind(leave, nurse, los) ~ age + sex +  pstat +  build + cardiac + resp, 
               data=Hernior)
Anova(Hern.mod) 

## ----hern.rsq-----------------------------------------------------------------
Hern.summary <- summary(Hern.mod)
unlist(lapply(Hern.summary, function(x) x$r.squared))

## ----hern-glance--------------------------------------------------------------
glance.mlm(Hern.mod)

## ----hern2--------------------------------------------------------------------
# test overall regression
(predictors <- rownames(coef(Hern.mod))[-1])
Regr <- linearHypothesis(Hern.mod, predictors)
print(Regr, digits=5, SSP=FALSE)

## ----hern-pairs---------------------------------------------------------------
clr <- c("red", "darkgray", "blue", "darkgreen", "magenta", "brown", "black")
vlab <- c("LeaveCondition\n(leave)", 
          "NursingCare\n(nurse)", 
          "LengthOfStay\n(los)")

hyp <- list("Regr" = predictors)
pairs(Hern.mod, 
      hypotheses=hyp, 
      col=clr, var.labels=vlab, 
      fill=c(TRUE,FALSE), fill.alpha = 0.1, 
      cex=1.25)

## ----hern-canL----------------------------------------------------------------
Hern.canL <- candiscList(Hern.mod)

## ----hern.canL2, eval=FALSE---------------------------------------------------
#  plot(Hern.canL)

## ----hern-can1----------------------------------------------------------------
plot(Hern.canL, term="pstat")
plot(Hern.canL, term="build")

## ----hern-can2----------------------------------------------------------------
plot(Hern.canL, term="age")
plot(Hern.canL, term="cardiac")

## -----------------------------------------------------------------------------
str(SocGrades)

## ----grades1------------------------------------------------------------------
data(SocGrades)
grades.mod <- lm(cbind(midterm1, midterm2, final, eval) ~ 
	                     class + sex + gpa + boards + hssoc + pretest, 
                 data=SocGrades)
Anova(grades.mod, test="Roy")

## ----grades2------------------------------------------------------------------
grades.mod2 <- update(grades.mod, . ~ .^2)
Anova(grades.mod2, test="Roy")

## ----grades3------------------------------------------------------------------
grades.mod3 <- update(grades.mod, . ~ . + class:sex - hssoc - pretest)
Anova(grades.mod3, test="Roy")

## ----grades-pairs-------------------------------------------------------------
pairs(grades.mod3)

## ----grades-HE3D-code, eval=FALSE---------------------------------------------
#  heplot3d(grades.mod3, wire=FALSE)

## ----grades-HE3D--------------------------------------------------------------
knitr::include_graphics("fig/grades-HE3D.png")

## ----grades4------------------------------------------------------------------
# calculate canonical results for all terms
grades.can <- candiscList(grades.mod3)
# extract canonical R^2s
unlist(lapply(grades.can, function(x) x$canrsq))

## ----grades-can-class---------------------------------------------------------
 par(xpd=TRUE, mar=c(4,4,1,1)+.1)
# plot class effect in canonical space
 heplot(grades.can, term="class", 
        scale=4, fill=TRUE, var.col="black", var.lwd=2)

## ----grades-can-all-----------------------------------------------------------
plot(grades.can, term="sex")
plot(grades.can, term="gpa")

