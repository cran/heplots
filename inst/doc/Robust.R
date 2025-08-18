## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  fig.height=5,
  fig.width=5,
  fig.align = "center",
  # results='hide',
  # fig.keep='none',
  fig.path='fig/robust-',
  echo=TRUE,
  collapse = TRUE,
  comment = "#>"
)
options(digits = 4)
set.seed(1071)
options(width=80, continue="  ")

## ----setup--------------------------------------------------------------------
library(heplots)
library(candisc)
library(ggplot2)
library(dplyr)
library(mvinfluence)

## ----echo=FALSE, out.width="70%"----------------------------------------------
knitr::include_graphics(here::here("man", "figures", "weight-functions.jpg"))

## ----echo=FALSE, fig.align='center', out.width="70%", fig.cap="Flowchart for the iteratively reweighted least squares algorithm"----
knitr::include_graphics(here::here("man", "figures", "IRWLS-flowchart.jpg"))

## ----pottery-data, message=FALSE----------------------------------------------
library(heplots)
library(carData)
library(car)

# Load the pottery data
data(Pottery, package = "carData")
head(Pottery)

## ----pottery-structure--------------------------------------------------------
str(Pottery)

## -----------------------------------------------------------------------------
table(Pottery$Site)

## ----pottery-mlm--------------------------------------------------------------
# Classical MANOVA model
pottery.mlm <- lm(cbind(Al, Fe, Mg, Ca, Na) ~ Site, data = Pottery)
Anova(pottery.mlm)

## ----pottery-cqplot, fig.cap="Chisquare QQ plot for the Pottery model."-------
cqplot(pottery.mlm, id.n = 5)

## ----pottery-inflplot, fig.cap = "Influence plot for the Pottery model."------
res <- influencePlot(pottery.mlm, id.n = 2)
res |>
  arrange(desc(CookD))

## ----pottery-rlm--------------------------------------------------------------
# Robust MANOVA model
pottery.rlm <- robmlm(cbind(Al, Fe, Mg, Ca, Na) ~ Site, data = Pottery)
Anova(pottery.rlm)

## ----pottery-coefs------------------------------------------------------------
b.mlm <- coef(pottery.mlm)
b.rlm <- coef(pottery.rlm)

reldiff <- function(x, y, pct=TRUE) {
  res <- abs(x - y) / x
  if (pct) res <- 100 * res
  res
}

reldiff(b.mlm, b.rlm)

## ----pottery-weights, fig.cap="Weights from robust MANOVA fitting showing potential outliers"----
# Plot the weights from robust fitting
plot(pottery.rlm, col=Pottery$Site, segments=TRUE)
xloc <- c(7.5, 15.5, 19.5, 24)
text(xloc, rep(c(1.0, 1.05), length=5), 
     levels(Pottery$Site), pos =3, xpd = TRUE)

## ----pottery-heplot, fig.cap="HE plot comparing classical (blue) and robust (red) MANOVA for Al vs Fe", fig.width=8, fig.height=6----
# Classical HE plot for Al and Fe
heplot(pottery.mlm, variables = c("Al", "Fe"), 
       main = "Classical vs Robust MANOVA: Al vs Fe",
       col = c("blue", "blue"), fill = TRUE, fill.alpha = 0.2)

# Overlay robust HE plot
heplot(pottery.rlm, variables = c("Al", "Fe"), 
       add = TRUE, error.ellipse = TRUE,
       col = c("red", "red"), 
       fill = TRUE, fill.alpha = 0.2, lty = 2)

# Add legend
legend("topright", 
       legend = c("Classical", "Robust"), 
       col = c("blue", "red"), 
       lty = c(1, 2), 
       fill = c("blue", "red")
       )

## ----pottery-pairs, fig.cap = "Pairwise HE plots for all response variables in the robust model `pottery.rlm`"----
pairs(pottery.rlm, 
      fill=TRUE, fill.alpha = 0.1)

