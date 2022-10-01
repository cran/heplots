## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  fig.height=5,
  fig.width=5,
  # results='hide',
  # fig.keep='none',
  fig.path='fig/manova-',
  echo=TRUE,
  collapse = TRUE,
  comment = "#>"
  )


## ----setup, echo=FALSE--------------------------------------------------------
set.seed(1071)
options(width=80, digits=5, continue="  ")
library(heplots)
library(candisc)
library(car)

## ----plastic-str--------------------------------------------------------------
data(Plastic, package="heplots")
str(Plastic)

## ---- plastic-mod-------------------------------------------------------------
plastic.mod <- lm(cbind(tear, gloss, opacity) ~ rate*additive, data=Plastic)
Anova(plastic.mod, test.statistic="Roy")

## ---- plastic-univar----------------------------------------------------------
Anova(update(plastic.mod, tear ~ .))

Anova(update(plastic.mod, gloss ~ .))

Anova(update(plastic.mod, opacity ~ .))

## ---- plastic1a, echo=-1------------------------------------------------------
par(mar = c(4,4,1,1)+.1)
## Compare evidence and effect scaling 
colors = c("red", "darkblue", "darkgreen", "brown")
heplot(plastic.mod, size="evidence", 
       col=colors, cex=1.25,
       fill=TRUE, fill.alpha=0.1)
heplot(plastic.mod, size="effect", 
       add=TRUE, lwd=5, term.labels=FALSE, col=colors)

## ---- plastic1----------------------------------------------------------------
par(mar = c(4,4,1,1)+.1)
# Compare evidence and effect scaling 
colors = c("red", "darkblue", "darkgreen", "brown")
heplot(plastic.mod, size="evidence", 
       col=colors, cex=1.25,
       fill=TRUE, fill.alpha=0.05)
heplot(plastic.mod, size="effect", 
       add=TRUE, lwd=5, term.labels=FALSE, col=colors)

## add interaction means
intMeans <- termMeans(plastic.mod, 'rate:additive', abbrev.levels=2)
points(intMeans[,1], intMeans[,2], pch=18, cex=1.2, col="brown")
text(intMeans[,1], intMeans[,2], rownames(intMeans), 
     adj=c(0.5, 1), col="brown")
lines(intMeans[c(1,3),1], intMeans[c(1,3),2], col="brown")
lines(intMeans[c(2,4),1], intMeans[c(2,4),2], col="brown")

## ---- plastic-mod1------------------------------------------------------------
plastic.mod

## ---- plastic-tests-----------------------------------------------------------
print(linearHypothesis(plastic.mod, c("rateHigh", "additiveHigh"), title="Main effects"), 
      SSP=FALSE)

print(linearHypothesis(plastic.mod, c("rateHigh", "additiveHigh", "rateHigh:additiveHigh"), 
                       title="Groups"), SSP=FALSE)

## ---- plastic2----------------------------------------------------------------
par(mar = c(4,4,1,1)+.1)
heplot(plastic.mod, 
       hypotheses=list("Group" =  c("rateHigh", "additiveHigh", "rateHigh:additiveHigh ")),
       col=c(colors, "purple"),
       fill = TRUE, fill.alpha = 0.1,
       lwd=c(2, 3, 3, 3, 2), cex=1.25)
heplot(plastic.mod, 
       hypotheses=list("Main effects" = c("rateHigh", "additiveHigh")), 
       add=TRUE,
       col=c(colors, "darkgreen"), cex=1.25)

## ---- plastic1-HE3D-code, eval=FALSE------------------------------------------
#  colors = c("pink", "darkblue", "darkgreen", "brown")
#  heplot3d(plastic.mod, col=colors)

## ---- plastic1-HE3D-----------------------------------------------------------
knitr::include_graphics("fig/plastic-HE3D.png")

## ---- MJdata------------------------------------------------------------------
data(MockJury, package = "heplots")
str(MockJury)

## ---- MJdata1-----------------------------------------------------------------
table(MockJury$Attr)
table(MockJury$Attr, MockJury$Crime)

## ---- jury.mod1---------------------------------------------------------------
(jury.mod1 <- lm( cbind(phyattr, happy, independent, sophisticated) ~ Attr, data=MockJury))
Anova(jury.mod1, test="Roy")

## ---- jury-mod1-HE------------------------------------------------------------
par(mar = c(4,4,1,1)+.1)
heplot(jury.mod1, main="HE plot for manipulation check",
       fill = TRUE, fill.alpha = 0.1)

## ---- jury-mod1-pairs---------------------------------------------------------
pairs(jury.mod1)

## ---- jury-can1a--------------------------------------------------------------
jury.can <- candisc(jury.mod1)
jury.can

## ---- jury-can1---------------------------------------------------------------
par(xpd=TRUE, mar=c(4,4,3,1)+.1)
heplot(jury.can, 
       rev.axes = TRUE,
       fill = c(TRUE,FALSE),
       prefix="Canonical dimension", 
       main="Canonical HE plot")

## ---- jury-mod2---------------------------------------------------------------
# influence of Attr of photo and nature of crime on Serious and Years
jury.mod2 <- lm( cbind(Serious, Years) ~ Attr * Crime, data=MockJury)
Anova(jury.mod2, test="Roy")

## ---- jury-mod2-HE------------------------------------------------------------
par(mar=c(4,4,3,1)+.1)
heplot(jury.mod2)

## ---- jury-mod3-HE------------------------------------------------------------
# stepdown test (ANCOVA), controlling for Serious
jury.mod3 <- lm( Years ~ Serious + Attr * Crime, data=MockJury)
t(coef(jury.mod3))
Anova(jury.mod3)

## ---- jury-mod3-eff-----------------------------------------------------------
library(effects)
jury.eff <- allEffects(jury.mod3)
plot(jury.eff, ask=FALSE)

## ----skulls-------------------------------------------------------------------
knitr::include_graphics("fig/skulls.jpg")

## ---- skulls1-----------------------------------------------------------------
data(Skulls)
str(Skulls)
table(Skulls$epoch)

## ---- skulls2-----------------------------------------------------------------
# make shorter labels for epochs
Skulls$epoch <- factor(Skulls$epoch, labels=sub("c","",levels(Skulls$epoch)))
# assign better variable labels
vlab <- c("maxBreadth", "basibHeight", "basialLength", "nasalHeight")

## ---- skulls3-----------------------------------------------------------------
means <- aggregate(cbind(mb, bh, bl, nh) ~ epoch, data=Skulls, FUN=mean)[,-1]
rownames(means) <- levels(Skulls$epoch)
means

## ---- skulls4-----------------------------------------------------------------
pairs(means, vlab,
      panel = function(x, y) {
          text(x, y, levels(Skulls$epoch))
          lines(x,y)
      })

## ---- skulls-bwplot-----------------------------------------------------------
library(lattice)
library(reshape2)
sklong <- melt(Skulls, id="epoch")

bwplot(value ~ epoch | variable, data=sklong, scales="free", 
	ylab="Variable value", xlab="Epoch",
	strip=strip.custom(factor.levels=paste(vlab,
	                                       " (", levels(sklong$variable), ")",
	                                       sep="")),
	panel = function(x,y, ...) {
		panel.bwplot(x, y, ...)
		panel.linejoin(x,y, col="red", ...)
	}) 

## ---- skulls5-----------------------------------------------------------------
# fit manova model
sk.mod <- lm(cbind(mb, bh, bl, nh) ~ epoch, data=Skulls)
Anova(sk.mod)

## ---- skulls5a----------------------------------------------------------------
coef(sk.mod)

## ---- skulls6-----------------------------------------------------------------
coef(sk.mod)["epoch.L",]
print(linearHypothesis(sk.mod, "epoch.L"), SSP=FALSE) # linear component

## ---- skulls6a----------------------------------------------------------------
print(linearHypothesis(sk.mod, c("epoch.Q", "epoch.C", "epoch^4")), SSP=FALSE)

## ---- skulls-HE-pairs---------------------------------------------------------
pairs(sk.mod, variables=c(1,4,2,3),
	hypotheses=list(Lin="epoch.L", 
	                NonLin=c("epoch.Q", "epoch.C", "epoch^4")), 
	var.labels=vlab[c(1,4,2,3)])

