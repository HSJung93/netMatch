library(plm)
library(lmtest)
library(car)
library(stargazer)

set.seed(20201104)

# Making Z and Fixed Effect Variables
## setting params for basic variables
N = 10
T = 2
Z_bin_prob = .5

## simulating variables and treatment
(Units = rep(runif(N, -1, 1),T))
(Units.factor = rep(as.character(1:N) , T))
(Times = rep(runif(T, -10, 10), each = N))
(Times.factor = rep(as.character(1:T), each=N))
(Z_conti = runif(n = N*T, min=0, max=1))
(Z_bin = rbinom(n = N*T, size = 1, prob = Z_bin_prob))
(X = rep(0:1, c(12,8)))

## make dataframe from variables above
dat.y = data.frame(Units, Units.factor, Times, Times.factor, Z_conti, Z_bin, X)
dat.y

# Simulating Y Variables
## subsetting dataframe
### the length of the dat.output exceeds the length of the input dataframe because of the lag terms, so we make new dataframe dat.y with right length.

## making epsilon for simulating Y from the other varibles
dat.y$eps = rnorm(N*T, 0, 1)

## setting params for simulating Y form X and Z and Fixed effects terms.
beta.conti = .2
beta.bin = -.1
beta.treat = 2

dat.y$Y = with(dat.y, Units + Times + beta.treat*X + beta.conti*Z_conti + beta.bin*Z_bin + eps)
dat.y

pdat = pdata.frame(dat.y, index = c("Units.factor", "Times.factor"), drop.index = TRUE, row.name=TRUE)
head(pdat)
head(attr(pdat, "index"))

ols = lm(Y ~ X, data= dat.y)
ols2 = lm(Y ~ X + Z_conti + Z_bin, data= dat.y)

fe_ow = plm(Y ~ X , index = c("Units.factor"), data= dat.y, model = "within")
fe_ow2 = plm(Y ~ X + Z_conti + Z_bin, index = c("Units"), data= dat.y, model = "within")

fe_tw = plm(Y ~ X , index = c("Units.factor", "Times.factor"), data= dat.y, model = "within", effect="twoway")
fe_tw2 = plm(Y ~ X + Z_conti + Z_bin, index = c("Units.factor", "Times.factor"), data= dat.y, model = "within", effect="twoway")

summary(fe_tw)
summary(fe_ow_lm)

stargazer(ols, ols2, fe_ow, fe_ow2, fe_tw, fe_tw2)

plot(dat.y$X, dat.y$Y.plus.small)
abline(lm(dat.y$Y.plus.small ~ dat.y$X))

yhat_fe_ow = fe_ow$fitted
plot(dat.y$X, dat.y$Y.plus.small)
abline(lm(dat.y$Y.plus.small ~ dat.y$X))
scatterplot(yhat_fe_ow ~dat.y$X|dat.y$Units, boxplots = FALSE, smooth=FALSE)
