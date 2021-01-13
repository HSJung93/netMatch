generate.network = function(n, p) {

    # Generate matrix values, sampling 0 or 1 with given probabilities
    p2 = p/2
    matvals = c(
        sample(c(0, 1), (n * (n - 1)/2 - 2*n) , replace = TRUE, prob = c(1 - p,p)), 
        sample(c(0, 1), (2*n), replace = TRUE, prob = c(1 - p2, p2)))

    # From the values above, generate a symmetric matrix
    networkmat = matrix(rep(0, n * n), ncol = n)
    mv = 1
    for (i in 1:n) {
        for (j in 1:n) {
            if (i > j) {
                networkmat[i, j] = matvals[mv]
                networkmat[j, i] = matvals[mv]
                mv = mv + 1
            }
        }
    }
    return(networkmat)
}

n = 100
p = 0.4

(network <- generate.network(n, p))

(theta.z = runif(n, 1, 3))
(z = as.matrix(network) %*% as.matrix(theta.z))
as.vector(scale(z))

dat.z <- data.frame(network)
(dat.z$treat = ifelse(as.vector(scale(z)) > 0, 1, 0))

theta.x = runif(n, -1, -.5)
theta.true = 2

dat = dat.z

dat$y = as.matrix(dat.z) %*% as.matrix(c(theta.x, theta.true))

dim(dat)
colnames(dat)

ols = lm(y ~ treat, data= dat)
summary(ols)

colnames(dat)

y = dat.z$treat
#x = as.matrix(subset(df, !grepl("CRI", rownames(df))))
(x = model.matrix(treat~., dat.z))

#[,colnames(net.07) != "CRI"]

set.seed(0111)
train = sample(1:nrow(x), nrow(x)*0.8)
test = (-train)
ytest = y[test]

library(tidyverse)
library(caret)

# fitControl = trainControl(method = "repeatedcv", number = 10, repeats = 7)

# rf_fit <- train(x[train, ], y[train] , method = "rf", trControl = fitControl, verbose = F)
# lasso_fit <- train(x[train, ], y[train] , method = "lasso", trControl = fitControl, verbose = F)

#predict(rf_fit, newdata= x[test, ]) %>% confusionMatrix(ytest) 

#cv.lasso = cv.glmnet(x[train, ], y[train], alpha=1)
#lasso.coef = predict(cv.lasso, type = "coefficients", s=cv.lasso$lambda.min)
#lasso.prediction = predict(cv.lasso, s=cv.lasso$lambda.min, newx = x[test,])

# plot(cv.lasso)
# plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
# plot(cv.lasso$glmnet.fit, xvar="norm", label=TRUE)

# a = cv.lasso$lambda.min
# b = cv.lasso$lambda.1se
# c = coef(cv.lasso, s=cv.lasso$lambda.min)

# small.lambda.index = which(cv.lasso$lambda == cv.lasso$lambda.min)
# small.lambda.betas = cv.lasso$glmnet.fit$beta[,small.lambda.index]
# # fit.lasso <- glmnet(as.matrix(x), y, family="gaussian", alpha=1)

m_ps = glm(y ~ x, family = binomial(), maxit = 1000)
fitControl = trainControl(method = "repeatedcv", number = 10, repeats = 7)
rf_fit <- train(x, y, method = "rf", trControl = fitControl, verbose = F)
library(glmnet)

cv.lasso = cv.glmnet(x, y, alpha=1)

dat$logit = predict(m_ps, type = "response")
dat$lasso = predict(cv.lasso, s=cv.lasso$lambda.min, newx= x)
dat$randomForest = predict(rf_fit, newdata = x)

dat$weight.lg = ifelse(dat$treat == 1, 1/dat$logit, 1/(1-dat$logit))
dat$weight.ls = ifelse(dat$treat == 1, 1/dat$lasso, 1/(1-dat$lasso))
dat$weight.rf = ifelse(dat$treat == 1, 1/dat$randomForest, 1/(1-dat$randomForest))

summary(dat)

lg.ATE = lm(y ~ treat, data= dat, weight = weight.lg)
ls.ATE = lm(y ~ treat, data= dat, weight = weight.ls)
rf.ATE = lm(y ~ treat, data= dat, weight = weight.rf)

summary(lg.ATE)
summary(ls.ATE)
summary(rf.ATE)

write_csv(dat, "data/processed/propensitySimul.csv")
