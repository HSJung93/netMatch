library(igraph)
library(tidyverse)
library(gridExtra)
library(grid)
library(GGally)
library(network)
library(sna)
library(extrafont)
library(phenoDist)
library(countrycode)
library(xtable)
library(statnet)
library(ergm)

net_87 = as.undirected(graph_from_adjacency_matrix(as.matrix(read.csv("data/processed/snapshot/87_ISO3.csv", header=T, row.names=1))))
net_97 = as.undirected(graph_from_adjacency_matrix(as.matrix(read.csv("data/processed/snapshot/97_ISO3.csv", header=T, row.names=1))))
net_07 = as.undirected(graph_from_adjacency_matrix(as.matrix(read.csv("data/processed/snapshot/07_ISO3.csv", header=T, row.names=1))))
net_17 = as.undirected(graph_from_adjacency_matrix(as.matrix(read.csv("data/processed/snapshot/17_ISO3.csv", header=T, row.names=1))))

jpeg("networks.jpg", width=500, height=370) 
par(mfrow=c(2, 2), mar = c(2,2,2,2))
plot(net_87, vertex.label = NA, vertex.size = 1, vertex.color = "black", main = "RTA Network of 1987", cex.main = 2.5) + box()
plot(net_97, vertex.label = NA, vertex.size = 1, vertex.color = "black", main = "RTA Network of 1997", cex.main = 2.5) + box()
plot(net_07, vertex.label = NA, vertex.size = 1, vertex.color = "black", main = "RTA Network of 2007", cex.main = 2.5) + box()
plot(net_17, vertex.label = NA, vertex.size = 1, vertex.color = "black", main = "RTA Network of 2017", cex.main = 2.5) + box()
dev.off()

 
ceb_87 = cluster_edge_betweenness(net_87)
ceb_97 = cluster_edge_betweenness(net_97)
ceb_07 = cluster_edge_betweenness(net_07)
ceb_17 = cluster_edge_betweenness(net_17)

plot(ceb_87, net_87)
plot(ceb_97, net_97)
plot(ceb_07, net_07)
plot(ceb_17, net_17)

modularity(ceb_87)
modularity(ceb_97)
modularity(ceb_07)
modularity(ceb_17)

period = 1958:2017
make_modularities = function(period){
    modularities = list()
    for ( i in period ){
        file = paste0("data/processed/series/", i, "_ISO3.csv")
        net = as.undirected(graph_from_adjacency_matrix(as.matrix(read.csv(file, header=T, row.names=1))))
        ceb = cluster_edge_betweenness(net)
        modularities[i] = modularity(ceb)
        }
    return(modularities)
}
modularities_5817 = make_modularities(period)

x = modularities_5817
names(x) <- seq_along(x)
cleaned = Filter(Negate(is.null), x)
df = as_tibble(cleaned)
df2 = as_tibble(cbind(YEAR = names(df), t(df)))

write_csv(df2, "data/processed/modulariteis_5817.csv")


modulist = read_csv("data/processed/modulariteis_5817.csv")

ggplot(modulist, aes(x = YEAR,y = V2)) + 
    geom_point() + 
    labs(title="Modularity of RTA Network", 
        caption = "Source: WTO RTA Database",
        x = "year", 
        y = "modularity") + 
    theme(text = element_text(size = 10, family = "LM Roman 10")) + 
    geom_vline(xintercept = 2007, linetype = 6)

ggsave("modularities.jpg", width=5, height=3)

#####

V(net_07)
which( V(net_07) == "USA", arr.ind=TRUE)
net_07

similarity(net_07, method="jaccard")[116,]
similarity(net_07, method="dice")[116,]
similarity(net_07, method="invlogweighted")[116,]

as_adjacency_matrix(net_07)
as_data_frame(net_07)

net_07_mat = as.matrix(read.csv("data/processed/series/2005_ISO3.csv", header=T, row.names=1))
#net_07_mat['USA','USA']
net_07_mat['USA',"CRI"]
net_07['USA',]

colnames(net_07_mat)

make_angular = function(net){
    angular_similarities = c()
    for (i in V(net)) {
        angular_similarities[i] = angular_similarity(net['USA',], net[i,])
        }
    return(angular_similarities)
}

angular_similarities = make_angular(net_07)
angular_similarities

df_na_cow = tibble(
    country = colnames(net_07_mat),
    cowcode2 = countrycode(country, origin = "iso3c", destination = "cown"),
    jaccard = similarity(net_07, method="jaccard")[116,],
    dice = similarity(net_07, method="dice")[116,],
    invlog = similarity(net_07, method = "invlogweighted")[116,],
    angular = angular_similarities
)

df_similar = df_na_cow %>% filter(!is.na(cowcode2)) 
order_jaccard = df_similar %>% arrange(desc(jaccard))  %>% select(country,  jaccard)
order_dice = df_similar %>% arrange(desc(dice))  %>% select(country,  dice)
order_angular = df_similar %>% arrange(desc(angular))  %>% select(country,  angular)

most_similar = cbind(cbind(order_jaccard[2:11,], order_dice[2:11,]), order_angular[2:11,])
most_similar
xtable(most_similar)


order_jaccard$jaccard == NA


tail(order_dice)
tail(order_angular)



########
plot(net_07)
summary(net_07~edges)
graph.density(net_07)
mean(degree(net_07))

jpeg("output/degreeDistribution.jpg", width=500, height=400) 
plot(degree.distribution(net_07), xlab="Degree", ylab="Proportion", main="Degree Distribution of FTA Network in 2007") 
dev.off()

diameter(net_07)
degree(net_07)

plot(net_07, usearrows=FALSE,displaylabels=TRUE,
vertex.cex=degree(net_07),  label.pos=3,label.cex=.7, edge.lwd=0.5,edge.col="grey75")

## In order to use ergm do not use igraph object but use statnet object
net07 = as.network(as.matrix(read.csv("data/processed/snapshot/07_ISO3.csv", header=T, row.names=1)), matrix.type = "adjacency")

summary(net07)
mod1 <- ergm(net07~edges , control=control.ergm(seed=10))
plot(net07, 
     vertex.col = "tomato", 
     vertex.cex = 1)

theta <- mod1$coef 

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob(theta)

mod4 <- ergm(net07~edges + gwesp(0.25, fixed=TRUE), control=control.ergm(seed=10))
summary(mod4)
xtable(summary(mod4))

mod4.fit <- gof(mod4, model + esp)
summary(mod4.fit)
plot(mod4.fit)
library(latticeExtra)
mcmc.diagnostics(mod4)


net.07 = read.csv("data/processed/snapshot/07_ISO3.csv", header=T, row.names=1)

library(randomForest)
library(caret)
library(MASS)
library(glmnet)
library(caret)

#y = net.07$CRI
#x = net.07[,colnames(net.07) != "CRI"]
df <- net.07
df <- cbind(newColName = rownames(df), df)
rownames(df) <- NULL
y = df$CRI
#x = as.matrix(subset(df, !grepl("CRI", rownames(df))))
x = model.matrix(CRI~., df)[,colnames(net.07) != "CRI"]

set.seed(0111)
train = sample(1:nrow(x), nrow(x)*0.8)
test = (-train)
ytest = y[test]

library(tidyverse)
library(caret)

fitControl = trainControl(method = "repeatedcv", number = 10, repeats = 7)

rf_fit <- train(x[train, ], y[train] , method = "rf", trControl = fitControl, verbose = F)
lasso_fit <- train(x[train, ], y[train] , method = "lasso", trControl = fitControl, verbose = F)

predict(rf_fit, newdata= x[test, ]) %>% confusionMatrix(ytest) 

cv.lasso = cv.glmnet(x[train, ], y[train], alpha=1)
lasso.coef = predict(cv.lasso, type = "coefficients", s=cv.lasso$lambda.min)
lasso.prediction = predict(cv.lasso, s=cv.lasso$lambda.min, newx = x[test,])

plot(cv.lasso)
plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
plot(cv.lasso$glmnet.fit, xvar="norm", label=TRUE)

a = cv.lasso$lambda.min
b = cv.lasso$lambda.1se
c = coef(cv.lasso, s=cv.lasso$lambda.min)

small.lambda.index = which(cv.lasso$lambda == cv.lasso$lambda.min)
small.lambda.betas = cv.lasso$glmnet.fit$beta[,small.lambda.index]
# fit.lasso <- glmnet(as.matrix(x), y, family="gaussian", alpha=1)

m_ps = glm(y ~ x, family = binomial(), maxit = 1000)
fitControl = trainControl(method = "repeatedcv", number = 10, repeats = 7)
rf_fit <- train(x, y, method = "rf", trControl = fitControl, verbose = F)
cv.lasso = cv.glmnet(x, y, alpha=1)

prs_df <- data.frame(
    ISO3 = rownames(net.07),
    logit = predict(m_ps, type = "response"), 
    lasso = predict(cv.lasso, s=cv.lasso$lambda.min, newx= x), 
    randomForest = predict(rf_fit, newdata = x),
    CRI = m_ps$model$y
    )
head(prs_df)

write_csv(prs_df, "data/processed/propensity.csv")

summary(prs_df$lasso)

dim(x)

cv.rf = randomForest(x[train, ], y[train])
cv.rf
rf.prediction = predict(cv.rf)

library(caret)