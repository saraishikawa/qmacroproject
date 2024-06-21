library(mlogit)
data(Heating)
dim(Heating)
head(Heating, 5)

# create model matrix and pull out response
xH <- model.matrix( depvar ~ ., data=Heating[,-1])[,-1]
yH <- Heating$depvar
table(yH)

# mnlogit in glmnet
library(glmnet)
netfit <- glmnet(xH, yH, family="multinomial")

# some predictions at lambda100
LMIN <- min(netfit$lambda) 
pnet <- drop( predict(netfit, xH, s=LMIN, type="response") )
newi <- c(1,14,21,4,24)
pnet[newi,]
yH[newi]

head( levels(yH)[apply(pnet,1,which.max)] )

hpHat <- pnet[,"hp"]> 0.1
table(predHP=hpHat==1, trueHP=yH=="hp")
8/(8+56) # precision

boxplot( pnet[cbind(1:nrow(xH),as.numeric(yH))] ~ yH, col=2:6, ylab="p.hat for y", xlab="", varwidth=TRUE)

# coefficients for lambda100
Bnet <- coef(netfit,s=min(netfit$lambda))
Bnet <- do.call(cbind, Bnet)
colnames(Bnet) <- levels(yH)
round(Bnet, 4)

exp( (Bnet["income","hp"] - Bnet["income","gc"]) )

# path plots
par(mfrow=c(1,5)) ## note we can use xvar="lambda" to plot against log lambda
plot(netfit, xvar="lambda") 

# with cross validation
cv.netfit <- cv.glmnet(xH, yH, family="multinomial")

log(cv.netfit$lambda.min)
Bcv <- do.call(cbind, coef(cv.netfit,s="lambda.min"))
colnames(Bcv) <- levels(yH)
round(Bcv, 4)


## distributed multinomial regression
library(parallel)
cl = makeCluster(detectCores())
cl
library(distrom)
dmrfit <- dmr(cl, xH, yH)
length(dmrfit)
dmrfit[[1]]

# coefficients at lambda100s (different for each class)
round(coef(dmrfit),4)

# path plots
par(mfrow=c(1,5))
for(k in names(dmrfit)) plot(dmrfit[[k]], main=k)  
