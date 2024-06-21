credit <- read.csv("credit.csv")

credit$history = factor(credit$history, 
levels=c("A30","A31","A32","A33","A34"))
levels(credit$history) = 
c("good","good","poor","poor","terrible")
credit$foreign <- factor(credit$foreign, 
levels=c("A201","A202"), labels=c("foreign","german"))
credit$rent <- factor(credit$housing=="A151")
credit$purpose <- factor(credit$purpose,
levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(credit$purpose) <-c("newcar","usedcar",
rep("goods/repair",4),"edu",NA,"edu","biz","biz")
credit <- credit [,c("Default", "duration", "amount",
"installment","age","history","purpose","foreign","rent")]

dim(credit)
head(credit)

library(gamlr)
source("naref.R")
credx <- sparse.model.matrix (Default ~. ^2, data =
naref(credit))
default <- credit$Default
set.seed(10101)
credscore <- cv.gamlr(credx, default, family = "binomial" )

par(mfrow=c(1,2))
plot(credscore$gamlr)
plot(credscore)

pred <- predict(credscore$gamlr , credx, type =
"response")
pred <- drop(pred)
boxplot(pred ~ default, xlab="default", ylab="prob of
default", col=c("pink","dodgerblue"))

test <- sample.int(1000,500)
credhalf <- gamlr(credx[-test,], default[-test],
family = "binomial")
predoos <- predict(credhalf, credx[test,],
type="response")
defaultoos <- default[test]
source("roc.R")

par(mai=c(.9,.9,.2,.1), mfrow=c(1,2))
roc(p=pred, y=default, bty="n", main="in-sample")
points(x= 1-mean((pred<.2)[default==0]),
y=mean((pred>.2)[default==1]), cex=1.5, pch=20,
col='red')

legend("bottomright",fill=c("red","blue"),
legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")
roc(p=predoos, y=defaultoos, bty="n",
main="out-of-sample")
points(x= 1-mean((predoos<.2)[defaultoos==0]),
y=mean((predoos>.2)[defaultoos==1]), cex=1.5, pch=20,
col='red')
points(x= 1-mean((predoos<.5)[defaultoos==0]),
y=mean((predoos>.5)[defaultoos==1]), cex=1.5, pch=20,
col='blue')

