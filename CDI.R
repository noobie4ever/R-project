d<-read.table("C:/Users/Administrator/Desktop/R/CDI.txt")
#project
#1.43
#Regress the predictor with different variable.
#Calling ANOVA to assess SSE,SSR,etc
fit<-lm(V8~V5)
summary(fit)
anovafit<-anova(fit)
anovafit
plot(V5, V8, xlab="total population", ylab="number of active physicians", main="total population vs number of active physicians")
abline(fit, col="red")
qqplot(V5,V8)
#b
fit2<-lm(V8~V9)
summary(fit2)
anovafit2<-anova(fit2)
anovafit2
plot(V9,V8, xlab="hospital beds", ylab="number of active physicians", main="hospital beds vs number of active physicians")
abline(fit2, col="red")
qqplot(V9,V8)
#c
fit3<-lm(V8~V16)
summary(fit3)
anovafit3<-anova(fit3)
anovafit3
plot(V16, V9, xlab="total personal income", ylab="active physicians", main="total personal income vs active physicians")
abline(fit3, col="red")
qqplot(V16,V8)
#1.44 Compare the regression models among different region
#a
fit4<-lm(V15[V17==1]~V12[V17==1])
summary(fit4)
fit5<-lm(V15[V17==2]~V12[V17==2])
summary(fit5)
fit6<-lm(V15[V17==3]~V12[V17==3])
summary(fit6)
fit7<-lm(V15[V17==4]~V12[V17==4])
summary(fit7)
#1.44 b
fit8<-lm(V15~V12)
summary(fit8)
plot(V12,V15)
abline(fit4,col="red")
abline(fit8,col="green")
abline(fit5,col="blue")
abline(fit6,col="yellow")
abline(fit7,col="purple")
#1.44c
anovafit4<-anova(fit4)
anovafit4
anovafit5<-anova(fit5)
anovafit5
anovafit6<-anova(fit6)
anovafit6
anovafit7<-anova(fit7)
anovafit7
#2.63 Construct slope confidence intervals for different region
alpha = 0.1
p = 1-alpha/2
n=length(V12[V17==1])
beta4.ub=522.16+qt(p, n-2)*37.13
beta4.lb=522.16-qt(p, n-2)*37.13
beta5.ub=238.67+qt(p, n-2)*27.23
beta5.lb=238.67-qt(p, n-2)*27.23
beta6.ub=330.61+qt(p, n-2)*27.13
beta6.lb=330.61-qt(p, n-2)*27.13
beta7.ub=440.32+qt(p, n-2)*45.37
beta7.lb=440.32-qt(p, n-2)*45.37
qf(0.9,1,75)
#3.25 Analyze which variable is more helpful to the prediction
fit<-lm(V8~V5)
summary(fit)
res1<-fit$residuals
plot(V5, res1, xlab="total population", ylab="residuals", main="total population vs residuals")
abline(h=0)
qqnorm(res1)
#b
fit2<-lm(V8~V9)
summary(fit2)
res2<-fit2$residuals
plot(V9,res2, xlab="hospital beds", ylab="residuals", main="hospital beds vs residuals")
abline(h=0)
qqnorm(res2)
#c
fit3<-lm(V8~V16)
summary(fit3)
res3<-fit3$residuals
plot(V16, res3, xlab="total personal income", ylab="residuals", main="total personal income vs residuals")
abline(h=0)
qqnorm(res3)


#Part II
x1<-d$V5
x2<-d$V4
x3<-d$V16
x4<-x1/x2
x5<-d$V7
data.frame(x1)
data.frame(x2)
x1x2<-x1*x2
x2x3<-x2*x3
x1x3<-x1*x3
#analyze which group of variables are better for the prediction
#a
stem(x5)
#b
?pairs
pairs(~x1+x2+x3)
pairs(~populationdensity+percent16+x3)
cor(x1,x2,x3)
b<-data.frame(populationdensity,
              percent16,
              x3)
cor(b)
#c#d
y<-d$V8
model1<-lm(y~x1+x2+x3)
summary(model1)
model2<-lm(y~x4+x5+x3)
summary(model2)
anova(model1)
ssr1<-sum(anova(model1)[1:3,2])
ssto1<-sum(anova(model1)[1:4,2])
r<-ssr1/ssto1
anova(model2)
ssr2<-sum(anova(model2)[1:3,2])
ssto2<-sum(anova(model2)[1:4,2])
r2<-ssr2/ssto2
#e
res<-model1$residuals
fitted<-model1$fitted.values
plot(fitted,res)
abline(h=0)
plot(x1,res)
abline(h=0)
plot(x2,res)
abline(h=0)
plot(x3,res)
abline(h=0)
plot(x1x2,res)
abline(h=0)
plot(x1x3,res)
abline(h=0)
plot(x2x3,res)
abline(h=0)
qqplot(x1+x2+x3,y, main="qq plot")
#model2
res2<-model2$residuals
fitted2<-model2$fitted.values
plot(fitted2,res,xlab="fittedvalue")
abline(h=0)
plot(x4,res2,xlab = "x1")
abline(h=0)
plot(x5,res2,xlab = "x2")
abline(h=0)
plot(x3,res2,xlab = "x3")
abline(h=0)
x4x5<-x4*x5
x4x3<-x4*x3
x5x3<-x5*x3
plot(x4x5,res2,xlab = "x1x2")
abline(h=0)
plot(x4x3,res2,xlab = "x1x3")
abline(h=0)
plot(x5x3,res2,xlab = "x2x3")
abline(h=0)
qqplot(x4+x5+x3,y, xlab="x1+x2+x3", main="qq plot")

#f
model12<-lm(y~x1+x2+x3+x1x2+x1x3+x2x3)
summary(model12)
anova(model12)
ssr11<-sum(anova(model12)[1:6,2])
ssto11<-sum(anova(model12)[1:7,2])
r3<-ssr11/ssto11
model22<-lm(y~x4+x5+x3+x4x5+x4x3+x5x3)
anova(model22)
ssr22<-sum(anova(model22)[1:6,2])
ssto22<-sum(anova(model22)[1:7,2])
r4<-ssr22/ssto22

#partII
#a
x2<-d$V16
x3<-d$V4
x4<-d$V7
x5<-d$V9
model<-lm(y~x1+x2)
anova(model)
sse<-anova(model)[3,2]
modelx3<-lm(y~x1+x2+x3)
anova(modelx3)
ssrx3<-anova(modelx3)[3,2]
r2x3<-ssrx3/sse
modelx4<-lm(y~x1+x2+x4)
anova(modelx4)
ssrx4<-anova(modelx4)[3,2]
r2x4<-ssrx4/sse
modelx5<-lm(y~x1+x2+x5)
anova(modelx5)
ssrx5<-anova(modelx5)[3,2]
r2x5<-ssrx5/sse
anova(model, modelx5)
#c
ssrx5/144259
qf(0.99,1,436)
#d coefficient of partial determination
# using F test to find out whether adding the best pair to the model.
modelx3x4<-lm(y~x1+x2+x3+x4)
anova(modelx3x4)
ssrx3x4<-sum(anova(modelx3x4)[3:4,2])
r2x3x4<-ssrx3x4/sse

modelx3x5<-lm(y~x1+x2+x3+x5)
anova(modelx3x5)
ssrx3x5<-sum(anova(modelx3x5)[3:4,2])
r2x3x5<-ssrx3x5/sse

modelx4x5<-lm(y~x1+x2+x4+x5)
anova(modelx4x5)
ssrx4x5<-sum(anova(modelx4x5)[3:4,2])
r2x4x5<-ssrx4x5/sse
mse1.45<-anova(modelx4x5)[5,3]
(ssrx4x5/2)/mse1.45
anova(modelx4x5,model)
qf(0.99,2,435)
