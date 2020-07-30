getwd()
setwd("/Users/Anne/Desktop/STAT 6021/Project1")
data<-read.csv(file='clean_diamond_data.csv')
attach(data)

clarity <- factor(clarity)
color <- factor(color)
cut <- factor(cut)

levels(clarity)

View(data)

plot(price~carat, main='Scatterplot of diamond price against carat')

a1<-subset(data,cut=="Astor Ideal") 
a2<-subset(data,cut=="Good") 
a3<-subset(data,cut=="Ideal") 
a4<-subset(data,cut=="Very Good") 

reg1<-lm(price~carat,data=a1)
reg2<-lm(price~carat,data=a2)
reg3<-lm(price~carat,data=a3)
reg4<-lm(price~carat,data=a4)

points(a2$carat,a2$price, pch=2, col="red") 
points(a3$carat,a3$price, pch=8, col="blue")
points(a4$carat,a4$price, pch=12, col="yellow")
abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="blue")
abline(reg4,lty=4, col="yellow")
legend("topleft", c("Astor Ideal","Good","Ideal","Very Good"), lty=c(1,2,3,4), pch=c(1,2,8,12), col=c("black","red","blue","yellow"))

summary(reg1)
summary(reg2)
summary(reg3)
summary(reg4)

result1<-lm(price~carat*cut)
summary(result1)





