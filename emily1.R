getwd()
setwd("C:/Users/emkmu/OneDrive/Documents/SDS/STAT 6021/Project 1")
getwd()

data<-read.csv("clean_diamond_data.csv")

data
attach(data)

clarity <- factor(clarity)
color <- factor(color)
cut <- factor(cut)

result <- lm(price~carat+color+cut+clarity)
summary(result)

# Check the VIF

library(faraway)

vif(result)

# Re-level Cut and Clarity

clarity<-relevel(clarity, ref = "SI2")
cut<-relevel(cut, ref = "Good")

result2 <- lm(price~carat+color+cut+clarity)
vif(result2)


# Step Regression
library(leaps)
allreg <- regsubsets(price ~carat+cut+clarity+color, data=data, nbest=9)
best <- as.data.frame(summary(allreg)$outmat)
best$p <- as.numeric(substr(rownames(best),1,1))+1
best$r2 <- summary(allreg)$rsq
best$adjr2 <- summary(allreg)$adjr2
best$mse <- (summary(allreg)$rss)/(dim(data)[1]-best$p)
best$cp <- summary(allreg)$cp
best$bic <- summary(allreg)$bic
best

##sort by various criteria
best[order(best$r2,decreasing=TRUE),]
best[order(best$adjr2,decreasing = TRUE),]
best[order(best$mse),]
best[order(best$cp),]
best[order(best$bic),]
