
rm(list=ls())
setwd("~/Desktop/Nepal data 2019")
system('defaults write org.R-project.R force.LANG en_US.UTF-8')
install.packages(c("psych"), dependencies = TRUE)
library(psych)
d3 <- read.table("lastdata.2.csv", header=TRUE, fileEncoding="cp932", sep=",", fill=TRUE, na.strings="")

ktm <- d3[d3$group2=="KTM",]
school <- d3[d3$group2=="school",]
rural <- d3[d3$group2=="rural",]
describe(d3)

#within-group mean comparison by help-seeking methods
#school
schoolhs <- school[,c("allinformalmea","HS7",
                      "allformalmea","alltraditionalmea")]
d3 <- stack(schoolhs)
name <- c(1:nrow(schoolhs))
d4 <- data.frame(d3,name)
colnames(d4) <- c("y","x","id")
d4$x <- as.factor(d4$x)
d4$id <- as.factor(d4$id)
head(d4)
library(car)
mod.1<- lm(cbind(allinformalmea,HS7,allformalmea,alltraditionalmea)~1, school)
hs <- c("allinformalmea","HS7","allformalmea","alltraditionalmea")
hs<- data.frame(hs)
result.1<- Anova(mod.1, idata=hs, idesign=~hs)
summary(result.1, multivariate=TRUE)

result.2 <- aov(y ~ x + id, data=d4)
summary(result.2)

library(DescTools)
EtaSq(result.2,type = 2, anova = FALSE)

pairwise.t.test(d4$y, d4$x, p.adjust.method="bonferroni", paired=TRUE)

#urban

urbanhs <- ktm[,c("allinformalmea","HS7",
                  "allformalmea","alltraditionalmea")]
urban1 <- stack(urbanhs)
name <- c(1:nrow(urbanhs))
urban2 <- data.frame(urban1,name)
colnames(urban2) <- c("y","x","id")
urban2$x <- as.factor(urban2$x)
urban2$id <- as.factor(urban2$id)
head(urban2)
library(car)
mod.1<- lm(cbind(allinformalmea,HS7,allformalmea,alltraditionalmea)~1, ktm)
hs <- c("allinformalmea","HS7","allformalmea","alltraditionalmea")
hs<- data.frame(hs)
urbanhs<- Anova(mod.1, idata=hs, idesign=~hs)
summary(urbanhs, multivariate=FALSE)

result.2 <- aov(y ~ x + id, data=urban2)
summary(result.2)
EtaSq(result.2,type = 2, anova = FALSE)

pairwise.t.test(urban2$y, urban2$x, p.adjust.method="bonferroni", paired=TRUE)

#rural
ruralhs <- rural[,c("allinformalmea","HS7",
                    "allformalmea","alltraditionalmea")]
rural1 <- stack(ruralhs)
name <- c(1:nrow(ruralhs))
rural2 <- data.frame(rural1,name)
colnames(rural2) <- c("y","x","id")
rural2$x <- as.factor(rural2$x)
rural2$id <- as.factor(rural2$id)
head(rural2)
library(car)
ruralhs<- lm(cbind(allinformalmea,HS7,allformalmea,alltraditionalmea)~1, rural)
hs <- c("allinformalmea","HS7","allformalmea","alltraditionalmea")
hs<- data.frame(hs)
ruralanova<- Anova(ruralhs, idata=hs, idesign=~hs)
summary(ruralanova, multivariate=FALSE)

result.2 <- aov(y ~ x + id, data=rural2)
summary(result.2)
EtaSq(result.2,type = 2, anova = FALSE)
pairwise.t.test(rural2$y, rural2$x, p.adjust.method="bonferroni", paired=TRUE)


