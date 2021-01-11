rm(list=ls())
setwd("~/Desktop/Nepal data 2019")
system('defaults write org.R-project.R force.LANG en_US.UTF-8')
install.packages(c("psych"), dependencies = TRUE)
library(psych)
d3 <- read.table("regression-stepwisedata.csv", header=TRUE, fileEncoding="cp932", sep=",", fill=TRUE, na.strings="")

ktm <- d3[d3$group2=="KTM", ]
rural <- d3[d3$group2=="rural", ]
school <- d3[d3$group2=="school", ]


plot(school$income4,school$allformalmea)
plot(school$income2,school$allformalmea)
table(d3$education2)
d3$education5 <- factor(d3$education, levels=c("higher2","none","primary","secondary","university")
                           , labels=c("sec<", "none","pri","sec<","sec<")) 

#hierachical liniear regression#################################
#college

informal1 <- lm(allinformalmea~ sex2+psyedu2+timeHP3+income1
                ,data=school) 
summary(informal1)

informal2 <- lm(allinformalmea~   sex2+psyedu2+timeHP3+income1+
                  stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
                
                ,data=school) 
summary(informal2)
anova(informal1,informal2)
informal3 <- lm(allinformalmea~   sex2+psyedu2+timeHP3+income1+
                  stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
                +ssSum:networksize2+stigmaSum:networkdensity
                
                ,data=school) 
summary(informal3)
anova(informal1,informal2,informal3)


library(apaTables)
apa.reg.table(informal1, 
              informal2,informal3,filename = "college-informal regression.doc", table.number = 3)

#################
phy1 <- lm(HS7~ sex2+psyedu2+timeHP3+income1
                ,data=school) 
summary(phy1)

phy2 <- lm(HS7~   sex2+psyedu2+timeHP3+income1+
                  stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
                
                ,data=school) 
summary(phy2)
anova(phy1,phy2)
phy3 <- lm(HS7~   sex2+psyedu2+timeHP3+income1+
                  stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
           +ssSum:networksize2+stigmaSum:networkdensity
                
                ,data=school) 
summary(phy3)
anova(phy1,phy2,phy3)
apa.reg.table(phy1,phy2,phy3,filename = 
                "college-phy regression.doc", table.number = 3)

########

formal1 <- lm(allformalmea~ sex2+psyedu2+timeHP3+income1
                ,data=school) 
summary(formal1)

formal2 <- lm(allformalmea~   sex2+psyedu2+timeHP3+income1+
                  stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
                
                ,data=school) 
summary(formal2)
anova(formal1,formal2)
formal3 <- lm(allformalmea~   sex2+psyedu2+timeHP3+income1+
                  stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
                 +ssSum:networksize2+stigmaSum:networkdensity
                ,data=school) 
summary(formal3)
anova(formal2,formal3)
apa.reg.table(formal1, formal2,formal3,filename = 
                "college-psy regression.doc", table.number = 3)


########
tra1 <- lm(alltraditionalmea~ sex2+psyedu2+timeHP3+income1
              ,data=school) 
summary(tra1)

tra2 <- lm(alltraditionalmea~   sex2+psyedu2+timeHP3+income1+
             stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
              
              ,data=school) 
summary(tra2)
anova(tra1,tra2)
tra3 <- lm(alltraditionalmea~   sex2+psyedu2+timeHP3+income1+
             stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
           +ssSum:networksize2+stigmaSum:networkdensity
              ,data=school) 
summary(tra3)
anova(tra1,tra2,tra3)
apa.reg.table(tra1, tra2,tra3,filename = 
                "college-tra regression.doc", table.number = 3)

#hierachical liniear regression#################################
#Urban
informal1 <- lm(allinformalmea~  sex2+psyedu2+timeHP3+income1
                ,data=ktm) 
summary(informal1)

informal2 <- lm(allinformalmea~   sex2+psyedu2+timeHP3+income1+
                  stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
                ,data=ktm) 
summary(informal2)
anova(informal1,informal2)
informal3 <- lm(allinformalmea~   sex2+psyedu2+timeHP3+income1+
                  stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
                +ssSum:networksize2+stigmaSum:networkdensity
                ,data=ktm) 
summary(informal3)
anova(informal1,informal2,informal3)
apa.reg.table(informal1,informal2,informal3,filename = 
                "urban-informal regression.doc", table.number = 3)
#################
phy1 <- lm(HS7~ sex2+psyedu2+timeHP3+income1
           ,data=ktm) 
summary(phy1)
phy2 <- lm(HS7~ sex2+psyedu2+timeHP3+income1+
             stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
          
           ,data=ktm) 
summary(phy2)
anova(phy1,phy2)
phy3 <- lm(HS7~   sex2+psyedu2+timeHP3+income1+
             stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
           +ssSum:networksize2+stigmaSum:networkdensity
           
           ,data=ktm) 
summary(phy3)
anova(phy1,phy2,phy3)
apa.reg.table(phy1,phy2,phy3,filename = 
                "urban-phy regression.doc", table.number = 3)
########
formal1 <- lm(allformalmea~ sex2+psyedu2+timeHP3+income1
              ,data=ktm) 
summary(formal1)
formal2 <- lm(allformalmea~   sex2+psyedu2+timeHP3+income1+
                stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
                ,data=ktm) 
summary(formal2)
anova(formal1,formal2)
formal3 <- lm(allformalmea~   sex2+psyedu2+timeHP3+income1+
                stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
              +ssSum:networksize2+stigmaSum:networkdensity
              ,data=ktm) 
summary(formal3)
anova(formal1,formal2,formal3)
apa.reg.table(formal1,formal2,formal3,filename = 
                "urban-psy regression.doc", table.number = 3)
########
table(age2)
plot(ktm$age2,ktm$alltraditionalmea)
plot(ktm$age2,ktm$ssSum)
tra1 <- lm(alltraditionalmea~ sex2+psyedu2+timeHP3+income1
           ,data=ktm) 
summary(tra1)

tra2 <- lm(alltraditionalmea~   sex2+psyedu2+timeHP3+income1+
             stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
           ,data=ktm) 
summary(tra2)
anova(tra1,tra2)
tra3 <- lm(alltraditionalmea~ sex2+psyedu2+timeHP3+income1+
             stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
           +ssSum:networksize2+stigmaSum:networkdensity
           
           ,data=ktm) 
summary(tra3)
anova(tra2,tra3)
apa.reg.table(tra1,tra2,tra3,filename = 
                "urban-tra regression.doc", table.number = 3)

plot(ktm$age2,ktm$alltraditionalmea)
plot(ktm$age2,ktm$networksize2)
plot(ktm$alltraditionalmea,ktm$networksize2)
table(ktm$age)
#hierachical liniear regression#################################
#Rural

table(rural$education)
rural$education5 <- factor(rural$education, levels=c("higher2","none","primary","secondary","university")
                           , labels=c("sec<", "none","pri","sec<","sec<")) 
table(rural$education5)
informal1 <- lm(allinformalmea~ sex2+psyedu2+timeHP3+income1+education5
                ,data=rural) 
summary(informal1)


informal2 <- lm(allinformalmea~    sex2+psyedu2+timeHP3+income1+education5+
                  stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
                ,data=rural) 
summary(informal2)
anova(informal1,informal2)
informal3 <- lm(allinformalmea~    sex2+psyedu2+timeHP3+income1+education5+
                  stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
                +ssSum:networksize2+stigmaSum:networkdensity
                ,data=rural) 
summary(informal3)
anova(informal2,informal3)
apa.reg.table(informal1,informal2,informal3,filename = 
                "rural-informal regression.doc", table.number = 3)

plot(rural$education5,rural$allformalmea)
plot(rural$education5,rural$allinformalmea)


#################
phy1 <- lm(HS7~ sex2+psyedu2+timeHP3+income1+education5
           ,data=rural) 
summary(phy1)

phy2 <- lm(HS7~   sex2+psyedu2+timeHP3+income1+education5+
             stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
           ,data=rural) 
summary(phy2)
anova(phy1,phy2)
phy3 <- lm(HS7~   sex2+psyedu2+timeHP3+income1+education5+
             stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
           +ssSum:networksize2+stigmaSum:networkdensity
           ,data=rural) 
summary(phy3)
anova(phy2,phy3)
apa.reg.table(phy1,phy2,phy3,filename = 
                "rural-phy regression.doc", table.number = 3)
########
formal1 <- lm(allformalmea~ sex2+psyedu2+timeHP3+income1+education5
              ,data=rural) 
summary(formal1)

formal2 <- lm(allformalmea~   sex2+psyedu2+timeHP3+income1+education5+
                stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
              ,data=rural) 
summary(formal2)
anova(formal1,formal2)
formal3 <- lm(allformalmea~   sex2+psyedu2+timeHP3+income1+education5+
                stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
              +ssSum:networksize2+stigmaSum:networkdensity
              ,data=rural) 
summary(formal3)
anova(formal2,formal3)
apa.reg.table(formal1,formal2,formal3,filename = 
                "rural-psy regression.doc", table.number = 3)
########
tra1 <- lm(alltraditionalmea~ sex2+psyedu2+timeHP3+income1+education5
           ,data=rural) 
summary(tra1)

tra2 <- lm(alltraditionalmea~   sex2+psyedu2+timeHP3+income1+education5+
             stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
           ,data=rural) 
summary(tra2)
anova(tra1,tra2)
tra3 <- lm(alltraditionalmea~   sex2+psyedu2+timeHP3+income1+education5+
             stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
           +ssSum:networksize2+stigmaSum:networkdensity
           ,data=rural) 

summary(tra3)
anova(tra2,tra3)
apa.reg.table(tra1,tra2,tra3,filename = 
                "rural-tra regression.doc", table.number = 3)


#hierachical liniear regression#################################
#ALL
table(d3$age5)
d3$age5 <- factor(d3$age2, levels=c("18-24","24-29","30-39","40-49","50-59","60over")
                           , labels=c("18-24", "24-29","30-49","30-49","50<","50<")) 

informal1 <- lm(allinformalmea~ group2+psyedu2+timeHP3+income1+age5+education5
                ,data=d3) 
summary(informal1)

informal2 <- lm(allinformalmea~   group2+psyedu2+timeHP3+income1+age5+education5+
                  stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
                ,data=d3) 
summary(informal2)
anova(informal1,informal2)
informal3 <- lm(allinformalmea~   group2+psyedu2+timeHP3+income1+age5+education5+
                  stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
                +stigmaSum:networkdensity+ssSum:networkmea
                ,data=d3) 
summary(informal3)
anova(informal2,informal3)
#################
formal1 <- lm(HS7~ group2+psyedu2+timeHP3+income1+age5+education5
                ,data=d3) 
summary(formal1)

formal2 <- lm(HS7~   group2+psyedu2+timeHP3+income1+age5+education5+
                stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
                ,data=d3) 
summary(formal2)
anova(formal1,formal2)
formal3 <- lm(HS7~    group2+psyedu2+timeHP3+income1+age5+education5+
                stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
              +stigmaSum:networkdensity+ssSum:networkmea
                ,data=d3) 
summary(formal3)
anova(formal2,formal3)
########
psy1 <- lm(allformalmea~ group2+psyedu2+timeHP3+income1+age5+education5
                ,data=d3) 
summary(psy1)

psy2 <- lm(allformalmea~   group2+psyedu2+timeHP3+income1+age5+education5+
             stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
                ,data=d3) 
summary(psy2)
anova(psy1,psy2)
psy3 <- lm(allformalmea~   group2+psyedu2+timeHP3+income1+age5+education5+
             stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
           +stigmaSum:networkdensity+ssSum:networkmea
                ,data=d3) 
summary(psy3)
anova(psy2,psy3)
########
traditional1 <- lm(alltraditionalmea~ group2+psyedu2+timeHP3+income1+age5+education5
                ,data=d3) 
summary(traditional1)

traditional2 <- lm(alltraditionalmea~   group2+psyedu2+timeHP3+income1+age5+education5+
                  stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
                ,data=d3) 
summary(traditional2)
anova(traditional1,traditional2)
traditional3 <- lm(alltraditionalmea~   group2+psyedu2+timeHP3+income1+age5+education5+
                  stigmaSum+disSum+ssSum+networksize2+networkmea+networkdensity
                +stigmaSum:networkdensity+ssSum:networkmea
                ,data=d3) 
summary(traditional3)
anova(traditional2,traditional3)

install.packages("apaTables",dep=T)
library(apaTables)
