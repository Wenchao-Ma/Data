########  The Chinese Character Size Test   ######## 
# require packages
library(psych)
library(readxl)
library(readr)
library(mirt)
library(Rmisc)
library(effectsize)

## prepare data  ##
setwd("~/Desktop/data and analysis")
CCST_Normdata <- read_excel("CCST Norm data.xlsx")
CCST_Item_Bank <- read_excel("CCST Item Bank.xlsx")
data <- as.data.frame(CCST_Normdata[,-(1:13)])
#load("Normdata_CCST.RData")

# average test time
mean(CCST_Normdata$TestTime_s/60)
# 60% test time
sum(CCST_Normdata$TestTime_s<16*60)/7459

####  IRT model comparison #####
myRasch <- mirt(data, model=1, itemtype="Rasch") # default discrimination = 1
my2PL <- mirt(data, model=1, itemtype="2PL")
my3PL <- mirt(data, model=1, itemtype="3PL") 

# Model Comparison, only for nested models
anova(myRasch, my2PL) #(the smaller the better! The 2PL has smaller AIC, SABIC, BIC than the Rasch model)
anova(my2PL, my3PL) #(the smaller the better! The 2PL has smaller BIC than the 3PL model)
summary(my2PL)


####    Individual's  Character Recognition ability and Character Size  #### 
# using 2PL IRT model
theta <- fscores(my2PL) 
CharacterRecognition <- fscores(my2PL, full.scores.SE=TRUE) #when full.scores == TRUE, also return the standard errors associated with each respondent
colnames(CharacterRecognition) <- c("CR_theta", "CR_SE")
characterSize <- expected.test(my2PL, theta)/525*3500          #Character size formula 
characterSize_SE <- sqrt(characterSize*(3500-characterSize)/525)     #Standard error of measurement of the Character size according to a BRM article (Chen, Engel & Wang 2020)

result <- cbind(CCST_Normdata[,1:7],CharacterRecognition,characterSize,characterSize_SE,CCST_Normdata[,12:13])
write.csv(result,"Result.csv")



###### Table1. Item bank Quality #####
a<-coef(my2PL, as.data.frame = TRUE, IRTpars = TRUE)
coef <- as.data.frame(t(matrix(a[1:2100],ncol = ncol(data))))
colnames(coef) <- c("IRT.Discrim", "IRT.Difficu", "IRT.Guess", "IRT.Inatt")
rownames(coef) <- variable.names(data)
describe(coef)
temp <- describeBy(coef[,c(2,1)],group = CCST_Item_Bank$Level,mat=TRUE) #Table1
matrix(round(temp$mean,2),ncol=2,nrow=11)
write.csv(coef,"coef.csv")

###### Figure 2. test reliability  ####
Theta <- matrix(seq(-5,5,.01))
info <- cbind(Theta,
              testinfo(my2PL,Theta,which.items = 1:525), #infoTotal 
              testinfo(my2PL,Theta,which.items = which(CCST_Item_Bank$TestletG1==1)),#infoG1
              testinfo(my2PL,Theta,which.items = which(CCST_Item_Bank$TestletG2==1)),#infoG2
              testinfo(my2PL,Theta,which.items = which(CCST_Item_Bank$TestletG3==1)),#infoG3
              testinfo(my2PL,Theta,which.items = which(CCST_Item_Bank$TestletG4==1)),#infoG4
              testinfo(my2PL,Theta,which.items = which(CCST_Item_Bank$TestletG5==1)))#infoG5-6
colnames(info) <- c("Theta","Item Bank","Booklet G1","Booklet G2","Booklet G3","Booklet G4","BookletG5-6")
write.csv(info,file = "info.CSV")

testinfo(my2PL,Theta)

# Figure 2A
plot(x=Theta,y=info[,3], xlab="Character Recognition(θ)",ylab="Test Information",type="l")
lines(x=Theta,y=info[,4])
lines(x=Theta,y=info[,5])
lines(x=Theta,y=info[,6])
lines(x=Theta,y=info[,7])
# Figure 2B
plot(x=Theta,y=info[,2], xlab="Character Recognition(θ)",ylab="Test Information",type="l")




###### Figure 3. 95% CI of the Character Size Scores.  ###### 
####  Character Size ####
Theta <- matrix(seq(-5,5,.1))
CharacterSize <- expected.test(my2PL, Theta)/525*3500          #Character size formula 
CharacterSize_SE <- sqrt(CharacterSize*(3500-CharacterSize)/525)     #Standard error of measurement of the Character size according to a BRM article (Chen, Engel & Wang 2020)
upperCI <- CharacterSize+CharacterSize_SE*1.96  # upper 95% Confidence Interval
lowerCI <- CharacterSize-CharacterSize_SE*1.96  # lower 95% Confidence Interval

#Figure3
plot(x=Theta,y=CharacterSize,type="l",cex = 3)
lines(x=Theta,y=lowerCI, col = "blue")
lines(x=Theta,y=upperCI, col = "blue")


###### Table 2. criterion-related validity of the CCST ##### 
round(cor(CCST_Normdata[which(CCST_Normdata$grade=="G2"),c(10,12,13)], use = "pairwise.complete.obs"),2) #grade 2
round(cor(CCST_Normdata[which(CCST_Normdata$grade=="G3"),c(10,12,13)], use = "pairwise.complete.obs"),2) #grade 3
round(cor(CCST_Normdata[which(CCST_Normdata$grade=="G4"),c(10,12,13)], use = "pairwise.complete.obs"),2) #grade 4
round(cor(CCST_Normdata[which(CCST_Normdata$grade=="G5"),c(10,12,13)], use = "pairwise.complete.obs"),2) #grade 5
round(cor(CCST_Normdata[which(CCST_Normdata$grade=="G6"),c(10,12,13)], use = "pairwise.complete.obs"),2) #grade 6


###### Table 3. The Total, grade, gender, and regional norms of character size. ####
t1 <- describeBy(characterSize,CCST_Normdata$grade,mat=TRUE)  
t2 <- describeBy(characterSize,list(CCST_Normdata$grade,CCST_Normdata$gender),mat=TRUE)  
t3 <- describeBy(characterSize,list(CCST_Normdata$grade,CCST_Normdata$region),mat=TRUE)  
t4 <- describeBy(characterSize,CCST_Normdata$gender,mat=TRUE)  
t5 <- describeBy(characterSize,CCST_Normdata$region,mat=TRUE)  
#t <- rbind(rbind(t1$mean,t1$sd),rbind(t2$mean[1:6],t2$sd[1:6]),rbind(t2$mean[7:12],t2$sd[7:12]),rbind(t3$mean[1:6],t3$sd[1:6]),rbind(t3$mean[13:18],t3$sd[13:18]),rbind(t3$mean[7:12],t3$sd[7:12]))
#rownames(t) <- c("Total_Mean","Total_SD","Boys_Mean","Boys_SD","Girls_Mean","Girls_SD","Central urban_Mean","Central urban_SD","Suburban_Mean","Suburban_SD","Rural_Mean","Rural_SD")

append(CI(CharacterSize,ci=0.95),sd(CharacterSize))                       # Total samples
cbind(group.CI(CharacterSize~grade,CCST_Normdata,ci=0.95),t1$sd)          # Grade Norms
cbind(group.CI(CharacterSize~gender,CCST_Normdata,ci=0.95),t4$sd)         # Gender Norms
cbind(group.CI(CharacterSize~grade+gender,CCST_Normdata,ci=0.95),t2$sd)   # Gender Norms by grade
cbind(group.CI(CharacterSize~region,CCST_Normdata,ci=0.95),t5$sd)         # Region Norms 
cbind(group.CI(CharacterSize~grade+region,CCST_Normdata,ci=0.95),t3$sd)   # Region Norms by grade



model <- aov(CharacterSize ~ grade*region*gender, data=CCST_Normdata)
#view summary of three-way ANOVA
summary(model)
eta_squared(model)



###### Figure 4. Proportion of Character Size Levels.  ######
CCST_Normdata$CharacterSize_Level <- rep("Level1",7459)
CCST_Normdata$CharacterSize_Level[which(CCST_Normdata$CharacterSize>1600&CCST_Normdata$CharacterSize<=2500)] <- "Level2"
CCST_Normdata$CharacterSize_Level[which(CCST_Normdata$CharacterSize>2500&CCST_Normdata$CharacterSize<=3000)] <- "Level3"
CCST_Normdata$CharacterSize_Level[which(CCST_Normdata$CharacterSize>3000)] <- "Level4"
cbind(CCST_Normdata$grade,CCST_Normdata$CharacterSize,CCST_Normdata$CharacterSize_Level)
(df <- table(as.data.frame(cbind(CCST_Normdata$grade,CCST_Normdata$CharacterSize_Level))))
(Fig4 <- round(df / rowSums(df) * 100,2))

# Results
plot(Fig4,type="h")  #plots are




