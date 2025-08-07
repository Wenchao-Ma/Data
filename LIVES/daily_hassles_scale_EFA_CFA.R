#To export and create datafiles 
library (Rcmdr)

#first we remove item 21 and 22. We ran then ans EFA, and we saw cross-loading on item 20, 9 and 12--> we removed item 20 and 12. 

# EFA on  half-sample N=574 of W6 without item 20, 21,22, and 12!
library(psych)

fa.parallel(data, fm="pa")
fa_cont1<-fa(data, nfactors=5, rotate="oblimin", fm="pa" )
fa_cont1
plot(fa_cont1)
fa.diagram(fa_cont1)

#CFA on other half sample N=596
# 5 factor model with 18 item 
library (lavaan)
library (semPlot)

model.1 <- 'SF = ~ T6_dailyhassles_1 + T6_dailyhassles_2 + T6_dailyhassles_3 
			SR = ~ T6_dailyhassles_8 + T6_dailyhassles_9 + T6_dailyhassles_10  + T6_dailyhassles_11 
			SPr  = ~ T6_dailyhassles_16 +T6_dailyhassles_17 + T6_dailyhassles_18 + T6_dailyhassles_19 
			Sph  = ~ T6_dailyhassles_4 + T6_dailyhassles_5 + T6_dailyhassles_6 + T6_dailyhassles_7 
			SE = ~ T6_dailyhassles_13 + T6_dailyhassles_14 + T6_dailyhassles_15  
			'
first.fit <- cfa(model.1, std.lv=TRUE, data=data)
summary (first.fit, fit.measures=TRUE, standardized=TRUE)
semPaths (first.fit)

# unidimensional model 

model.2 <- 'daily hassles = ~ T6_dailyhassles_1 + T6_dailyhassles_2 + T6_dailyhassles_3 + T6_dailyhassles_8 + T6_dailyhassles_9 + T6_dailyhassles_10  + T6_dailyhassles_11 + T6_dailyhassles_16 +T6_dailyhassles_17 + T6_dailyhassles_18 + T6_dailyhassles_19 + T6_dailyhassles_4 + T6_dailyhassles_5 + T6_dailyhassles_6 + T6_dailyhassles_7 + T6_dailyhassles_13 + T6_dailyhassles_14 + T6_dailyhassles_15'
second.fit <- cfa(model.2, std.lv=TRUE, data=data)
summary (second.fit, fit.measures=TRUE, standardized=TRUE)
semPaths (second.fit)

# hierarchical model 

model.3 <- 'SF = ~ T6_dailyhassles_1 + T6_dailyhassles_2 + T6_dailyhassles_3 
			SR = ~ T6_dailyhassles_8 + T6_dailyhassles_9 + T6_dailyhassles_10  + T6_dailyhassles_11 
			SPr  = ~ T6_dailyhassles_16 +T6_dailyhassles_17 + T6_dailyhassles_18 + T6_dailyhassles_19 
			Sph  = ~ T6_dailyhassles_4 + T6_dailyhassles_5 + T6_dailyhassles_6 + T6_dailyhassles_7 
			SE = ~ T6_dailyhassles_13 + T6_dailyhassles_14 + T6_dailyhassles_15
			DH = ~ SF + SR + SPr + Sph + SE   '
third.fit <- cfa(model.3, std.lv=TRUE, data=data)
summary (third.fit, fit.measures=TRUE, standardized=TRUE)

semPaths (third.fit)
anova (first.fit, third.fit)
semPaths (third.fit)

#invariance test 

library(semTools)

invariance1.model<- 'SF = ~ T6_dailyhassles_1 + T6_dailyhassles_2 + T6_dailyhassles_3 
			SR = ~ T6_dailyhassles_8 + T6_dailyhassles_9 + T6_dailyhassles_10  + T6_dailyhassles_11 
			SPr  = ~ T6_dailyhassles_16 +T6_dailyhassles_17 + T6_dailyhassles_18 + T6_dailyhassles_19 
			Sph  = ~ T6_dailyhassles_4 + T6_dailyhassles_5 + T6_dailyhassles_6 + T6_dailyhassles_7 
			SE = ~ T6_dailyhassles_13 + T6_dailyhassles_14 + T6_dailyhassles_15
			DH = ~ SF + SR + SPr + Sph + SE   '
inva1.fit<- cfa(invariance1.model, data=data)
summary (inva1.fit,fit.measures=TRUE, standardized=TRUE)
model1<- measurementInvariance(invariance1.model, 
                      data = data, 
                      group = "T6_Interv_lang")
#partial invariance

partialInvariance(model1, "scalar")

#_____________________________________________________
#Analyses with WLSMV estimator for CFA

model.1 <- 'SF = ~ T6_dailyhassles_1 + T6_dailyhassles_2 + T6_dailyhassles_3 
			SR = ~ T6_dailyhassles_8 + T6_dailyhassles_9 + T6_dailyhassles_10  + T6_dailyhassles_11 
			SPr  = ~ T6_dailyhassles_16 +T6_dailyhassles_17 + T6_dailyhassles_18 + T6_dailyhassles_19 
			Sph  = ~ T6_dailyhassles_4 + T6_dailyhassles_5 + T6_dailyhassles_6 + T6_dailyhassles_7 
			SE = ~ T6_dailyhassles_13 + T6_dailyhassles_14 + T6_dailyhassles_15  
			'
first.fit <- cfa(model.1, std.lv=TRUE, estimator = "WLSMV", data=data)
summary (first.fit, fit.measures=TRUE, standardized=TRUE)
semPaths (first.fit)
# unidimensional model 

model.2 <- 'daily hassles = ~ T6_dailyhassles_1 + T6_dailyhassles_2 + T6_dailyhassles_3 + T6_dailyhassles_8 + T6_dailyhassles_9 + T6_dailyhassles_10  + T6_dailyhassles_11 + T6_dailyhassles_16 +T6_dailyhassles_17 + T6_dailyhassles_18 + T6_dailyhassles_19 + T6_dailyhassles_4 + T6_dailyhassles_5 + T6_dailyhassles_6 + T6_dailyhassles_7 + T6_dailyhassles_13 + T6_dailyhassles_14 + T6_dailyhassles_15'
second.fit <- cfa(model.2, std.lv=TRUE, estimator = "WLSMV", data=data)
summary (second.fit, fit.measures=TRUE, standardized=TRUE)
semPaths (second.fit)
# hierarchical model 

model.3 <- 'SF = ~ T6_dailyhassles_1 + T6_dailyhassles_2 + T6_dailyhassles_3 
			SR = ~ T6_dailyhassles_8 + T6_dailyhassles_9 + T6_dailyhassles_10  + T6_dailyhassles_11 
			SPr  = ~ T6_dailyhassles_16 +T6_dailyhassles_17 + T6_dailyhassles_18 + T6_dailyhassles_19 
			Sph  = ~ T6_dailyhassles_4 + T6_dailyhassles_5 + T6_dailyhassles_6 + T6_dailyhassles_7 
			SE = ~ T6_dailyhassles_13 + T6_dailyhassles_14 + T6_dailyhassles_15
			DH = ~ SF + SR + SPr + Sph + SE   '
third.fit <- cfa(model.3, std.lv=TRUE, data=data)
summary (third.fit, fit.measures=TRUE, standardized=TRUE)
semPaths (third.fit)


