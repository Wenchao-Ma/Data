#sink("VGDS-G_CFA.txt", append = FALSE, type = c("output", "message"), split = FALSE)

library(lavaan)
library(foreign)
library(lavaanPlot)
library(psych)

options(max.print = 99999)

#cfa German sample
VGDS_Germany <- read.spss("VGDS-G.sav", use.value.labels = FALSE, to.data.frame = TRUE)

CFA_model <- 'cognitive =~ COG1 + COG2 + COG3 +COG4 +COG5 + COG6 +COG7
              emotional =~ EMOT1 + EMOT2 + EMOT3 + EMOT4 + EMOT5
              controller =~ CON1 + CON2 + CON3 + CON4
              exertional =~ PHY1 + PHY2 + PHY3 + PHY4
              social =~SOC1 + SOC2 + SOC3 + SOC4 +SOC5 + SOC6'

fit <- cfa(CFA_model, data=VGDS_Germany, estimator = "MLR")
summary(fit, standardized=TRUE, fit.measure=TRUE)
lavResiduals(fit, type = "cor")

lavaanPlot(
  model = fit, 
  node_options = list(shape = "box", fontname = "Times"), 
  edge_options = list(color = "black"), 
  coef = TRUE, 
  stand = TRUE)

CFA_model_2 <- 'cognitive =~ COG1 + COG2 + COG3 +COG4 +COG5 + COG6 +COG7
              emotional =~ EMOT1 + EMOT2 + EMOT3 + EMOT4 + EMOT5
              controller =~ CON1 + CON2 + CON3
              exertional =~ PHY1 + PHY2 + PHY3 + PHY4
              social =~SOC1 + SOC2 + SOC3 + SOC4 +SOC5 + SOC6'

fit_2 <- cfa(CFA_model_2, data=VGDS_Germany, estimator = "MLR")
summary(fit_2, standardized=TRUE, fit.measure=TRUE)
lavResiduals(fit_2, type = "cor")

lavaanPlot(
  model = fit_2, 
  node_options = list(shape = "box", fontname = "Times"), 
  edge_options = list(color = "black"), 
  coef = TRUE, 
  stand = TRUE)

fit.stats <- rbind(
  fitmeasures(fit, fit.measures = c("chisq", "df", "p-value", "rmsea", "srmr", "tli", "cfi", "aic", "bic")),
  fitmeasures(fit_2, fit.measures = c("chisq", "df", "p-value", "rmsea", "srmr", "tli", "cfi", "aic", "bic")))
  rownames(fit.stats) <- c("26 items", "25 items")
fit.stats

#sink()

#sink("VGDS-G_Validation.txt", append = FALSE, type = c("output", "message"), split = FALSE)

#Validation measures

SEM_NASATLX <- 'cognitive =~ COG1 + COG2 + COG3 + COG4 + COG5 + COG6 + COG7
                emotional =~ EMOT1 + EMOT2 + EMOT3 + EMOT4 + EMOT5
                controller =~ CON1 + CON2 + CON3
                exertional =~ PHY1 + PHY2 + PHY3 + PHY4
                social =~ SOC1 + SOC2 + SOC3 + SOC4 +SOC5 + SOC6
                NASA_TLX =~ TLX1 + TLX2 + TLX3 + TLX5 + TLX6
         
                NASA_TLX ~ cognitive + emotional + controller + exertional + social'

fit_NASATLX <- sem(SEM_NASATLX, data = VGDS_Germany, estimator = "MLR")
summary(fit_NASATLX, standardized = TRUE, fit.measure=TRUE, rsquare = TRUE)

SEM_Predictive <- 'cognitive =~ COG1 + COG2 + COG3 + COG4 + COG5 + COG6 + COG7
                emotional =~ EMOT1 + EMOT2 + EMOT3 + EMOT4 + EMOT5
                controller =~ CON1 + CON2 + CON3
                exertional =~ PHY1 + PHY2 + PHY3 + PHY4
                social =~ SOC1 + SOC2 + SOC3 + SOC4 +SOC5 + SOC6

                Meffort ~ cognitive + emotional + controller + exertional + social
                Eeffort ~ cognitive + emotional + controller + exertional + social
                Peffort ~ cognitive + emotional + controller + exertional + social
                Seffort ~ cognitive + emotional + controller + exertional + social'

fit_Predictive <- sem(SEM_Predictive, data = VGDS_Germany, estimator = "MLR")
summary(fit_Predictive, standardized = TRUE, fit.measure=TRUE, rsquare = TRUE)


SEM_Concurrent <-   'cognitive =~ COG1 + COG2 + COG3 + COG4 + COG5 + COG6 + COG7
                    emotional =~ EMOT1 + EMOT2 + EMOT3 + EMOT4 + EMOT5
                    controller =~ CON1 + CON2 + CON3
                    exertional =~ PHY1 + PHY2 + PHY3 + PHY4
                    social =~SOC1 + SOC2 + SOC3 + SOC4 +SOC5 + SOC6
                    ENJOYMENT =~ Enjoy1 + Enjoy2 + Enjoy3
                    APPRECIATION =~ Meaning1 + Meaning2 + Meaning3
                    AUTONOMY =~ Auto1 + Auto2 + Auto3
                    COMPETENCE =~ Comp1 + Comp2 + Comp3
                    RELATEDNESS =~ Rel1 + Rel2 + Rel3

                    ENJOYMENT ~ cognitive + emotional + controller + exertional + social  
                    APPRECIATION ~ cognitive + emotional + controller + exertional + social  
                    AUTONOMY ~ cognitive + emotional + controller + exertional + social                    
                    COMPETENCE ~ cognitive + emotional + controller + exertional + social                    
                    RELATEDNESS ~ cognitive + emotional + controller + exertional + social
                    Story ~ cognitive + emotional + controller + exertional + social
                    Gameplay ~ cognitive + emotional + controller + exertional + social
                    Control ~ cognitive + emotional + controller + exertional + social
                    Sound ~ cognitive + emotional + controller + exertional + social
                    Graphics ~ cognitive + emotional + controller + exertional + social
                    Overall ~ cognitive + emotional + controller + exertional + social'

fit_Concurrent <- sem(SEM_Concurrent, data = VGDS_Germany, estimator = "MLR")
summary(fit_Concurrent, standardized = TRUE, fit.measure=TRUE, rsquare = TRUE)

SEM_Meffort <- 'cognitive =~ COG1 + COG2 + COG3 + COG4 + COG5 + COG6 + COG7
                emotional =~ EMOT1 + EMOT2 + EMOT3 + EMOT4 + EMOT5
                controller =~ CON1 + CON2 + CON3
                exertional =~ PHY1 + PHY2 + PHY3 + PHY4
                social =~ SOC1 + SOC2 + SOC3 + SOC4 +SOC5 + SOC6

                Meffort ~ cognitive + emotional + controller + exertional + social'

fit_Meffort <- sem(SEM_Meffort, data = VGDS_Germany, estimator = "MLR")
summary(fit_Meffort, standardized = TRUE, fit.measure=TRUE, rsquare = TRUE)

SEM_Eeffort <- 'cognitive =~ COG1 + COG2 + COG3 + COG4 + COG5 + COG6 + COG7
                emotional =~ EMOT1 + EMOT2 + EMOT3 + EMOT4 + EMOT5
                controller =~ CON1 + CON2 + CON3
                exertional =~ PHY1 + PHY2 + PHY3 + PHY4
                social =~ SOC1 + SOC2 + SOC3 + SOC4 +SOC5 + SOC6

                Eeffort ~ cognitive + emotional + controller + exertional + social'

fit_Eeffort <- sem(SEM_Eeffort, data = VGDS_Germany, estimator = "MLR")
summary(fit_Eeffort, standardized = TRUE, fit.measure=TRUE, rsquare = TRUE)

SEM_Peffort <- 'cognitive =~ COG1 + COG2 + COG3 + COG4 + COG5 + COG6 + COG7
                emotional =~ EMOT1 + EMOT2 + EMOT3 + EMOT4 + EMOT5
                controller =~ CON1 + CON2 + CON3
                exertional =~ PHY1 + PHY2 + PHY3 + PHY4
                social =~ SOC1 + SOC2 + SOC3 + SOC4 +SOC5 + SOC6

                Peffort ~ cognitive + emotional + controller + exertional + social'

fit_Peffort <- sem(SEM_Peffort, data = VGDS_Germany, estimator = "MLR")
summary(fit_Peffort, standardized = TRUE, fit.measure=TRUE, rsquare = TRUE)

SEM_Seffort <- 'cognitive =~ COG1 + COG2 + COG3 + COG4 + COG5 + COG6 + COG7
                emotional =~ EMOT1 + EMOT2 + EMOT3 + EMOT4 + EMOT5
                controller =~ CON1 + CON2 + CON3
                exertional =~ PHY1 + PHY2 + PHY3 + PHY4
                social =~ SOC1 + SOC2 + SOC3 + SOC4 +SOC5 + SOC6

              Seffort ~ cognitive + emotional + controller + exertional + social'

fit_Seffort <- sem(SEM_Seffort, data = VGDS_Germany, estimator = "MLR")
summary(fit_Seffort, standardized = TRUE, fit.measure=TRUE, rsquare = TRUE)

SEM_ENJOYMENT <- 'cognitive =~ COG1 + COG2 + COG3 + COG4 + COG5 + COG6 + COG7
                  emotional =~ EMOT1 + EMOT2 + EMOT3 + EMOT4 + EMOT5
                  controller =~ CON1 + CON2 + CON3
                  exertional =~ PHY1 + PHY2 + PHY3 + PHY4
                  social =~SOC1 + SOC2 + SOC3 + SOC4 +SOC5 + SOC6
                  ENJOYMENT =~ Enjoy1 + Enjoy2 + Enjoy3

                  ENJOYMENT ~ cognitive + emotional + controller + exertional + social'

fit_ENJOYMENT <- sem(SEM_ENJOYMENT, data = VGDS_Germany, estimator = "MLR")
summary(fit_ENJOYMENT, standardized = TRUE, fit.measure=TRUE, rsquare = TRUE)

SEM_APPRECIATION <- 'cognitive =~ COG1 + COG2 + COG3 + COG4 + COG5 + COG6 + COG7
                    emotional =~ EMOT1 + EMOT2 + EMOT3 + EMOT4 + EMOT5
                    controller =~ CON1 + CON2 + CON3
                    exertional =~ PHY1 + PHY2 + PHY3 + PHY4
                    social =~SOC1 + SOC2 + SOC3 + SOC4 +SOC5 + SOC6
                    APPRECIATION =~ Meaning1 + Meaning2 + Meaning3

                    APPRECIATION ~ cognitive + emotional + controller + exertional + social'

fit_APPRECIATION <- sem(SEM_APPRECIATION, data = VGDS_Germany, estimator = "MLR")
summary(fit_APPRECIATION, standardized = TRUE, fit.measure=TRUE, rsquare = TRUE)

SEM_AUTONOMY <- 'cognitive =~ COG1 + COG2 + COG3 + COG4 + COG5 + COG6 + COG7
                  emotional =~ EMOT1 + EMOT2 + EMOT3 + EMOT4 + EMOT5
                  controller =~ CON1 + CON2 + CON3
                  exertional =~ PHY1 + PHY2 + PHY3 + PHY4
                  social =~SOC1 + SOC2 + SOC3 + SOC4 +SOC5 + SOC6
                  AUTONOMY =~ Auto1 + Auto2 + Auto3

                  AUTONOMY ~ cognitive + emotional + controller + exertional + social'

fit_AUTONOMY <- sem(SEM_AUTONOMY, data = VGDS_Germany, estimator = "MLR")
summary(fit_AUTONOMY, standardized = TRUE, fit.measure=TRUE, rsquare = TRUE)

SEM_COMPETENCE <- 'cognitive =~ COG1 + COG2 + COG3 + COG4 + COG5 + COG6 + COG7
                   emotional =~ EMOT1 + EMOT2 + EMOT3 + EMOT4 + EMOT5
                   controller =~ CON1 + CON2 + CON3
                   exertional =~ PHY1 + PHY2 + PHY3 + PHY4
                   social =~SOC1 + SOC2 + SOC3 + SOC4 +SOC5 + SOC6
                   COMPETENCE =~ Comp1 + Comp2 + Comp3

                   COMPETENCE ~ cognitive + emotional + controller + exertional + social'

fit_COMPETENCE <- sem(SEM_COMPETENCE, data = VGDS_Germany, estimator = "MLR")
summary(fit_COMPETENCE, standardized = TRUE, fit.measure=TRUE, rsquare = TRUE)

SEM_RELATEDNESS <- 'cognitive =~ COG1 + COG2 + COG3 + COG4 + COG5 + COG6 + COG7
                    emotional =~ EMOT1 + EMOT2 + EMOT3 + EMOT4 + EMOT5
                    controller =~ CON1 + CON2 + CON3
                    exertional =~ PHY1 + PHY2 + PHY3 + PHY4
                    social =~SOC1 + SOC2 + SOC3 + SOC4 +SOC5 + SOC6
                    RELATEDNESS =~ Rel1 + Rel2 + Rel3

                    RELATEDNESS ~ cognitive + emotional + controller + exertional + social'

fit_RELATEDNESS <- sem(SEM_RELATEDNESS, data = VGDS_Germany, estimator = "MLR")
summary(fit_RELATEDNESS, standardized = TRUE, fit.measure=TRUE, rsquare = TRUE)

#sink()

#sink("VGDS-G_MeasurementInvariance_1.txt", append = FALSE, type = c("output", "message"), split = FALSE) 

# measurement invariance tests
VGDS_both <- read.spss("VGDS & VGDS-G.sav", use.value.labels = FALSE, to.data.frame = TRUE)

overallfit <- cfa(CFA_model_2, 
                  data=VGDS_both)
summary(overallfit, fit.measure=TRUE)

describeBy(VGDS_both, group = VGDS_both$GROUP)

COG_difference <- t.test(VGDS_both$COG ~ VGDS_both$GROUP)
EMO_difference <- t.test(VGDS_both$EMO ~ VGDS_both$GROUP)
CON_difference <- t.test(VGDS_both$CON ~ VGDS_both$GROUP)
PHY_difference <- t.test(VGDS_both$PHY ~ VGDS_both$GROUP)
SOC_difference <- t.test(VGDS_both$SOC ~ VGDS_both$GROUP)

COG_difference
EMO_difference
CON_difference
PHY_difference
SOC_difference

#configural invariance
configural_model0 <- cfa(CFA_model_2, 
                         data = VGDS_both, 
                         group = "GROUP", estimator = "MLR")
summary(configural_model0, fit.measure=TRUE)



#weak (metric) invariance (equal loadings, varying intercepts, varying residuals)
weak_model1a <- cfa(CFA_model_2, 
                    data = VGDS_both, 
                    group = "GROUP", 
                    group.equal = "loadings", estimator = "MLR")
summary(weak_model1a, fit.measure=TRUE)

anova(weak_model1a, configural_model0)

fit.stats <- rbind(
  fitmeasures(configural_model0, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic", "bic")),
  fitmeasures(weak_model1a, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic", "bic")))
  rownames(fit.stats) <- c("configural invariance", "weak invariance")
fit.stats

lavTestScore(weak_model1a)
parTable(weak_model1a)

weak_model1b <- cfa(CFA_model_2, 
                    data = VGDS_both, 
                    group = "GROUP", 
                    group.equal = "loadings", 
                    group.partial = "cognitive=~COG7", estimator = "MLR")
summary(weak_model1b, fit.measure=TRUE)

lavTestScore(weak_model1b)
parTable(weak_model1b)

weak_model1c <- cfa(CFA_model_2, 
                    data = VGDS_both, 
                    group = "GROUP", 
                    group.equal = "loadings", 
                    group.partial = c("cognitive=~COG7", "exertional=~PHY2"), estimator = "MLR")
summary(weak_model1c, fit.measure=TRUE)

lavTestScore(weak_model1c)
parTable(weak_model1c)

anova(weak_model1c, configural_model0)

fit.stats <- rbind(fitmeasures(configural_model0, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic", "bic")),
                   fitmeasures(weak_model1a, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic", "bic")),
                   fitmeasures(weak_model1c, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic", "bic")))
rownames(fit.stats) <- c("configural invariance", "weak invariance", "weak invariance (partial)")
fit.stats

#sink()

#sink("VGDS-G_MeasurementInvariance_2.txt", append = FALSE, type = c("output", "message"), split = FALSE) 

#strong (scalar) invariance (equal loadings, equal intercepts, varying residuals)
strong_model2a <- cfa(CFA_model_2, 
                      data = VGDS_both, 
                      group = "GROUP", 
                      group.equal = c("loadings", "intercepts"), 
                      group.partial = c("cognitive=~COG7", "exertional=~PHY2"), estimator = "MLR")
summary(strong_model2a, fit.measure=TRUE)

anova(strong_model2a, weak_model1c)

lavTestScore(strong_model2a)

parTable(strong_model2a)

strong_model2b <- cfa(CFA_model_2, 
                      data = VGDS_both, 
                      group = "GROUP", 
                      group.equal = c("loadings", "intercepts"), 
                      group.partial = c("cognitive=~COG7", "exertional=~PHY2", "COG2 ~ 1", "COG7 ~ 1","EMO1 ~ 1","EMO2 ~ 1","EMO3 ~ 1","CON1 ~ 1","CON2 ~ 1","CON3 ~ 1","PHY1 ~ 1","PHY2 ~ 1","SOC1 ~ 1","SOC6 ~ 1"), estimator = "MLR")
summary(strong_model2b, fit.measure=TRUE)

lavTestScore(strong_model2b)

fit.stats <- rbind(
  fitmeasures(configural_model0, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic", "bic")),
  fitmeasures(weak_model1a, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic", "bic")),
  fitmeasures(weak_model1c, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic", "bic")), 
  fitmeasures(strong_model2a, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic", "bic")),
  fitmeasures(strong_model2b, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic", "bic")))
rownames(fit.stats) <- c("configural invariance", "weak invariance", "weak invariance (partial)","strong invariance", "strong invariance (partial)")
fit.stats

#sink()
#sink("VGDS-G_MeasurementInvariance_3.txt", append = FALSE, type = c("output", "message"), split = FALSE) 


#strict (full uniqueness) invariance (equal loadings, equal intercepts, equal residuals)
strict_model3a <- cfa(CFA_model_2, 
                     data = VGDS_both, 
                     group = "GROUP", 
                     group.equal = c("loadings", "intercepts", "residuals"),
                     group.partial = c("cognitive=~COG7", "exertional=~PHY2", "COG2 ~ 1", "COG7 ~ 1","EMO1 ~ 1","EMO2 ~ 1","EMO3 ~ 1","CON1 ~ 1","CON2 ~ 1","CON3 ~ 1","PHY1 ~ 1","PHY2 ~ 1","SOC1 ~ 1","SOC6 ~ 1"), estimator = "MLR")
summary(strict_model3a, fit.measure=TRUE)

anova(strict_model3a, strong_model2b)

lavTestScore(strict_model3a)

parTable(strict_model3a)

strict_model3b <- cfa(CFA_model_2, 
                      data = VGDS_both, 
                      group = "GROUP", 
                      group.equal = c("loadings", "intercepts", "residuals"),
                      group.partial = c("cognitive=~COG7", "exertional=~PHY2", "COG2 ~ 1", "COG7 ~ 1","EMO1 ~ 1","EMO2 ~ 1","EMO3 ~ 1","CON1 ~ 1","CON2 ~ 1","CON3 ~ 1","PHY1 ~ 1","PHY2 ~ 1","SOC1 ~ 1","SOC6 ~ 1","COG3~~COG3","COG6~~COG6","EMO2~~EMO2","EMO5~~EMO5","CON1~~CON1","CON2~~CON2","CON3~~CON3","PHY2~~PHY2","PHY3~~PHY3","SOC1~~SOC1","SOC2~~SOC2","SOC3~~SOC3","SOC4~~SOC4","SOC5~~SOC5","SOC6~~SOC6"))
summary(strict_model3b, fit.measure=TRUE)

anova(strict_model3b, strong_model2b)

lavTestScore(strict_model3b)

fit.stats <- rbind(
  fitmeasures(configural_model0, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic", "bic")),
  fitmeasures(weak_model1a, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic", "bic")),
  fitmeasures(weak_model1c, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic", "bic")),
  fitmeasures(strong_model2a, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic", "bic")),
  fitmeasures(strong_model2b, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic", "bic")),
  fitmeasures(strict_model3a, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic", "bic")),
  fitmeasures(strict_model3b, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic", "bic")))
rownames(fit.stats) <- c("configural", "weak invariance", "weak invariance (partial)", "strong invariance","strong invariance (partial)", "strict invariance", "strict invariance (partial)")
fit.stats

#sink()