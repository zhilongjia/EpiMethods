# Notebook for EpiMethods

## Data wrangling

-   `cut`: `cut(mpg$cty, c(0, 14, 18, 40), right = TRUE)`

-   `na.omit`, `complete.cases()` 

- `prop.table` Displays percentage summary for a table.

-   `TableOne::CreateTableOne` strata = trans argument means that the summary is stratified by the trans variable.

-   `table1` The formula-like syntax `(\~ cyl + drv + hwy + cty \| trans)` indicates that the summary should be stratified by the trans variable.
-  `DataExplorer::plot_missing(analytic.data1)`

- `Hmisc::describe(analytic)`

- `var.cluster <- Hmisc::varclus(~., data = analytic[sel.names])`

- `psych::describeBy(analytic$cholesterol, analytic$gender)`

## Accessing data

### Data sources:

-   Canadian Community Health Survey (CCHS): 

    - vital health-related data, including health status, healthcare utilization, and health determinants.
    - 2001-2007 (sample size: 65k per year)- 2015 (updated sampling methods) - 2022 (updating content &  electronic questionnaire (EQ) )

-   National Health and Nutrition Examination Survey (NHANES) 
    - `nhanesA`, `RNHANES`
    - continuous NHANES (e.g., NHANES 1999-2000 to NHANES 2017-2018).

-   National Health Interview Survey (NHIS)
    - population disease prevalence, extent of disability, and use of health care services.
    - NHIS provides data annually.
    - each sampled person has a household number (HHX), family number (FMX), and a person number within family (FPX). 

-   European Social Survey (ESS) `essurvey`

-   Vanderbilt Biostatistics Datasets

-  the right heart catheterization (RHC) dataset 

- the mortality data in dat format from the CDC website

### important note

*  research questions can fall into two main categories: predictive or causal.\*


### Steps
- Download and Subsetting to retain only the useful variables
- Merging all the datasets
- Check Target population and avoid zero-cell cross-tabulation
- Recode values
- Check missingness
- Check data summaries
- Creating Table 1



### survey features

Strata - Cluster/primary sampling unit (PSU) - Weight



### Code Chunk

`analytic.miss$smoke <- relevel(as.factor(analytic.miss$smoke), ref='Never smoker')`

`analytic.miss$weight <- analytic.miss$weight/3 `


#### Subset the data (subset variables)
#### Making variable names the same
#### Variables, Covariates code
#### Dimension testing
#### Check the data for missingness
#### Look for zero-cells
#### Set appropriate reference
#### Complete data options
#### Weights
`analytic.miss$weight <- analytic.miss$weight/3`
#### Fixing variable types
`analytic.miss[var.names] <- lapply(analytic.miss[var.names] , factor)`
#### Setting Design
```
w.design0 <- svydesign(id=~1, weights=~weight, data=analytic.miss)
```
#### Bivariate analysis (Stratified by exposure & outcome)
```
require(tableone)
tab2 <- svyCreateTableOne(var = var.names, strata= "OA", data=w.design, test = TRUE)
tab3 <- svyCreateTableOne(var = var.names, strata= "CVD", data=w.design, test = TRUE)
```
#### Testing association (Rao-Scott and Thomas-Rao modifications Chi-square tests )
```
svychisq(~CVD+OA,design=w.design, statistic="Chisq")
svychisq(~CVD+OA,design=w.design, statistic="F") 
```

#### imputation process
```
DataExplorer::plot_missing(analyticx)
imputation <- parlmice(analyticx, m=5, maxit=5, cluster.seed=504007)
impdata$miss<-1
m <- 5
for (i in 1:m){
  impdata$miss[impdata$.imp == i] <- analytic.miss2$miss
  print(table(impdata$miss[impdata$.imp == i]))
}

library(mitools) 
m <- 5
set.seed(123)
allImputations <-  imputationList(lapply(1:m, 
                                         function(n)  
                                           subset(impdata, subset=.imp==n)))

require(jtools)
require(survey)
data.list <- vector("list", m)
model.formula <- as.formula("cholesterol~diabetes+gender+born+race+bmi")
scope <- list(upper = ~ diabetes+gender+born+race+bmi,
              lower = ~ diabetes)
for (i in 1:m) {
  analytic.i <- allImputations$imputations[[i]]
  w.design0.i <- svydesign(id=~psu, strata=~strata, weights=~weight,
                        data=analytic.i, nest = TRUE)
  w.design.i <- subset(w.design0.i, miss == 0)
  fit <- svyglm(model.formula, design=w.design.i)
  fitstep <- step(fit, scope = scope, trace = FALSE,
                              direction = "backward")
  data.list[[i]] <- fitstep
}
pooled.estimates <- MIcombine(fits2)
summary(pooled.estimates)
summary(pooled.estimates,logeffect=TRUE, digits = 2) # OR
```

#### Data check

skimr::skim(ObsData)


#### NHANES

```
nhanesTables(data_group='DEMO', year=2013)
nhanesTableVars(data_group='DEMO', nh_table='DEMO_H', 
                namesonly = FALSE)
```

##### factoring
```
anadata[factor.names] <- apply(X = anadata[factor.names],
                               MARGIN = 2, FUN = as.factor)

anadata[numeric.names] <- apply(X = anadata[numeric.names],
                                MARGIN = 2, FUN =function (x) 
                                  as.numeric(as.character(x)))
```



`svyCreateTableOne`

```
# Table 1
tab1 <- svyCreateTableOne(var = c("heart.attack", "sex"), strata= "diabetes", 
                          data = w.design0, test = FALSE)
print(tab1)

library(Publish)

# Design-adjusted logistic
fit1 <- svyglm(I(heart.attack == "Yes") ~ diabetes + sex, design = w.design0, family = binomial)
publish(fit1)
################################################################################


tab_CCHL <- tableone::CreateTableOne(data=cmh_sub_tidy_selected, vars=c("Community_belonging", "sex","Race", "age", "income"), strata=c("outcome"), includeNA = TRUE)
print(tab_CCHL)

fit1 <- glm(I(outcome == "good") ~ Community_belonging +age + sex + Race, data = cmh_sub1, family = binomial)
Publish::publish(fit1)
```

##### diagnostics
```

analytic2$lev <- hat(model.matrix(fit0))
plot(analytic2$lev)


collinearity <- olsrr::ols_vif_tol(fit3)
64 collinearity[collinearity$VIF>4,]
```

#### Weights
```
# Revised weight - weight divided by 6 cycles
dat.full$svy.weight <- dat.full$survey.weight/6
```

#### Comparing models
```
jtools::export_summs(fit23, fit23i)
jtools::plot_summs(fit23, fit23i)
```

#### Logistic for complex survey
```
formula1 <- as.formula(I(CVD=="event") ~ OA + age + sex + married + race + 
              edu + income + bmi + phyact + doctor + stress + 
              smoke + drink + fruit + bp + diab + province + immigrate)

# Formula <- formula(paste("HICP ~ ", paste(predictors, 2 collapse=" + ")))

fit3 <- svyglm(formula1, 
              design = w.design, 
              family = binomial(logit))
publish(fit3)

#
w.design0 <- svydesign(ids=~psu, 
                       weights=~weight, 
                       strata=~strata,
                       data = analytic.full.data, 
                       nest = TRUE)

# retain only those that have complete observation / no missing
w.design <- subset(w.design0, miss == 0)
fit3 <- svyglm(formula1,design = w.design,family = quasibinomial(logit))
AIC(fit3)
psrsq(fit3, method = "Cox-Snell")
psrsq(fit3, method = "Nagelkerke")
```

#### Backward Elimination
```
#  Wald test for bmi
round(sort(summary(fit3)$coef[,"Pr(>|t|)"]),2)
regTermTest(fit3,~bmi) 
fit4 <- update(fit3, .~. -bmi) 
anova(fit3, fit4)
AIC(fit3,fit4) 

scope <- list(upper = ~ gender + born + race + education + 
                married + income + bmi + diabetes,
              lower = ~ gender + race + bmi + diabetes)

fit3 <- step(fit1, scope = scope, trace = FALSE,
                k = 2, direction = "backward")

#Odds Ratios
publish(fit3)

```
#### Interaction
```
fit4 <- update(fit3, .~. + interaction(gender, married))
anova(fit3, fit4)
```

#### Performance

##### ROC
```
require(ROCR)
# WeightedROC may not be on cran for all R versions
# devtools::install_github("tdhock/WeightedROC")

library(WeightedROC)
svyROCw <- function(fit=fit,outcome=analytic2$CVD=="event", weight = NULL){
  # ROC curve for
  # Survey Data with Logistic Regression
  if (is.null(weight)){ # require(ROCR)
    prob <- predict(fit, type = "response")
  pred <- prediction(as.vector(prob), outcome)
  perf <- performance(pred, "tpr", "fpr")
  auc <- performance(pred, measure = "auc")
  auc <- auc@y.values[[1]]
  roc.data <- data.frame(fpr = unlist(perf@x.values), tpr = unlist(perf@y.values), 
      model = "Logistic")
  with(data = roc.data,plot(fpr, tpr, type="l", xlim=c(0,1), ylim=c(0,1), lwd=1,
     xlab="1 - specificity", ylab="Sensitivity",
     main = paste("AUC = ", round(auc,3))))
  mtext("Unweighted ROC")
  abline(0,1, lty=2)
  } else { # library(WeightedROC)
    outcome <- as.numeric(outcome)
  pred <- predict(fit, type = "response")
  tp.fp <- WeightedROC(pred, outcome, weight)
  auc <- WeightedAUC(tp.fp)
  with(data = tp.fp,plot(FPR, TPR, type="l", xlim=c(0,1), ylim=c(0,1), lwd=1,
     xlab="1 - specificity", ylab="Sensitivity",
     main = paste("AUC = ", round(auc,3))))
  abline(0,1, lty=2)
  mtext("Weighted ROC")
  }
}
svyROCw(fit=fit0,outcome=analytic2$CVD=="event", weight = analytic2$corrected.weight)

```
##### Archer and Lemeshow test (weight)
```
AL.gof <- function(fit=fit, data = analytic2, 
                   weight = "corrected.weight"){
  # Archer-Lemeshow Goodness of Fit Test for
  # Survey Data with Logistic Regression
  r <- residuals(fit, type="response") 
  f<-fitted(fit) 
  breaks.g <- c(-Inf, quantile(f,  (1:9)/10), Inf)
  breaks.g <- breaks.g + seq_along(breaks.g) * .Machine$double.eps
  g<- cut(f, breaks.g)
  data2g <- cbind(data,r,g)
  newdesign <- svydesign(id=~1, 
                         weights=as.formula(paste0("~",weight)), 
                        data=data2g)
  decilemodel<- svyglm(r~g, design=newdesign) 
  res <- regTermTest(decilemodel, ~g)
  return(res) 
}
AL.gof(fit0, analytic2, weight ="corrected.weight")
```

##### Archer and Lemeshow test (weight, psu, strata)
```
AL.gof2 <- function(fit=fit7, data = analytic, 
                   weight = "corrected.weight", psu = "psu", strata= "strata"){
  # Archer-Lemeshow Goodness of Fit Test for
  # Survey Data with Logistic Regression
  r <- residuals(fit, type="response") 
  f<-fitted(fit) 
  breaks.g <- c(-Inf, quantile(f,  (1:9)/10), Inf)
  breaks.g <- breaks.g + seq_along(breaks.g) * .Machine$double.eps
  g<- cut(f, breaks.g)
  data2g <- cbind(data,r,g)
  newdesign <- svydesign(id=as.formula(paste0("~",psu)),
                         strata=as.formula(paste0("~",strata)),
                         weights=as.formula(paste0("~",weight)), 
                        data=data2g, nest = TRUE)
  decilemodel<- svyglm(r~g, design=newdesign) 
  res <- regTermTest(decilemodel, ~g)
  return(res) 
}
```

#### Test of association when cholesterol is binary
```
#Rao-Scott modifications (chi-sq)
svychisq(~cholesterol.bin + gender, design = w.design, statistic = "Chisq")

#Thomas-Rao modifications (F)
svychisq(~cholesterol.bin + gender, design = w.design, statistic = "F") 
```

#### Vis

```
svyboxplot(blood.pressure~race, design = analytic.design, col="grey", 
           ylab="Blood Pressure", 
           xlab ="Race")
```


#### Variable selection

```
model.formula <- as.formula("I(cholesterol.bin=='healthy')~
                            diabetes+gender+born+race+bmi")

scope <- list(upper = ~ diabetes+gender+born+race+bmi, lower = ~ diabetes)

fit <- svyglm(model.formula, design=w.design, # subset design
              family=quasibinomial)

fitstep <- step(fit,  scope = scope, trace = FALSE, direction = "backward")
publish(fitstep) # final model

```

### Propensity score matching with MI
```
imputation <- mice(data = dat.with.miss, maxit = 0, print = FALSE)

pred <- imputation$pred

pred["strata",] <- 0

# Do not use survey weight or PSU variable as auxiliary variables
pred[,"studyid"] <- pred["studyid",] <- 0
pred[,"psu"] <- pred["psu",] <- 0
pred[,"survey.weight"] <- pred["survey.weight",] <- 0


imputation <- mice(data = dat.with.miss, 
                   seed = 123, 
                   predictorMatrix = pred,
                   method = meth, 
                   m = 3, 
                   maxit = 5, 
                   print = FALSE)
impdata <- mice::complete(imputation, action="long")

m <- 3
allImputations <- imputationList(lapply(1:m, 
                                        function(n) 
                                          subset(impdata, 
                                                 subset=.imp==n)))

match.statm <- SMDm <- tab1m <- vector("list", m) 
fit.from.PS <- vector("list", m)

for (i in 1:m) {
  analytic.i <- allImputations$imputations[[i]]
  # Rename the weight variable into survey.weight
  names(analytic.i)[names(analytic.i) == "weight"] <- "survey.weight"
  
  # Specify the PS model to estimate propensity scores
  ps.formula <- as.formula(I(born=="Others") ~ 
                             race + age + married + education + 
                             gender + bmi + systolicBP)

  # Propensity scores
  ps.fit <- glm(ps.formula, data = analytic.i, family = binomial("logit"))
  analytic.i$PS <- fitted(ps.fit)
  
  # Match exposed and unexposed subjects 
  set.seed(123)
  match.obj <- matchit(ps.formula, data = analytic.i, 
                       distance = analytic.i$PS, 
                       method = "nearest", 
                       replace = FALSE,
                       caliper = 0.2, 
                       ratio = 1)
  match.statm[[i]] <- match.obj
  analytic.i$PS <- match.obj$distance
  
  # Extract matched data
  matched.data <- match.data(match.obj) 
  
  # Balance checking
  cov <- c("race", "age", "married", "education", "gender", "bmi", "systolicBP")
  
  tab1m[[i]] <- CreateTableOne(strata = "born", 
                               vars = cov, data = matched.data, 
                               test = FALSE, smd = TRUE)
  SMDm[[i]] <- ExtractSmd(tab1m[[i]])
  
  # Setup the design with survey features
  analytic.i$matched <- 0
  analytic.i$matched[analytic.i$ID %in% matched.data$ID] <- 1
  
  # Survey setup for full data
  w.design0 <- svydesign(strata = ~strata, id = ~psu, weights = ~survey.weight, 
                         data = analytic.i, nest = TRUE)
  
  # Subset matched data
  w.design.m <- subset(w.design0, matched == 1)
  
  # Outcome model (double adjustment)
  out.formula <- as.formula(I(diabetes == "Yes") ~ 
                              born + race + age + married + 
                              education + gender + bmi + systolicBP)
  fit.from.PS[[i]] <- svyglm(out.formula, design = w.design.m, 
                     family = quasibinomial("logit"))
}    

pooled.estimates <- MIcombine(fit.from.PS)
summary(pooled.estimates, digits = 2, logeffect=TRUE)

OR <- round(exp(pooled.estimates$coefficients), 2) 
OR <- as.data.frame(OR)
CI <- round(exp(confint(pooled.estimates)), 2)
OR <- cbind(OR, CI)
OR[2,]
```

#### Cox for complex survey data
```
dat.full$svy.weight <- dat.full$survey.weight/6 
dat$svy.weight <- dat$survey.weight/6 

w.design0 <- svydesign(strata = ~strata, id = ~psu, weights = ~svy.weight, 
                       data = dat.full, nest = TRUE)
w.design1 <- subset(w.design0, miss == 0)

# Kaplan-Meier plot
fit0.f <- svykm(Surv(stime, status) ~ caff, design = w.design.f)
plot(fit0.f)

# Cox PH
fit2.f <- svycoxph(Surv(stime, status) ~ caff + age + race + education + smoking + 
                     alcohol +  carbohyd + physical.activity + bmi.cat + htn +
                     macrovascular + insulin + survey.cycle, design = w.design.f)
publish(fit2.f)


# proportional hazard assumption
cox.zph(fit2.f)

```

## Questions

- read CCHS into R
- what is the conclusion of chapter Effect modification by doning the different sex reference analysis? and what should be noted in this kind of analysis?

-  Use the strata variable as an auxiliary variable in the imputation model. Do not use survey weight or PSU variable as auxiliary variables.

- machinelearningQ  answers are incorrect.

## Links
- https://ehsanx.github.io/interaction/
- Regression Methods in Biostatistics Linear, Logistic, Survival, and Repeated Measures Models https://link.springer.com/book/10.1007/978-1-4614-1353-0
- Applied Survey Data Analysis https://doi.org/10.1201/9781315153278

- Flexible Imputation of Missing Data, Second Edition https://doi.org/10.1201/9780429492259