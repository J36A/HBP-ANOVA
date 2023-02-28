## If the package is not already installed then use ##
install.packages('NHANES') ; install.packages('tidyverse') ; install.packages('fastDummies')
install.packages('corrplot')
install.packages('car')
install.packages('glmnet')
install.packages('rms')
install.packages('MASS')
install.packages("ggpubr")
library(ggpubr)
library(glmnet)
library(tidyverse)
library(NHANES)
library('fastDummies')
library(corrplot)
library(car)
library(rms)
library(MASS)
small.nhanes <- na.omit(NHANES[NHANES$SurveyYr=="2011_12"
                               & NHANES$Age > 17,c(1,3,4,8:11,13,17,20,21,25,46,50,51,52,61)])
small.nhanes <- as.data.frame(small.nhanes %>%
                                group_by(ID) %>% filter(row_number()==1) )
nrow(small.nhanes)
## Checking whether there are any ID that was repeated. If not ##
## then length(unique(small.nhanes$ID)) and nrow(small.nhanes) are same ##
length(unique(small.nhanes$ID))

## check na in data set ##
for (variable in colnames(small.nhanes)) {
  if (sum(is.na(small.nhanes$variable))) {
    print(variable)
  }
}

## box plot ##
pdf("boxplot.pdf", height = 8, width = 16)
bp1<-ggboxplot(y = "BPSysAve", x = "SmokeNow", data = small.nhanes, xlab = "Smoke or not",
        ylab = "blood pressure", main = "boxplot of blood pressure ~ smoke", x.text.angle = 90)
bp2<-ggboxplot(y = "BPSysAve", x = "Gender", data = small.nhanes, xlab = "Gender",
        ylab = "blood pressure", main = "boxplot of blood ~ pressure", x.text.angle = 90)
bp3<-ggboxplot(y = "BPSysAve", x = "Race3", data = small.nhanes, xlab = "Race",
        ylab = "blood pressure", main = "boxplot of blood pressure ~ race", x.text.angle = 90)
bp4<-ggboxplot(y = "BPSysAve", x = "Education", data = small.nhanes, xlab = "Education level",
        ylab = "blood pressure", main = "boxplot of blood pressure ~ education", x.text.angle = 90)
bp5<-ggboxplot(y = "BPSysAve", x = "MaritalStatus", data = small.nhanes, xlab = "Marital status",
        ylab = "blood pressure", main = "boxplot of blood pressure ~ Marital status", x.text.angle = 90)
bp6<-ggboxplot(y = "BPSysAve", x = "Depressed", data = small.nhanes, xlab = "depressed level",
        ylab = "blood pressure", main = "boxplot of blood pressure ~ depressed level", x.text.angle = 90)
bp<-ggarrange(bp1, bp2, bp3, bp4, bp5, bp6,
          ncol = 3, nrow = 2)
annotate_figure(bp,
                top = text_grob("Bpxplot of some catagorical predicators", color = "red", face = "bold", size = 14),
                fig.lab = "Figure 1", fig.lab.face = "bold"
)
dev.off()

## create dummy variables ##
small.nhanes <- dummy_cols(small.nhanes, select_columns = c('Gender', 'Race3', 'Education',
                                                            'MaritalStatus', 'HHIncome', 'Depressed',
                                                            'SleepTrouble', 'PhysActive', 'SmokeNow'),
                           remove_selected_columns = TRUE, remove_first_dummy = TRUE)

## Create training and test set ##
set.seed(1002656486)
train <- small.nhanes[sample(seq_len(nrow(small.nhanes)), size = 400),]
nrow(train)
length(which(small.nhanes$ID %in% train$ID))
test <- small.nhanes[!small.nhanes$ID %in% train$ID,]
nrow(test)

## distribution of BPSysAve ##
pdf("distribution.pdf", height = 8, width = 16)
x <- train$BPSysAve
hist(x, breaks=10, xlab="Combined systolic blood pressure reading",
        main="figure 3: distribution of BPSysAve")
dev.off()

## correlation ##

res <- cor(train[,-1])
res1 <- cor(train[,c("Weight", "Height", "BMI")])
res2 <- cor(train[,c("Poverty", "HHIncome_ 5000-9999", "HHIncome_10000-14999", 
                     "HHIncome_15000-19999", "HHIncome_25000-34999", 
                     "HHIncome_20000-24999", "HHIncome_35000-44999", 
                     "HHIncome_45000-54999", "HHIncome_55000-64999",       
                     "HHIncome_65000-74999", "HHIncome_75000-99999", 
                     "HHIncome_more 99999")])
res3 <- cor(train[,c("Race3_Black", "Race3_Hispanic", "Race3_Mexican",
                  "Race3_White", "Race3_Other")])
res4 <- cor(train[,c("Education_9 - 11th Grade", "Education_High School", 
                     "Education_Some College", "Education_College Grad", "Poverty")])
par(mfrow=c(2,2))
corrplot(res1, type = "upper", order = "hclust", tl.col = "black")
corrplot(res2, type = "upper", order = "hclust", tl.col = "black")
corrplot(res3, type = "upper", order = "hclust", tl.col = "black")
corrplot(res4, type = "upper", order = "hclust", tl.col = "black")
mtext("Figure 2", side = 2, line = -2, outer = TRUE)
mtext("correlation plot of some predictors", side = 3, line = -2, outer = TRUE)

## simple analysis of smokenow ##
train%>%
  group_by(SmokeNow_Yes)%>% 
  summarise(Mean=mean(BPSysAve), Max=max(BPSysAve), Min=min(BPSysAve),
            Median=median(BPSysAve), Std=sd(BPSysAve))
sum(train$SmokeNow_Yes)
## linear regression model with all variable ##
model.lm <- lm(BPSysAve ~ ., data = train[,-c(1)])

###########################
model <- lm(log(BPSysAve) ~ ., data = train[,-c(1)])
summary(model)

resid <- rstudent(model)
fitted <- predict(model)

pdf("myQ_Q10.pdf", height = 8, width = 16)
par(family = 'serif', mfrow = c(1,2))
qqnorm(resid)
qqline(resid)
plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")
#abline(lm(resid ~ fitted), lwd = 2, col = "blue")
dev.off()
###########################

vif(model.lm)
## According to vif and correlation table, weight, height, Race3_Black, 
## Education_College Grade, HHIncome_25000-34999, HHIncom_35000-44999, 
## HHIcom_75000-99999, HHIncome_more 99999, HHIncome_20000-24999, HHIcome_45000-54999,
## HHIncome_55000-64999 and HHIncome_65000-74999 won't be used in models.
model.vif <- lm(BPSysAve ~ Age + BMI + Depressed_Most + Depressed_Several + 
                  `Education_9 - 11th Grade` + 
                  `Education_High School` + `Education_Some College` + Gender_male +
                  `HHIncome_ 5000-9999` + `HHIncome_10000-14999` + `HHIncome_15000-19999` +
                  MaritalStatus_LivePartner + MaritalStatus_Married + MaritalStatus_NeverMarried +
                  MaritalStatus_Separated + MaritalStatus_Widowed + PhysActive_Yes +
                  Race3_White + SleepHrsNight + SleepTrouble_Yes + SmokeNow_Yes, data = train)
res.vif <- cor(train[,c('Age', 'BMI', 'Depressed_Most', 'Depressed_Several',
                        'Education_9 - 11th Grade', 'Education_High School',
                        'Education_Some College', 'Gender_male', 'HHIncome_ 5000-9999', 'HHIncome_10000-14999',
                        'HHIncome_15000-19999', 'MaritalStatus_LivePartner', 'MaritalStatus_Married', 'MaritalStatus_NeverMarried',
                        'MaritalStatus_Separated', 'MaritalStatus_Widowed', 'PhysActive_Yes', 'Race3_White',
                        'SleepHrsNight', 'SleepTrouble_Yes', 'SmokeNow_Yes')])
## perform prediction ##
pred.y <- predict(model.vif, newdata = test, type = "response")

##prediction error ##
mean((test$BPSysAve - pred.y)^2)

###########################
resid <- rstudent(model.vif)
fitted <- predict(model.vif)

pdf("vifQQplot.pdf", height = 8, width = 16)
par(family = 'serif', mfrow = c(1,2))
qqnorm(resid)
qqline(resid)
plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")
#abline(lm(resid ~ fitted), lwd = 2, col = "blue")
dev.off()
###########################

## variable selection - step wise regression ##
## AIC ##
n <- nrow(train)
sel.var.aic <- step(model.vif, trace = 0, k = 2, direction = "both") 
sel.var.aic<-attr(terms(sel.var.aic), "term.labels")   
sel.var.aic
## BIC ##
model.vif <- lm(BPSysAve ~ Age + BMI + Depressed_Most + Depressed_Several + 
                  `Education_9 - 11th Grade` + 
                  `Education_High School` + `Education_Some College` + Gender_male +
                  `HHIncome_ 5000-9999` + `HHIncome_10000-14999` + `HHIncome_15000-19999` +
                  MaritalStatus_LivePartner + MaritalStatus_Married + MaritalStatus_NeverMarried +
                  MaritalStatus_Separated + MaritalStatus_Widowed + PhysActive_Yes +
                  Race3_White + SleepHrsNight + SleepTrouble_Yes + SmokeNow_Yes, data = train)
n <- nrow(train)
sel.var.bic <- step(model.vif, trace = 0, k = log(n), direction = "both") 
sel.var.bic<-attr(terms(sel.var.bic), "term.labels")   
sel.var.bic
## lasso ##
## Perform cross validation to choose lambda ##
set.seed(1002656486)
cv.out <- cv.glmnet(x = as.matrix(train[,c('Age', 'BMI', 'Depressed_Most', 'Depressed_Several',
                                           'Education_9 - 11th Grade', 'Education_High School',
                                           'Education_Some College', 'Gender_male', 'HHIncome_ 5000-9999', 'HHIncome_10000-14999',
                                           'HHIncome_15000-19999', 'MaritalStatus_LivePartner', 'MaritalStatus_Married', 'MaritalStatus_NeverMarried',
                                           'MaritalStatus_Separated', 'MaritalStatus_Widowed', 'PhysActive_Yes', 'Race3_White',
                                           'SleepHrsNight', 'SleepTrouble_Yes', 'SmokeNow_Yes')]),
                    y = train$BPSysAve, standardize = T, alpha = 1)
plot(cv.out)
best.lambda <- cv.out$lambda.1se
best.lambda
co<-coef(cv.out, s = "lambda.1se")

thresh <- 0.00
# select variables #
inds<-which(abs(co) > thresh )
variables<-row.names(co)[inds]
sel.var.lasso<-variables[!(variables %in% '(Intercept)')]
sel.var.lasso

## fit a ridge penalty ##
model.ridge <- glmnet(x = as.matrix(train[,c('Age', 'BMI', 'Depressed_Most', 'Depressed_Several',
                                             'Education_9 - 11th Grade', 'Education_High School',
                                             'Education_Some College', 'Gender_male', 'HHIncome_ 5000-9999', 'HHIncome_10000-14999',
                                             'HHIncome_15000-19999', 'MaritalStatus_LivePartner', 'MaritalStatus_Married', 'MaritalStatus_NeverMarried',
                                             'MaritalStatus_Separated', 'MaritalStatus_Widowed', 'PhysActive_Yes', 'Race3_White',
                                             'SleepHrsNight', 'SleepTrouble_Yes', 'SmokeNow_Yes')]),
                                    y = train$BPSysAve, standardize = T, alpha = 0)
predict(model.ridge, type="coefficients", s=best.lambda)
## perform prediction ##
pred.y.ridge <- predict(model.ridge, newx = as.matrix(test[,c('Age', 'BMI', 'Depressed_Most', 'Depressed_Several',
                                                              'Education_9 - 11th Grade', 'Education_High School',
                                                              'Education_Some College', 'Gender_male', 'HHIncome_ 5000-9999', 'HHIncome_10000-14999',
                                                              'HHIncome_15000-19999', 'MaritalStatus_LivePartner', 'MaritalStatus_Married', 'MaritalStatus_NeverMarried',
                                                              'MaritalStatus_Separated', 'MaritalStatus_Widowed', 'PhysActive_Yes', 'Race3_White',
                                                              'SleepHrsNight', 'SleepTrouble_Yes', 'SmokeNow_Yes')]), type = "response")

## prediction error ##
mean((test$BPSysAve - pred.y.ridge)^2)

## lasso penalty ##
model.lasso <- glmnet(x = as.matrix(train[,c('Age', 'BMI', 'Depressed_Most', 'Depressed_Several',
                                             'Education_9 - 11th Grade', 'Education_High School',
                                             'Education_Some College', 'Gender_male', 'HHIncome_ 5000-9999', 'HHIncome_10000-14999',
                                             'HHIncome_15000-19999', 'MaritalStatus_LivePartner', 'MaritalStatus_Married', 'MaritalStatus_NeverMarried',
                                             'MaritalStatus_Separated', 'MaritalStatus_Widowed', 'PhysActive_Yes', 'Race3_White',
                                             'SleepHrsNight', 'SleepTrouble_Yes', 'SmokeNow_Yes')]),
                      y = train$BPSysAve, standardize = T, alpha = 1)
predict(model.lasso, type="coefficients", s=best.lambda)
## perform prediction ##
pred.y.lasso <- predict(model.lasso, newx = as.matrix(test[,c('Age', 'BMI', 'Depressed_Most', 'Depressed_Several',
                                                              'Education_9 - 11th Grade', 'Education_High School',
                                                              'Education_Some College', 'Gender_male', 'HHIncome_ 5000-9999', 'HHIncome_10000-14999',
                                                              'HHIncome_15000-19999', 'MaritalStatus_LivePartner', 'MaritalStatus_Married', 'MaritalStatus_NeverMarried',
                                                              'MaritalStatus_Separated', 'MaritalStatus_Widowed', 'PhysActive_Yes', 'Race3_White',
                                                              'SleepHrsNight', 'SleepTrouble_Yes', 'SmokeNow_Yes')]), type = "response")
## prediction error ##
mean((test$BPSysAve - pred.y.lasso)^2)

### Cross Validation and prediction performance of AIC based selection ###
ols.aic <- ols(BPSysAve ~ ., data = train[,which(colnames(train) %in% c(sel.var.aic, "BPSysAve"))], 
               x=T, y=T, model = T)

## 10 fold cross validation ##    
aic.cross <- calibrate(ols.aic, method = "crossvalidation", B = 10)
## Calibration plot ##
pdf("aic_cross.pdf", height = 8, width = 16)
plot(aic.cross, las = 1, xlab = "Predicted BPSysAve", main = "Cross-Validation calibration with AIC")
dev.off()

cor(train[,c("Age", "BMI", "SleepTrouble_Yes", "Gender_male", "MaritalStatus_NeverMarried", "PhysActive_Yes")])

###########################
model <- lm(BPSysAve ~ ., data = train[,which(colnames(train) %in% c(sel.var.aic, "BPSysAve"))])
summary(model)
resid <- rstudent(model)
fitted <- predict(model)

pdf("aicQQplot.pdf", height = 8, width = 16)
par(family = 'serif', mfrow = c(1,2))
qqnorm(resid)
qqline(resid)
plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")
#abline(lm(resid ~ fitted), lwd = 2, col = "blue")
dev.off()
###########################

## Test Error ##
pred.aic <- predict(ols.aic, newdata = test[,which(colnames(train) %in% c(sel.var.aic, "BPSysAve"))])
## Prediction error ##
pred.error.AIC <- mean((test$BPSysAve - pred.aic)^2)

## Power Transformation ##
bc <- powerTransform(ols.aic)
summary(bc)
coef(bc, round=TRUE)
bc.aic <- lm(bcPower(BPSysAve, bc$roundlam) ~ ., data = train[,which(colnames(train) %in% c(sel.var.aic, "BPSysAve"))])
summary(bc.aic)

###########################
resid <- rstudent(bc.aic)
fitted <- predict(bc.aic)

pdf("bcaicQQplot.pdf", height = 8, width = 16)
par(family = 'serif', mfrow = c(1,2))
qqnorm(resid)
qqline(resid)
plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")
#abline(lm(resid ~ fitted), lwd = 2, col = "blue")
dev.off()
###########################

## Test Error ##
pred.bc.aic <- predict(bc.aic, newdata = test[,which(colnames(train) %in% c(sel.var.aic, "BPSysAve"))])
## Prediction error ##
pred.error.bc.AIC <- mean((test$BPSysAve - pred.bc.aic)^2)


### Cross Validation and prediction performance of BIC based selection ###
ols.bic <- ols(BPSysAve ~ ., data = train[,which(colnames(train) %in% c(sel.var.bic, "BPSysAve"))], 
               x=T, y=T, model = T)

## 10 fold cross validation ##    
bic.cross <- calibrate(ols.bic, method = "crossvalidation", B = 10)
## Calibration plot ##
pdf("bic_cross.pdf", height = 8, width = 16)
plot(bic.cross, las = 1, xlab = "Predicted LPSA", main = "Cross-Validation calibration with BIC")
dev.off()

cor(train[,c("Age", "BMI", "SleepTrouble_Yes")])

## Test Error ##
pred.bic <- predict(ols.bic, newdata = test[,which(colnames(train) %in% c(sel.var.bic, "BPSysAve"))])
## Prediction error ##
pred.error.BIC <- mean((test$BPSysAve - pred.bic)^2)

###########################
model <- lm(BPSysAve ~ ., data = train[,which(colnames(train) %in% c(sel.var.bic, "BPSysAve"))])
summary(model)
resid <- rstudent(model)
fitted <- predict(model)

pdf("bicQQplot.pdf", height = 8, width = 16)
par(family = 'serif', mfrow = c(1,2))
qqnorm(resid)
qqline(resid)
plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")
#abline(lm(resid ~ fitted), lwd = 2, col = "blue")
dev.off()
###########################

## Power Transformation ##
bc <- powerTransform(ols.bic)
summary(bc)
coef(bc, round=TRUE)
bc.bic <- lm(bcPower(BPSysAve, bc$roundlam) ~ ., data = train[,which(colnames(train) %in% c(sel.var.aic, "BPSysAve"))])
summary(bc.bic)

###########################
resid <- rstudent(bc.bic)
fitted <- predict(bc.bic)

pdf("bcbicQQplot.pdf", height = 8, width = 16)
par(family = 'serif', mfrow = c(1,2))
qqnorm(resid)
qqline(resid)
plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")
#abline(lm(resid ~ fitted), lwd = 2, col = "blue")
dev.off()
###########################

## Test Error ##
pred.bc.bic <- predict(bc.bic, newdata = test[,which(colnames(train) %in% c(sel.var.aic, "BPSysAve"))])
## Prediction error ##
pred.error.bc.BIC <- mean((test$BPSysAve - pred.bc.bic)^2)

### Cross Validation and prediction performance of lasso based selection ###
ols.lasso <- ols(BPSysAve ~ ., data = train[,which(colnames(train) %in% c(sel.var.lasso, "BPSysAve"))], 
                 x=T, y=T, model = T)

## 10 fold cross validation ##    
lasso.cross <- calibrate(ols.lasso, method = "crossvalidation", B = 10)
## Calibration plot ##
pdf("lasso_cross.pdf", height = 8, width = 16)
plot(lasso.cross, las = 1, xlab = "Predicted LPSA", main = "Cross-Validation calibration with LASSO")
dev.off()

## Test Error ##
pred.lasso <- predict(ols.lasso, newdata = test[,which(colnames(train) %in% c(sel.var.lasso, "BPSysAve"))])
## Prediction error ##
pred.error.lasso <- mean((test$BPSysAve - pred.lasso)^2)

###########################
model <- lm(BPSysAve ~ ., data = train[,which(colnames(train) %in% c(sel.var.lasso, "BPSysAve"))])
summary(model)
resid <- rstudent(model)
fitted <- predict(model)

pdf("lassoQQplot.pdf", height = 8, width = 16)
par(family = 'serif', mfrow = c(1,2))
qqnorm(resid)
qqline(resid)
plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")
#abline(lm(resid ~ fitted), lwd = 2, col = "blue")
dev.off()
###########################

print(c(pred.error.AIC, pred.error.BIC, pred.error.lasso))

