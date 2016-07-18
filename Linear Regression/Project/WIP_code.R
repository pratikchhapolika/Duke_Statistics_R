#See http://www.r-bloggers.com/regression-on-categorical-variables/

library(ggplot2)
library(dplyr)
library(statsr)
library(knitr)
library(scales)
library(formattable)
library(car)
library(MASS)

tmpMLR <- lm(audience_score ~., data=movies_filtered)
summary(tmpMLR)

VARIABLE=c("",gsub("[-^0-9]", "", names(unlist(tmpMLR$xlevels))))
MODALITY=c("",as.character(unlist(tmpMLR$xlevels)))
names=data.frame(VARIABLE,MODALITY,NOMVAR=c("(Intercept)",paste(VARIABLE,MODALITY,sep="")[-1]))
regression=data.frame(NOMVAR=names(coefficients(tmpMLR)), COEF=as.numeric(coefficients(tmpMLR)))
merge(names,regression,all.x=TRUE)

merge(names,regression)

library(xtable)
htlmtable <- xtable(merge(names,regression))
print(htlmtable,type="html")

#See http://www.statmethods.net/stats/rdiagnostics.html
outlierTest(tmpMLR)

qqPlot(tmpMLR)

sresid <- studres(tmpMLR) 
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit) 

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(tmpMLR)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(tmpMLR)

# Evaluate Collinearity
vif(tmpMLR) # variance inflation factors 
sqrt(vif(tmpMLR)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot 
crPlots(tmpMLR)
# Ceres plots 
ceresPlots(tmpMLR)


#See this:  https://rpubs.com/FelipeRego/MultipleLinearRegressionInRFirstSteps
#See this https://rpubs.com/FelipeRego - really good stuff