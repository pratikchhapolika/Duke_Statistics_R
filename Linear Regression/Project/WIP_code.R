#See http://www.r-bloggers.com/regression-on-categorical-variables/

library(ggplot2)
library(dplyr)
library(statsr)
library(knitr)
library(scales)
library(formattable)
library(car)
library(MASS)
library(psych)

tmpMLR <- lm(audience_score ~., data=movies_filtered)
pairs.panels(movies_filtered)
plot(movieMLR, pch=16, which=1)
#Note how the residuals plot of this last model shows some important points still lying 
#far away from the middle area of the graph.

movies_filtered2 <- select(movies_filtered, genre:audience_score)
tmpMLR <- lm(audience_score ~., data=movies_filtered2)

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
#See http://www.r-bloggers.com/the-relative-importance-of-predictors-let-the-games-begin/


#Create a new varaible where best_anything = Yes if any best* = Yes
movies_new <- mutate(movies, best_any=ifelse(best_pic_nom=="yes", 1, 
               ifelse(best_pic_win=="yes", 1,
               ifelse(best_actor_win=="yes", 1,
               ifelse(best_actress_win=="yes", 1,
               ifelse(best_dir_win=="yes", 1,
               ifelse(top200_box=="yes", 1, 0)))))))

#Good to know
library(plyr)
count(movies_filtered, "genre")
