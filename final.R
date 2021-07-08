library(ggplot2)
library(dplyr)
library(lmtest)
library(tidyr)
final <- final %>%
  mutate(ali1= lag(final$li1),
         bli2= lag(final$li2))
final["ali1"]<- c(lag(final$li1))
final["bli2"]<- c(lag(final$li2))
final <- final %>%
  mutate(cli1= lag(final$li1,2),
         dli2= lag(final$li2,2))
final["cli1"]<- c(lag(final$li1,2))
final["dli2"]<- c(lag(final$li2,2))
final<- final%>%
  separate(Date, into = c("year", "quarter"), sep=4)
finala<- subset(final, year>1950)
finalb<- subset(finala, year<2011)
finalc<- subset(final, year>=2011)
finalb$GDPIMPR<- as.factor(finalb$GDPIMPR)
finalb$year<- as.numeric(finalb$year)

#a to perform likelihood test for the all the four models
modela<- glm(data=finalb, finalb$GDPIMPR~1, family = "binomial")
summary(modela)
modelb<- glm(data=finalb, finalb$GDPIMPR~ finalb$ali1, family= "binomial")
summary(modelb)
modelc <- glm(data=finalb, finalb$GDPIMPR~ finalb$bli2, family= "binomial")
summary(modelc)
modeld <- glm(data= finalb, finalb$GDPIMPR~ finalb$ali1 + finalb$bli2, 
              family="binomial")
summary(modeld)
lmtest::lrtest(modela, modeld)
lmtest::lrtest(modelb, modeld)
lmtest::lrtest(modelc, modeld)

#b TO find the Mcfadden Rsq of for logit models.
model2.2 <- glm(data= finalb, finalb$GDPIMPR ~ finalb$ali1 + finalb$dli2, family="binomial")
summary(model2.2)
model2.3 <- glm(data=finalb , finalb$GDPIMPR ~ finalb$bli2 + finalb$cli1, family="binomial")
summary(model2.3)
model2.4 <- glm(data= finalb, finalb$GDPIMPR ~ finalb$cli1 + finalb$dli2, family="binomial")
summary(model2.4)
mcfadden2<- 1- logLik(modeld)/logLik(modela)
mcfadden2
mcfadden2<- 1- logLik(model2.2)/logLik(modela)
mcfadden2
mcfadden2<- 1- logLik(model2.3)/logLik(modela)
mcfadden2
mcfadden2<- 1- logLik(model2.4)/logLik(modela)
mcfadden2

#c prediction-realization table
finalc<- subset(final, year>=2011)
finalc$GDPIMPR<- as.factor(finalc$GDPIMPR)
model2.3a <- glm(data=finalc , finalc$GDPIMPR ~ finalc$bli2 + finalc$cli1, 
                 family="binomial")
summary(model2.3a)
pp <- predict(model2.3a, newdata = finalc)
matrix<- table(actual = finalc$GDPIMPR, predicted= pp >0.5)
matrix
hit_rate<- (matrix[[1,1]] + matrix[[2,2]])/sum(matrix)
hit_rate

#d
finalb$GrowthRate<-as.numeric(finalb$GrowthRate)
finalb <- finalb %>%
  mutate(LOGGDP_lag1 = lag(LOGGDP),
         GrowthRate_lag = lag(GrowthRate))
model4 <- lm(data = finalb, GrowthRate ~ T + LOGGDP_lag1 + GrowthRate_lag )
summary(model4)

#e
finalb <- finalb %>%
  mutate(GrowthRate_lag= lag(GrowthRate))
finalb$GrowthRate_lag<-as.numeric(finalb$GrowthRate_lag)
model5 <- lm(data = finalb, GrowthRate ~ GrowthRate_lag + ali1 + bli2)
summary(model5)
model5a <- lm(data = finalb, GrowthRate ~ GrowthRate_lag + ali1 + dli2)
summary(model5a)
model5b <- lm(data = finalb, GrowthRate ~ GrowthRate_lag +cli1+ bli2)
summary(model5b)
model5c <- lm(data = finalb, GrowthRate ~ GrowthRate_lag +cli1 + dli2)
summary(model5c)

#f
finalb$GrowthRate<-as.numeric(finalb$GrowthRate)
bgtest(formula =GrowthRate ~ GrowthRate_lag + ali1 + bli2 , order=1, data =finalb)

#g
finalc <- finalc %>%
  mutate(GrowthRate_lag= lag(GrowthRate))
finalc$GrowthRate_lag<-as.numeric(finalc$GrowthRate_lag)
finalc$GrowthRate<-as.numeric(finalc$GrowthRate)
prediction <- predict(model5, newdata = finalc)
finalc$prediction<-as.numeric(finalc$prediction)
rmse <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}
rmse <- rmse(actual = finalc$GrowthRate, predicted = prediction)
rmse





