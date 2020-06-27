#choose dataset
startup <- read.csv(file.choose())
View(startup)
summary(startup)
pairs(startup)
startup_1<- startup[,-4]
View(startup_1)
cor(startup_1)
library(corpcor)
cor2pcor(cor(startup_1))

#linear model
model.startup <- lm(Profit~.,data=startup_1)
summary(model.startup)

# Multicollinearity check
model.startup_1<-lm(Profit~R.D.Spend,data=startup_1)
summary(model.startup_1)

model.startup_2<-lm(Profit~Administration,data=startup_1)
summary(model.startup_2)

model.startup_3<-lm(Profit~Marketing.Spend,data=startup_1)
summary(model.startup_3)

library(car)
vif(model.startup)

#Diagnostic plot
influence.measures(model.startup)

# plotting Influential measures 
influenceIndexPlot(model.startup) 
influencePlot(model.startup)

# Regression after deleting the 50th observation
model_1<-lm(Profit~.,data=startup_1[-50,])
summary(model_1)
influenceIndexPlot(model_1) 
plot(model_1)

model_2<-lm(Profit~.,data=startup_1[-c(47,49,50),])
summary(model_2)
influenceIndexPlot(model_2) 
plot(model_2)

model_3<-lm(Profit~.,data=startup_1[-c(15,46,47,49,50),])
summary(model_3)
influenceIndexPlot(model_3) 
plot(model_3)

final_model<- lm(Profit~.,data=startup_1[-c(7,15,16,28,37,39,46,47,49,50),])
summary(final_model)
influenceIndexPlot(final_model)
plot(final_model)

hist(residuals(final_model))
