#choose dataset
comp<- read.csv(file.choose())
View(comp)
summary(comp)
pairs(comp)
comp_1<- comp[,-c(1,7,8,9)]
View(comp_1)
cor(comp_1)
library(corpcor)
cor2pcor(cor(comp_1))

#linear model
model.comp <- lm(price~.,data=comp_1)
summary(model.comp)

# Multicollinearity check
model.comp_1<-lm(price~speed,data=comp_1)
summary(model.comp_1)

model.comp_2<-lm(price~hd,data=comp_1)
summary(model.comp_2)

model.comp_3<-lm(price~ram,data=comp_1)
summary(model.comp_3)

model.comp_4<-lm(price~screen,data=comp_1)
summary(model.comp_4)

model.comp_5<-lm(price~ads,data=comp_1)
summary(model.comp_5)

model.comp_6<-lm(price~trend,data=comp_1)
summary(model.comp_6)

#library(car)
library(carData)
vif(model.comp)

#Diagnostic plot
influence.measures(model.comp)

# plotting Influential measures 
influenceIndexPlot(model.comp) 
influencePlot(model.comp)

# Regression after deleting the 50th observation
model_1<-lm(price~.,data=comp_1[-c(1441,1701),])
summary(model_1)
influenceIndexPlot(model_1) 
plot(model_1)

model_2<-lm(price~.,data=comp_1[-c(994,1043,1441,1701,4074,4283),])
summary(model_2)
influenceIndexPlot(model_2) 
plot(model_2)

model_3<-lm(price~.,data=comp_1[-c(20,994,1043,1123,1441,1701,3784,4074,4283,4478),])
summary(model_3)
influenceIndexPlot(model_3) 
plot(model_3)

final_model<-lm(price~.,data=comp_1[-c(20,25,982,994,1043,1123,1441,1701,3784,4074,4283,4478,4686,5961),])
summary(final_model)
influenceIndexPlot(final_model)
plot(final_model)

hist(residuals(final_model))
