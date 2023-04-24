library("dplyr")
library("plotrix")

data<-read.csv("E://Education//Semester 4//Works//Theory//Fundamentals Of Statistics//Source//Project Data//SentryImpactData.csv",)
print(data)

#' Finding astroids which have most probability to impact earth.
sortedProbability<-data[order(data$Cumulative.Impact.Probability,decreasing = TRUE),]
head(sortedProbability,3)

#' Finding astroids which have most number of impacts with earth.
sortedImpacts<-data[order(data$Possible.Impacts,decreasing = TRUE),]
head(sortedImpacts,3)

#' Fiding largest asteroids to impact earth.
sortedDiameters<-data[order(data$Asteroid.Diameter..km.,decreasing = TRUE),]
head(sortedDiameters,3)

#' Finding near future asteroid imapcts.
nearFutureImpacts<-data[data$Period.Start>=as.numeric(format(Sys.Date(),"%Y")),]
head(nearFutureImpacts)

#' Finding correlation between the variables of dataset.
between(abs(cor(data$Possible.Impacts,data$Cumulative.Impact.Probability)),0,0.5)
between(abs(cor(data$Possible.Impacts,data$Asteroid.Velocity)),0,0.5)
between(abs(cor(data$Possible.Impacts,data$Asteroid.Diameter..km.)),0,0.5)

between(abs(cor(data$Cumulative.Impact.Probability,data$Asteroid.Velocity)),0,0.5)
between(abs(cor(data$Cumulative.Impact.Probability,data$Asteroid.Diameter..km.)),0,0.5)

between(abs(cor(data$Asteroid.Velocity,data$Asteroid.Diameter..km.)),0,0.5)


#' Linear regression with the dataset.
summary(lm(data$Possible.Impacts~data$Cumulative.Impact.Probability+data$Asteroid.Diameter..km.+data$Asteroid.Velocity))
summary(lm(data$Cumulative.Impact.Probability~data$Possible.Impacts+data$Asteroid.Diameter..km.+data$Asteroid.Velocity))
summary(lm(data$Asteroid.Diameter..km.~data$Cumulative.Impact.Probability+data$Possible.Impacts+data$Asteroid.Velocity))
summary(lm(data$Asteroid.Velocity~data$Cumulative.Impact.Probability+data$Possible.Impacts+data$Asteroid.Diameter..km.))

#' Prediction of Possible Impacts
x<-data$Possible.Impacts
y<-data$Asteroid.Velocity
linearModel<-lm(x~y)
summary(linearModel)
new_Asteroid.Velocity<-data.frame(y=as.numeric(readline("Enter Asteroid Velocity to Predict Number of impacts :")))
new_Possible.Impacts<-predict(linearModel,new_Asteroid.Velocity)
cat("Predicted number of asteroid impacts :",new_Possible.Impacts,"\n")

#' Prediction of Asteroid Diameter
x<-data$Asteroid.Diameter..km.
y<-data$Asteroid.Velocity
linearModel<-lm(x~y)
summary(linearModel)
new_Asteroid.Velocity<-data.frame(y=as.numeric(readline("Enter Asteroid Velocity to Predict Asteroid Diameter :")))
new_Asteroid.Diameter<-predict(linearModel,new_Asteroid.Velocity)
cat("Predicted asteroid diameter : ",new_Asteroid.Diameter,"\n")

#' Prediction of Asteroid velocity
x<-data$Asteroid.Velocity
y<-data$Possible.Impacts
z<-data$Asteroid.Diameter..km.
model<-lm(x~y+z)
summary(model)
new_Possible.Impacts_And_Asteroid.Diameter<-data.frame(y=as.numeric(readline("Enter Possible Impacts to predict Asteroid Velocity :")),z=as.numeric(readline("Enter Asteroid Diameter to predict Asteroid Velocity :")))
new_Asteroid.Velocity<-predict(model,new_Possible.Impacts_And_Asteroid.Diameter)
cat("Predicted asteroid velocity : ",new_Asteroid.Velocity,"\n")


#' Exploratory Data Analysis
sampleData<-sample_n(data,20)
print(sampleData)

#' Analysing the number of impacts of each asteroid
par(las=2) # To align x-label’s vertically
par(mar = c(6, 4, 2, 2)) # To give margins (bottom,left,top,right)
barplot(names.arg=sampleData$Object.Name,sampleData$Possible.Impacts,main="Possible impacts of each asteroid",ylab="Possible number of impacts", col=rainbow(length(sampleData)), border="light blue")

#' Analysing how number of possible impacts changes has year orders with line graph
plotData<-sampleData[order(sampleData$Period.Start),] # Sorting data with respective to year.
plot(plotData$Possible.Impacts,type ='o',col="blue",xlab="Incresing order of years", ylab="Number of possible impacts",main="Count of possible impacts")

#' Analysing how number of possible impacts changes for first 52 years and second 52 years with multi line graph :
firstGroup<-plotData[1:10,] # Dividing into first half
secondGroup<-plotData[(11:20),] # Dividing into second half
plot(firstGroup$Possible.Impacts,type='o',col='red',xlab='Incresing order of years',ylab='Number of possible impacts',main='Comparing number of possible impacts for first and second 52 years')
lines(secondGroup$Possible.Impacts,type = 'o',col='blue')

#' Analysing percentages of possible impacts for asteroids with pie graph:
x<-firstGroup$Possible.Impacts
labels<-firstGroup$Object.Name
piepercent<-round(100*x/sum(x),1)
pie(x,labels =piepercent,main = "Percentages of asteroid impacts",col=rainbow(length(firstGroup)))
legend("topright",labels,cex = 0.6,fill=rainbow(length(x)))




