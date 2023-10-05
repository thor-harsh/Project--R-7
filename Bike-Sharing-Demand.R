bike<-read.csv('bikeshare.csv')
head(bike)
ggplot(bike,aes(x=temp,y=count,color=temp)) + geom_point(alpha=0.4)
bike$datetime<-as.POSIXct(bike$datetime)
p<-ggplot(bike,aes(x=datetime,y=count,color=temp))+geom_point(aes(color=temp),alpha=0.5)
p+scale_color_continuous(low='cyan',high='red')+theme_bw()
library(corrplot)
library(ggplot2)
num.col<-bike[,c('temp','count')]
is.data.frame(cor(num.col))

cor_df<-as.data.frame(cor(num.col))
cor_df
colnames(bike)
ggplot(bike,aes(x=factor(season),y=count))+geom_boxplot(aes(color=factor(season)))


bike$hour<-sapply(bike$datetime,function(x) format(x,"%H"))
head(bike$hour)

head(bike,1)
library(dplyr)
ggplot(data=filter(bike,workingday==1),aes(x=hour,y=count,color=temp))+
  geom_point(position=position_jitter(w=1, h=0),alpha=0.5)+
  scale_color_gradientn(colors = c('dark blue','blue','light blue','light green','yellow','orange','red'))
  
ggplot(data=,aes(x=hour,y=count,color=temp))+
  geom_point(position = position_jitter(w=1,h=0))+
               scale_color_gradient(low='purple',high='orange')

colnames(wbike)
temp.model<-lm(formula=count~temp,data=bike)
summary(temp.model)  

#Calculating prediction manually using the formula
bike_rental<-9.1705*25+6.0462
bike_rental

#Calculating prediction using the model
temp.test<-data.frame(temp=c(25))
temp.test
bike_rental_final<-predict(temp.model,temp.test)
bike_rental_final


predict(model,temp=25)
bike$hour<-sapply(bike$hour,as.numeric)
str(bike)
model_final<-lm(count~. -casual - registered - datetime - atemp,data=bike)
summary(model_final)
