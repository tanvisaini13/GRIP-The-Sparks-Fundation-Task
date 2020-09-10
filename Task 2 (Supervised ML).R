library(ISLR)
library(ggplot2)
library(caret)
#Importing Data
score_data<-read.csv("student_scores.csv")
#Data Overview
summary(score_data)
head(score_data)

ggplot(score_data,aes(Hours,Scores))+
  geom_point(alpha=0.5, color="blue", size = 3)+
  ylim(10,100)+xlim(1,10)+ggtitle("Hours vs Scores")

#Splitting Data for Training and Testing
sample.size<-round(0.70*nrow(score_data),0)  
sample.size
set.seed(23)   
train_indice<- sample(seq_len(nrow(score_data)),size = sample.size) 
train<-score_data[train_indice,] 
test<-score_data[-train_indice,]  

#Training Data Overview
summary(train)
head(train)

#Fitting simple linear regression model
model<-lm(Scores~Hours,data=train)
summary(model)

#Regression Line
plot(Scores~Hours, data=score_data,col="lightblue",pch=19,cex=1.2,
     xlim=c(1,10),ylim=c(10,100), main="Scores vs Hours")
abline(model, col="green")

#Making predictions using test data
pred<-data.frame(round(predict(model,test),0))
colnames(pred)<-"Scores"
pred

#Predicted vs Actual test Scores plot
df<-data.frame(test$Hours,pred$Scores,test$Scores)
df
plot(test$Scores, type="l",col="red",xlim=c(1,8),ylim = c(10,100),
     xlab = "Hours",ylab = "Scores", main = "Predicted vs Observed Scores")
lines(pred,type="l",col="blue")
legend(1, 100, legend=c("Observed", "Predicted"),
       col=c("red", "blue"), lty=1:1, cex=0.8)
#Evaluating the model using RMSE
root_mean_se<-RMSE(pred = pred$Scores,obs = test$Scores)
root_mean_se

#Predicting value for given hours studied
pred.score<-round(predict(model,newdata = data.frame(Hours=c(9.5))),0)
sprintf("If a student studies for 9.25 hours in a day their predicted score will be %d",pred.score)


