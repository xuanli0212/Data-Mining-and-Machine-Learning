#Generate overall
data$overalltime<-0.15*as.numeric(rownames(data))
debora<-data[which(data$user=='debora'),]
jose<-data[which(data$user=='jose_carlos'),]
katia<-data[which(data$user=='katia'),]
wallace<-data[which(data$user=='wallace'),]


#time by person
debora$rownumber <- 1:nrow(debora)
jose$rownumber <- 1:nrow(jose)
katia$rownumber <- 1:nrow(katia)
wallace$rownumber <- 1:nrow(wallace)


debora$timebyPerson<-0.15*debora$rownumber
jose$timebyPerson<-0.15*jose$rownumber
katia$timebyPerson<-0.15*katia$rownumber
wallace$timebyPerson<-0.15*wallace$rownumber

#time by each classification
debora$timebyClass<-0.15
jose$timebyClass<-0.15
katia$timebyClass<-0.15
wallace$timebyClass<-0.15

  ###turn categorical to factor 
library(plyr)
debora$y <- mapvalues(debora$class, from = c("sitting", "sittingdown","standing","standingup","walking"), to = c("1", "2","3","4","5"))
wallace$y <- mapvalues(wallace$class, from = c("sitting", "sittingdown","standing","standingup","walking"), to = c("1", "2","3","4","5"))
jose$y <- mapvalues(jose$class, from = c("sitting", "sittingdown","standing","standingup","walking"), to = c("1", "2","3","4","5"))
katia$y <- mapvalues(katia$class, from = c("sitting", "sittingdown","standing","standingup","walking"), to = c("1", "2","3","4","5"))

#generate timebyClass
time1<-debora$timebyClass
time2<-jose$timebyClass
time3<-katia$timebyClass
time4<-wallace$timebyClass
for(i in 1:(length(time1)-1)){
  y<-debora$y
  if(y[i]==y[i+1]){
    time1[i+1]=time1[i]+0.15}
    else{
      time1[i+1]=0.15}
}
debora$timebyClass<-time1

for(i in 1:(nrow(jose)-1)){
  y<-jose$y
  if(y[i]==y[i+1]){
    time2[i+1]=time2[i]+0.15}
  else{
    time2[i+1]=0.15}
}
jose$timebyClass<-time2

for(i in 1:(length(time3)-1)){
  y<-katia$y
  if(y[i]==y[i+1]){
    time3[i+1]=time3[i]+0.15}
  else{
    time3[i+1]=0.15}
}
katia$timebyClass<-time3

for(i in 1:(length(time4)-1)){
  y<-wallace$y
  if(y[i]==y[i+1]){
    time4[i+1]=time4[i]+0.15}
  else{
    time4[i+1]=0.15}
}
wallace$timebyClass<-time4

#Different Moving Pattern with x/y/z research


sit<-debora[which(debora$y==1),]
sitdata<-sit[100:150,]
sitdata.matrix = sitdata[,8:19]
sitdata.matrix=as.matrix(sitdata.matrix)
class(sitdata.matrix)<-"numeric"
sitdata.scaled = apply(sitdata.matrix,2,function(x)(x-mean(x)))

sitdown<-debora[which(debora$y==2),]
sddata<-sitdown[100:200,]
sddata.matrix = as.matrix(sddata[,7:18])
class(sddata.matrix)<-"numeric"
sddata.scaled = apply(sddata.matrix,2,function(x)(x-mean(x)))

stand<-debora[which(debora$y==3),]
stdata<-stand[100:200,]
stdata.matrix = as.matrix(stdata[,7:18])
class(stdata.matrix)<-"numeric"
stdata.scaled = apply(stdata.matrix,2,function(x)(x-mean(x)))

standup<-debora[which(debora$y==4),]
standupdata<-standup[100:200,]
standupdata.matrix = as.matrix(standupdata[,7:18])
class(standupdata.matrix)<-"numeric"
standupdata.scaled = apply(standupdata.matrix,2,function(x)(x-mean(x)))

walking<-debora[which(debora$y==5),]
walkdata<-walking[100:150,]
walkdata.matrix = as.matrix(walkdata[8:19])
class(walkdata.matrix)<-"numeric"
walkdata.scaled = apply(walkdata.matrix,2,function(x)(x-mean(x)))

par(mfrow=c(1,2))
plot(0,0,main="Sitting Pattern",xlab="time",ylab="Acceleration",xlim=c(0,15),ylim=c(-10,10))
lines(sitdata$timebyClass,sitdata.scaled[,1],col="red",lty=1,lwd=2)
lines(sitdata$timebyClass,sitdata.scaled[,2],col="blue",lty=5,lwd=2)
lines(sitdata$timebyClass,sitdata.scaled[,3],col="green",lty=3,lwd=2)
legend(6,9,c("X","Y","Z"),lty=c(1,5,3),col=c("red","blue","green"),cex=0.5,horiz = TRUE)

plot(0,0,main="Sitting-Down Pattern",xlab="time",ylab="Acceleration",xlim=c(0,15),ylim=c(-45,70))
lines(sddata$timebyClass,sddata.scaled[,1],col="red",lty=1,lwd=2)
lines(sddata$timebyClass,sddata.scaled[,2],col="blue",lty=5,lwd=2)
lines(sddata$timebyClass,sddata.scaled[,3],col="green",lty=3,lwd=2)
legend(6,70,c("X","Y","Z"),lty=c(1,5,3),col=c("red","blue","green"),cex=0.5,horiz = TRUE)

par(mfrow=c(1,2))

plot(0,0,main="Standing Pattern",xlab="time",ylab="Acceleration",xlim=c(0,15),ylim=c(-10,15))
lines(sddata$timebyClass,stdata.scaled[,1],col="red",lty=1,lwd=2)
lines(sddata$timebyClass,stdata.scaled[,2],col="blue",lty=5,lwd=2)
lines(sddata$timebyClass,stdata.scaled[,3],col="green",lty=3,lwd=2)
legend(6,15,c("X","Y","Z"),lty=c(1,5,3),col=c("red","blue","green"),cex=0.5,horiz = TRUE)

plot(0,0,main="Standing-Up Pattern",xlab="time",ylab="Acceleration",xlim=c(0,15),ylim=c(-50,60))
lines(standupdata$timebyClass,standupdata.scaled[,1],col="red",lty=1,lwd=2)
lines(standupdata$timebyClass,standupdata.scaled[,2],col="blue",lty=5,lwd=2)
lines(standupdata$timebyClass,standupdata.scaled[,3],col="green",lty=3,lwd=2)
legend(6,60,c("X","Y","Z"),lty=c(1,5,3),col=c("red","blue","green"),cex=0.5,horiz = TRUE)

par(mfrow=c(1,2))

plot(0,0,main="Walking Pattern",xlab="time",ylab="Acceleration",xlim=c(0,15),ylim=c(-50,100))
lines(walkdata$timebyClass,walkdata.scaled[,1],col="red",lty=1,lwd=2)
lines(walkdata$timebyClass,walkdata.scaled[,2],col="blue",lty=5,lwd=2)
lines(walkdata$timebyClass,walkdata.scaled[,3],col="green",lty=3,lwd=2)
legend(6,100,c("X","Y","Z"),lty=c(1,5,3),col=c("red","blue","green"),cex=0.5,horiz = TRUE)




##Different sensor
sitdata<-sit[100:150,]
par(mfrow=c(1,3))
plot(sitdata$timebyclass,sitdata.scaled[,10],pch=20,cex=0.8,col="orange",main="Sitting X ",xlab="time",ylab="X Acceleration",xlim=c(15,23),ylim=c(-7,13))
lines(sitdata$timebyclass,sitdata.scaled[,1],col="red",lty=1,lwd=2)
lines(sitdata$timebyclass,sitdata.scaled[,4],col="blue",lty=5,lwd=2)
lines(sitdata$timebyclass,sitdata.scaled[,7],col="green",lty=3,lwd=2)
lines(sitdata$timebyclass,sitdata.scaled[,10],col="orange",lty=1,lwd=1.5)
legend(19,13,c("waist","left thigh","right arm","right ankle"),lty=c(1,5,3,1),col=c("red","blue","green","orange"),cex=1)

plot(sitdata$timebyclass,sitdata.scaled[,11],pch=20,cex=0.8,col="orange",main="Sitting Y",xlab="time",ylab="Y Acceleration",xlim=c(15,23),ylim=c(-7,13))
lines(sitdata$timebyclass,sitdata.scaled[,2],col="red",lty=1,lwd=2)
lines(sitdata$timebyclass,sitdata.scaled[,5],col="blue",lty=5,lwd=2)
lines(sitdata$timebyclass,sitdata.scaled[,8],col="green",lty=3,lwd=2)
lines(sitdata$timebyclass,sitdata.scaled[,11],col="orange",lty=1,lwd=1.5)
#legend(19,13,c("waist","left thigh","right arm","right ankle"),lty=c(1,5,3,1),col=c("red","blue","green","orange"),cex=0.5)

plot(sitdata$timebyclass,sitdata.scaled[,12],pch=20,cex=0.8,col="orange",main="Sitting Z ",xlab="time",ylab="Z Acceleration",xlim=c(15,23),ylim=c(-7,13))
lines(sitdata$timebyclass,sitdata.scaled[,3],col="red",lty=1,lwd=2)
lines(sitdata$timebyclass,sitdata.scaled[,6],col="blue",lty=5,lwd=2)
lines(sitdata$timebyclass,sitdata.scaled[,9],col="green",lty=3,lwd=2)
lines(sitdata$timebyclass,sitdata.scaled[,12],col="orange",lty=1,lwd=1.5)
#legend(19,13,c("waist","left thigh","right arm","right ankle"),lty=c(1,5,3,1),col=c("red","blue","green","orange"),cex=0.5)




plot(stdata$timebyClass,stdata.scaled[,10],pch=20,cex=0.8,col="orange",main="Sit-down with Different Sensors",xlab="time",ylab="Acceleration",xlim=c(15,30),ylim=c(-7,13))
lines(stdata$timebyClass,stdata.scaled[,1],col="red",lty=1,lwd=2)
lines(stdata$timebyClass,stdata.scaled[,4],col="blue",lty=5,lwd=2)
lines(stdata$timebyClass,stdata.scaled[,7],col="green",lty=3,lwd=2)
lines(stdata$timebyClass,stdata.scaled[,10],col="orange",lty=1,lwd=1.5)
legend(23,13,c("waist","left thigh","right arm","right ankle"),lty=c(1,5,3,1),col=c("red","blue","green","orange"),cex=0.5)

par(mfrow=c(1,2))
plot(sddata$timebyClass,sddata.scaled[,10],pch=20,cex=0.8,col="orange",main="Standing with Different Sensors",xlab="time",ylab="Acceleration",xlim=c(15,30),ylim=c(-50,70))
lines(sddata$timebyClass,sddata.scaled[,1],col="red",lty=1,lwd=2)
lines(sddata$timebyClass,sddata.scaled[,4],col="blue",lty=5,lwd=2)
lines(sddata$timebyClass,sddata.scaled[,7],col="green",lty=3,lwd=2)
lines(sddata$timebyClass,sddata.scaled[,10],col="orange",lty=1,lwd=1.5)
legend(23,70,c("waist","left thigh","right arm","right ankle"),lty=c(1,5,3,1),col=c("red","blue","green","orange"),cex=0.5)

plot(standupdata$timebyClass,standupdata.scaled[,10],pch=20,cex=0.8,col="orange",main="Stand-up with Different Sensors",xlab="time",ylab="Acceleration",xlim=c(15,30),ylim=c(-50,70))
lines(standupdata$timebyClass,standupdata.scaled[,1],col="red",lty=1,lwd=2)
lines(standupdata$timebyClass,standupdata.scaled[,4],col="blue",lty=5,lwd=2)
lines(standupdata$timebyClass,standupdata.scaled[,7],col="green",lty=3,lwd=2)
lines(standupdata$timebyClass,standupdata.scaled[,10],col="orange",lty=1,lwd=1.5)
legend(23,70,c("waist","left thigh","right arm","right ankle"),lty=c(1,5,3,1),col=c("red","blue","green","orange"),cex=0.5)


###
par(mfrow=c(1,3))
plot(walkdata$timebyclass,walkdata.scaled[,10],pch=20,cex=0.8,col="orange",main="Walking X",xlab="time",ylab="Acceleration",xlim=c(15,23),ylim=c(-100,150))
lines(walkdata$timebyclass,walkdata.scaled[,1],col="red",lty=1,lwd=2)
lines(walkdata$timebyclass,walkdata.scaled[,4],col="blue",lty=5,lwd=2)
lines(walkdata$timebyclass,walkdata.scaled[,7],col="green",lty=3,lwd=2)
lines(walkdata$timebyclass,walkdata.scaled[,10],col="orange",lty=1,lwd=1.5)
legend(19,150,c("waist","left thigh","right arm","right ankle"),lty=c(1,5,3,1),col=c("red","blue","green","orange"),cex=1)

plot(walkdata$timebyclass,walkdata.scaled[,10],pch=20,cex=0.8,col="orange",main="Walking Y",xlab="time",ylab="Acceleration",xlim=c(15,23),ylim=c(-100,150))
lines(walkdata$timebyclass,walkdata.scaled[,2],col="red",lty=1,lwd=2)
lines(walkdata$timebyclass,walkdata.scaled[,5],col="blue",lty=5,lwd=2)
lines(walkdata$timebyclass,walkdata.scaled[,8],col="green",lty=3,lwd=2)
lines(walkdata$timebyclass,walkdata.scaled[,11],col="orange",lty=1,lwd=1.5)
#legend(19,150,c("waist","left thigh","right arm","right ankle"),lty=c(1,5,3,1),col=c("red","blue","green","orange"),cex=1)

plot(walkdata$timebyclass,walkdata.scaled[,10],pch=20,cex=0.8,col="orange",main="Walking Z",xlab="time",ylab="Acceleration",xlim=c(15,23),ylim=c(-100,150))
lines(walkdata$timebyclass,walkdata.scaled[,3],col="red",lty=1,lwd=2)
lines(walkdata$timebyclass,walkdata.scaled[,6],col="blue",lty=5,lwd=2)
lines(walkdata$timebyclass,walkdata.scaled[,9],col="green",lty=3,lwd=2)
lines(walkdata$timebyclass,walkdata.scaled[,12],col="orange",lty=1,lwd=1.5)
#legend(19,150,c("waist","left thigh","right arm","right ankle"),lty=c(1,5,3,1),col=c("red","blue","green","orange"),cex=1)


