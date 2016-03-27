
sit= subset(project,project$class == "sitting")
sit.down = subset(project,project$class == "sittingdown")
stand = subset(project, project$class == "standing")
stand.up = subset(project, project$class =="standingup")
walk = subset(project, project$class =="walking")

sit$z4 = as.numeric(sit$z4)
sit.down$z4 = as.numeric(sit.down$z4)
stand$z4 = as.numeric(stand$z4)
stand.up$z4 = as.numeric(stand.up$z4)
walk$z4 = as.numeric(walk$z4)

# Average function 
sit.avg = 0
sit.avg = as.data.frame(sit.avg)
avg = 0
cal.ave = function(x){
  i = 1
  a = 0
  for(i in 1:floor((length(x))/20) ){
    avg[i] = mean(x[(a+1):(a+40)])
    a = a + 20}
  return(avg)   
}


sit.avg = sapply(sit[,8:19], FUN = cal.ave)
sit.down.avg = sapply(sit.down[,8:19], FUN = cal.ave)
stand.avg = sapply(stand[,8:19], FUN = cal.ave)
stand.up.avg = sapply(stand.up[,8:19], FUN = cal.ave)
walk.avg = sapply(walk[,8:19], FUN = cal.ave)


# Standard deviation function
sit.std = 0
sit.std = as.data.frame(sit.std)
std = 0
cal.std = function(x){
  i = 1
  a = 0
  for(i in 1:floor((length(x))/20) ){
    std[i] = sd(x[(a+1):(a+40)])
    a = a + 20}
  return(std)   
}


sit.std = sapply(sit[,8:19], FUN = cal.std)
sit.down.std = sapply(sit.down[,8:19], FUN = cal.std)
stand.std = sapply(stand[,8:19], FUN = cal.std)
stand.up.std = sapply(stand.up[,8:19], FUN = cal.std)
walk.std = sapply(walk[,8:19], FUN = cal.std)

# Range function

sit.range = 0
sit.range = as.data.frame(sit.range)
range = 0
cal.range = function(x){
  i = 1
  a = 0
  for(i in 1:floor((length(x))/20) ){
    range[i] = (max(x[(a+1):(a+40)]))-(min(x[(a+1):(a+40)]))
    a = a + 20}
  return(range)   
}


sit.range = sapply(sit[,8:19], FUN = cal.range)
sit.down.range = sapply(sit.down[,8:19], FUN = cal.range)
stand.range = sapply(stand[,8:19], FUN = cal.range)
stand.up.range = sapply(stand.up[,8:19], FUN = cal.range)
walk.range = sapply(walk[,8:19], FUN = cal.range)

#absolute 

# Average Absolute deviation function

sit.aad = 0
sit.aad = as.data.frame(sit.aad)
sit.down.aad = 0
sit.down.aad = as.data.frame(sit.down.aad)
stand.aad = 0
stand.aad = as.data.frame(stand.aad)
stand.up.aad = 0
stand.up.aad = as.data.frame(stand.up.aad)
walkad = 0
walk.aad = as.data.frame(walk.aad)

aad = 0
cal.aad = function(x){
  i = 1
  a = 0
  y = 0
  y = as.data.frame(y)
  for(i in 1:floor((length(x))/20)){
    dataset = x[(a+1):(a+40)]
    total = 0
    dif = 0
    y = dataset
    for(j in 1: 39){
      dif[j] = y[j+1]-y[j]
      
      j=j+1
    }
    total = sum(dif)
    aad[i] = total/40
    a = a + 20
  }
  return(aad)
}


sit.aad = sapply(sit[,8:19], FUN = cal.aad)
sit.down.aad = sapply(sit.down[,8:19], FUN = cal.aad)
stand.aad = sapply(stand[,8:19], FUN = cal.range)
stand.up.aad = sapply(stand.up[,8:19], FUN = cal.aad)
walk.aad = sapply(walk[,8:19], FUN = cal.aad)


# length


sit.avglength = 0
sit.down.avglength = 0
stand.avglength = 0
stand.up.avglength = 0
walk.avglength = 0

sit.avglength = as.data.frame(sit.avglength)
sit.down.avglength = as.data.frame(sit.down.avglength)
stand.avglength = as.data.frame(stand.avglength)
stand.up.avglength = as.data.frame(stand.up.avglength)
walk.avglength = as.data.frame(walk.avglength)

call.length<-function(x){
  length<-data.frame(matrix(0, nrow=nrow(x), ncol = 4))
  for(i in 2:nrow(x)){
      length$X1[i-1]<-sqrt((x[i,8]-x[(i-1),8])^2+(x[i,9]-x[(i-1),9])^2+(x[i,10]-x[(i-1),10])^2)
      length$X2[i-1]<-sqrt((x[i,11]-x[(i-1),11])^2+(x[i,12]-x[(i-1),12])^2+(x[i,13]-x[(i-1),13])^2)
      length$X3[i-1]<-sqrt((x[i,14]-x[(i-1),14])^2+(x[i,15]-x[(i-1),15])^2+(x[i,16]-x[(i-1),16])^2)
      length$X4[i-1]<-sqrt((x[i,17]-x[(i-1),17])^2+(x[i,18]-x[(i-1),18])^2+(x[i,19]-x[(i-1),19])^2)
  }
  return(length)
}


sit.length<-call.length(sit)
sit.down.length<-call.length(sit.down)
stand.length<-call.length(stand)
walk.length<-call.length(walk)
stand.up.length<-call.length(stand.up)

sit.avglength<-sapply(sit.length,FUN=cal.ave)
sit.down.avglength<-sapply(sit.down.length,cal.ave)
stand.avglength<-sapply(stand.length,cal.ave)
walk.avglength<-sapply(walk.length,cal.ave)
stand.up.avglength<-sapply(stand.up.length,cal.ave)
sit.avglength<-sapply(sit.length,FUN=cal.ave)
colnames(sit.avglength)= c('dist1','dist2','dist3','dist4')
sit.down.avglength<-sapply(sit.down.length,cal.ave)
colnames(sit.down.avglength)= c('dist1','dist2','dist3','dist4')
stand.avglength<-sapply(stand.length,cal.ave)
colnames(stand.avglength)= c('dist1','dist2','dist3','dist4')
walk.avglength<-sapply(walk.length,cal.ave)
colnames(walk.avglength)= c('dist1','dist2','dist3','dist4')
stand.up.avglength<-sapply(stand.up.length,cal.ave)
colnames(stand.up.avglength)= c('dist1','dist2','dist3','dist4')

#Peak 

cal.peak<-function(x)
for(i in 1:length(x)){
  if(x[(i-4)<x(i-)])
  
}



