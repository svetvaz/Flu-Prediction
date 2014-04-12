setwd('/Users/arvind/machineLearning/project/data')
fluData <- read.table('compute_target.csv',sep=',',header=TRUE,fill = TRUE)
#mean <- 1.288 # precomputed using flags
fluData$WeightedILIPCT
length(fluData$WeightedILIPCT)
ILI<-0
j<-1
mean<-mean(fluData$WeightedILIPCT,na.rm=TRUE)
SD1<-sd(fluData$WeightedILIPCT,na.rm=TRUE)+mean
SD2<-sd(fluData$WeightedILIPCT,na.rm=TRUE)*2+mean
SD3<-sd(fluData$WeightedILIPCT,na.rm=TRUE)*3+mean
SD4<-sd(fluData$WeightedILIPCT,na.rm=TRUE)*4+mean
SD5<-sd(fluData$WeightedILIPCT,na.rm=TRUE)*5+mean
SD6<-sd(fluData$WeightedILIPCT,na.rm=TRUE)*6+mean
SD7<-sd(fluData$WeightedILIPCT,na.rm=TRUE)*7+mean
SD8<-sd(fluData$WeightedILIPCT,na.rm=TRUE)*8+mean
SD9<-sd(fluData$WeightedILIPCT,na.rm=TRUE)*9+mean
for(i in fluData$WEIGHTED){
  if(i<mean){
    ILI[j]<-1
  }
  else if(i<SD1){
 
    ILI[j]<-2
  }
  else if(i<SD2){
    
    ILI[j]<-3
  }
  else if(i<SD3){
    
    ILI[j]<-4
  }
  else if(i<SD4){
   
    ILI[j]<-5
  }
  else if (i<SD5){
    
    ILI[j]<-6
  }
  else if(i<SD6){
   
    ILI[j]<-7
  }
  else if(i<SD7){
    
    ILI[j]<-8
  }
  else if(i<SD7){
   
    ILI[j]<-9
  }
  else if(i<SD8){
   
    ILI[j]<-10
  }
  j<-j+1
}
m<-cbind(fluData,ILI)
write.table(m,file='testData1.csv',append=FALSE,sep=',',row.names=FALSE)
