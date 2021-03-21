setInternet2(use=TRUE)
library(data.table)
library(quantmod)
library(PerformanceAnalytics)
library(lubridate)
library(zoo)

Data=read.csv(file.choose(),header=T) #Data_CL2 Nymex in 07Dec_Trend folder
colnames(Data)<-c("DATE","HIGH","LOW","OPEN","LAST","CLOSE","Vol")

init_Pf_value=1000000 #initial investment=1 million
Point_val=1


SMA50=matrix(NA,nrow=nrow(Data),ncol=1)
SMA100=matrix(NA,nrow=nrow(Data),ncol=1)
TR=matrix(NA,nrow=nrow(Data),ncol=1)
ATR=matrix(NA,nrow=nrow(Data),ncol=1)
NContracts=matrix(NA,nrow=nrow(Data),ncol=1)

Hclose=matrix(NA,nrow=nrow(Data),ncol=1)
Lclose=matrix(NA,nrow=nrow(Data),ncol=1)

Buy=matrix(NA,nrow=nrow(Data),ncol=5)
Sell=matrix(NA,nrow=nrow(Data),ncol=5)

for(i in 1:nrow(Data)){
  for(j in 2:ncol(Data)){
    if(is.na(Data[i,j]==1)){
      Data[i,j]=Data[i-1,j]
    }
  }
}
  
  
for(i in 2:nrow(Data)){
  TR[i,1]=max(Data[i,2],Data[i-1,6])-min(Data[i,3],Data[i-1,6])
}

for(i in 50:nrow(Data)){
  SMA50[i,1]=sum(na.omit(Data[(i-49):i,6]))/50 
  Hclose[i,1]=max(na.omit(Data[(i-49):i,6]))
  Lclose[i,1]=min(na.omit(Data[(i-49):i,6]))
}

for(i in 100:nrow(Data)){
  SMA100[i,1]=sum(na.omit(Data[(i-99):i,6]))/100
}

for(i in 101:nrow(Data)){
  ATR[i,1]=sum(na.omit(TR[(i-99):i,1]))/100
  NContracts[i,1]=round((0.002*init_Pf_value)/(ATR[i,1]*Point_val),digits=0)
}


Data1=cbind(Data,SMA50,SMA100,TR,ATR,NContracts,Hclose,Lclose)

CL2<-xts(Data1[,2:14],order.by=as.Date(Data[,1],format="%d-%m-%Y"))

plot(CL2[,5],main="CL2 NYMEX Historical prices")
par(new=T)
plot(CL2[,7],col="red") #SMA50 - Fast
par(new=T)
plot(CL2[,8],col="green") #SMA100 - Slow


##################
##Core_Strategy###
##################

for(i in 101:length(CL2)){ 
  if(CL2[i,7]>CL2[i,8]){  #long posn loop
    if(CL2[i,5]==CL2[i,12]){
      Buy[i,1]=1 
    }
  }
  if(CL2[i,7]<CL2[i,8]){
    if(CL2[i,5]==CL2[i,13]){ #Short posn loop
      Sell[i,1]=1 
    }
  }
}

#Long position details
for(i in 101:nrow(CL2)){   
if(is.na(Buy[i,1])==0 & Buy[i,1]==1){
  Buy[i,2]=(-as.numeric(CL2[i,5])*as.numeric(CL2[i,11]))  #Money spent to buy
  Buy[i,3]=CL2[i,11]  #vol bought
    for(j in (i+1):nrow(CL2)){
      if((as.numeric(CL2[i,5])-as.numeric(CL2[j,5]))>(3*as.numeric(CL2[i,10]))){  #long is closed if there is 3 ATR drop
        Buy[j,1]=-1
        if(is.na(Buy[j,2])==1){Buy[j,2]=(as.numeric(CL2[j,5])*as.numeric(CL2[i,11]))
        } else{Buy[j,2]=Buy[j,2]+(as.numeric(CL2[j,5])*as.numeric(CL2[i,11]))}  #Money received by closing long posn
        if(is.na(Buy[j,3])==1){Buy[j,3]=-CL2[i,11]} else{Buy[j,3]=Buy[j,3]-CL2[i,11]}  #vol sold
        break
      }
    }
  } 
}

#Short position details
for(i in 101:nrow(CL2)){   
  if(is.na(Sell[i,1])==0 & Sell[i,1]==1){
    Sell[i,2]=as.numeric(CL2[i,5])*as.numeric(CL2[i,11])  #Money received by shorting
    Sell[i,3]=-CL2[i,11]  #vol sold
    for(j in (i+1):nrow(CL2)){
      if((as.numeric(CL2[j,5])-as.numeric(CL2[i,5]))>(3*as.numeric(CL2[i,10]))){  #short is closed if there is 3 ATR increase in price      
        Sell[j,1]=-1
        if(is.na(Sell[j,2])==1){Sell[j,2]=(-as.numeric(CL2[i,5])*as.numeric(CL2[i,11]))
        } else{Sell[j,2]=Sell[j,2]+(-as.numeric(CL2[i,5])*as.numeric(CL2[i,11]))}  #Money spent by closing short posn
        if(is.na(Sell[j,3])==1){Sell[j,3]=CL2[i,11]} else{Sell[j,3]=Sell[j,3]+CL2[i,11]}  #vol bought
        break
      }
    }
  } 
}

#Cumulative vol, P&L for Long posn

Buy_vol=0
PL_Buy=0
for(i in 101:nrow(CL2)){
  if(is.na(Buy[i,3])==0){
    Buy_vol=Buy_vol+Buy[i,3]
    Buy[i,4]=Buy_vol  #Cum buy volume
  }
  if(is.na(Buy[i,2])==0){
    PL_Buy=PL_Buy+Buy[i,2]
    Buy[i,5]=PL_Buy   #Cum P&L for buy
  }
}

#Cumulative vol, P&L for Short posn


Sell_vol=0
PL_Sell=0
for(i in 101:nrow(CL2)){
  if(is.na(Sell[i,3])==0){
    Sell_vol=Sell_vol+Sell[i,3]
    Sell[i,4]=Sell_vol  #Cum short volume
  }
  if(is.na(Sell[i,2])==0){
    PL_Sell=PL_Sell+Sell[i,2]
    Sell[i,5]=PL_Sell  #Cum P&L for short
  }
}

#Combined results
Tvol1=0
Tvol2=0
TPL1=0
TPL2=0

Strategy=matrix(NA,nrow=nrow(CL2),ncol=2)
for(i in 1:nrow(Strategy)){
  if(is.na(Buy[i,4])==0){
    Tvol1=Buy[i,4]
    Strategy[i,1]=Tvol1+Tvol2
    TPL1=Buy[i,5]
    Strategy[i,2]=TPL1+TPL2
    }
  if(is.na(Sell[i,4])==0){
    Tvol2=Sell[i,4]
    Strategy[i,1]=Tvol1+Tvol2
    TPL2=Sell[i,5]
    Strategy[i,2]=TPL1+TPL2
    }
}

Buy_f=cbind(as.character(Data[,1]),Buy)
Sell_f=cbind(as.character(Data[,1]),Sell)
Strategy_f=cbind(as.character(Data[,1]),Strategy)

CL2_Buy<-xts(Buy_f[,2:6],order.by=as.Date(Data[,1],format="%d-%m-%Y"))
CL2_Sell<-xts(Sell_f[,2:6],order.by=as.Date(Data[,1],format="%d-%m-%Y"))
CL2_strategy<-xts(Strategy_f[,2:3],order.by=as.Date(Data[,1],format="%d-%m-%Y"))

par(mfrow=c(3,1))
plot(CL2_Buy[,4],type="h",main="CL2_LongPosition",ylab="Cumulative Volume")
plot(CL2_Sell[,4],type="h",main="CL2_ShortPosition",ylab="Cumulative Volume")
plot(CL2_strategy[,1],type="h",main="CL2_Complete_Strategy",ylab="Cumulative Volume")

par(mfrow=c(3,1))
plot(CL2_Buy[,5],type="h",main="CL2_LongPosition",ylab="Cumulative P&L")
plot(CL2_Sell[,5],type="h",main="CL2_ShortPosition",ylab="Cumulative P&L")
plot(CL2_strategy[,2],type="h",main="CL2_Complete_Strategy",ylab="Cumulative P&L")

colnames(CL2_Buy)=c("1","2","3","Buy_Cum_Vol","Buy_Cum_PL")
colnames(CL2_Sell)=c("1","2","3","Sell_Cum_Vol","Sell_Cum_PL")
colnames(CL2_strategy)=c("CompleteStrategy_Cum_Vol","CompleteStrategy_Cum_PL")

Result=cbind(CL2,CL2_Buy[,4:5],CL2_Sell[,4:5],CL2_strategy[,1:2])
write.csv(Result,file = "Strategy_Result.csv")
write.csv(Data,file ="data.csv")