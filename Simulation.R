library('xlsx')
lib.unionls<-function(x,y){
  temp1<-c(x,y)
  temp2<-duplicated(temp1)
  output<-c()
  for(i in 1:length(temp2)){
    if(temp2[i]==F){
      output<-c(output,temp1[i])
    }
  }
  sort(output)
}
lib.coltonum<-function(x,ncol){
  output<-c()
  for(i in 1:nrow(x)){
    output<-c(output,as.numeric(x[i,ncol]))
  }
  output
}
lib.ordbycoln<-function(x,ncol){
  tempcn<-colnames(x)
  tempx<-as.data.frame(x)
  templs<-sort(lib.unionls(lib.coltonum(x,ncol),c()))
  tempr<-c()
  output<-c(rep(NA,times=ncol(tempx)))
  for(i in 1:length(templs)){
    tempr<-tempx[tempx[ncol]==templs[1],]
    output<-rbind(output,tempr)
    templs<-templs[-1]
  }
  output<-output[-1,]
  colnames(output)<-tempcn
  output
}

set.seed(876855)
inv<-200000
k<-0.2
lambda<-1/(-365*4)
c1<-0.1
c2<-0.2
c3<-0.1
c4<-0.3
c5<-0.2
c<-rep(0,times=100)
EAaddress<-c()
tokens<-floor(runif(100,min=0,max=100000))
j<-1
while(j<101){  
  aux<-"0x"
  i<-1
  while(i<11){
    aux<-paste(aux,as.hexmode(floor(runif(1,min=0,max=2^16))),sep="")
    i<-i+1
  }
  j<-j+1
  EAaddress<-c(EAaddress,aux)
}
tab1<-cbind(EAaddress,tokens)
tab2<-cbind(EAaddress,c,c,c,c,c)
for(j in 1:100){
  for(i in 2:6){
    tab2[j,i]<-sample(0:1,1)
  }
}
tab2<-as.data.frame(tab2)
projinv<-floor(runif(150,min=1000,max=80000))
#projretaux<-abs(rnorm(150,mean=10,sd=5))
#projretaux<-abs(rnorm(150,mean=0.5,sd=0.3))
projret<-floor(runif(150,min=1000,max=80000))
#projret<-floor(projinv*projretaux)
projtim<-floor(runif(150,min=1,max=365*6))
projaddress<-c()
i<-1
while(i<151){
  projaddress<-c(projaddress,EAaddress[sample(1:100,1)])
  i<-i+1
}
tab3<-as.data.frame(cbind(projaddress,projinv,projret,projtim))
tab1<-lib.ordbycoln(tab1,2)
tab1sel<-tab1[91:100,1:2]
PAR<-c()
for(i in 1:10){
  coeftok<-as.numeric(as.character(tab1sel[i,2]))
  auxinc<-tab2[tab2[1]==as.character(tab1sel[i,1]),]
  auxinc1<-as.numeric(as.character(auxinc[2][1,1]))
  auxinc2<-as.numeric(as.character(auxinc[3][1,1]))
  auxinc3<-as.numeric(as.character(auxinc[4][1,1]))
  auxinc4<-as.numeric(as.character(auxinc[5][1,1]))
  auxinc5<-as.numeric(as.character(auxinc[6][1,1]))
  coefinc<-1+c1*auxinc1+c2*auxinc2+c3*auxinc3+c4*auxinc4+c5*auxinc5
  auxhist<-tab3[tab3[1]==as.character(tab1sel[i,1]),]
  auxhistn<-nrow(auxhist)
  auxcoefhist<-0
  if(auxhistn>0){
    auxcoefhist<-0
    for(j in 1:auxhistn){
      auxcoefhist<-auxcoefhist+((as.numeric(as.character(auxhist[j,3]))/as.numeric(as.character(auxhist[j,2])))-1)*exp(lambda*as.numeric(as.character(auxhist[j,4])))
    }
  }
  coefhist<-1+k*auxcoefhist
  if(coefhist>0.5){
    coefend<-coeftok*coefinc*coefhist
  }else{
    coefend<-0
  }
  PAR<-c(PAR,coefend)
}
PARsum<-sum(PAR)
for(i in 1:10){
  PAR[i]<-PAR[i]/PARsum
}
tabPA<-cbind(tab1sel[1],PAR*inv)
colnames(tab1)<-c("Address","Tokens")
colnames(tab2)<-c("Address","Inc 1","Inc 2","Inc 3","Inc 4","Inc 5")
colnames(tab3)<-c("Address","Inversion","Retorno","Dias desde Cierre")
colnames(tabPA)<-c("Address","PA")

write.xlsx(tab1,"simulation.xlsx",sheetName = 'Tokens de EA', col.names = T,showNA = F,row.names = F)
write.xlsx(tab2,"simulation.xlsx",sheetName = "Incentivos de EA", col.names = T,showNA = F,row.names = F,append = T)
write.xlsx(tab3,"simulation.xlsx",sheetName = "Historial Projectos de EA", col.names = T,showNA = F,row.names = F,append = T)
write.xlsx(tabPA,"simulation.xlsx",sheetName = "PA asignados", col.names = T,showNA = F,row.names = F,append = T)




write.csv(tab1, file = "Tokens de EA.csv")
write.csv(tab2, file = "Incentivos de EA.csv")
write.csv(tab3, file = "Historial Projectos de EA.csv")
write.csv(tabPA, file = "PA asignados.csv")
