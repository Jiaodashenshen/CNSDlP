dt<-read.csv("/data.csv")
libr<-dt
l1<-list()
leng = length(levels(factor(libr$N1)))
N1<-matrix(NA,10,leng)
for(i in 1:7){
  for(j in 1:leng)
  {
    k<-0
    for( t in 1:length(libr$N1)){
      print(t)
      print(libr$N1)
      if(libr$N1[t]==colnames(table(libr$class,libr$N1))[j])
        k<-k+1
    }

    N1[i,j]<-phyper(table(libr$class,libr$N1)[i,j]-1,k,length(libr$N1)-k, sum(table(libr$class,libr$N1)[i,]),lower.tail=FALSE)
    
    for( x in 1:length(libr$class))
    {
      if(N1[i,j]<0.11 && as.numeric(libr$class[x])== i && libr$N1[x]==colnames(table(libr$class,libr$N1))[j])
        l1<-rbind(l1,paste("N1",i,colnames(table(libr$class,libr$N1))[j],as.character(libr$x[x])))
    }
  }
}
colnames(N1)<-colnames(table(libr$class,libr$N1))
