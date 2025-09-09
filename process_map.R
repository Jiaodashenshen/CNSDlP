
library(tiff)
library(EBImage)




path = "/data"
drug<-list.files(path)

for(i in 1:length(drug)){
  ## read raw images and calculate the pixel difference between images
  path1 = paste0(path,drug[i],"/")
  path3 = paste0(path,drug[i],"/",drug[i],".RData")
  path2 = paste0(path,drug[i],"/",drug[i],"_club","/")
  xl<-list.files(path1)
  print(xl)
  for (h in 1:length(xl)) {
    cat(h,"\n")
    x<-list.files(paste0(path1,xl[h],"/"))
    print(length(x))
    BMP<-lapply(1:length(x), function(t){
      cat(x[t],"\n")
      dirs <- list.files(paste0(path1,xl[h],"/",x[t],"/"))
      files_1<-list.files(file.path(paste0(path1,xl[h],"/",x[t],"/","pre_process","/")))
      #print("files1")
      #print(files_1)
      bmp_list<-lapply(files_1,function(d){
    
        cat(d,"\n")
        #files_2 <- list.files(file.path(paste0(path1,xl[h],"/",x[t],"/","test",t,"/"), d,"/"))
        files_2 <- list.files(file.path(paste0(path1,xl[h],"/",x[t],"/","pre_process","/"), d,"/"))
        lapply(files_2, function(f) {#
          tem<-readTIFF(file.path(paste0(path1,xl[h],"/",x[t],"/","pre_process","/"), d, f))
          
          #cat(f,"\n")
          #cat(file.path(paste0(path1,xl[h],"/",x[t],"/",z), d, f),"\n")
          tem
        })
      })
      CBM<-lapply(1:2, function(i){
        s<-length(bmp_list[[i]])
        #start <- s * 0.1
        #end <- s * 0.9
        #range_size <- end - start + 1
        
        D<-lapply(c((s*0.1):(s*0.9)), function(k){
          bmp_list[[i]][[k]]-bmp_list[[i]][[k-1]]
          
        })
        
        #A<-matrix(0,range_size,1)
        A<-matrix(0,dim(D[[1]])[1],dim(D[[1]])[2])
        for(m in 2:length(D)){
          A<-D[[m-1]]+A
        }
        Am<-A/length(D)
        Am
        
        
      })
      CBM
      
      
    })
    save(BMP,file = paste0(path1,drug[i],".RData")) 
  }
  cat(i,"\n")
  ## calculate the T score BAMs
  rawdat <- list()
  rawdat_1 <- list()
  text<-list()
  dirs <- list.files(path2)
  for(d in dirs){
    result <- strsplit(d, "\\D+")[[1]]
    num <- as.numeric(result[2])
    print(file.path(paste0(path2,d,"/","pre_process"),"/"))
    files <- list.files(file.path(paste0(path2,d,"/","pre_process"),"/"))
    rawdat_1<-lapply(files,function(f){
      files_1<-list.files(paste0(path2, d,"/","pre_process","/",f,"/"))
      rawdat_2<-lapply(files_1,function(e){
        tem<-readTIFF(paste0(path2, d,"/","pre_process","/",f,"/",e))
      })
    })
    
    for (num in 1:length(rawdat_1)) {
      for (num1 in 1:length(rawdat_1[[num]])) {
        text[[num1+(num-1)*701]]<-rawdat_1[[num]][[num1]]
      }
      
    }
    rawdat[[d]]<-text
    
  }
  
  
  load(paste0(path1,drug[i],".RData"))
  m=239
  u=141
  y1<-length(BMP)
  y2<-length(BMP[[1]])
  y<-y1*y2
  k = length(BMP)
  diffmat1<-array(NA, dim=c(m, u, y))
  
  #m=dim(BMP[[1]])[1]
  #u=dim(BMP[[1]])[2]
  for (j in 1:k) {
    print(j)
    diffmat1[,,2*j-1]<-resize(BMP[[j]][[1]],m,u)
    diffmat1[,,j*2]<-resize(BMP[[j]][[2]],m,u)
  }
  
  
  ##輸入三維矩陣
  tmatcal <- function(diffmat) {
    ##
    sdiffmat <- diffmat
    tmat <- tmat0 <- matrix(NA,dim(sdiffmat)[1],dim(sdiffmat)[2])
    for(i in 1:dim(diffmat)[1]){
      for(j in 1:dim(diffmat)[2]){
        x=diffmat[i,j,]
        xmean=mean(x)
        xsd=sd(x)
        tmat0[i,j]=xmean/xsd/sqrt(dim(sdiffmat)[3])
      }
    }
    tmat <- tmat0
    tmat
  }
  ## if we need count the difference between 1 and 2 .use this code
  
  #diffmat =diffmat1
  tma1<-tmatcal(diffmat1)
  tmatv<-tma1
  
  data<-tmatv
  
  
  #####################################
  ##plot brain activity map
  width = 5
  height = 8
  
  
  
  pdf(file = paste0("/data",drug[i],"/",drug[i],".pdf"),width,height )
  #data<-ptsf[[i]]
  r<-data
  ##r<-resize(r,w = 239,h= 141)
  r<-r*2
  b <- matrix(0,239,141)
  
  b[which(tem==1)]<-r[which(tem==1)]
  b[b>1]<-1
  b[b< (-1)]<- -1
  image(t(b[nrow(b):1, ]), axes=F, breaks=c(-3, seq(-1, 1, by=0.01), 3), useRaster=T,
        col=colorRampPalette(c("lightblue","white", 'lightcoral'))(202),main="")
  #image(t(r[nrow(r):1, ]), axes=F, breaks=c(-3, seq(-1, 1, by=0.01), 3), useRaster=T,
  #      col=colorRampPalette(c("steelblue","white","darkorange"))(202),main=names(tmatv)[i])
  dev.off()
}