library(tiff)
library(EBImage)
library(tiff)
library(EBImage)
library(ConsensusClusterPlus)


#1、process_map:generate the BAM from raw data
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


#2、Consensus clustering

#2.1、pca
ptmatl<-c(ptdt,data)
ptmatl <- ptmatl[which(!is.na(ptmatl))]
ptmat <- sapply(ptmatl, function(x) as.numeric(x))
ptmat <- t(ptmat)
ptmat[which(is.na(ptmat))] <- 0
pca <- prcomp(ptmat, center = TRUE, scale = FALSE)

#2.2、Consensus clustering
pc.use <- 20
featmat <- pca$x[, 1:pc.use]
cor.mat <- cor(t(featmat))
fet<-as.dist(1-cor.mat)
custom_pal <- colorRampPalette(c(rgb(202, 231, 248, maxColorValue = 255), "white",rgb(254, 124, 111, maxColorValue = 255)))(100)
pdf(file="clustering_result.pdf",width=10,height=10)
ress<-ConsensusClusterPlus(fet,maxK=15,reps=1500,pFeature=1,innerLinkage="average", finalLinkage="average",
                           clusterAlg="pam",distance="pearson",tmyPal = custom_pal)
dev.off()

#3、Hypergeometric test

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

#4、Pharmacological prediction model
library(reticulate)

py_config()
source_python("Pharmacological prediction interface.py")

# 4.1. define config
config <- list(
  smile_dir = "data/test.xlsx",       
  train_img_dir = "data/train/",      
  test_img_dir = "data/test/",        
  model_path = "data/model/rf_model.pkl",  
  output_path = "data/output/results.xlsx" 
)

# 4.2. initialize predictor
predictor <- CNSPredictor(config)

# 4.3. loading and preprocessing data

data <- predictor$load_and_preprocess_data()
x_train <- py_to_r(data[[1]])  
y_train <- py_to_r(data[[2]])  
x_test <- py_to_r(data[[3]])   
y_test <- py_to_r(data[[4]])  
test_names <- py_to_r(data[[5]]) 

# 4.4. train model
accuracy_history <- predictor$train_model(x_train, y_train)


# 4.5. evaluate model(can load trained model)
results_and_accuracy <- predictor$evaluate_model(
  config$model_path,
  x_test,
  y_test,
  test_names,
  threshold = 0.65  
)

# result
results <- py_to_r(results_and_accuracy[[1]])  
accuracy <- py_to_r(results_and_accuracy[[2]]) 

# 4.6. save result
predictor$save_results(results, config$output_path)

# 4.7. show result
cat("Accuracy：", sprintf("%.4f", accuracy), "\n")
cat("Prediction：\n")
print(head(data.frame(
  sample_name = results$names,
  pre_label = results$predictions,
  pre_score = results$scores,
  true_label = results$true_labels
)))