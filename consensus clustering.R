library(tiff)
library(EBImage)
library(ConsensusClusterPlus)
load("data.RData")
load("data.RData")
y1<-length(BMP)
y2<-length(BMP[[1]])
y<-y1*y2
bmp<-vector("list",y)
for (i in 1:length(BMP)) {
  bmp[2*i-1]=BMP[[i]][1]
  bmp[2*i]=BMP[[i]][2]
}
BMP<-ptsf
#1、pca
data<-c(ptdt,BMP)
ptmatl<-c(ptdt,data)
ptmatl <- ptmatl[which(!is.na(ptmatl))]
ptmat <- sapply(ptmatl, function(x) as.numeric(x))
ptmat <- t(ptmat)
ptmat[which(is.na(ptmat))] <- 0
pca <- prcomp(ptmat, center = TRUE, scale = FALSE)
#2、phenoprint of 20 pcs
pdf(file="20pcs.pdf", width=7, height=7)
for(i in 1:ncol(eigenfish)) {
  r<-eigenfish[, i]
  b<- matrix(0,239,141)
  b[which(tem==1)]<-r[which(tem==1)]
  fish <- matrix(b, nrow=239)
  image(t(fish[rev(1:nrow(fish)), ]), axes=F, breaks=c(-100, seq(-1, 1, by=0.01), 100), 
        col=colorRampPalette(c("darkblue", "white", "darkred"))(202), 
        main=paste("PC ", i, sep=""))
}
dev.off()
#3、varexp
varexp <- (pca$sdev^2)/sum(pca$sdev^2)
pdf(file="top20.pdf", width=14, height=7)
barplot(100*varexp, col=c(rep('lightcoral', 20), rep(gray(0.5), length(varexp-20))), 
        main=paste("Top 20 PCs account for ", round(100*sum(varexp[1:20])), 
                   "% total variance", sep=""), xlab="Principal Components", ylab="% variance explained")
graphics.off()
#4、Consensus clustering
pc.use <- 20
featmat <- pca$x[, 1:pc.use]
cor.mat <- cor(t(featmat))
fet<-as.dist(1-cor.mat)
custom_pal <- colorRampPalette(c(rgb(202, 231, 248, maxColorValue = 255), "white",rgb(254, 124, 111, maxColorValue = 255)))(100)
pdf(file="clustering_result.pdf",width=10,height=10)
ress<-ConsensusClusterPlus(fet,maxK=15,reps=1500,pFeature=1,innerLinkage="average", finalLinkage="average",
                           clusterAlg="pam",distance="pearson",tmyPal = custom_pal)
dev.off()

