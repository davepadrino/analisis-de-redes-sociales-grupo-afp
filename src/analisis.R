library(ProjectTemplate)
library(tm)
library(cluster) 
library(fpc)
library(FactoMineR)

setwd("C:/Users/Alex/Documents/Asignacion_1")
load.project()


#-----------------------------------Hierarchal Clustering------------------------#
#First calculate distance between words & then cluster them according to similarity.


tdm2 <- removeSparseTerms(tdm, sparse = 0.955)
m2 <- as.matrix(tdm2)

d = dist(m2 , method="euclidian")
model<- hclust(d, method = "ward.D2")
plot(model)
rect.hclust(model, k =3)


#----------------------------PCA----------------------------------------#

tdm2 <- removeSparseTerms(tdm, sparse = 0.955)
m2 <- as.matrix(tdm2)
model <- PCA(t(m2), ncp = 5, axes = c(1,2))
plot(model, axes = c(1, 2),choix ="ind",col.ind = "red", label="none",new.plot = TRUE)


#----------------------------PCA----------------------------------------#



#------------------------------K-MEANS---------------------------------#

tdm2 <- removeSparseTerms(tdm, sparse = 0.955)
m2 <- as.matrix(tdm2)
d <- dist(m2, method="euclidian")   
kfit <- kmeans(d, 2)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

#------------------------------K-MEANS---------------------------------#