library(ProjectTemplate)
library(tm)
library(dplyr)
library(ggplot2)
require(sm)

#procesamiento
# create.project('Asignacion_1')

#Needed <- c("tm","dplyr","sm")
#install.packages(Needed, dependencies = TRUE)

#Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
#install.packages(Needed, dependencies=TRUE)   
#install.packages("FactoMineR")
setwd("C:/Users/Alex/Documents/Asignacion_1")
load.project()


load(file = "data/tw.RData")
fuente <- tw$text

# lee el documento UTF-8 y lo convierte a ASCII
fuente = iconv(txt, to="ASCII//TRANSLIT")

# construye un corpus
corpus <- Corpus(VectorSource(fuente), readerControl = list(language = "es"))


unwanted_array = list('á'='a', 'é'='e', 'í'='i','ó'='o','ú'='u', 'ü'='u')

##elimina acentos y caracteres especiales
acento <- function(x) {
  x <- chartr(paste(names(unwanted_array), collapse=''),
              paste(unwanted_array, collapse=''),x)
}

removeURL <- function(x) gsub("(f|ht)tp(s?)(://)?(.*)[.][a-z]+['/']+[A-Za-z0-9]+", "", x)
doc<- tm_map(corpus, removeURL)
removeURL2 <- function(x) gsub("https(:?)(/?)(/?)", "", x)
doc <- tm_map(doc, removeURL2)
remove6d <- function(x) gsub("(#6)D?d?", "", x)
doc <- tm_map(doc, tolower)
doc <- tm_map(doc, remove6d)
doc <- tm_map(doc,removePunctuation)
doc<- tm_map(doc,stripWhitespace)
doc <- tm_map(doc, tolower)
doc <- tm_map(doc, removeWords, c(stopwords("spanish"), "rt"))
doc <- tm_map(doc, acento)
doc <- tm_map(doc, PlainTextDocument) #This tells R to treat your preprocessed documents as text documents.


dtm <- DocumentTermMatrix(doc,control = list(wordLengths = c(2, Inf)))
tdm <- TermDocumentMatrix(doc,control = list(wordLengths = c(2, Inf)))

#save (dtm,tdm,file="data/preprocesamiento.RData")


freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)

wf <- data.frame(word=names(freq), freq=freq) 
#head(wf)

#necesito guardar el tdm y leerlo para el procesamiento
#si es posible guardar también el dtm 

##--------------------PROCESAMIENTO------------------------------#
library(cluster) 
library(fpc)
library(FactoMineR)


#Hierarchal Clustering
#First calculate distance between words & then cluster them according to similarity.

  
tdm2 <- removeSparseTerms(tdm, sparse = 0.955)
m2 <- as.matrix(tdm2)

d = dist(m2 , method="euclidian")
#model<- hclust(d^2, method = "ward.D")
model<- hclust(d, method = "ward.D2")
plot(model)
rect.hclust(model, k =3)



########################################PCA########################################

tdm2 <- removeSparseTerms(tdm, sparse = 0.955)
m2 <- as.matrix(tdm2)
model <- PCA(t(m2), ncp = 7, axes = c(1,2))
plot(model, axes = c(1, 2),choix ="ind",col.ind = "red", label="none",new.plot = TRUE)


###################################################################################



########################################K-MEANS########################################



tdm2 <- removeSparseTerms(tdm, sparse = 0.955)
m2 <- as.matrix(tdm2)
d <- dist(m2, method="euclidian")   
kfit <- kmeans(d, 2)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)


###-------------------------------K-MEANS---------------------------------------####

