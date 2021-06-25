library(qdap)
library(qdapDictionaries)
library(qdapRegex)
library(qdapTools)
library(RColorBrewer)
library(Xplortext)
library(htmltools)
library(wordcloud2)
library(gridExtra)
library(hrbrthemes)
library("plot3D")
library(rgl)
library(sqldf)

################## LLibreries ################## 

library(readxl) #llegir el xlsx
library(tm) #minado de paraules
library(SnowballC) #
library(wordcloud) #graficar nubol de paraules
library(ggplot2) #grafiques
library(dplyr) #modelardades
library(readr) #llegirdocs
library(cluster) #cluster
library(gapminder)  # base de datos
library(stringi)      # The string concat operator %s+%.
library(stringr)      # String manipulation. tidyverse.
library(glue)         # Format strings.
library(magrittr)    # Pipelines for data processing: %>% %T>% %<>%.
library(rattle.data)  # Weather dataset.
library(scales)     # commas(), percent().
library(textclean) 
library(lubridate)
library(ckanr)     # Access data from CKAN.
library(tidyr)     # Tidy the dataset: gather().
library(xtable)    # Format R data frames as LaTeX tables.
library(mdsr)
library(SimilaR)
library(SimilarityMeasures)
library(affinitymatrix)
library(irlba) #pca amb text
library(ggvis) #plots
library(broom)
library(tidyverse)
library(qdap) ##procesament de text
library(viridisLite)#plots
library(NLP) #analisi
library("FactoMineR")
library("factoextra")
library(ca)
library(Xplortext)
library(eply)
#########
#library(descr)
library(graphics)
##install.packages("descr")
library(pastecs)
#library(cluster)
library(class)
#library(FactoMineR)
#library(factoextra)
#library(ggplot2)
library(rgl)
library(Matrix)
library(BaylorEdPsych)
#library(mvnmle)
library(foreign)
######

setwd("D:/uni/TGF/Base de dades")
data <- read_excel("Data_TFG_2.xlsx")
a<-which(is.na(data[,"Project Description"]))
data <- data[-a,]
names(data)
data_1 <- data[,c("Project ID","Project Description","Year",
                  "Programme","CLC","status","Country",
                  "CompanyName","Category","TRL","Gender","Startup ID",
                  "Funding","Valuation","Funding Stage")]
b<-names(data_1)
names(data_1)<-c("doc_id","text",b[3:length(b)])
data_1$Programme <-as.factor(data_1$Programme)
data_1$CLC <- as.factor(data_1$CLC)
data_1$status <- as.factor(data_1$status)
data_1$`Country` <- as.factor(data_1$`Country`)
data_1$Category <- as.factor(data_1$Category)
data_1$TRL <- as.numeric(data_1$TRL)
data_1$Gender <- as.factor(data_1$Gender)
data_1$`Funding Stage` <- as.factor(data_1$`Funding Stage`)
data_1$Valuation <- as.factor(data_1$Valuation)
data_1$`Startup ID` <- as.factor(data_1$`Startup ID`)
summary(data_1)
#docs <- data.frame(doc_id = data_1$doc_id, text = data_1$text, stringsAsFactors = F)


text_corpus <- VCorpus(DataframeSource(data_1))
text_corpus
text_corpus[[1]]
head(meta(text_corpus))
i <- 30
content(text_corpus[[i]])
#sum(is.na(data_1))


# Create the document-term matrix from the corpus
text_dtm_0 <- DocumentTermMatrix(text_corpus, control = list(weighting = weightTf))

# Print out coffee_dtm data
text_dtm_0

# Convert coffee_dtm to a matrix
text_m_0 <- as.matrix(text_dtm_0)

# Print the dimensions of coffee_m
dim(text_m_0)
sum(text_m_0)
term_frequencia_0 <- colSums(text_m_0)
### Eliminar les ""



### Preporcesing
tryTolower <- function(x){
  # Torna Na si hi ha un error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # Si no error
  if (!inherits(try_error, 'error'))
    y = tolower(x) ## minuscules
  return(y)
}
custom.stopwords <- c(stopwords('english'), "also",'lol', 'smh', 'delta',"will","can") #stopwords
##afeguir patien a les stopwords???
clean.corpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(tryTolower)) #NA
  corpus <- tm_map(corpus, removeWords, custom.stopwords) # stopwords
  corpus <- tm_map(corpus, removePunctuation) #elimina puntuació
  corpus <- tm_map(corpus, stripWhitespace) #elimina espais de sobre
  corpus <- tm_map(corpus, removeNumbers) #elimina els numeros
  corpus <- tm_map(corpus,stemDocument) #funcio agrupar paraules eliminar sufixos
  corpus <- tm_map(corpus,noquote)
  
  return(corpus)
}

corpus_clean <- clean.corpus(text_corpus)
#corpus_clean <- gsub('"','',corpus_clean)
content(text_corpus[[749]])
content(corpus_clean[[749]])
n <- nrow(data)



# Create the document-term matrix from the corpus
text_dtm <- DocumentTermMatrix(corpus_clean, control = list(weighting = weightTf))

# Print out coffee_dtm data
text_dtm

# Convert coffee_dtm to a matrix
text_m <- as.matrix(text_dtm)

# Print the dimensions of coffee_m
dim(text_m)

term_frequencia_1 <- colSums(text_m)
### Eliminar les ""


#write.csv(colnames(text_m),"paraules1.csv")
b<- colnames(text_m)
dim(text_m)
b1 <- data.frame(id.x = 1:15100,nom = b)

comp_dict <- read.table("D:/uni/TGF/Base de dades/paraules_ord_netes_1.txt", 
                        quote="\"", comment.char="")
c1 <- data.frame(id.x=1:15100,nom_arreglat = comp_dict$V1)

junta_b_c <- merge(b1,c1,by = "id.x")
m<-1
for (m in 1:3892){
  
  a<-(content(corpus_clean[[m]]))
  d<- c()
  a_split <- unlist(strsplit(a, split=" "))
  
  for (i in 1:length(a_split)){
    for(j in 1:nrow(junta_b_c)){
      if(a_split[i] == junta_b_c$nom[j]){
        a_split[i] <- junta_b_c$nom_arreglat[j]
        if(j%%500==0){print(j)}
        break()
      }
    }
    
    d <- paste(d,a_split[i],sep=" ")
  }
  d<-str_trim(d)  
  content(corpus_clean[[m]])<-d
  if(m%%500==0){print(m)}
  
}
(corpus_clean)


# Create the document-term matrix from the corpus
text_dtm <- DocumentTermMatrix(corpus_clean, control = list(weighting = weightTf))

# Print out coffee_dtm data
text_dtm

# Convert coffee_dtm to a matrix
text_m <- as.matrix(text_dtm)

# Print the dimensions of coffee_m
dim(text_m)

term_frequencia_1 <- colSums(text_m)







## Eliminar paraules
tdm2 <- removeSparseTerms(text_dtm , 0.99) #1%
text_m2 <- as.matrix(tdm2)
dim(text_m2)

#write.csv(colnames(text_m2),"paraules_2.csv")
################## Recodificació de les praules ######
term_frequencia <- colSums(text_m2)
#View(text_m2[,c(41,42,43,44)])
#View(text_m2[,c(51,52)])
#View(text_m2[,c(67,68)])
#View(text_m2[,c(72,73)])
#View(text_m2[,c(231,232,233)])
#View(text_m2[,c(234,235)])
#View(text_m2[,c(350,351)])
#content(text_corpus[["2018-HS-0253"]])
#content(corpus_clean[["2018-HS-0253"]])

View(term_frequencia)
term_frequencia <- sort(term_frequencia,
                        decreasing = TRUE)


b<- colnames(text_m2)
b1 <- data.frame(id.x = 1:913,nom = b)

comp_dict <- read.table("D:/uni/TGF/Base de dades/paraules_ord_netes_3.txt", 
                        quote="\"", comment.char="")
c1 <- data.frame(id.x=1:913,nom_arreglat = comp_dict$V1)

junta_b_c <- merge(b1,c1,by = "id.x")



####### Creacio del corpus amb paraules arreglades
for (m in 1:3743){
    
  a<-(content(corpus_clean[[m]]))
  d<- c()
  a_split <- unlist(strsplit(a, split=" "))

  for (i in 1:length(a_split)){
    for(j in 1:nrow(junta_b_c)){
      if(a_split[i] == junta_b_c$nom[j]){
        a_split[i] <- junta_b_c$nom_arreglat[j]
        break()
        }
      }
  
      d <- paste(d,a_split[i],sep=" ")
    }
  d<-str_trim(d)  
  content(corpus_clean[[m]])<-d
  }

#### Creació de la matriu ####
# Create the document-term matrix from the corpus
text_dtm_2 <- DocumentTermMatrix(corpus_clean, control = list(weighting = weightTf))

# Print out coffee_dtm data
text_dtm_2

# Convert coffee_dtm to a matrix
text_m_2 <- as.matrix(text_dtm_2)

# Print the dimensions of coffee_m
dim(text_m_2)
sum(text_m_2)


## Eliminar paraules
tdm2_2 <- removeSparseTerms(text_dtm_2 , 0.99) #1%
text_m2_2 <- as.matrix(tdm2_2)
dim(text_m2_2)
sum(text_m2_2)



### Visualizacion ######

term_frequencia <- colSums(text_m2_2)
#View(term_frequencia)
term_frequencia <- sort(term_frequencia,
                        decreasing = TRUE)
sum(text_m2_2)

barplot(term_frequencia[1:40],
        col="tan",las=2)


word_freq <- data.frame(term = names(term_frequencia),
                        num = term_frequencia)

View(word_freq)
#write.csv2(word_freq,"taula_de_freq_paraules.csv")
### WordCloud


demoFreq <- data.frame(word = names(term_frequencia),
                        freq = term_frequencia)
wordcloud2(demoFreq, color = "r'andom-light", backgroundColor = "grey")

View(demoFreq)
#b<-data_1$text[2:5]


#word_associate(aux$text[1:100],match.string = "patient",stopwords = stopwords("eng"),network.plot = TRUE,cloud.colors = c("red","blue"))

######## CA text mining #############

### Preparar les dades per el CA ####
text_m2_3<-(cbind(text_m2_2,as.data.frame(meta(text_corpus))))


text_m2_3$CompanyName <- NULL
text_m2_3$`Startup ID`<-NULL

class(text_m2_3)
dim(text_m2_2)
dim(text_m2_3)

a <-which(rowSums(text_m2_2)==0)
text_m2_3 <- text_m2_3[-a,]#eliminm les subisions que tenen 0 com a sum total
dim(text_m2_3)

text_m2_3$TRL <- as.numeric(text_m2_3$TRL)


str(text_m2_3[,871:881])


########## Descriptiva ###########

library(viridis)
library(hrbrthemes)
metadata <-text_m2_3[,871:881]
summary(metadata)

aa <- table(metadata$Year,metadata$status)
bb <- table(metadata$CLC,metadata$Year)
dd <- table(metadata$status,metadata$Year)
table(metadata$CLC)
table(metadata$Category)
ggplot(metadata, aes(fill=status, y=Year, x=Year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Studying 4 species..") +
  theme_ipsum() +
  xlab("")
#write.csv(metadata,"base_per_la_descriptiva.csv")
##### CA ################

res.ca <- CA(text_m2_3,quali.sup = c(872,873,874,875,876,878,880,881) ,quanti.sup = c(871,877,879),ncp = 10)
#res.ca <- CA(text_m2_3,quali.sup = c(881,882,883,884,885,886,888,889) ,quanti.sup = c(880,887),ncp = 10)
#res.ca <- CA(text_m2_2,ncp = 10)
print(res.ca)
eig.val <- get_eigenvalue(res.ca)


summary(res.ca)
fviz_screeplot(res.ca, addlabels=T,xlim = c(1,10))
fviz_cos2(res.ca, choice = "row", axes = 1:2,top=10)
fviz_cos2(res.ca, choice = "col", axes = 1:2,top=10)
plot(res.ca, invisible=c("row","col.sup","quali.sup"),label = "none",title = "CA Paraules")
plot(res.ca, invisible=c("col","col.sup","quali.sup"),label = "none",title = "CA Projectes")
plot(res.ca,choix = "CA" ,invisible=c("col","row","col.sup"),label = "none",title = "CA Files")
fviz_contrib(res.ca, choice = "col", axes = 1,top = 10)
fviz_contrib(res.ca, choice = "col", axes = 2,top = 10)
fviz_contrib(res.ca, choice = "col", axes = 3,top = 10)
fviz_contrib(res.ca, choice = "col", axes = 4,top = 10)
fviz_ca_col(res.ca,geom = "text",title = "CA paraules")

### Analisi per quadrants
plot(res.ca, invisible=c("row","col.sup","quali.sup"),title = "CA Paraules")
primer <-plot(res.ca, invisible=c("row","col.sup","quali.sup"),title = "CA Paraules",xlim = c(0,2.5),ylim = c(0,1.5))
primer
tercer_quart <- plot(res.ca, invisible=c("row","col.sup","quali.sup"),title = "CA Paraules",xlim = c(-1.5,1.5),ylim = c(-2,0))
tercer_quart
segon <- plot(res.ca, invisible=c("row","col.sup","quali.sup"),title = "CA Paraules",xlim = c(-1.5,0),ylim = c(0,1.5))
segon


#plot(res.ca,selectCol = names())

col <- get_ca_col(res.ca)



###Top  paraules que més  contribueixen  a cada eix
contribucio_col <- as.data.frame(col$contrib)
coord_col <- as.data.frame(col$coord)
coord_col$names <- rownames(coord_col)
colnames(contribucio_col) <- c("Dim 1_con","Dim 2_con","Dim 3_con","Dim 4_con","Dim 5_con",
                               "Dim 6_con","Dim 7_con","Dim 8_con","Dim 9_con","Dim 10_con")
contribucio_col <- cbind(contribucio_col,names =rownames(contribucio_col))
#names(contribucio_col)

con <- 6 ##vegades la contribució

dim1 <- contribucio_col[order(-contribucio_col$`Dim 1_con`),c("Dim 1_con","names")]
colnames(dim1)<- c("Dim1_con","names_1")
X<-mean(dim1$Dim1_con)
n1<-sum(dim1$Dim1_con>con*X)
dim1 <- dim1[1:n1,]
#rownames(dim1)
dim1_pos_neg <- merge(dim1,coord_col,by.x = "names_1",by.y = "names")
dim1_pos_neg <- dim1_pos_neg[order(-dim1_pos_neg$`Dim1_con`),]
dim1_pos_neg <- dim1_pos_neg[,c(1,3)]
dim1_pos_neg$posneg <-ifelse(dim1_pos_neg$`Dim 1`>=0,"pos","neg")

d1 <- split(dim1_pos_neg,dim1_pos_neg$posneg)


dim2 <- contribucio_col[order(-contribucio_col$`Dim 2`),c("Dim 2_con","names")]
n2<-sum(dim2$`Dim 2_con`>con*X)
colnames(dim2)<- c("Dim2_con","names_2")
dim2 <- dim2[1:n2,]
dim2_pos_neg <- merge(dim2,coord_col,by.x = "names_2",by.y = "names")
dim2_pos_neg <- dim2_pos_neg[order(-dim2_pos_neg$`Dim2_con`),]
dim2_pos_neg <- dim2_pos_neg[,c(1,4)]
dim2_pos_neg$posneg <-ifelse(dim2_pos_neg$`Dim 2`>=0,"pos","neg")

d2 <- split(dim2_pos_neg,dim2_pos_neg$posneg)





dim3 <- contribucio_col[order(-contribucio_col$`Dim 3`),c("Dim 3_con","names")]
n3<-sum(dim3$`Dim 3_con`>con*X)
colnames(dim3)<- c("Dim3_con","names_3")
dim3 <- dim3[1:n3,]
dim3_pos_neg <- merge(dim3,coord_col,by.x = "names_3",by.y = "names")
dim3_pos_neg <- dim3_pos_neg[order(-dim3_pos_neg$`Dim3_con`),]
dim3_pos_neg <- dim3_pos_neg[,c(1,5)]
dim3_pos_neg$posneg <-ifelse(dim3_pos_neg$`Dim 3`>=0,"pos","neg")

d3 <- split(dim3_pos_neg,dim3_pos_neg$posneg)




dim4 <- contribucio_col[order(-contribucio_col$`Dim 4`),c("Dim 4_con","names")]
n4<-sum(dim4$`Dim 4_con`>con*X)
colnames(dim4)<- c("Dim4_con","names_4")
dim4 <- dim4[1:n4,]
dim4_pos_neg <- merge(dim4,coord_col,by.x = "names_4",by.y = "names")
dim4_pos_neg <- dim4_pos_neg[order(-dim4_pos_neg$`Dim4_con`),]
dim4_pos_neg <- dim4_pos_neg[,c(1,6)]
dim4_pos_neg$posneg <-ifelse(dim4_pos_neg$`Dim 4`>=0,"pos","neg")

d4 <- split(dim4_pos_neg,dim4_pos_neg$posneg)


dim5 <- contribucio_col[order(-contribucio_col$`Dim 5`),c("Dim 5_con","names")]
n5<-sum(dim5$`Dim 5_con`>con*X)
colnames(dim5)<- c("Dim5_con","names_5")
dim5 <- dim5[1:n5,]
dim5_pos_neg <- merge(dim5,coord_col,by.x = "names_5",by.y = "names")
dim5_pos_neg <- dim5_pos_neg[order(-dim5_pos_neg$`Dim5_con`),]
dim5_pos_neg <- dim5_pos_neg[,c(1,7)]
dim5_pos_neg$posneg <-ifelse(dim5_pos_neg$`Dim 5`>=0,"pos","neg")

d5 <- split(dim5_pos_neg,dim5_pos_neg$posneg)


dim6 <- contribucio_col[order(-contribucio_col$`Dim 6_con`),c("Dim 6_con","names")]
n6<-sum(dim6$`Dim 6_con`>con*X)
colnames(dim6)<- c("Dim6_con","names_6")
dim6 <- dim6[1:n6,]
dim6_pos_neg <- merge(dim6,coord_col,by.x = "names_6",by.y = "names")
dim6_pos_neg <- dim6_pos_neg[order(-dim6_pos_neg$`Dim6_con`),]
dim6_pos_neg <- dim6_pos_neg[,c(1,8)]
dim6_pos_neg$posneg <-ifelse(dim6_pos_neg$`Dim 6`>=0,"pos","neg")

d6 <- split(dim6_pos_neg,dim6_pos_neg$posneg)

dim7 <- contribucio_col[order(-contribucio_col$`Dim 7_con`),c("Dim 7_con","names")]
n7<-sum(dim7$`Dim 7_con`>con*X)
colnames(dim7)<- c("Dim7_con","names_7")
dim7 <- dim7[1:n7,]
dim7_pos_neg <- merge(dim7,coord_col,by.x = "names_7",by.y = "names")
dim7_pos_neg <- dim7_pos_neg[order(-dim7_pos_neg$`Dim7_con`),]
dim7_pos_neg <- dim7_pos_neg[,c(1,9)]
dim7_pos_neg$posneg <-ifelse(dim7_pos_neg$`Dim 7`>=0,"pos","neg")

d7 <- split(dim7_pos_neg,dim7_pos_neg$posneg)

dim8 <- contribucio_col[order(-contribucio_col$`Dim 8_con`),c("Dim 8_con","names")]
n8<-sum(dim8$`Dim 8_con`>con*X)
colnames(dim8)<- c("Dim8_con","names_8")
dim8 <- dim8[1:n8,]
dim8_pos_neg <- merge(dim8,coord_col,by.x = "names_8",by.y = "names")
dim8_pos_neg <- dim8_pos_neg[order(-dim8_pos_neg$`Dim8_con`),]
dim8_pos_neg <- dim8_pos_neg[,c(1,10)]
dim8_pos_neg$posneg <-ifelse(dim8_pos_neg$`Dim 8`>=0,"pos","neg")

d8 <- split(dim8_pos_neg,dim8_pos_neg$posneg)

dim9 <- contribucio_col[order(-contribucio_col$`Dim 9_con`),c("Dim 9_con","names")]
n9<-sum(dim9$`Dim 9_con`>con*X)
colnames(dim9)<- c("Dim9_con","names_9")
dim9 <- dim9[1:n9,]
dim9_pos_neg <- merge(dim9,coord_col,by.x = "names_9",by.y = "names")
dim9_pos_neg <- dim9_pos_neg[order(-dim9_pos_neg$`Dim9_con`),]
dim9_pos_neg <- dim9_pos_neg[,c(1,11)]
dim9_pos_neg$posneg <-ifelse(dim9_pos_neg$`Dim 9`>=0,"pos","neg")

d9 <- split(dim9_pos_neg,dim9_pos_neg$posneg)

dim10 <- contribucio_col[order(-contribucio_col$`Dim 10_con`),c("Dim 10_con","names")]
n10<-sum(dim10$`Dim 10_con`>con*X)
colnames(dim10)<- c("Dim10_con","names_10")
dim10 <- dim10[1:n10,]
dim10_pos_neg <- merge(dim10,coord_col,by.x = "names_10",by.y = "names")
dim10_pos_neg <- dim10_pos_neg[order(-dim10_pos_neg$`Dim10_con`),]
dim10_pos_neg <- dim10_pos_neg[,c(1,12)]
dim10_pos_neg$posneg <-ifelse(dim10_pos_neg$`Dim 10`>=0,"pos","neg")

d10<- split(dim10_pos_neg,dim10_pos_neg$posneg)





d1_pos <- d1$pos
d1_neg <- d1$neg

d2_pos <- d2$pos
d2_neg <- d2$neg

d3_pos <- d3$pos
d3_neg <- d3$neg
d4_pos <- d4$pos
d4_neg <- d4$neg
d5_pos <- d5$pos
d5_neg <- d5$neg

d6_pos <- d6$pos
d6_neg <- d6$neg

d7_pos <- d7$pos
d7_neg <- d7$neg

d8_pos <- d8$pos
d8_neg <- d8$neg

d9_pos <- d9$pos
d9_neg <- d9$neg

d10_pos <- d10$pos
d10_neg <- d10$neg


n <- max(dim(d1_neg)[1],dim(d1_pos)[1],dim(d2_neg)[1],dim(d2_pos)[1],
         dim(d3_neg)[1],dim(d3_pos)[1],dim(d4_neg)[1],dim(d4_pos)[1],
         dim(d5_neg)[1],dim(d5_pos)[1],dim(d6_neg)[1],dim(d6_pos)[1],
         dim(d7_neg)[1],dim(d7_pos)[1],dim(d8_neg)[1],dim(d8_pos)[1],
         dim(d9_neg)[1],dim(d9_pos)[1],dim(d10_neg)[1],dim(d10_pos)[1]) ###modificar aquest valor en funcio de 3 o 6
d1_pos[nrow(d1_pos):n,]<-0
d1_neg[nrow(d1_neg):n,]<-0
d2_pos[nrow(d2_pos):n,]<-0
d2_neg[nrow(d2_neg):n,]<-0
d3_pos[nrow(d3_pos):n,]<-0
d3_neg[nrow(d3_neg):n,]<-0
##d4_pos[nrow(d4_pos):n,]<-0
d4_neg[nrow(d4_neg):n,]<-0
d5_pos[nrow(d5_pos):n,]<-0
d5_neg[nrow(d5_neg):n,]<-0
d6_pos[nrow(d6_pos):n,]<-0
d6_neg[nrow(d6_neg):n,]<-0
d7_pos[nrow(d7_pos):n,]<-0
d7_neg[nrow(d7_neg):n,]<-0
d8_pos[nrow(d8_pos):n,]<-0
d8_neg[nrow(d8_neg):n,]<-0
d9_pos[nrow(d9_pos):n,]<-0
d9_neg[nrow(d9_neg):n,]<-0
d10_pos[nrow(d10_pos):n,]<-0
d10_neg[nrow(d10_neg):n,]<-0

colnames(d1_pos)<-c("names_1_pos",colnames(d1_pos)[2:3])
colnames(d1_neg)<-c("names_1_neg",colnames(d1_neg)[2:3])
colnames(d2_pos)<-c("names_2_pos",colnames(d2_pos)[2:3])
colnames(d2_neg)<-c("names_2_neg",colnames(d2_neg)[2:3])
colnames(d3_pos)<-c("names_3_pos",colnames(d3_pos)[2:3])
colnames(d3_neg)<-c("names_3_neg",colnames(d3_neg)[2:3])
colnames(d4_pos)<-c("names_4_pos",colnames(d4_pos)[2:3])
colnames(d4_neg)<-c("names_4_neg",colnames(d4_neg)[2:3])
colnames(d5_pos)<-c("names_5_pos",colnames(d5_pos)[2:3])
colnames(d5_neg)<-c("names_5_neg",colnames(d5_neg)[2:3])
colnames(d6_pos)<-c("names_6_pos",colnames(d6_pos)[2:3])
colnames(d6_neg)<-c("names_6_neg",colnames(d6_neg)[2:3])
colnames(d7_pos)<-c("names_7_pos",colnames(d7_pos)[2:3])
colnames(d7_neg)<-c("names_7_neg",colnames(d7_neg)[2:3])
colnames(d8_pos)<-c("names_8_pos",colnames(d8_pos)[2:3])
colnames(d8_neg)<-c("names_8_neg",colnames(d8_neg)[2:3])
colnames(d9_pos)<-c("names_9_pos",colnames(d9_pos)[2:3])
colnames(d9_neg)<-c("names_9_neg",colnames(d9_neg)[2:3])
colnames(d10_pos)<-c("names_10_pos",colnames(d10_pos)[2:3])
colnames(d10_neg)<-c("names_10_neg",colnames(d10_neg)[2:3])
Metakeys_7 <- cbind(d1_pos[,1],d1_neg[,1],
                  d2_pos[,1],d2_neg[,1],
                  d3_pos[,1],d3_neg[,1],
                  d4_pos[,1],d4_neg[,1],
                  d5_pos[,1],d5_neg[,1],
                  d6_pos[,1],d6_neg[,1],
                  d7_pos[,1],d7_neg[,1],
                  d8_pos[,1],d8_neg[,1],
                  d9_pos[,1],d9_neg[,1],
                  d10_pos[,1],d10_neg[,1])

colnames(Metakeys_7)<-c("names_1_pos","names_1_neg",
                      "names_2_pos","names_2_neg",
                      "names_3_pos","names_3_neg",
                      "names_4_pos","names_4_neg",
                      "names_5_pos","names_5_neg",
                      "names_6_pos","names_6_neg",
                      "names_7_pos","names_7_neg",
                      "names_8_pos","names_8_neg",
                      "names_9_pos","names_9_neg",
                      "names_10_pos","names_10_neg")
#View(Metakeys_3)
#View(Metakeys_6)
dim(Metakeys_3)
#dim(Metakeys_6)
#write.csv(Metakeys_3,"metakeys_3vegadesmes_10_3.csv")

### Coordenades de les Top paraules tres primers dim
windows()
fviz_ca_col(res.ca, geom = c("point","text"),
            select.col = list(name = c(rownames(dim1),rownames(dim2))),
            repel = TRUE,
            col.col = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            title= "Paraules mes contribueixen a l'eix 1",
            labelsize = 6)

fviz_ca_col(res.ca, geom = c("point","text"),
            select.col = list(name = c(rownames(dim2))),
            repel = TRUE,
            col.col = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            title= "Paraules mes contribueixen a l'eix 2",
            labelsize = 6)





plot(res.ca,
     arrows = c(FALSE, TRUE),
     selectCol = c(rownames(dim1)),
     invisible=c("row","col.sup","quali.sup"),
     autoLab = "yes",
     cex = 0.75,
     unselect = 0.9,
     pch = 19,
     
     )
windows()
a<-fviz_ca_col(res.ca,
               label="none",
               ylim =c(-2.25,2),
               xlim = c(-1.5,2.5),
               alpha.col =0.3
)
a
windows()
b<-fviz_ca_col(res.ca,
            axes = c(1,2),
            shape.col = 15,
            select.col = list(contrib=30),
            unselect = 0.9,
            #col.col ="contrib",
            repel = TRUE,
            ylim =c(-2.25,2),
            xlim = c(-1.5,2.5),
            
  )

x1<-res.ca$col$coord[,1]
x2<-res.ca$col$coord[,2]
plot(res.ca$quali.sup$coord)
#plot(b)+points(x1,x2,alpha = 0.1)
  
coordenades <- as.data.frame(col$coord)
coordenades <- cbind(coordenades,names = rownames(coordenades))

dim1_coord <- merge(dim1,coordenades,by.x = "names_1",by.y = "names")
rownames(dim1_coord)<-dim1_coord$names 

dim2_coord <- merge(dim2,coordenades,by.x = "names_2",by.y = "names")
rownames(dim2_coord)<-dim2_coord$names 

dim3_coord <- merge(dim3,coordenades,by.x = "names_3",by.y = "names")
rownames(dim3_coord)<-dim3_coord$names 

coor_paraules <- rbind(dim1_coord[,c("Dim 1","Dim 2","Dim 3")],
                       dim2_coord[,c("Dim 1","Dim 2","Dim 3")],
                       dim3_coord[,c("Dim 1","Dim 2","Dim 3")])
#coor_paraules$Dim <- as.factor(rep(1:3,each = 10))
#View(coor_paraules)
#library(ggrepel)






## Nom DIM 1 + Malaltia/Cancer
## Nom DIM 1 - Interfice usuaria
## Nom DIM 2 + Suport Malalt, relacionat amb 1-
## Nom DIM 2 - Dispositiu de seguiment i monotoritzar

plot(res.ca, invisible="row",title = "CA Paraules",xlim = c(0.75,2),ylim = c(-0.75,0.75),axes = 3:4)
## Nom Dim 3+ "Ferida"


plot(res.ca, invisible="row",title = "CA Paraules",xlim = c(-2,-1),ylim = c(-0.75,0.75),axes = 3:4)
## Nom Dim 3- Deteccio de malalties


plot(res.ca, invisible="row",title = "CA Paraules",xlim = c(-1,1),ylim = c(0.5,1.5),axes = 3:4)
## Nom DIM 4 + Nutricio, entrenament, profesionals mon salut

plot(res.ca, invisible="row",title = "CA Paraules",xlim = c(-1,1),ylim = c(-1.5,0),axes = 3:4)
## Nom DIM 4 - Cirurgies, operacions


plot(res.ca, invisible="row",title = "CA Paraules",xlim = c(-1,1),ylim = c(0.8,1.5),axes = c(3,5))
## Nom DIM 5 + Avis i prevenció

plot(res.ca, invisible="row",title = "CA Paraules",xlim = c(-1,1),ylim = c(-1.5,-0.5),axes = c(3,5))
## Nom DIM 5 - Rehabilitation


## neteja memoria

rm(a1)
rm(b1)
rm(c1)
rm(col)

rm(junta_b)
rm(junta_b_c)
rm(text_m)
rm(text_corpus)
rm(text_dtm)
##rm(text_dtm_2)
rm(tdm2)
##rm(text_m_2)
##rm(text_m2)

##rm(a)
##rm(d)
##rm(b)
##rm(a_split)
##rm(i)
##rm(j)
##rm(m)
##rm(n)

## Projectes més significatiu a cada dim

row <- get_ca_row(res.ca)
contribucio_col <- as.data.frame(row$contrib)
coord_col <- as.data.frame(row$coord)
coord_col$names <- rownames(coord_col)
colnames(contribucio_col) <- c("Dim 1_con","Dim 2_con","Dim 3_con","Dim 4_con","Dim 5_con",
                               "Dim 6_con","Dim 7_con","Dim 8_con","Dim 9_con","Dim 10_con")
contribucio_col <- cbind(contribucio_col,names =rownames(contribucio_col))
#names(contribucio_col)

con <- 6 ##vegades la contribució

dim1 <- contribucio_col[order(-contribucio_col$`Dim 1_con`),c("Dim 1_con","names")]
colnames(dim1)<- c("Dim1_con","names_1")
X<-mean(dim1$Dim1_con)
n1<-sum(dim1$Dim1_con>con*X)
dim1 <- dim1[1:n1,]
#rownames(dim1)
dim1_pos_neg <- merge(dim1,coord_col,by.x = "names_1",by.y = "names")
dim1_pos_neg <- dim1_pos_neg[order(-dim1_pos_neg$`Dim1_con`),]
dim1_pos_neg <- dim1_pos_neg[,c(1,3)]
dim1_pos_neg$posneg <-ifelse(dim1_pos_neg$`Dim 1`>=0,"pos","neg")

d1 <- split(dim1_pos_neg,dim1_pos_neg$posneg)


dim2 <- contribucio_col[order(-contribucio_col$`Dim 2`),c("Dim 2_con","names")]
n2<-sum(dim2$`Dim 2_con`>con*X)
colnames(dim2)<- c("Dim2_con","names_2")
dim2 <- dim2[1:n2,]
dim2_pos_neg <- merge(dim2,coord_col,by.x = "names_2",by.y = "names")
dim2_pos_neg <- dim2_pos_neg[order(-dim2_pos_neg$`Dim2_con`),]
dim2_pos_neg <- dim2_pos_neg[,c(1,4)]
dim2_pos_neg$posneg <-ifelse(dim2_pos_neg$`Dim 2`>=0,"pos","neg")

d2 <- split(dim2_pos_neg,dim2_pos_neg$posneg)





dim3 <- contribucio_col[order(-contribucio_col$`Dim 3`),c("Dim 3_con","names")]
n3<-sum(dim3$`Dim 3_con`>con*X)
colnames(dim3)<- c("Dim3_con","names_3")
dim3 <- dim3[1:n3,]
dim3_pos_neg <- merge(dim3,coord_col,by.x = "names_3",by.y = "names")
dim3_pos_neg <- dim3_pos_neg[order(-dim3_pos_neg$`Dim3_con`),]
dim3_pos_neg <- dim3_pos_neg[,c(1,5)]
dim3_pos_neg$posneg <-ifelse(dim3_pos_neg$`Dim 3`>=0,"pos","neg")

d3 <- split(dim3_pos_neg,dim3_pos_neg$posneg)




dim4 <- contribucio_col[order(-contribucio_col$`Dim 4`),c("Dim 4_con","names")]
n4<-sum(dim4$`Dim 4_con`>con*X)
colnames(dim4)<- c("Dim4_con","names_4")
dim4 <- dim4[1:n4,]
dim4_pos_neg <- merge(dim4,coord_col,by.x = "names_4",by.y = "names")
dim4_pos_neg <- dim4_pos_neg[order(-dim4_pos_neg$`Dim4_con`),]
dim4_pos_neg <- dim4_pos_neg[,c(1,6)]
dim4_pos_neg$posneg <-ifelse(dim4_pos_neg$`Dim 4`>=0,"pos","neg")

d4 <- split(dim4_pos_neg,dim4_pos_neg$posneg)


dim5 <- contribucio_col[order(-contribucio_col$`Dim 5`),c("Dim 5_con","names")]
n5<-sum(dim5$`Dim 5_con`>con*X)
colnames(dim5)<- c("Dim5_con","names_5")
dim5 <- dim5[1:n5,]
dim5_pos_neg <- merge(dim5,coord_col,by.x = "names_5",by.y = "names")
dim5_pos_neg <- dim5_pos_neg[order(-dim5_pos_neg$`Dim5_con`),]
dim5_pos_neg <- dim5_pos_neg[,c(1,7)]
dim5_pos_neg$posneg <-ifelse(dim5_pos_neg$`Dim 5`>=0,"pos","neg")

d5 <- split(dim5_pos_neg,dim5_pos_neg$posneg)


dim6 <- contribucio_col[order(-contribucio_col$`Dim 6_con`),c("Dim 6_con","names")]
n6<-sum(dim6$`Dim 6_con`>con*X)
colnames(dim6)<- c("Dim6_con","names_6")
dim6 <- dim6[1:n6,]
dim6_pos_neg <- merge(dim6,coord_col,by.x = "names_6",by.y = "names")
dim6_pos_neg <- dim6_pos_neg[order(-dim6_pos_neg$`Dim6_con`),]
dim6_pos_neg <- dim6_pos_neg[,c(1,8)]
dim6_pos_neg$posneg <-ifelse(dim6_pos_neg$`Dim 6`>=0,"pos","neg")

d6 <- split(dim6_pos_neg,dim6_pos_neg$posneg)

dim7 <- contribucio_col[order(-contribucio_col$`Dim 7_con`),c("Dim 7_con","names")]
n7<-sum(dim7$`Dim 7_con`>con*X)
colnames(dim7)<- c("Dim7_con","names_7")
dim7 <- dim7[1:n7,]
dim7_pos_neg <- merge(dim7,coord_col,by.x = "names_7",by.y = "names")
dim7_pos_neg <- dim7_pos_neg[order(-dim7_pos_neg$`Dim7_con`),]
dim7_pos_neg <- dim7_pos_neg[,c(1,9)]
dim7_pos_neg$posneg <-ifelse(dim7_pos_neg$`Dim 7`>=0,"pos","neg")

d7 <- split(dim7_pos_neg,dim7_pos_neg$posneg)

dim8 <- contribucio_col[order(-contribucio_col$`Dim 8_con`),c("Dim 8_con","names")]
n8<-sum(dim8$`Dim 8_con`>con*X)
colnames(dim8)<- c("Dim8_con","names_8")
dim8 <- dim8[1:n8,]
dim8_pos_neg <- merge(dim8,coord_col,by.x = "names_8",by.y = "names")
dim8_pos_neg <- dim8_pos_neg[order(-dim8_pos_neg$`Dim8_con`),]
dim8_pos_neg <- dim8_pos_neg[,c(1,10)]
dim8_pos_neg$posneg <-ifelse(dim8_pos_neg$`Dim 8`>=0,"pos","neg")

d8 <- split(dim8_pos_neg,dim8_pos_neg$posneg)

dim9 <- contribucio_col[order(-contribucio_col$`Dim 9_con`),c("Dim 9_con","names")]
n9<-sum(dim9$`Dim 9_con`>con*X)
colnames(dim9)<- c("Dim9_con","names_9")
dim9 <- dim9[1:n9,]
dim9_pos_neg <- merge(dim9,coord_col,by.x = "names_9",by.y = "names")
dim9_pos_neg <- dim9_pos_neg[order(-dim9_pos_neg$`Dim9_con`),]
dim9_pos_neg <- dim9_pos_neg[,c(1,11)]
dim9_pos_neg$posneg <-ifelse(dim9_pos_neg$`Dim 9`>=0,"pos","neg")

d9 <- split(dim9_pos_neg,dim9_pos_neg$posneg)

dim10 <- contribucio_col[order(-contribucio_col$`Dim 10_con`),c("Dim 10_con","names")]
n10<-sum(dim10$`Dim 10_con`>con*X)
colnames(dim10)<- c("Dim10_con","names_10")
dim10 <- dim10[1:n10,]
dim10_pos_neg <- merge(dim10,coord_col,by.x = "names_10",by.y = "names")
dim10_pos_neg <- dim10_pos_neg[order(-dim10_pos_neg$`Dim10_con`),]
dim10_pos_neg <- dim10_pos_neg[,c(1,12)]
dim10_pos_neg$posneg <-ifelse(dim10_pos_neg$`Dim 10`>=0,"pos","neg")

d10<- split(dim10_pos_neg,dim10_pos_neg$posneg)





d1_pos <- d1$pos
d1_neg <- d1$neg

d2_pos <- d2$pos
d2_neg <- d2$neg

d3_pos <- d3$pos
d3_neg <- d3$neg
d4_pos <- d4$pos
d4_neg <- d4$neg
d5_pos <- d5$pos
d5_neg <- d5$neg

d6_pos <- d6$pos
d6_neg <- d6$neg

d7_pos <- d7$pos
d7_neg <- d7$neg

d8_pos <- d8$pos
d8_neg <- d8$neg

d9_pos <- d9$pos
d9_neg <- d9$neg

d10_pos <- d10$pos
d10_neg <- d10$neg


n <- max(dim(d1_neg)[1],dim(d1_pos)[1],dim(d2_neg)[1],dim(d2_pos)[1],
         dim(d3_neg)[1],dim(d3_pos)[1],dim(d4_neg)[1],dim(d4_pos)[1],
         dim(d5_neg)[1],dim(d5_pos)[1],dim(d6_neg)[1],dim(d6_pos)[1],
         dim(d7_neg)[1],dim(d7_pos)[1],dim(d8_neg)[1],dim(d8_pos)[1],
         dim(d9_neg)[1],dim(d9_pos)[1],dim(d10_neg)[1],dim(d10_pos)[1]) ###modificar aquest valor en funcio de 3 o 6
d1_pos[nrow(d1_pos):n,]<-0
d1_neg[nrow(d1_neg):n,]<-0
d2_pos[nrow(d2_pos):n,]<-0
d2_neg[nrow(d2_neg):n,]<-0
d3_pos[nrow(d3_pos):n,]<-0
d3_neg[nrow(d3_neg):n,]<-0
d4_pos[nrow(d4_pos):n,]<-0
d4_neg[nrow(d4_neg):n,]<-0
d5_pos[nrow(d5_pos):n,]<-0
d5_neg[nrow(d5_neg):n,]<-0
##d6_pos[nrow(d6_pos):n,]<-0
d6_neg[nrow(d6_neg):n,]<-0
d7_pos[nrow(d7_pos):n,]<-0
d7_neg[nrow(d7_neg):n,]<-0
d8_pos[nrow(d8_pos):n,]<-0
d8_neg[nrow(d8_neg):n,]<-0
d9_pos[nrow(d9_pos):n,]<-0
d9_neg[nrow(d9_neg):n,]<-0
d10_pos[nrow(d10_pos):n,]<-0
d10_neg[nrow(d10_neg):n,]<-0

colnames(d1_pos)<-c("names_1_pos",colnames(d1_pos)[2:3])
colnames(d1_neg)<-c("names_1_neg",colnames(d1_neg)[2:3])
colnames(d2_pos)<-c("names_2_pos",colnames(d2_pos)[2:3])
colnames(d2_neg)<-c("names_2_neg",colnames(d2_neg)[2:3])
colnames(d3_pos)<-c("names_3_pos",colnames(d3_pos)[2:3])
colnames(d3_neg)<-c("names_3_neg",colnames(d3_neg)[2:3])
colnames(d4_pos)<-c("names_4_pos",colnames(d4_pos)[2:3])
colnames(d4_neg)<-c("names_4_neg",colnames(d4_neg)[2:3])
colnames(d5_pos)<-c("names_5_pos",colnames(d5_pos)[2:3])
colnames(d5_neg)<-c("names_5_neg",colnames(d5_neg)[2:3])
colnames(d6_pos)<-c("names_6_pos",colnames(d6_pos)[2:3])
colnames(d6_neg)<-c("names_6_neg",colnames(d6_neg)[2:3])
colnames(d7_pos)<-c("names_7_pos",colnames(d7_pos)[2:3])
colnames(d7_neg)<-c("names_7_neg",colnames(d7_neg)[2:3])
colnames(d8_pos)<-c("names_8_pos",colnames(d8_pos)[2:3])
colnames(d8_neg)<-c("names_8_neg",colnames(d8_neg)[2:3])
colnames(d9_pos)<-c("names_9_pos",colnames(d9_pos)[2:3])
colnames(d9_neg)<-c("names_9_neg",colnames(d9_neg)[2:3])
colnames(d10_pos)<-c("names_10_pos",colnames(d10_pos)[2:3])
colnames(d10_neg)<-c("names_10_neg",colnames(d10_neg)[2:3])
Metakeys_6 <- cbind(d1_pos[,1],d1_neg[,1],
                    d2_pos[,1],d2_neg[,1],
                    d3_pos[,1],d3_neg[,1],
                    d4_pos[,1],d4_neg[,1],
                    d5_pos[,1],d5_neg[,1],
                    d6_pos[,1],d6_neg[,1],
                    d7_pos[,1],d7_neg[,1],
                    d8_pos[,1],d8_neg[,1],
                    d9_pos[,1],d9_neg[,1],
                    d10_pos[,1],d10_neg[,1])

colnames(Metakeys_6)<-c("names_1_pos","names_1_neg",
                        "names_2_pos","names_2_neg",
                        "names_3_pos","names_3_neg",
                        "names_4_pos","names_4_neg",
                        "names_5_pos","names_5_neg",
                        "names_6_pos","names_6_neg",
                        "names_7_pos","names_7_neg",
                        "names_8_pos","names_8_neg",
                        "names_9_pos","names_9_neg",
                        "names_10_pos","names_10_neg")


View(Metakeys_6)
#write.csv(Metakeys_6,"projectes_mes_apoarten_cada_dim_6.csv")


## proves
summary(res.ca)
text_m2_3_year <- text_m2_3
text_m2_3_CLC <- text_m2_3
text_m2_3_year$Year <- as.factor(text_m2_3_year$Year)
text_m2_3_CLC[,871:881]<-NULL
text_m2_3_CLC$CLC <- text_m2_3_year$CLC
res.ca_Year <- CA(text_m2_3_year[,1:871],quali.sup = c(871),ncp = 9)
res.ca_CLC <-  CA(text_m2_3_CLC,quali.sup = c(871),ncp = 9)

plot(res.ca_Year,invisible = c("row","col"),label = "none",axes = 3:4)
plot(res.ca_CLC,invisible = c("row"),label = "none",unselect="grey60",selectCol = 1,axes = c(3,4))

plot(res.ca_Year,choix = "CA" ,invisible=c("col","row"),label = "none",title = "CA Files")


str(text_m2_3_year[,873])
res.ca$quali.sup
## CLustering agafem 9 dim


dim(text_m2_3)


str(text_m2_3[,871:881])
## Amb country
res.ca <- CA(text_m2_3,quali.sup = c(872,873,874,875,876,878,880,881) ,quanti.sup = c(871,877,879),ncp = 9)
## Sense country
text_m2_4<-text_m2_3
text_m2_4$Country <- NULL
str(text_m2_4[,871:880])
res.ca1 <- CA(text_m2_4,quali.sup = c(872,873,874,875,877,879,880) ,quanti.sup = c(871,876,878),ncp = 9)

res.hcpc <- HCPC(res.ca)
res.hcpc1 <- HCPC(res.ca1)
windows()
plot(res.hcpc,ind.names = FALSE, choice = "map",
     draw.tree=FALSE,tree.barplot=FALSE,axes=c(1,3),xlim = c(-3,3))

fviz_cluster(res.hcpc, geom = "point", main = "Factor map")
table(res.hcpc1$data.clust$clust)

res.hcpc$desc.var$test.chi2 #Estadistic per variable quali
res.hcpc$desc.axes$quanti.var ## Quina dim ediferencia millor els clústers

res.hcpc$desc.axes  # Quina dim més important per cada cluster
c6 <- res.hcpc$data$clust


### Que explica cada clúster

#### 1

res.hcpc$desc.axes$quanti$`1`[,c(1,2,3,6)]
plot(res.hcpc,ind.names = FALSE, choice = "map",
     draw.tree=FALSE,tree.barplot=FALSE,axes=c(4,1),xlim = c(-3,3))
#write.csv2(res.hcpc$desc.axes$quanti$`1`[,c(1,2,3,6)],"cluster1.csv")


n1<-sum(res.hcpc$desc.var$frequency$`1`[,6]>=5)
head(res.hcpc$desc.var$frequency$`1`,n1) #paraules que descriuen el cluster
aux_c1 <- as.data.frame(res.hcpc$desc.var$frequency$`1`)
head(aux_c1[order(aux_c1$`Intern freq`,decreasing  = T),],n1)## paraules mes freqüents en el clúster
#write.csv2(head(res.hcpc$desc.var$frequency$`1`,n1),"cluster1_paraules.csv")

head(res.hcpc$desc.var$category$`1`,20)#variables qual.categoriques que descriuen el cluster
#write.csv2(head(res.hcpc$desc.var$category$`1`,20),"cluster1_caracteristiques.csv")

res.hcpc$desc.var$quanti$`1`#variables qual.numeriques que descriuen el cluster

res.hcpc$desc.ind$para$`1`

#### 2
res.hcpc$desc.axes$quanti$`2`[,c(1,2,3,6)]
plot(res.hcpc,ind.names = FALSE, choice = "map",
     draw.tree=FALSE,tree.barplot=FALSE,axes=c(1,6),xlim = c(-3,3))
#write.csv2(res.hcpc$desc.axes$quanti$`2`[,c(1,2,3,6)],"cluster2.csv")


n2<-sum(res.hcpc$desc.var$frequency$`2`[,6]>=5)
head(res.hcpc$desc.var$frequency$`2`,n2) #paraules que descriuen el cluster
aux_c2 <- as.data.frame(res.hcpc$desc.var$frequency$`2`)
head(aux_c2[order(aux_c2$`Intern freq`,decreasing  = T),],n2)## paraules mes freqüents en el clúster
#write.csv2(head(res.hcpc$desc.var$frequency$`2`,n2),"cluster2_paraules.csv")

head(res.hcpc$desc.var$category$`2`,20)#variables qual.categoriques que descriuen el cluster
#write.csv2(head(res.hcpc$desc.var$category$`2`,20),"cluster2_caracteristiques.csv")

res.hcpc$desc.var$quanti$`3`#variables qual.numeriques que descriuen el cluster
#write.csv2(res.hcpc$desc.var$quanti$`3`,"cluster3_caracteristiques_num.csv")

res.hcpc$desc.ind$para$`3`

#### 3
res.hcpc$desc.axes$quanti$`3`[,c(1,2,3,6)]
plot(res.hcpc,ind.names = FALSE, choice = "map",
     draw.tree=FALSE,tree.barplot=FALSE,axes=c(5,2),xlim = c(-3,3))
#write.csv2(res.hcpc$desc.axes$quanti$`3`[,c(1,2,3,6)],"cluster3.csv")


n3<-sum(res.hcpc$desc.var$frequency$`3`[,6]>=5)
head(res.hcpc$desc.var$frequency$`3`,n3) #paraules que descriuen el cluster
aux_c3 <- as.data.frame(res.hcpc$desc.var$frequency$`3`)
head(aux_c3[order(aux_c3$`Intern freq`,decreasing  = T),],n3)## paraules mes freqüents en el clúster
#write.csv2(head(res.hcpc$desc.var$frequency$`3`,n3),"cluster3_paraules.csv")

head(res.hcpc$desc.var$category$`3`,20)#variables qual.categoriques que descriuen el cluster
#write.csv2(head(res.hcpc$desc.var$category$`3`,20),"cluster3_caracteristiques.csv")

res.hcpc$desc.var$quanti$`3`#variables qual.numeriques que descriuen el cluster
#write.csv2(res.hcpc$desc.var$quanti$`3`,"cluster3_caracteristiques_num.csv")

res.hcpc$desc.ind$para$`3`

#### 4
res.hcpc$desc.axes$quanti$`4`[,c(1,2,3,6)]
plot(res.hcpc,ind.names = FALSE, choice = "map",
     draw.tree=FALSE,tree.barplot=FALSE,axes=c(3,8),xlim = c(-3,3))
#write.csv2(res.hcpc$desc.axes$quanti$`4`[,c(1,2,3,6)],"cluster4.csv")


n4<-sum(res.hcpc$desc.var$frequency$`4`[,6]>=5)
head(res.hcpc$desc.var$frequency$`4`,n4) #paraules que descriuen el cluster
aux_c4 <- as.data.frame(res.hcpc$desc.var$frequency$`4`)
head(aux_c4[order(aux_c3$`Intern freq`,decreasing  = T),],n3)## paraules mes freqüents en el clúster
#write.csv2(head(res.hcpc$desc.var$frequency$`4`,n4),"cluster4_paraules.csv")

head(res.hcpc$desc.var$category$`4`,20)#variables qual.categoriques que descriuen el cluster
#write.csv2(head(res.hcpc$desc.var$category$`4`,20),"cluster4_caracteristiques.csv")

res.hcpc$desc.var$quanti$`4`#variables qual.numeriques que descriuen el cluster
#write.csv2(res.hcpc$desc.var$quanti$`4`,"cluster4_caracteristiques_num.csv")

res.hcpc$desc.ind$para$`4`


#### 5
res.hcpc$desc.axes$quanti$`5`[,c(1,2,3,6)]
plot(res.hcpc,ind.names = FALSE, choice = "map",
     draw.tree=FALSE,tree.barplot=FALSE,axes=c(3,2),xlim = c(-3,3))
#write.csv2(res.hcpc$desc.axes$quanti$`5`[,c(1,2,3,6)],"cluster5.csv")


n5<-sum(res.hcpc$desc.var$frequency$`5`[,6]>=5)
head(res.hcpc$desc.var$frequency$`5`,n5) #paraules que descriuen el cluster
aux_c5 <- as.data.frame(res.hcpc$desc.var$frequency$`5`)
head(aux_c5[order(aux_c5$`Intern freq`,decreasing  = T),],n5)## paraules mes freqüents en el clúster
#write.csv2(head(res.hcpc$desc.var$frequency$`5`,n5),"cluster5_paraules.csv")

head(res.hcpc$desc.var$category$`5`,20)#variables qual.categoriques que descriuen el cluster
#write.csv2(head(res.hcpc$desc.var$category$`5`,20),"cluster5_caracteristiques.csv")

res.hcpc$desc.var$quanti$`5`#variables qual.numeriques que descriuen el cluster
#write.csv2(res.hcpc$desc.var$quanti$`5`,"cluster5_caracteristiques_num.csv")

res.hcpc$desc.ind$para$`5`



#### 6
res.hcpc$desc.axes$quanti$`6`[,c(1,2,3,6)]
plot(res.hcpc,ind.names = FALSE, choice = "map",
     draw.tree=FALSE,tree.barplot=FALSE,axes=c(1,8),xlim = c(-3,3))
#write.csv2(res.hcpc$desc.axes$quanti$`6`[,c(1,2,3,6)],"cluster6.csv")


n6<-sum(res.hcpc$desc.var$frequency$`6`[,6]>=5)
head(res.hcpc$desc.var$frequency$`6`,n6) #paraules que descriuen el cluster
aux_c6 <- as.data.frame(res.hcpc$desc.var$frequency$`6`)
head(aux_c6[order(aux_c6$`Intern freq`,decreasing  = T),],n6)## paraules mes freqüents en el clúster
#write.csv2(head(res.hcpc$desc.var$frequency$`6`,n6),"cluster6_paraules.csv")

head(res.hcpc$desc.var$category$`6`,20)#variables qual.categoriques que descriuen el cluster
#write.csv2(head(res.hcpc$desc.var$category$`6`,20),"cluster6_caracteristiques.csv")


aux_c6 <- as.data.frame(res.hcpc$desc.var$category$`6`)
head(aux_c6[order(aux_c6$v.test,decreasing  =F),],n6)


res.hcpc$desc.var$quanti$`6`#variables qual.numeriques que descriuen el cluster
#write.csv2(res.hcpc$desc.var$quanti$`6`,"cluster6_caracteristiques_num.csv")

res.hcpc$desc.ind$para$`6`


# 3d plot
c2_3 <-res.hcpc$call$X$clust
Psi<-res.hcpc$call$X
plot3d(Psi[,1],Psi[,2],Psi[,3],pch=20,
       xlab = "eix1",ylab = "eix2",zlab = "eix3",col = c2_3,
)


