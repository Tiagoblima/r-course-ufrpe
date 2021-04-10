install.packages("tm")
install.packages("slam") 
install.packages("wordcloud")
install.packages('DT')

library(DT)
library(tm)
library(wordcloud)
library(readr)


speech <- read.csv('datasets/mvp.csv', encoding='UTF-8')



corpus <- Corpus(VectorSource(speech$querem))

corpus <- tm_map(corpus, content_transformer(tolower))

corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, removeWords, stopwords('portuguese'))



inspect(corpus)


tdm <-as.matrix(TermDocumentMatrix(corpus))

freq <-sort(rowSums(tdm), decreasing = T)

aux <-subset(freq, freq>2)

barplot(aux, las=2, col = rainbow(10))



ggColors <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
gg.cols <- ggColors(8)
bp.cols<- c("light blue","cornflowerblue", "coral2", brewer.pal(8,"Dark2"))

wordcloud(corpus, 
          scale=c(3.3,0.5), 
          max.words=100, 
          min.freq=1, 
          random.order=FALSE, 
          rot.per=0.40, 
         
          colors=brewer.pal(8,"Dark2"))
