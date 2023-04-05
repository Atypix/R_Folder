library("wordcloud")
library("RColorBrewer")
library("tm")
library("dplyr")

#Je charge le fichier
keywords <- read.csv(file="cupkiller-gobelets.csv", header=TRUE, sep=",",encoding = "UTF-8")

# Je filtre pour éviter le bruit
#keywords <- filter(keywords, keywords$Search.Volume >300)


# Je crée un corpus
dfCorpus = Corpus(VectorSource(keywords$Keyword)) 
dtm <- TermDocumentMatrix(dfCorpus)

m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)

mots <- data.frame(word = names(v))

# Je retire les termes contenus dans ma recherche
mots <- filter(mots,!grepl("gobelets",mots$word))

# je calcule le score
for ( i in 1:nrow(mots))
{
  listeExpressionAvec <- filter(keywords, grepl(paste("^",mots$word[i]," | ",mots$word[i]," | ",mots$word[i],"$",sep=""),keywords$Keyword))
  score <- as.integer(summarise(listeExpressionAvec,volumeRecherche = sum(listeExpressionAvec$Search.Volume))[1])
  
  mots$score[i] <- score
}

# Je génère mon tagcloud
set.seed(1234)
wordcloud(words = mots$word, freq = mots$score, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))