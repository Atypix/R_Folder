library("dplyr")
library(writexl)


# Je charge le fichier de suivi de positions
keywords <- read.csv(file="semrush.csv", header=TRUE, sep=",",encoding = "UTF-8")

# Je garde les colonnes qui m'intéressent 
keywords <- keywords[,c(1,2,4)]

# Je renomme les colonnes
colnames(keywords) = c("keyword","position","searchvolume")

# Je retire les requêtes marque
keywords <- filter(keywords,!grepl("eco",keywords$keyword))


# Je filtre sur les expressions sous la 15ème position
keywords <- filter(keywords,keywords$position <=15)

# Je charge mon fichier de CTR
ctr <- read.csv(file="CTR.txt", header=TRUE, sep=",",encoding = "UTF-8")
ctr <- ctr[,c(1,4,5)]
# J'estime le trafic
trafic <- inner_join(keywords,ctr, by="position")

trafic$trafic <- as.numeric(trafic$searchvolume) * as.numeric(trafic$ctr) 

potentiel <- keywords

# Je simule un gain de position

potentielP2 <- filter(potentiel, potentiel$position == 2)
potentielP2$position <- as.numeric(potentielP2$position) -1

potentielAutre <- filter(potentiel, potentiel$position > 2)
potentielAutre$position <- potentielAutre$position -2

# Je regroupe mes fichiers
potentiel <- rbind(potentielP2,potentielAutre)


# Je calcule le trafic potentiel
traficPotentiel <- inner_join(potentiel,ctr, by="position")

traficPotentiel$trafic <- as.numeric(traficPotentiel$searchvolume)* as.numeric(traficPotentiel$ctr )

# Je calcule les top mots clés à optimiser
top <- inner_join(trafic,traficPotentiel,by="keyword")
top$delta <- top$trafic.y - top$trafic.x 

#Je trie les données
top <- arrange(top, desc(delta))
top <- filter(keywords)

write_xlsx(x = top, path = "daily.xlsx", col_names = TRUE)