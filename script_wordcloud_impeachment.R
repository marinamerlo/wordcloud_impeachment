#instalando os pacotes necess�rios pra fazer a word
install.packages("SnowballC")
library(SnowballC)
install.packages('tm')
library(tm)
#abrindo o arquivo
dados <- read.csv("C:/Users/Marina/Desktop/planilha discursos impeachment.csv", sep=";", stringsAsFactors=FALSE)

#subset dos discursos de mulheres, homens, votos "sim" e votos "n�o.
#vamos desconsiderar as absten��es
dados_f <- subset(dados, G�nero=="F")
dados_m <- subset(dados, G�nero=="M")
dados_s <- subset(dados, X=="Sim")
dados_n <- subset(dados, X=="N�o")

#come�ando com mulheres
#limpando o arquivo e preparando para a an�lise
#vetor somente com as falas
falas_f <- Corpus(VectorSource(dados_f$Fala))
#deixando como um arquivo de texto �nico
falas_f  <- tm_map(falas_f , PlainTextDocument)
#removendo pontua��es, palavras sem significado e palavras que sabemos que se repetem bastante mas n�o dizem respeito ao voto
#Presidente � citado por mais de 300 deputados pois iniciam sua fala com 'Sr. Presidente", se dirigindo a Eduardo Cunha.
falas_f  <- tm_map(falas_f, removePunctuation)
falas_f <- tm_map(falas_f, removeWords, stopwords("pt"))
falas_f  <- tm_map(falas_f, removeWords, c( "sras", "srs", "para", "vai", "voto","presidente"))

#fazendo o gr�fico
graf.mulheres <- wordcloud(falas_f,scale=c(4,0.6), max.words=200, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(11, "PRGn"))

#repetindo para homens
falas_m <- Corpus(VectorSource(dados_m$Fala))
falas_m <- tm_map(falas_m , PlainTextDocument)
falas_m  <- tm_map(falas_m, removePunctuation)
falas_m <- tm_map(falas_m, removeWords, stopwords("pt"))
falas_m  <- tm_map(falas_m, removeWords, c( "sras", "srs", "para", "vai", "voto","presidente"))
graf.homens <- wordcloud(falas_m,scale=c(4,0.6), max.words=200, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(11, "PRGn"))

#repetindo para sim e n�o
falas_s <- Corpus(VectorSource(dados_s$Fala))
falas_s <- tm_map(falas_s , PlainTextDocument)
falas_s  <- tm_map(falas_s, removePunctuation)
falas_s <- tm_map(falas_s, removeWords, stopwords("pt"))
falas_s  <- tm_map(falas_s, removeWords, c( "sras", "srs", "para", "vai", "voto","presidente"))
graf.sim <- wordcloud(falas_s,scale=c(4,0.6), max.words=200, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(11, "PRGn"))

falas_n <- Corpus(VectorSource(dados_n$Fala))
falas_n <- tm_map(falas_n, PlainTextDocument)
falas_n  <- tm_map(falas_n, removePunctuation)
falas_n <- tm_map(falas_n, removeWords, stopwords("pt"))
falas_n  <- tm_map(falas_n, removeWords, c( "sras", "srs", "para", "vai", "voto","presidente"))
graf.nao <- wordcloud(falas_n,scale=c(4,0.6), max.words=200, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(11, "PRGn"))

#plotando todos os gr�ficos juntos

#sim e n�o
par(bg='black')
par(mfrow=c(1,2))
graf.sim <- wordcloud(falas_s,scale=c(4,0.2), max.words=200, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(11, "PuBu"))
graf.nao <- wordcloud(falas_n,scale=c(3,0.2), max.words=300, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(11, "YlOrRd"))

#homens e mulheres
par(bg='black')
par(mfrow=c(1,2))
graf.homens <-wordcloud(falas_m,scale=c(4,0.2), random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(11, "YlGn"))
graf.homens
graf.mulheres <-wordcloud(falas_f,scale=c(3,0.2), random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(11, "BuPu"))
