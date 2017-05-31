
#Paqueterías

#Cargamos las paqueterías necesarias para la implementación.
library(topicmodels) #Para la función LDA
library(plyr) #Para mapear funciones a objetos tipo lista
library(doSNOW) #Paralelizar
library(doParallel) #Paralelizar procesos
library(tm) #Para minería de texto
library(tidytext) #Manipulación de datos.
library(dplyr) #Manipulación de datos
library(ggplot2) #Visualización de resultados
`

#Asignamos la ruta en la que se encuentra nuestro archivo con texto
cname <- file.path("~", "Documentos", "Multivariado y Categoricos","años extropy")   #Ruta en donde está el archivo a aplicar TM

#Convertimos el archivo de texto en un documento de R para manipular y aplicar minería de texto.
docs <- Corpus(DirSource(cname))  
summary(docs) 
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, tolower)   

#Necesitamos quitar palabras no útiles para el análisis
domains <- function(path){
  require(stringr)
  doms <- str_split(readLines(path), pattern = " ")
  domains=character(length(doms))
  for(i in 1:length(doms))
    domains[i]=doms[[i]]  
  return(domains)
}
removedWords=c("can","url","list","now","years","good","right","now","without","also","much","stil","part","html","way","just","know","get","may","might","wrote","don","messageid","even","see","subject","like","one","one","make","think","spike","{","}","will", "fwd","re","re:","extropy","extropychat","extropy-chat","extropians","extrop","httplistsextropyorgmailmanlistinfocgiextropychat","can", "say","one","way","use",
               "also","howev","tell","will",
               "much","need","take","tend","even",
               "like","particular","rather","said",
               "get","well","make","ask","come","end",
               "first","two","help","often","may",
               "might","see","someth","thing","point",
               "post","look","right","now","think","‘ve ",
               "‘re ","anoth","put","set","new","good",
               "want","sure","kind","larg","yes,","day","etc",
               "quit","sinc","attempt","lack","seen","awar",
               "littl","ever","moreov","though","found","abl","don",
               "enough","far","earli","away","achiev","draw",
               "last","never","brief","bit","entir","brief","url","wrote",
               "great","lot")
dom=domains("~/Escritorio/direcciones.txt")
toremove=c(removedWords,dom)

toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " " , x))})
removehttp <- content_transformer(function(x,pattern) { return (gsub("http[A-Za-z]+ ", "", x))})
removehtml <- content_transformer(function(x,pattern) { return (gsub("html[A-Za-z]+ ", "", x))})


#Las removemos
docs <- tm_map(docs,removehttp)
docs <- tm_map(docs,removehtml)
docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, removeWords, toremove)  
docs <- tm_map(docs,stemDocument)
docs <- tm_map(docs, PlainTextDocument) 

#Creamos la matriz de términos del documento.
dtm <- DocumentTermMatrix(docs)  
dtm

#Quitamos terminos cuya frecuencia es mínima para el análisis
rowTotals <- apply(dtm,1,sum)
dtm.new   <- dtm[rowTotals> 0, ]
dtm.new


#Creamos las variables necesarias para el modelo.

# Número de topicos con los que iniciar
k <- 5

# fijamos los atributos del modelo
control_LDA_VEM <-
  list(estimate.alpha = TRUE, alpha = 50/k, estimate.beta = TRUE,
       verbose = 0, prefix = tempfile(), save = 0, keep = 0,
       seed = as.integer(100), nstart = 1, best = TRUE,
       var = list(iter.max = 200, tol = 10^-6),
       em = list(iter.max = 200, tol = 10^-4),
       initialize = "random")

topics<-5


#Inicializamos los cores para la paralelización.
# Inicializando cores
sfInit(parallel=TRUE, cpus=detectCores(logical=TRUE)-1, type="SOCK") # for snowfall
cl <- makeCluster(detectCores(logical=TRUE)-1, type = "SOCK") # for snow
registerDoSNOW(cl) # for snow

# Se mandan los datos y librerías a todos los cores
sfExport("dtm.new", "control_LDA_VEM") # for snowfall
sfLibrary(topicmodels) # for snowfall

clusterEvalQ(cl, library(topicmodels)) # for snow
clusterExport(cl, c("dtm.new", "control_LDA_VEM")) # for snow

#Obtenemos el modelo
wrapper <- function (d) topicmodels:::LDA(dtm, control = control_LDA_VEM, d)
best.model.PLYRP <<- llply(seq, function(d){topicmodels:::LDA(dtm.new, control = control_LDA_VEM, d)}, .parallel = TRUE)
stopCluster(cl)
sfStop() 

#Obtenemos la matriz de tópicos y probabilidades
ap_topics <- tidy(best.model.PLYRP[[1]], matrix = "beta")

#Obtenemos las 5 palabras más probables de cada tópico.
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()