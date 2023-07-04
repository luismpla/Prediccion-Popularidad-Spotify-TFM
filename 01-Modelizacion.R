setwd("C:/Users/Usuario/Desktop/TFM/Spotify and Genius Track Dataset")

#### Paquetes ####
library(parallel)
library(doParallel)
library(plyr)
library(dummies)
library(ggplot2)
library(naniar)
library(MASS)
library(caret)
library(tidyverse)
library(randomForest)
library(skimr)
library(corrplot)
library(pROC)
library(stats)


source("Funciones/funcion steprepetido.R")
source("Funciones/cruzadas avnnet y lin.R")
source("Funciones/cruzada rf continua.R")
source("Funciones/cruzada gbm continua.R")
source("Funciones/cruzada xgboost continua.R")
source("Funciones/cruzada SVM continua lineal.R")
source("Funciones/cruzada SVM continua polinomial.R")
source("Funciones/cruzada SVM continua RBF.R")
source("Funciones/cruzadas ensamblado continuas fuente.R")


# Paralelización
GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # number of cores, convention to leave 1 core for OS
registerDoParallel(cluster) # register the parallel processing


# Lectura ficheros
spotify_final1 <- read.csv("spotify_final1.csv")
glimpse(spotify_final1)


# Sampleo
set.seed(12345)

spotify_sample1 <-
  spotify_final1 |> 
  group_by(track_popularity) |> 
  slice_sample(prop = 0.2) |> 
  ungroup()


#### Preparación: estandar y dummies ####

dput(names(spotify_sample1))

# c("X", "track_popularity", "track_acousticness", "track_country", 
# "track_danceability", "track_disc_number", "track_energy", "track_instrumentalness", 
# "track_key", "track_liveness", "track_loudness", "track_mode", 
# "track_speechiness", "track_tempo", "track_time_signature", "track_number", 
# "track_valence", "artist_popularity", "artist_followers", "album_type", 
# "album_tracks", "lyric_mean_syllables_word", "lyric_mean_words_sentence", 
# "lyric_n_sentences", "lyric_n_words", "lyric_sentence_similarity", 
# "lyric_vocabulary_wealth", "track_num_countries", "market_Africa", 
# "market_America", "market_Asia", "market_Europa", "market_Oceania", 
# "genre_rap", "genre_pop", "genre_rock", "genre_house", "genre_indie", 
# "genre_dance", "genre_folk", "genre_classical", "genre_hiphop", 
# "genre_electro", "track_duration_min", "artist_followers_categories", 
# "album_release_centurie", "album_release_year")


dput(names(spotify_sample1)[sapply(spotify_sample1, is.numeric)])
dput(names(spotify_sample1)[sapply(spotify_sample1, is.character)])

vardep <- "track_popularity"

listconti <- c("track_acousticness", "track_danceability", 
               "track_energy", "track_instrumentalness", "track_key", "track_liveness", 
               "track_loudness", "track_speechiness", "track_tempo", "track_time_signature", 
               "track_number", "track_valence", "artist_popularity", "artist_followers", 
               "album_tracks", "lyric_mean_syllables_word", "lyric_mean_words_sentence", 
               "lyric_n_sentences", "lyric_n_words", "lyric_sentence_similarity", 
               "lyric_vocabulary_wealth", "track_num_countries", "track_duration_min", "album_release_year")

listclass <- c("track_country", "track_disc_number", "track_mode", "album_type", 
               "artist_followers_categories", "album_release_centurie", "market_Africa", 
               "market_America", "market_Asia", "market_Europa", "market_Oceania", 
               "genre_rap", "genre_pop", "genre_rock", "genre_house", "genre_indie", 
               "genre_dance", "genre_folk", "genre_classical", "genre_hiphop", 
               "genre_electro")


# Me quedo solo con las variables de interés (listconti, listclass y vardep)
spotify<-spotify_sample1[,c(listconti,listclass,vardep)]

# Borro observaciones con NA en el caso de que haya
spotify<-na.omit(spotify)

# Estandarización de las variables continuas
means <-apply(spotify[,listconti],2,mean,na.rm=TRUE)
sds<-sapply(spotify[,listconti],sd,na.rm=TRUE)

spotify2<-scale(spotify[,listconti], center = means, scale = sds)

spotify<-data.frame(cbind(spotify2,spotify[,c(listclass,vardep)]))

# Obtener dummies de las variables categóricas
spotifybis<-dummy.data.frame(spotify, listclass, sep = ".")


# Lista de Frecuencias de las variables categóricas

# Niveles con pocas observaciones no deben ser tenidos en cuenta 
# en modelos de machine learning para evitar sobreajuste

frecu<-ldply(spotify[,listclass],function(x) t(rbind(names(table(x)),table(x))))
names(frecu)<-c("variable","nivel","frecuencia")
frecu$frecuencia<-as.numeric(frecu$frecuencia)

frecu

# No hay problema, hay muchas observaciones de cada categoría
# Quitamos position o division con menos de 20

frecu20<-frecu[frecu$frecuencia<20,]

# Obtengo listado de los niveles en el mismo formato que las dummies,
# con separador .
frecu20$dum<-paste(frecu20$variable,frecu20$nivel,sep=".")
listamal<-dput(frecu20$dum)

# Borro las dummies de spotifybis que coinciden con la lista
spotifybis[,listamal]<-NULL



# Preparo matrices X e Y para la selección de variables

archivo1<-spotifybis

dput(names(archivo1))

vardep<-"track_popularity"
nombres1<-  
  c("track_acousticness", "track_danceability", "track_energy", 
    "track_instrumentalness", "track_key", "track_liveness", "track_loudness", 
    "track_speechiness", "track_tempo", "track_time_signature", "track_number", 
    "track_valence", "artist_popularity", "artist_followers", "album_tracks", 
    "lyric_mean_syllables_word", "lyric_mean_words_sentence", "lyric_n_sentences", 
    "lyric_n_words", "lyric_sentence_similarity", "lyric_vocabulary_wealth", 
    "track_num_countries", "track_duration_min", "album_release_year", 
    "track_country.FI", "track_country.BE", "track_country.AR", "track_disc_number.unique", 
    "track_disc_number.various", "track_mode.mayor", "track_mode.minor", 
    "album_type.album", "album_type.single", "album_type.compilation", 
    "artist_followers_categories.0_100K", "artist_followers_categories.100K_500K", 
    "artist_followers_categories.500K_1M", "artist_followers_categories.1M_5M", 
    "artist_followers_categories.>5M", "album_release_centurie.S_XX", 
    "album_release_centurie.S_XXI", "market_Africa.0", "market_Africa.1", 
    "market_America.0", "market_America.1", "market_Asia.0", "market_Asia.1", 
    "market_Europa.0", "market_Europa.1", "market_Oceania.0", "market_Oceania.1", 
    "genre_rap.0", "genre_rap.1", "genre_pop.0", "genre_pop.1", "genre_rock.0", 
    "genre_rock.1", "genre_house.0", "genre_house.1", "genre_indie.0", 
    "genre_indie.1", "genre_dance.0", "genre_dance.1", "genre_folk.0", 
    "genre_folk.1", "genre_classical.0", "genre_classical.1", "genre_hiphop.0", 
    "genre_hiphop.1", "genre_electro.0", "genre_electro.1")
y<-archivo1[,vardep]
x<-archivo1[,nombres1]



#### Selección de variables ####

# Selección de variables criterio AIC
listap<-steprepetido(data=archivo1,vardep=c("track_popularity"),
                     listconti=nombres1,
                     sinicio=12345,sfinal=12385,porcen=0.7,criterio="AIC")
tablap<-listap[[1]]
# Se eligen las selecciones que tengan el menor y el mayor nº de variables en conjunto
dput(listap[[2]][[10]]) #32 variables
dput(listap[[2]][[21]]) #39 variables


# Selección de variables criterio BIC
listap2<-steprepetido(data=archivo1,vardep=c("track_popularity"),
                      listconti=nombres1,
                      sinicio=12345,sfinal=12385,porcen=0.7,criterio="BIC")
tablap2<-listap2[[1]]
# Se eligen las selecciones que tengan el menor y el mayor nº de variables en conjunto
dput(listap2[[2]][[4]]) #18 variables
dput(listap2[[2]][[3]]) #24 variables



##### Construcción de modelos #####

#Parametros necesarios
null<-lm(track_popularity~1, data=archivo1)

#MODELO 1 -- StepAIC_menor (32)
full1<-lm(track_popularity~artist_popularity+track_speechiness+
            genre_classical.0+track_num_countries+album_tracks+
            track_country.AR+artist_followers_categories.0_100K+
            track_danceability+lyric_vocabulary_wealth+genre_dance.0+
            genre_hiphop.0+track_time_signature+track_duration_min+
            album_release_centurie.S_XX+track_valence+lyric_sentence_similarity+
            genre_house.0+market_Asia.0+track_country.BE+genre_rap.0+
            track_number+album_release_year+genre_pop.0+track_liveness+
            genre_electro.0+lyric_n_words+track_disc_number.unique+
            market_Europa.0+album_type.album+market_America.1+track_key+market_Africa.0,
          data = archivo1)
ModeloStepAIC_menor<-step(null, scope=list(lower=null, upper=full1), direction="both")

#MODELO 2 -- StepAIC_mayor (39)
full2<-lm(track_popularity~artist_popularity+track_speechiness+
            genre_classical.0+track_num_countries+album_tracks+
            track_country.AR+artist_followers_categories.0_100K+
            track_danceability+lyric_vocabulary_wealth+market_America.0+
            genre_dance.0+genre_hiphop.0+track_time_signature+
            track_duration_min+album_release_centurie.S_XX+
            track_valence+lyric_sentence_similarity+genre_house.0+
            market_Asia.0+track_country.BE+genre_rap.0+track_number+
            album_release_year+genre_pop.0+track_liveness+genre_electro.0+
            lyric_n_words+track_disc_number.unique+lyric_n_sentences+
            track_tempo+artist_followers_categories.100K_500K+
            market_Europa.0+album_type.album+track_key+lyric_mean_words_sentence+
            genre_indie.0+market_Africa.0+market_Oceania.0+track_instrumentalness,
          data = archivo1)
ModeloStepAIC_mayor<-step(null, scope=list(lower=null, upper=full2), direction="both")

#MODELO 3 -- BackAIC_menor (32)
full3<-lm(track_popularity~artist_popularity+track_speechiness+
            genre_classical.0+track_num_countries+album_tracks+
            track_country.AR+artist_followers_categories.0_100K+
            track_danceability+lyric_vocabulary_wealth+genre_dance.0+
            genre_hiphop.0+track_time_signature+track_duration_min+
            album_release_centurie.S_XX+track_valence+lyric_sentence_similarity+
            genre_house.0+market_Asia.0+track_country.BE+genre_rap.0+
            track_number+album_release_year+genre_pop.0+track_liveness+
            genre_electro.0+lyric_n_words+track_disc_number.unique+
            market_Europa.0+album_type.album+market_America.1+track_key+market_Africa.0,
          data = archivo1)
ModeloBackAIC_menor<-step(full3, scope=list(lower=null, upper=full3), direction="backward")

#MODELO 4 -- BackAIC_mayor (39)
full4<-lm(track_popularity~artist_popularity+track_speechiness+
            genre_classical.0+track_num_countries+album_tracks+
            track_country.AR+artist_followers_categories.0_100K+
            track_danceability+lyric_vocabulary_wealth+market_America.0+
            genre_dance.0+genre_hiphop.0+track_time_signature+
            track_duration_min+album_release_centurie.S_XX+
            track_valence+lyric_sentence_similarity+genre_house.0+
            market_Asia.0+track_country.BE+genre_rap.0+track_number+
            album_release_year+genre_pop.0+track_liveness+genre_electro.0+
            lyric_n_words+track_disc_number.unique+lyric_n_sentences+
            track_tempo+artist_followers_categories.100K_500K+
            market_Europa.0+album_type.album+track_key+lyric_mean_words_sentence+
            genre_indie.0+market_Africa.0+market_Oceania.0+track_instrumentalness,
          data = archivo1)
ModeloBackAIC_mayor<-step(full4, scope=list(lower=null, upper=full4), direction="backward")

#MODELO 5 -- StepBIC_menor (18)
full5<-lm(track_popularity~artist_popularity+track_speechiness+
            genre_classical.0+track_num_countries+album_tracks+
            track_country.AR+artist_followers_categories.0_100K+
            track_danceability+lyric_vocabulary_wealth+album_type.single+
            genre_dance.0+track_time_signature+track_duration_min+
            lyric_sentence_similarity+genre_house.0+market_Asia.0+
            market_America.1+genre_rap.1,
          data = archivo1)
ModeloStepBIC_menor<-step(null, scope=list(lower=null, upper=full5), direction="both",k=log(nrow(archivo1)))

#MODELO 6 -- StepBIC_mayor (24)
full6<-lm(track_popularity~artist_popularity+track_speechiness+
            genre_classical.0+track_num_countries+album_tracks+
            track_country.AR+artist_followers_categories.0_100K+
            track_danceability+lyric_vocabulary_wealth+album_type.single+
            genre_dance.0+genre_hiphop.0+track_time_signature+
            track_duration_min+album_release_centurie.S_XX+
            lyric_sentence_similarity+genre_house.0+market_Asia.0+
            track_country.BE+track_number+market_America.1+album_release_year+
            market_Europa.0+genre_rap.1,
          data = archivo1)
ModeloStepBIC_mayor<-step(null, scope=list(lower=null, upper=full6), direction="both",k=log(nrow(archivo1)))

#MODELO 7 -- BackBIC_menor (18)
full7<-lm(track_popularity~artist_popularity+track_speechiness+
            genre_classical.0+track_num_countries+album_tracks+
            track_country.AR+artist_followers_categories.0_100K+
            track_danceability+lyric_vocabulary_wealth+album_type.single+
            genre_dance.0+track_time_signature+track_duration_min+
            lyric_sentence_similarity+genre_house.0+market_Asia.0+
            market_America.1+genre_rap.1,
          data = archivo1)
ModeloBackBIC_menor<-step(full7, scope=list(lower=null, upper=full7), direction="backward",k=log(nrow(archivo1)))

#MODELO 8 -- BackBIC_mayor (24)
full8<-lm(track_popularity~artist_popularity+track_speechiness+
            genre_classical.0+track_num_countries+album_tracks+
            track_country.AR+artist_followers_categories.0_100K+
            track_danceability+lyric_vocabulary_wealth+album_type.single+
            genre_dance.0+genre_hiphop.0+track_time_signature+
            track_duration_min+album_release_centurie.S_XX+
            lyric_sentence_similarity+genre_house.0+market_Asia.0+
            track_country.BE+track_number+market_America.1+album_release_year+
            market_Europa.0+genre_rap.1,
          data = archivo1)
ModeloBackBIC_mayor<-step(full8, scope=list(lower=null, upper=full8), direction="backward",k=log(nrow(archivo1)))


##### Regresión lineal #####

medias1<-cruzadalin(data=archivo1,
                    vardep="track_popularity",listconti=
                      c("artist_popularity", "track_speechiness", "genre_classical.0", 
                        "track_num_countries", "track_number", "track_country.AR", 
                        "artist_followers_categories.0_100K", "album_tracks", 
                        "genre_rap.0", "track_duration_min", "album_type.album", 
                        "market_Asia.0", "genre_dance.0", "lyric_vocabulary_wealth", 
                        "track_danceability", "track_disc_number.unique", 
                        "lyric_n_words", "market_America.1", 
                        "lyric_sentence_similarity", "genre_electro.0", 
                        "track_country.BE", "market_Europa.0", "track_time_signature", 
                        "track_liveness", "genre_pop.0", "genre_house.0", 
                        "track_valence", "genre_hiphop.0"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias1[[1]]$modelo="Modelo 1"

medias2<-cruzadalin(data=archivo1,
                    vardep="track_popularity",listconti=
                      c("artist_popularity", "track_speechiness", "genre_classical.0", 
                          "track_num_countries", "track_number", "track_country.AR", 
                          "artist_followers_categories.0_100K", "album_tracks", 
                          "genre_rap.0", "track_duration_min", "album_type.album", 
                          "market_Asia.0", "genre_dance.0", "lyric_vocabulary_wealth", 
                          "track_danceability", "track_disc_number.unique", 
                          "lyric_n_words", "lyric_n_sentences", "market_America.0", 
                          "genre_electro.0", "lyric_sentence_similarity", 
                          "track_country.BE", "artist_followers_categories.100K_500K", 
                          "track_tempo", "market_Europa.0", "track_liveness", 
                          "genre_pop.0", "track_time_signature", "genre_house.0", 
                          "track_valence", "lyric_mean_words_sentence", 
                          "track_instrumentalness", "market_Oceania.0"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias2[[1]]$modelo="Modelo 2"

medias3<-cruzadalin(data=archivo1,
                    vardep="track_popularity",listconti=
                      c("artist_popularity", "track_speechiness", "genre_classical.0", 
                          "track_num_countries", "album_tracks", "track_country.AR", 
                          "artist_followers_categories.0_100K", "track_danceability", 
                          "lyric_vocabulary_wealth", "genre_dance.0", 
                          "track_time_signature", "track_duration_min", 
                          "album_release_centurie.S_XX", "track_valence", 
                          "lyric_sentence_similarity", "genre_house.0", 
                          "market_Asia.0", "track_country.BE", "genre_rap.0", 
                          "track_number", "album_release_year", "genre_pop.0", 
                          "track_liveness", "genre_electro.0", "lyric_n_words", 
                          "track_disc_number.unique", "market_Europa.0", 
                          "album_type.album", "market_America.1"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias3[[1]]$modelo="Modelo 3"

medias4<-cruzadalin(data=archivo1,
                    vardep="track_popularity",listconti=
                      c("artist_popularity", "track_speechiness", "genre_classical.0", 
                          "track_num_countries", "album_tracks", "track_country.AR", 
                          "artist_followers_categories.0_100K", "track_danceability", 
                          "lyric_vocabulary_wealth", "market_America.0", 
                          "genre_dance.0", "track_time_signature", 
                          "track_duration_min", "album_release_centurie.S_XX", 
                          "track_valence", "lyric_sentence_similarity", 
                          "genre_house.0", "market_Asia.0", "track_country.BE", 
                          "genre_rap.0", "track_number", "album_release_year", 
                          "genre_pop.0", "track_liveness", "genre_electro.0", 
                          "lyric_n_words", "track_disc_number.unique", 
                          "lyric_n_sentences", "track_tempo", 
                          "artist_followers_categories.100K_500K", 
                          "market_Europa.0", "album_type.album", 
                          "lyric_mean_words_sentence", "market_Oceania.0", 
                          "track_instrumentalness"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias4[[1]]$modelo="Modelo 4"

medias5<-cruzadalin(data=archivo1,
                    vardep="track_popularity",listconti=
                      c("artist_popularity", "track_speechiness", 
                          "genre_classical.0", "track_num_countries", 
                          "album_tracks", "track_country.AR", 
                          "artist_followers_categories.0_100K", "album_type.single", 
                          "genre_rap.1", "market_Asia.0", "track_duration_min", 
                          "genre_dance.0", "track_danceability", 
                          "lyric_vocabulary_wealth", "market_America.1", 
                          "lyric_sentence_similarity", "genre_house.0", 
                          "track_time_signature"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias5[[1]]$modelo="Modelo 5"

medias6<-cruzadalin(data=archivo1,
                    vardep="track_popularity",listconti=
                      c("artist_popularity", "track_speechiness", "genre_classical.0", 
                          "track_num_countries", "track_number", "track_country.AR", 
                          "artist_followers_categories.0_100K", "album_tracks", 
                          "genre_rap.1", "track_duration_min", "album_type.single", 
                          "market_Asia.0", "genre_dance.0", "lyric_vocabulary_wealth", 
                          "track_danceability", "market_America.1", 
                          "lyric_sentence_similarity", "genre_house.0", 
                          "track_country.BE", "market_Europa.0", "track_time_signature"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias6[[1]]$modelo="Modelo 6"

medias7<-cruzadalin(data=archivo1,
                    vardep="track_popularity",listconti=
                      c("artist_popularity", "track_speechiness", "genre_classical.0", 
                          "track_num_countries", "album_tracks", "track_country.AR", 
                          "artist_followers_categories.0_100K", "track_danceability", 
                          "lyric_vocabulary_wealth", "album_type.single", 
                          "genre_dance.0", "track_time_signature", 
                          "track_duration_min", "lyric_sentence_similarity", 
                          "genre_house.0", "market_Asia.0", "market_America.1", 
                          "genre_rap.1"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias7[[1]]$modelo="Modelo 7"

medias8<-cruzadalin(data=archivo1,
                    vardep="track_popularity",listconti=
                      c("artist_popularity", "track_speechiness", "genre_classical.0", 
                          "track_num_countries", "album_tracks", "track_country.AR", 
                          "artist_followers_categories.0_100K", "track_danceability", 
                          "lyric_vocabulary_wealth", "album_type.single", 
                          "genre_dance.0", "track_time_signature", "track_duration_min", 
                          "album_release_centurie.S_XX", "lyric_sentence_similarity", 
                          "genre_house.0", "market_Asia.0", "track_country.BE", 
                          "track_number", "market_America.1", "album_release_year", 
                          "market_Europa.0", "genre_rap.1"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias8[[1]]$modelo="Modelo 8"


union1<-rbind(medias1[[1]],medias2[[1]],medias3[[1]],medias4[[1]],
              medias5[[1]],medias6[[1]],medias7[[1]],medias8[[1]])

union1$RMSE<-sqrt(union1$error)

par(cex.axis=1, las=1, mar=c(5, 4, 1, 2) + 0.3)
boxplot(data=union1,col="#1ed760",RMSE~modelo)

# Nos quedamos con el Modelo 6
vardep="track_popularity"
variables<-c("artist_popularity", "track_speechiness", "genre_classical.0", 
               "track_num_countries", "track_number", "track_country.AR", 
               "artist_followers_categories.0_100K", "album_tracks", 
               "genre_rap.1", "track_duration_min", "album_type.single", 
               "market_Asia.0", "genre_dance.0", "lyric_vocabulary_wealth", 
               "track_danceability", "market_America.1", "lyric_sentence_similarity", 
               "genre_house.0", "track_country.BE", "market_Europa.0", 
               "track_time_signature")

data2<-archivo1[,c(variables,vardep)]



#### Tuneo de algoritmos ####

##### Redes #####

# Cálculo de nodos para redes: 
# 21 variables input, 17281 obs , con 20 obs por parámetro implica 17281/20=864 parámetros max.
# (864-1)/(21+2)=38 nodos max.

set.seed(12345)

control<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE)

avnnetgrid <- expand.grid(size=c(3,10,17,24,31,38),decay=c(0.1,0.01,0.001),bag=FALSE)

redavnnet <- train(track_popularity~., 
                   data=data2,method="avNNet",linout = TRUE,maxit=100,
                   trControl=control,tuneGrid=avnnetgrid)

# Voy incorporando los resultados a completo
completo<-data.frame()
completo<-rbind(completo,redavnnet$results)

completo<-completo[order(completo$RMSE),]


ggplot(completo, aes(x=factor(size), y=RMSE, 
                     color=factor(decay))) +
  geom_point(position=position_dodge(width=0.5),size=3)



medias9<-cruzadaavnnet(data=data2,
                          vardep="track_popularity",listconti=
                            c("artist_popularity", "track_speechiness", "genre_classical.0", 
                              "track_num_countries", "track_number", "track_country.AR", 
                              "artist_followers_categories.0_100K", "album_tracks", 
                              "genre_rap.1", "track_duration_min", "album_type.single", 
                              "market_Asia.0", "genre_dance.0", "lyric_vocabulary_wealth", 
                              "track_danceability", "market_America.1", "lyric_sentence_similarity", 
                              "genre_house.0", "track_country.BE", "market_Europa.0", 
                              "track_time_signature"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=10,
                          repeticiones=5,itera=100,size=c(31),decay=c(0.01))

medias9[[1]]$modelo="Redes"



##### Bagging #####

# La función cruzadarf permite plantear bagging PARA VDEP CONTINUA
# (para bagging hay que poner mtry=numero de variables independientes)
# No se puede plotear oob

rfbis<-randomForest(track_popularity~.,
                    data=data2,
                    mtry=21,ntree=200,sampsize=300,nodesize=10,replace=TRUE,importance=T)

plot(rfbis$mse)

# Con 50 iteraciones suficiente porque ya se normaliza



#Estudio de observaciones mínimas por nodo

control<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE) 

set.seed(12345)

nodo10<-train(track_popularity~.,
               data=data2,
               method="rf",trControl=control,
               metry=21,linout = TRUE,ntree=200,nodesize=10,replace=TRUE)
nodo10

nodo20<-train(track_popularity~.,
               data=data2,
               method="rf",trControl=control,
               metry=21,linout = TRUE,ntree=200,nodesize=20,replace=TRUE)
nodo20

nodo30<-train(track_popularity~.,
               data=data2,
               method="rf",trControl=control,
               metry=21,linout = TRUE,ntree=200,nodesize=30,replace=TRUE)
nodo30

nodo40<-train(track_popularity~.,
               data=data2,
               method="rf",trControl=control,
               metry=21,linout = TRUE,ntree=200,nodesize=40,replace=TRUE)
nodo40

# Nos quedamos con nodesize=10


medias10<-cruzadarf(data=data2,
                       vardep="track_popularity",listconti=
                         c("artist_popularity", "track_speechiness", "genre_classical.0", 
                           "track_num_countries", "track_number", "track_country.AR", 
                           "artist_followers_categories.0_100K", "album_tracks", 
                           "genre_rap.1", "track_duration_min", "album_type.single", 
                           "market_Asia.0", "genre_dance.0", "lyric_vocabulary_wealth", 
                           "track_danceability", "market_America.1", "lyric_sentence_similarity", 
                           "genre_house.0", "track_country.BE", "market_Europa.0", 
                           "track_time_signature"),
                       listclass=c(""),grupos=4,sinicio=1234,repe=10,
                       nodesize=10,mtry=21,ntree=50,replace=TRUE)

medias10[[1]]$modelo="Bagging"



##### Random Forest #####

# TUNEADO DE MTRY CON CARET

# Para tuneado del mtry ponemos muchas variables inicialmente pues 
# el randomforest es bastante robusto a variables malas. 
# Esto no excluye una buena selección de variables, 
# pero por simplificar lo hacemos así en este ejemplo.

set.seed(12345)
rfgrid<-expand.grid(mtry=c(3,5,7,10,13,15,17,19,21))

control<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE)

rf<- train(track_popularity~.,data=data2,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=300,nodesize=10,replace=TRUE,
           importance=TRUE)

rf

# El mejor mtry es 7 con un RMSE de 9.710212

# IMPORTANCIA DE VARIABLES

final<-rf$finalModel

tabla<-as.data.frame(importance(final))
tabla<-tabla[order(-tabla$IncNodePurity),]
tabla


par(mar = c(12, 5, 0, 0))
barplot(tabla$IncNodePurity,names.arg=rownames(tabla), las = 2, cex.names = 0.8)


# PARA PLOTEAR EL ERROR OOB A MEDIDA QUE AVANZAN LAS ITERACIONES
# SE USA DIRECTAMENTE EL PAQUETE randomForest

set.seed(12345)

rfbis<-randomForest(track_popularity~.,
                    data=data2,
                    mtry=7,ntree=200,nodesize=10,replace=TRUE)

par(mar = c(4, 4, 1, 1) + 0.1, las = 1, cex.names = 0.8)
plot(rfbis$mse)



# Sacamos medias 

medias11<-cruzadarf(data=data2,
                       vardep="track_popularity",listconti=
                       c("artist_popularity", "track_speechiness", "genre_classical.0", 
                         "track_num_countries", "track_number", "track_country.AR", 
                         "artist_followers_categories.0_100K", "album_tracks", 
                         "genre_rap.1", "track_duration_min", "album_type.single", 
                         "market_Asia.0", "genre_dance.0", "lyric_vocabulary_wealth", 
                         "track_danceability", "market_America.1", "lyric_sentence_similarity", 
                         "genre_house.0", "track_country.BE", "market_Europa.0", 
                         "track_time_signature"),
                       listclass=c(""),grupos=4,sinicio=1234,repe=10,
                       nodesize=10,mtry=7,ntree=50,replace=TRUE,sampsize=1000)

medias11[[1]]$modelo="Random Forest"




##### Gradient Boosting #####

# TUNEADO DE GRADIENT BOOSTING CON CARET

# Caret permite tunear estos parámetros básicos:
#  
# 	shrinkage (parámetro v de regularización, mide la velocidad de ajuste, a menor v, más lento y necesita más iteraciones, pero es más fino en el ajuste)
# 	n.minobsinnode: tamaño máximo de nodos finales (el principal parámetro que mide la complejidad)
# 	n.trees=el número de iteraciones (árboles)
# 	interaction.depth (2 para árboles binarios)

set.seed(12345)

control<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE)

gbmgrid<-expand.grid(shrinkage=c(0.001,0.01,0.03,0.05,0.1,0.2),
                     n.minobsinnode=c(10,20,30,50),
                     n.trees=c(500,1000,2000,5000),
                     interaction.depth=c(2))


gbm<-train(track_popularity~artist_popularity+track_speechiness+genre_classical.0+
             track_num_countries+track_number+track_country.AR+
             artist_followers_categories.0_100K+album_tracks+
             genre_rap.1+track_duration_min+album_type.single+
             market_Asia.0+genre_dance.0+lyric_vocabulary_wealth+
             track_danceability+market_America.1+lyric_sentence_similarity+
             genre_house.0+track_country.BE+market_Europa.0+
             track_time_signature,
            data=data2,
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="gaussian", bag.fraction=1,verbose=FALSE)

gbm
plot(gbm)

# El mejor valor para el modelo es n.trees = 5000, interaction.depth = 2, shrinkage = 0.03
# and n.minobsinnode = 20.


# ESTUDIO DE EARLY STOPPING
# Probamos a fijar algunos parámetros para ver como evoluciona
# en función de las iteraciones

gbmgrid<-expand.grid(shrinkage=c(0.03),
                     n.minobsinnode=c(20),
                     n.trees=c(50,100,300,500,1000,2000,3000,5000),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE) 

gbm<- train(track_popularity~artist_popularity+track_speechiness+genre_classical.0+
              track_num_countries+track_number+track_country.AR+
              artist_followers_categories.0_100K+album_tracks+
              genre_rap.1+track_duration_min+album_type.single+
              market_Asia.0+genre_dance.0+lyric_vocabulary_wealth+
              track_danceability+market_America.1+lyric_sentence_similarity+
              genre_house.0+track_country.BE+market_Europa.0+
              track_time_signature,
            data=data2,
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="gaussian", bag.fraction=1,verbose=FALSE)

gbm
plot(gbm)

# Nos quedamos con 1000 árboles



# IMPORTANCIA DE VARIABLES
summary(gbm)
tabla<-summary(gbm)
par(mar = c(12, 5, 0, 0) + 0.1)
barplot(tabla$rel.inf,names.arg=row.names(tabla), las = 2, cex.names = 0.8)


medias12<-cruzadagbm(data=data2, 
                     vardep="track_popularity",listconti=
                       c("artist_popularity", "track_speechiness", "genre_classical.0", 
                         "track_num_countries", "track_number", "track_country.AR", 
                         "artist_followers_categories.0_100K", "album_tracks", 
                         "genre_rap.1", "track_duration_min", "album_type.single", 
                         "market_Asia.0", "genre_dance.0", "lyric_vocabulary_wealth", 
                         "track_danceability", "market_America.1", "lyric_sentence_similarity", 
                         "genre_house.0", "track_country.BE", "market_Europa.0", 
                         "track_time_signature"),
                       listclass=c(""),grupos=4,sinicio=1234,repe=10,
                       n.minobsinnode=20,shrinkage=0.03,n.trees=1000,interaction.depth=2)

medias12[[1]]$modelo="Gradient Boosting"





##### Xgboost #####

# TUNEADO DE XGBOOST CON CARET

# Caret permite tunear estos parámetros básicos:
# nrounds (# Boosting Iterations)=número de iteraciones
# max_depth (Max Tree Depth)=profundida máxima de los árboles
# eta (Shrinkage)=parámetro v gradient boosting
# gamma (Minimum Loss Reduction)=gamma
# cte regularización. Dejar a 0 por defecto
# colsample_bytree (Subsample Ratio of Columns) 
# % Sorteo variables antes de cada árbol, 
# al estilo de random forest pero antes del árbol, no en cada nodo. Dejar
# a 1 por defecto.
# min_child_weight (Minimum Sum of Instance Weight).
# observaciones mínimas en el nodo final. Similar al minobsinnode del gbm.
# # subsample (Subsample Percentage)
# % Sorteo de observaciones antes de cada árbol , al estilo de random forest.
# Dejar a 1 por defecto.

set.seed(12345)

xgbmgrid<-expand.grid(
  min_child_weight=c(5,10,20,30),
  eta=c(0.001,0.01,0.03,0.05,0.1,0.2),
  nrounds=c(100,500,1000,5000),
  max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE)

xgbm<- train(track_popularity~artist_popularity+track_speechiness+genre_classical.0+
               track_num_countries+track_number+track_country.AR+
               artist_followers_categories.0_100K+album_tracks+
               genre_rap.1+track_duration_min+album_type.single+
               market_Asia.0+genre_dance.0+lyric_vocabulary_wealth+
               track_danceability+market_America.1+lyric_sentence_similarity+
               genre_house.0+track_country.BE+market_Europa.0+
               track_time_signature,
             data=data2,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)

xgbm

plot(xgbm)


# The final values used for the model were nrounds = 1000, max_depth = 6, 
# eta = 0.01, gamma = 0, colsample_bytree = 1, min_child_weight = 5 and subsample = 1.



# ESTUDIO DE EARLY STOPPING
# Probamos a fijar algunos parámetros para ver como evoluciona
# en función de las iteraciones

xgbmgrid<-expand.grid(eta=c(0.01),
                      min_child_weight=c(5),
                      nrounds=c(50,100,300,500,1000,2000,3000,5000),
                      max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

set.seed(12345)
control<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE) 

xgbm<- train(track_popularity~artist_popularity+track_speechiness+genre_classical.0+
               track_num_countries+track_number+track_country.AR+
               artist_followers_categories.0_100K+album_tracks+
               genre_rap.1+track_duration_min+album_type.single+
               market_Asia.0+genre_dance.0+lyric_vocabulary_wealth+
               track_danceability+market_America.1+lyric_sentence_similarity+
               genre_house.0+track_country.BE+market_Europa.0+
               track_time_signature,
             data=data2,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)

plot(xgbm)


# IMPORTANCIA DE VARIABLES
varImp(xgbm)
plot(varImp(xgbm))


medias13<-cruzadaxgbm(data=data2,
                       vardep="track_popularity",listconti=
                         c("artist_popularity", "track_speechiness", "genre_classical.0", 
                           "track_num_countries", "track_number", "track_country.AR", 
                           "artist_followers_categories.0_100K", "album_tracks", 
                           "genre_rap.1", "track_duration_min", "album_type.single", 
                           "market_Asia.0", "genre_dance.0", "lyric_vocabulary_wealth", 
                           "track_danceability", "market_America.1", "lyric_sentence_similarity", 
                           "genre_house.0", "track_country.BE", "market_Europa.0", 
                           "track_time_signature"),
                       listclass=c(""),grupos=4,sinicio=1234,repe=10,
                       min_child_weight=5,eta=0.01,nrounds=500,max_depth=6,
                       gamma=0,colsample_bytree=1,subsample=1,
                       alpha=0,lambda=0)

medias13[[1]]$modelo="Xgboost"



# Comparamos los modelos de árboles

union1<-rbind(medias10[[1]],medias11[[1]],medias12[[1]],medias13[[1]])

union1$RMSE<-sqrt(union1$error)

par(cex.axis=1, las=1, mar=c(5, 4, 1, 2) + 0.3)
boxplot(data=union1,col="#1ed760",RMSE~modelo)






##### SVM #####

#  SVM LINEAL: SOLO PARÁMETRO C

# Vamos probando varios parámetros de C que nos den el mejor accuracy
# Aumentar C implica menor sesgo y mayor sobreajuste. Su rango depende mucho de los datos

set.seed(12345)
SVMlingrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10))

control<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE)

SVMlin<- train(track_popularity~artist_popularity+track_speechiness+genre_classical.0+
                 track_num_countries+track_number+track_country.AR+
                 artist_followers_categories.0_100K+album_tracks+
                 genre_rap.1+track_duration_min+album_type.single+
                 market_Asia.0+genre_dance.0+lyric_vocabulary_wealth+
                 track_danceability+market_America.1+lyric_sentence_similarity+
                 genre_house.0+track_country.BE+market_Europa.0+
                 track_time_signature,
            data=data2,
            method="svmLinear",trControl=control,
            tuneGrid=SVMlingrid,verbose=FALSE)

SVMlin

data_SVMlin<-as.data.frame(SVMlin$results)

ggplot(data_SVMlin, aes(x=factor(C), y=RMSE))+ 
  geom_point(position=position_dodge(width=0.5),size=3)


#  SVM RBF: PARÁMETROS C, sigma

# RBF: Aumentar gamma en la función RBF implica menor sesgo y mayor sobreajuste.

SVMrbfgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10),
                     sigma=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10))

control<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE) 

SVMrbf<- train(track_popularity~artist_popularity+track_speechiness+genre_classical.0+
                 track_num_countries+track_number+track_country.AR+
                 artist_followers_categories.0_100K+album_tracks+
                 genre_rap.1+track_duration_min+album_type.single+
                 market_Asia.0+genre_dance.0+lyric_vocabulary_wealth+
                 track_danceability+market_America.1+lyric_sentence_similarity+
                 genre_house.0+track_country.BE+market_Europa.0+
                 track_time_signature,
            data=data2,
            method="svmRadial",trControl=control,
            tuneGrid=SVMrbfgrid,verbose=FALSE)

SVMrbf

dat<-as.data.frame(SVMrbf$results)

ggplot(dat, aes(x=factor(C), y=RMSE, 
                color=factor(sigma)))+ 
  geom_point(position=position_dodge(width=0.5),size=3)



medias14<-cruzadaSVM(data=data2,
                     vardep="track_popularity",listconti=
                       c("artist_popularity", "track_speechiness", "genre_classical.0", 
                         "track_num_countries", "track_number", "track_country.AR", 
                         "artist_followers_categories.0_100K", "album_tracks", 
                         "genre_rap.1", "track_duration_min", "album_type.single", 
                         "market_Asia.0", "genre_dance.0", "lyric_vocabulary_wealth", 
                         "track_danceability", "market_America.1", "lyric_sentence_similarity", 
                         "genre_house.0", "track_country.BE", "market_Europa.0", 
                         "track_time_signature"),
                     listclass=c(""),grupos=4,sinicio=1234,repe=10,
                     C=0.1)

medias14[[1]]$modelo="SVM Lineal"


medias15<-cruzadaSVMRBF(data=data2,
                        vardep="track_popularity",listconti=
                          c("artist_popularity", "track_speechiness", "genre_classical.0", 
                            "track_num_countries", "track_number", "track_country.AR", 
                            "artist_followers_categories.0_100K", "album_tracks", 
                            "genre_rap.1", "track_duration_min", "album_type.single", 
                            "market_Asia.0", "genre_dance.0", "lyric_vocabulary_wealth", 
                            "track_danceability", "market_America.1", "lyric_sentence_similarity", 
                            "genre_house.0", "track_country.BE", "market_Europa.0", 
                            "track_time_signature"),
                        listclass=c(""),grupos=4,sinicio=1234,repe=10,
                        C=2,sigma=0.01)

medias15[[1]]$modelo="SVM Radial"


unionSVM<-rbind(medias14[[1]],medias15[[1]])

unionSVM$RMSE<-sqrt(unionSVM$error)

par(cex.axis=1, las=1, mar=c(5, 4, 1, 2) + 0.3)
boxplot(data=unionSVM,col="#1ed760",RMSE~modelo)

# Vemos que el mejor boxplot es el Radial




# Comparamos todos los algoritmos

medias6[[1]]$modelo="Regresión"

uniontotal<-rbind(medias6[[1]],medias9[[1]],medias10[[1]],medias11[[1]],
                  medias12[[1]],medias13[[1]],medias14[[1]],medias15[[1]])

uniontotal$RMSE<-sqrt(uniontotal$error)

par(cex.axis=1, las=1, mar=c(5, 4, 1, 2) + 0.3)
boxplot(data=uniontotal,col="#1ed760",RMSE~modelo)

uni<-uniontotal
uni$modelo <- with(uni,
                   reorder(modelo,RMSE,mean))

par(cex.axis=1, las=1, mar=c(5, 4, 1, 2) + 0.3)
boxplot(data=uni,RMSE~modelo,col="#1ed760")





#### Ensamblado ####

set.seed(12345)
archivo<-data2
vardep<-"track_popularity"
listconti<-c("artist_popularity", "track_speechiness", "genre_classical.0", 
             "track_num_countries", "track_number", "track_country.AR", 
             "artist_followers_categories.0_100K", "album_tracks", 
             "genre_rap.1", "track_duration_min", "album_type.single", 
             "market_Asia.0", "genre_dance.0", "lyric_vocabulary_wealth", 
             "track_danceability", "market_America.1", "lyric_sentence_similarity", 
             "genre_house.0", "track_country.BE", "market_Europa.0", 
             "track_time_signature")
listclass<-c("")
grupos<-4
sinicio<-1234
repe<-10

# APLICACIÓN CRUZADAS PARA ENSAMBLAR

medias1<-cruzadalin(data=archivo,
                    vardep=vardep,listconti=listconti,
                    listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe)

medias1bis<-as.data.frame(medias1[1])
medias1bis$modelo<-"Regresión"
predi1<-as.data.frame(medias1[2])
predi1$reg<-predi1$pred


medias2<-cruzadaavnnet(data=archivo,
                       vardep=vardep,listconti=listconti,
                       listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                       repeticiones=5,itera=100,size=c(31),decay=c(0.01))

medias2bis<-as.data.frame(medias2[1])
medias2bis$modelo<-"Redes"
predi2<-as.data.frame(medias2[2])
predi2$avnnet<-predi2$pred


medias3<-cruzadarf(data=archivo,
                   vardep=vardep,listconti=listconti,
                   listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                   nodesize=10,mtry=21,ntree=50,replace=TRUE)

medias3bis<-as.data.frame(medias3[1])
medias3bis$modelo<-"Bagging"
predi3<-as.data.frame(medias3[2])
predi3$bagm<-predi3$pred


medias4<-cruzadarf(data=archivo,
                   vardep=vardep,listconti=listconti,
                   listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                   nodesize=10,mtry=7,ntree=50,replace=TRUE,sampsize=1000)

medias4bis<-as.data.frame(medias4[1])
medias4bis$modelo<-"Random Forest"
predi4<-as.data.frame(medias4[2])
predi4$rf<-predi4$pred


medias5<-cruzadagbm(data=archivo,
                    vardep=vardep,listconti=listconti,
                    listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                    n.minobsinnode=20,shrinkage=0.03,n.trees=1000,interaction.depth=2)

medias5bis<-as.data.frame(medias5[1])
medias5bis$modelo<-"Gradient Boosting"
predi5<-as.data.frame(medias5[2])
predi5$gbm<-predi5$pred


medias6<-cruzadaxgbm(data=archivo,
                     vardep=vardep,listconti=listconti,
                     listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                     min_child_weight=5,eta=0.01,nrounds=500,max_depth=6,
                     gamma=0,colsample_bytree=1,subsample=1,
                     alpha=0,lambda=0)

medias6bis<-as.data.frame(medias6[1])
medias6bis$modelo<-"Xgboost"
predi6<-as.data.frame(medias6[2])
predi6$xgbm<-predi6$pred


medias7<-cruzadaSVM(data=archivo,
                    vardep=vardep,listconti=listconti,
                    listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                    C=0.1)

medias7bis<-as.data.frame(medias7[1])
medias7bis$modelo<-"SVM Lineal"
predi7<-as.data.frame(medias7[2])
predi7$svmlin<-predi7$pred


medias8<-cruzadaSVMRBF(data=archivo,
                       vardep=vardep,listconti=listconti,
                       listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                       C=2,sigma=0.01)

medias8bis<-as.data.frame(medias8[1])
medias8bis$modelo<-"SVM Radial"
predi8<-as.data.frame(medias8[2])
predi8$svmrbf<-predi8$pred


union1<-rbind(medias1bis,medias2bis,medias3bis,medias4bis,
              medias5bis,medias6bis,medias7bis,medias8bis)

union1$RMSE<-sqrt(union1$error)

par(cex.axis=1, las=1, mar=c(5, 4, 1, 2) + 0.3)
boxplot(data=union1,col="#1ed760",RMSE~modelo)


# CONSTRUCCIÓN DE TODOS LOS ENSAMBLADOS
# SE UTILIZARÁN LOS ARCHIVOS SURGIDOS DE LAS FUNCIONES LLAMADOS predi1, predi2...

unipredi<-cbind(predi1,predi2,predi3,predi4,predi5,predi6,predi7,predi8)

# Esto es para eliminar columnas duplicadas
unipredi<- unipredi[, !duplicated(colnames(unipredi))]

# Construcción de ensamblados

unipredi$predi9<-(unipredi$reg+unipredi$avnnet)/2
unipredi$predi10<-(unipredi$reg+unipredi$bagm)/2
unipredi$predi11<-(unipredi$reg+unipredi$rf)/2
unipredi$predi12<-(unipredi$reg+unipredi$gbm)/2
unipredi$predi13<-(unipredi$reg+unipredi$xgbm)/2
unipredi$predi14<-(unipredi$reg+unipredi$svmrbf)/2
unipredi$predi15<-(unipredi$avnnet+unipredi$bagm)/2
unipredi$predi16<-(unipredi$avnnet+unipredi$rf)/2
unipredi$predi17<-(unipredi$avnnet+unipredi$gbm)/2
unipredi$predi18<-(unipredi$avnnet+unipredi$xgbm)/2
unipredi$predi19<-(unipredi$avnnet+unipredi$svmrbf)/2
unipredi$predi20<-(unipredi$bagm+unipredi$rf)/2
unipredi$predi21<-(unipredi$bagm+unipredi$gbm)/2
unipredi$predi22<-(unipredi$bagm+unipredi$xgbm)/2
unipredi$predi23<-(unipredi$bagm+unipredi$svmrbf)/2
unipredi$predi24<-(unipredi$rf+unipredi$gbm)/2
unipredi$predi25<-(unipredi$rf+unipredi$xgbm)/2
unipredi$predi26<-(unipredi$rf+unipredi$svmrbf)/2
unipredi$predi27<-(unipredi$gbm+unipredi$xgbm)/2
unipredi$predi28<-(unipredi$gbm+unipredi$svmrbf)/2
unipredi$predi29<-(unipredi$xgbm+unipredi$svmrbf)/2

unipredi$predi30<-(unipredi$avnnet+unipredi$bagm+unipredi$rf)/3
unipredi$predi31<-(unipredi$avnnet+unipredi$bagm+unipredi$gbm)/3
unipredi$predi32<-(unipredi$avnnet+unipredi$bagm+unipredi$xgbm)/3
unipredi$predi33<-(unipredi$avnnet+unipredi$bagm+unipredi$svmrbf)/3
unipredi$predi34<-(unipredi$avnnet+unipredi$rf+unipredi$gbm)/3
unipredi$predi35<-(unipredi$avnnet+unipredi$rf+unipredi$xgbm)/3
unipredi$predi36<-(unipredi$avnnet+unipredi$rf+unipredi$svmrbf)/3
unipredi$predi37<-(unipredi$avnnet+unipredi$gbm+unipredi$xgbm)/3
unipredi$predi38<-(unipredi$avnnet+unipredi$gbm+unipredi$svmrbf)/3
unipredi$predi39<-(unipredi$avnnet+unipredi$xgbm+unipredi$svmrbf)/3
unipredi$predi40<-(unipredi$bagm+unipredi$rf+unipredi$gbm)/3
unipredi$predi41<-(unipredi$bagm+unipredi$rf+unipredi$xgbm)/3
unipredi$predi42<-(unipredi$bagm+unipredi$rf+unipredi$svmrbf)/3
unipredi$predi43<-(unipredi$bagm+unipredi$gbm+unipredi$xgbm)/3
unipredi$predi44<-(unipredi$bagm+unipredi$gbm+unipredi$svmrbf)/3
unipredi$predi45<-(unipredi$bagm+unipredi$xgbm+unipredi$svmrbf)/3
unipredi$predi46<-(unipredi$rf+unipredi$gbm+unipredi$xgbm)/3
unipredi$predi47<-(unipredi$rf+unipredi$gbm+unipredi$svmrbf)/3
unipredi$predi48<-(unipredi$rf+unipredi$xgbm+unipredi$svmrbf)/3
unipredi$predi49<-(unipredi$gbm+unipredi$xgbm+unipredi$svmrbf)/3

unipredi$predi50<-(unipredi$avnnet+unipredi$bagm+unipredi$rf+unipredi$gbm)/4
unipredi$predi51<-(unipredi$avnnet+unipredi$bagm+unipredi$rf+unipredi$xgbm)/4
unipredi$predi52<-(unipredi$avnnet+unipredi$bagm+unipredi$gbm+unipredi$xgbm)/4
unipredi$predi53<-(unipredi$avnnet+unipredi$rf+unipredi$gbm+unipredi$xgbm)/4
unipredi$predi54<-(unipredi$bagm+unipredi$rf+unipredi$gbm+unipredi$xgbm)/4

unipredi$predi55<-(unipredi$avnnet+unipredi$bagm+unipredi$rf+unipredi$gbm+unipredi$xgbm)/5


# Listado de modelos a considerar

dput(names(unipredi))

listado<-c("reg", "avnnet", "bagm", "rf", "gbm", "xgbm", "svmlin", "svmrbf", 
           "predi9", "predi10", "predi11", "predi12", "predi13", "predi14", 
           "predi15", "predi16", "predi17", "predi18", "predi19", "predi20", 
           "predi21", "predi22", "predi23", "predi24", "predi25", "predi26", 
           "predi27", "predi28", "predi29", "predi30", "predi31", "predi32", 
           "predi33", "predi34", "predi35", "predi36", "predi37", "predi38", 
           "predi39", "predi40", "predi41", "predi42", "predi43", "predi44", 
           "predi45", "predi46", "predi47", "predi48", "predi49", "predi50", 
           "predi51", "predi52", "predi53", "predi54", "predi55")

repeticiones<-nlevels(factor(unipredi$Rep))
unipredi$Rep<-as.factor(unipredi$Rep)
unipredi$Rep<-as.numeric(unipredi$Rep)


medias0<-data.frame(c())

for (prediccion in listado)
{
  paso <-unipredi[,c("obs",prediccion,"Rep")]
  paso$error<-(paso[,c(prediccion)]-paso$obs)^2
  paso<-paso %>%group_by(Rep)%>%summarize(error=mean(error))   
  paso$modelo<-prediccion  
  medias0<-rbind(medias0,paso) 
} 


# Finalmente boxplot 

medias0$RMSE<-sqrt(medias0$error)

par(cex.axis=0.8, las=2, mar=c(5, 4, 1, 2) + 0.3)
boxplot(data=medias0,col="#1ed760",RMSE~modelo)


# PRESENTACION TABLA MEDIAS

tablamedias<-medias0 %>%
  group_by(modelo) %>%
  summarize(RMSE=mean(RMSE))     

tablamedias<-tablamedias[order(tablamedias$RMSE),]

tablamedias

# ORDENACIÓN DEL FACTOR MODELO POR LAS MEDIAS EN ERROR
# PARA EL GRÁFICO

medias0$modelo <- with(medias0,
                       reorder(modelo,RMSE, mean))

par(cex.axis=0.8, las=2, mar=c(5, 4, 1, 2) + 0.3)
boxplot(data=medias0,col="#1ed760",RMSE~modelo)


# Se pueden escoger listas pero el factor hay que pasarlo a character
# para que no salgan en el boxplot todos los niveles del factor

# Hacemos un boxplot con los 5 modelos con menor tasa de fallos

listadobis<-c("predi51", "predi35", "predi30", "predi55", "predi50")

listadobis<-c("reg", "avnnet", "bagm", "rf", "gbm", "xgbm", "svmlin", "svmrbf",
              "predi51", "predi35", "predi30", "predi55", "predi50")

medias0$modelo<-as.character(medias0$modelo)

mediasver<-medias0[medias0$modelo %in% listadobis,]

mediasver$modelo <- with(mediasver,
                         reorder(modelo,RMSE, mean))

par(cex.axis=1, las=1, mar=c(5, 4, 1, 2) + 0.3)
boxplot(data=mediasver,col="#1ed760",RMSE~modelo)






#### Predicción ####

#MEJOR MODELO -----> RANDOM FOREST

SVMgrid<-expand.grid(C=c(20),
                     sigma=c(0.005))

control<-trainControl(method = "repeatedcv",
                      number=10, repeats = 60,savePredictions = "all")

SVMrbft<- train(data=transformadas,Valor~CUT+Edad+Remates+Pases90+Remates90+
                  CAP90+Bundesliga+LFP+PremierLeague+SerieA+Contrato0+
                  Contrato1+Contrato2+Contrato3+Equipo1+Equipo2+
                  SQRTDD90+OPTEdadH+OPTFilasBlancoH+OPTGC90M+OPTGNPH+
                  OPTGNPMH+OPTA90MH+OPTMinutosH+OPTMinutosMH+OPTPJH+
                  OPTPR90H+OPTPPAPMH+OPTPPPLMH+OPTPTLH+OPTTA90L+
                  OPTTAP90H+OPTxA90H,
                method="svmRadial",trControl=control,
                tuneGrid=SVMgrid,verbose=FALSE)

SVMrbft


setwd("C:/Users/pablo/OneDrive/Escritorio/TFM")
library(caret)
prediccion <- read_xlsx("C:/Users/pablo/OneDrive/Escritorio/TFM/predict.xlsx")

valor <-predict(SVMrbft,prediccion)

valor <- as.data.frame(valor)

prediccion <- cbind(valor, prediccion)

prediccion$Valor <- prediccion$valor

prediccion$valor <- NULL

writexl::write_xlsx(prediccion, "valorFinal.xlsx")

