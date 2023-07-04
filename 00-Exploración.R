#### Paquetes ####

# Importamos los paquetes necesarios
rm(list = ls())

# Paquetes de manipulación y visualización de datos
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(scales)
library(tidyr)
library(dplyr)
library(stringr)
library(sysfonts)
library(showtext)

# Paquetes de limpieza y preprocesamiento de datos
library(outliers)
library(skimr)

# Paquetes de modelado de datos
library(tidymodels)
library(caret)
library(rsample)
library(yardstick)

# Paquetes de estadísticas y correlación
library(corrr)
library(corrplot)

# Paquetes de lectura y escritura de datos
library(readr)
library(haven)

# Paquetes de otros propósitos
library(knitr)
library(zoo)
library(purrr)
library(rpart.plot)


#### Análisis exploratorio inicial ####

# Importamos los ficheros de datos
spotify_albums_bruto <- read.csv("bbdd_bruto/spotify_albums.csv")
spotify_artists_bruto <- read.csv("bbdd_bruto/spotify_artists.csv")
spotify_tracks_bruto <- read.csv("bbdd_bruto/spotify_tracks.csv")
lyrics_features_bruto <- read.csv("bbdd_bruto/lyrics_features.csv")

glimpse(spotify_albums_bruto)
glimpse(spotify_artists_bruto)
glimpse(spotify_tracks_bruto)
glimpse(lyrics_features_bruto)


##### Limpieza datasets #####

# Renombre y descarte de variables en SPOTIFY_ALBUMS
spotify_albums <- spotify_albums_bruto |> 
  rename(album_id = id, album_name = name, album_release_date = release_date,
         album_tracks = total_tracks) |> 
  select(-X, -artist_id, -external_urls, -href, -uri, -type, -images, 
         -track_name_prev, -available_markets, -release_date_precision)

# Renombre y descarte de variables en SPOTIFY_ARTISTS
spotify_artists <- spotify_artists_bruto |> 
  rename(artist_id = id, artist_name = name, artist_followers = followers,
         artist_genres = genres) |> 
  select(-X, -type, -track_name_prev)

# Renombre y descarte de variables en SPOTIFY_TRACKS
spotify_tracks <- spotify_tracks_bruto |> 
  select(-X, -analysis_url, -href, -preview_url, -track_href, -uri, 
         -type, -track_name_prev) |> 
  rename_all(~ paste0("track_", .)) |> 
  rename(track_number = track_track_number, artist_id = track_artists_id, 
         album_id = track_album_id)

# Dentro de spotify_tracks, la variable artist_id viene detallada en listas 
# ya que una pista de reproducción suele ser parte de más de un artista. 
# Esto hará que no pueda localizar la pista con su artista correspondiente 
# al unir los dos dataset, por lo que nos quedaremos con el primero que nos 
# aparezca de cada lista, que normalmente es el artista principal
spotify_tracks$artist_id <- sub("\\['([^']*)'.*", "\\1", spotify_tracks$artist_id)

# Renombre y descarte de variables en LYRICS_FEATURES
lyrics_features <- lyrics_features_bruto |> 
  select(-X) |> 
  rename_all(~ paste0("lyric_", .)) |> 
  rename(track_id = lyric_track_id)

# Unión de todos los dataset a través de la función merge
spotify1 <- merge(spotify_tracks, spotify_artists, by = c("artist_id"), all.x = TRUE)
spotify2 <- merge(spotify1, spotify_albums, by = c("album_id"), all.x = TRUE)
spotify3 <- merge(spotify2, lyrics_features, by = c("track_id"), all.x = TRUE)
spotify_fusion <- spotify3

# La columna track_id aparece duplicada en cada uno de los dataset, 
# por lo que nos quedamos tan solo con una de ellas
spotify_fusion <- spotify_fusion |> 
  select(-track_id, -track_id.y) |> 
  rename(track_id = track_id.x)

spotify_fusion <- spotify_fusion |>
  mutate(across(where(is.integer), as.numeric)) |> 
  mutate(album_release_date = as.Date(album_release_date)) |> 
  filter(!is.na(lyric_n_words)) |> 
  filter(!is.na(album_release_date))

spotify_explor <- spotify_fusion

spotify_fusion |> skim()


#### Exploración individual de las variables ####

##### Variable objetivo #####

###### track_popularity ######

summary(spotify_fusion$track_popularity)

ggplot(spotify_fusion, aes(track_popularity)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(track_popularity)), 
               color = '#FF0000', linetype = 'dashed', size = 0.6)

spotify_fusion <- spotify_fusion |> select(c(track_popularity, everything()))


##### Variables cualitativas #####

###### track_available_markets ###### 

spotify_fusion |> 
  count(track_available_markets, sort = TRUE) |> 
  mutate(porc = 100*n/sum(n))

spotify_fusion$track_num_countries <- sapply(spotify_fusion$track_available_markets, 
                                             function(x){
                                               length((unique(unlist(strsplit(x, ",")))))
                                             })

spotify_fusion |> 
  count(track_num_countries, sort = TRUE) |> 
  mutate(porc = 100*n/sum(n))

num_country_popularity <- spotify_fusion %>%
  group_by(track_num_countries) %>%
  summarize(mean_popularity = mean(track_popularity)) |>  
  arrange(desc(mean_popularity)) |>  print()

ggplot(spotify_fusion,
       aes(x = track_num_countries, y = track_popularity)) + geom_point()

# Definir los intervalos para la variable "track_num_countries"
intervals_track_num_countries <- seq(0, max(spotify_fusion$track_num_countries), by = 10)

# Calcular la media de "track_popularity" para cada intervalo
means_track_num_countries <- tapply(spotify_fusion$track_popularity,
                cut(spotify_fusion$track_num_countries, 
                    breaks = intervals_track_num_countries),
                mean)

# Crear un gráfico de barras para visualizar las medias por intervalo
barplot(means_track_num_countries, 
        xlab = "Número de países", 
        ylab = "Popularidad promedio del track",
        main = "Relación entre el número de países y la popularidad del track",
        col = "#71EB9D")

# Importamos el Excel con todos los países y nos quedamos solo con el código ISO y 
# su continente correspondiente
library(readxl)
iso_3166_1 <- read_excel("ISO-3166-Countries-with-Regional-Codes.xlsx") |>
  select("alpha-2", "region") |> rename("code" = "alpha-2", "continent" = "region")

# Obtenemos los nombres de los continentes
continentes <- unique(iso_3166_1$continent)

# Creamos una lista vacía para almacenar los códigos de países por continente
continente <- list()

# Bucle para recorrer los continentes y asignar los códigos de países correspondientes
for (i in 1:length(continentes)) {
  continente[[continentes[i]]] <- 
    unique(iso_3166_1$code[iso_3166_1$continent == continentes[i]])
}

# Ahora sacamos vectores de los códigos de cada continente, 
# para posteriormente crear las columnas por continente
codigos_africa <- continente$Africa
codigos_america <- continente$Americas
codigos_asia <- continente$Asia
codigos_europa <- continente$Europe
codigos_oceania <- continente$Oceania


# Columna del mercado de África
spotify_fusion$market_Africa <- ifelse(
  grepl("DZ|AO|NA|BJ|BW|IO|BF|BI|CV|CM|CF|TD|KM|CG|CD|CI|DJ|EG|
        GQ|ER|SZ|ET|TF|GA|GM|GH|GN|GW|KE|LS|LR|LY|MG|MW|ML|MR|
        MU|YT|MA|MZ|NA|NE|NG|RE|RW|SH|ST|SN|SC|SL|SO|ZA|SS|SD|
        TZ|TG|TN|UG|EH|ZM|ZW", 
        spotify_fusion$track_available_markets), 1, 0)

# Columna del mercado de América
spotify_fusion$market_America <- ifelse(
  grepl("AI|AG|AR|AW|BS|BB|BZ|BM|BO|BQ|BV|BR|CA|KY|CL|CO|CR|CU|
        CW|DM|DO|EC|SV|FK|GF|GL|GD|GP|GT|GY|HT|HN|JM|MQ|MX|MS|
        NI|PA|PY|PE|PR|BL|KN|LC|MF|PM|VC|SX|GS|SR|TT|TC|US|UY|
        VE|VG|VI", 
        spotify_fusion$track_available_markets), 1, 0)

# Columna del mercado de Asia
spotify_fusion$market_Asia <- ifelse(
  grepl("AF|AM|AZ|BH|BD|BT|BN|KH|CN|CY|GE|HK|IN|ID|IR|IQ|IL|JP|
        JO|KZ|KP|KR|KW|KG|LA|LB|MO|MY|MV|MN|MM|NP|OM|PK|PS|PH|
        QA|SA|SG|LK|SY|TW|TJ|TH|TL|TR|TM|AE|UZ|VN|YE", 
        spotify_fusion$track_available_markets), 1, 0)

# Columna del mercado de Europa
spotify_fusion$market_Europa <- ifelse(
  grepl("AX|AL|AD|AT|BY|BE|BA|BG|HR|CZ|DK|EE|FO|FI|FR|DE|GI|GR|
        GG|VA|HU|IS|IE|IM|IT|JE|LV|LI|LT|LU|MT|MD|MC|ME|NL|MK|
        NO|PL|PT|RO|RU|SM|RS|SK|SI|ES|SJ|SE|CH|UA|GB", 
        spotify_fusion$track_available_markets), 1, 0)

# Columna del mercado de Oceania
spotify_fusion$market_Oceania <- ifelse(
  grepl("AS|AU|CX|CC|CK|FJ|PF|GU|HM|KI|MH|FM|NR|NC|NZ|NU|NF|MP|
        PW|PG|PN|WS|SB|TK|TO|TV|UM|VU|WF", 
        spotify_fusion$track_available_markets), 1, 0)

market_cols <- spotify_fusion[, grep("^market_", colnames(spotify_fusion))]
proporcion_market <- colMeans(market_cols)
proporcion_market <- sort(proporcion_market, decreasing = TRUE)
market_prop <- data.frame(
  market = names(proporcion_market),
  proporcion_market = proporcion_market)
print(market_prop)

spotify_fusion  |> 
  pivot_longer(starts_with("market_"), names_to = "market", values_to = "market_value") |>
  filter(market_value == 1) |> 
  group_by(market) |> 
  summarize(mean_popularity = mean(track_popularity)) |> 
  arrange(desc(mean_popularity))


###### track_country ###### 

spotify_fusion |> 
  count(track_country, sort = TRUE) |> 
  mutate(porc = 100*n/sum(n)) |> print()

# Agrupa los datos por país y calcula la media de popularidad para cada grupo
country_popularity <- spotify_fusion %>%
  group_by(track_country) %>%
  summarize(mean_popularity = mean(track_popularity)) |> 
  arrange(desc(mean_popularity)) |>  print()


###### track_id ###### 

duplicates_track_id <- duplicated(spotify_fusion$track_id)
num_duplicates_track_id <- sum(duplicates_track_id)
num_duplicates_track_id


###### track_name ###### 

spotify_fusion |> 
  count(track_name, sort = TRUE) |> 
  mutate(porc = 100*n/sum(n))


###### track_playlist ###### 

spotify_fusion |> 
  count(track_playlist, sort = TRUE) |> 
  mutate(porc = 100*n/sum(n))

# Agrupamos los datos por playlist y calculamos la media de popularidad para cada grupo
playlist_popularity <- spotify_fusion |> 
  group_by(track_playlist) |> 
  summarize(mean_popularity = mean(track_popularity)) |> 
  arrange(desc(mean_popularity)) 

# Agrupamos las playlists por la media de popularidad que tiene cada una, de 10 en 10
playlist_popularity <- playlist_popularity |> 
  mutate(playlist_mean = cut(mean_popularity, 
                              breaks = seq(-1, 100, 10), 
                              labels = paste0("playlist_", seq(0, 90, 10))))

# Juntamos las playlists de media 70, 80 y 90 al tener poca representatividad
playlist_popularity <- playlist_popularity |> 
  mutate(playlist_mean = fct_collapse(playlist_mean, 
                                      playlist_70 = c("playlist_70", "playlist_80", "playlist_90")))

# Agregamos la nueva categorización playlist_mean a spotify_fusion
spotify_fusion <- left_join(spotify_fusion, playlist_popularity, by = "track_playlist")

# Renombramos la columna track_playlist por playlist_mean
spotify_fusion <- spotify_fusion |> select(-c(mean_popularity))         

spotify_fusion |> 
  count(playlist_mean, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))


###### artist_genres ###### 

spotify_fusion |> 
  count(artist_genres, sort = TRUE) |> 
  mutate(porc = 100*n/sum(n))

# Creamos las nuevas variables con los géneros musicales más escuchados
spotify_fusion$genre_rap <- ifelse(grepl("rap", spotify_fusion$artist_genres), 1, 0)
spotify_fusion$genre_pop <- ifelse(grepl("pop", spotify_fusion$artist_genres), 1, 0)
spotify_fusion$genre_rock <- ifelse(grepl("rock", spotify_fusion$artist_genres), 1, 0)
spotify_fusion$genre_house <- ifelse(grepl("house", spotify_fusion$artist_genres), 1, 0)
spotify_fusion$genre_indie <- ifelse(grepl("indie", spotify_fusion$artist_genres), 1, 0)
spotify_fusion$genre_dance <- ifelse(grepl("dance", spotify_fusion$artist_genres), 1, 0)
spotify_fusion$genre_folk <- ifelse(grepl("folk", spotify_fusion$artist_genres), 1, 0)
spotify_fusion$genre_classical <- ifelse(grepl("classical", spotify_fusion$artist_genres), 1, 0)
spotify_fusion$genre_hiphop <- ifelse(grepl("hip hop", spotify_fusion$artist_genres), 1, 0)
spotify_fusion$genre_electro <- ifelse(grepl("electro", spotify_fusion$artist_genres), 1, 0)

genre_cols <- spotify_fusion[, grep("^genre_", colnames(spotify_fusion))]
proporcion_genre <- colMeans(genre_cols)
proporcion_genre <- sort(proporcion_genre, decreasing = TRUE)
genre_prop <- data.frame(
  genre = names(proporcion_genre),
  proporcion_genre = proporcion_genre)
print(genre_prop)

spotify_fusion  |> 
  pivot_longer(starts_with("genre_"), names_to = "genre", values_to = "genre_value") |>
  filter(genre_value == 1) |> 
  group_by(genre) |> 
  summarize(mean_popularity = mean(track_popularity)) |> 
  arrange(desc(mean_popularity))


###### artist_name ###### 

spotify_fusion |> 
  count(artist_name, sort = TRUE) |> 
  mutate(porc = 100*n/sum(n))


###### album_type ###### 

spotify_fusion |> 
  count(album_type, sort = TRUE) |> 
  mutate(porc = 100*n/sum(n))

# Agrupamos los datos por genero y calculamos la media de popularidad para cada grupo
album_type_popularity <- spotify_fusion |> 
  group_by(album_type) |> 
  summarize(mean_popularity = mean(track_popularity)) |> 
  arrange(desc(mean_popularity)) |>  print()


###### album_name ###### 

spotify_fusion |> 
  count(album_name, sort = TRUE) |> 
  mutate(porc = 100*n/sum(n))


##### Dependencia entre cualitativas #####

spotify_fusion <- spotify_fusion |>
  mutate(across(where(is.character), as_factor))

chisq <-
  tibble("variable" = spotify_fusion |> select(where(is.factor)) |> names(),
         "p_value" = spotify_fusion |> select(where(is.factor)) |>
           map_dbl(.f= function(x) { chisq.test(spotify_fusion$track_popularity, x)$p.value}))

chisq |> arrange(desc(p_value))

chisq |> filter(p_value > 0.05)


##### Variables cuantitativas ##### 

###### track_acousticness ###### 

spotify_fusion |> 
  mutate(track_acousticness = round(track_acousticness, 1)) |>
  count(track_acousticness, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(track_acousticness)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(track_acousticness)), 
               color = '#FF0000', linetype = 'dashed', size = 0.6)


###### track_danceability ###### 

spotify_fusion |> 
  mutate(track_danceability = round(track_danceability, 1)) |>
  count(track_danceability, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(track_danceability)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(track_danceability)), 
               color = '#FF0000', linetype = 'dashed', size = 0.6)


###### track_disc_number ###### 

spotify_fusion |> 
  count(track_disc_number, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(track_disc_number)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(track_disc_number)), 
               color = '#FF0000', linetype = 'dashed', linewidth = 1)

spotify_fusion <- spotify_fusion |> 
  mutate(track_disc_number = ifelse(track_disc_number == 1, "unique", "various"))


###### track_duration_ms ###### 

spotify_fusion$track_duration_min <- spotify_fusion$track_duration_ms / 60000
spotify_fusion$track_duration_min <- spotify_fusion$track_duration_min |> round(2)

summary(spotify_fusion$track_duration_min)

spotify_fusion |> 
  mutate(track_duration_min = round(track_duration_min, 1)) |>
  count(track_duration_min, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(track_duration_min)) +
    geom_bar(color = "#17A54A", fill = "#71EB9D", bins = 45) +
    scale_x_continuous(breaks = seq(0, max(spotify_fusion$track_duration_min), 5)) +
    theme_minimal()


###### track_energy ###### 

spotify_fusion |> 
  mutate(track_energy = round(track_energy, 1)) |>
  count(track_energy, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(track_energy)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(track_energy)), 
               color = '#FF0000', linetype = 'dashed', size = 0.6)


###### track_instrumentalness ###### 

spotify_fusion |> 
  mutate(track_instrumentalness = round(track_instrumentalness, 1)) |>
  count(track_instrumentalness, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(track_instrumentalness)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(track_instrumentalness)), 
               color = '#FF0000', linetype = 'dashed', size = 0.6)


###### track_key ###### 

spotify_fusion |> 
  count(track_key, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(track_key)) +
  geom_bar(color = "#17A54A", fill = "#71EB9D") +    
  scale_x_continuous(breaks = seq(0, max(spotify_fusion$track_key), 1)) +
  theme_minimal()


###### track_liveness ###### 

spotify_fusion |> 
  mutate(track_liveness = round(track_liveness, 1)) |>
  count(track_liveness, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(track_liveness)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(track_liveness)), 
               color = '#FF0000', linetype = 'dashed', size = 0.6)


###### track_loudness ###### 

summary(spotify_fusion$track_loudness)

ggplot(spotify_fusion, aes(track_loudness)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(track_loudness)), 
               color = '#FF0000', linetype = 'dashed', size = 0.6)


###### track_mode ###### 

spotify_fusion |> 
  count(track_mode, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

spotify_fusion <- spotify_fusion |> 
  mutate(track_mode = ifelse(track_mode == 0, "minor", "mayor"))


###### track_speechiness ###### 

summary(spotify_fusion$track_speechiness)

spotify_fusion |> 
  mutate(track_speechiness = round(track_speechiness, 1)) |>
  count(track_speechiness, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(track_speechiness)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(track_speechiness)), 
               color = '#FF0000', linetype = 'dashed', size = 0.6)


###### track_tempo ###### 

summary(spotify_fusion$track_tempo)

spotify_fusion |> 
  mutate(track_tempo = round(track_tempo, 1)) |>
  count(track_tempo, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(track_tempo)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(track_tempo)), 
               color = '#FF0000', linetype = 'dashed', size = 0.6)


###### track_time_signature ###### 

summary(spotify_fusion$track_time_signature)

spotify_fusion |> 
  mutate(track_time_signature = round(track_time_signature, 1)) |>
  count(track_time_signature, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(track_time_signature)) +
  geom_bar(color = "#17A54A", fill = "#71EB9D") +    
  scale_x_continuous(breaks = seq(0, max(spotify_fusion$track_time_signature), 1)) +
  theme_minimal()


###### track_number ###### 

spotify_fusion |> 
  mutate(track_number = round(track_number, 1)) |>
  count(track_number, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))


###### track_valence ###### 

summary(spotify_fusion$track_valence)

spotify_fusion |> 
  mutate(track_valence = round(track_valence, 1)) |>
  count(track_valence, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(track_valence)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(track_valence)), 
               color = '#FF0000', linetype = 'dashed', size = 0.6)


###### artist_popularity ###### 

summary(spotify_fusion$artist_popularity)

spotify_fusion |> 
  mutate(artist_popularity = round(artist_popularity, 1)) |>
  count(artist_popularity, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(artist_popularity)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(artist_popularity)), 
               color = '#FF0000', linetype = 'dashed', size = 0.6)


###### artist_followers ###### 

summary(spotify_fusion$artist_followers)

spotify_fusion$artist_followers_categories <- cut(spotify_fusion$artist_followers, 
                                       breaks = c(-1, 10e4, 50e4, 1e6, 5e6, 50e6), 
                                       labels = c("0_100K", "100K_500K", 
                                                  "500K_1M", "1M_5M", ">5M"))

spotify_fusion |> 
  count(artist_followers_categories, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(artist_followers_categories)) +
    geom_bar(color = "#17A54A", fill = "#71EB9D") +
    theme_minimal()


###### album_tracks ###### 

summary(spotify_fusion$album_tracks)

spotify_fusion |> 
  mutate(album_tracks = round(album_tracks, 1)) |>
  count(album_tracks, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(album_tracks)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(album_tracks)), 
               color = '#FF0000', linetype = 'dashed', size = 0.6)


###### lyric_mean_syllables_word ###### 

summary(spotify_fusion$lyric_mean_syllables_word)

spotify_fusion |> 
  mutate(lyric_mean_syllables_word = round(lyric_mean_syllables_word, 1)) |>
  count(lyric_mean_syllables_word, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(lyric_mean_syllables_word)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(lyric_mean_syllables_word)), 
               color = '#FF0000', linetype = 'dashed', size = 0.6)


###### lyric_mean_words_sentence ###### 

summary(spotify_fusion$lyric_mean_words_sentence)

spotify_fusion |> 
  mutate(lyric_mean_words_sentence = round(lyric_mean_words_sentence, 1)) |>
  count(lyric_mean_words_sentence, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(lyric_mean_words_sentence)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(lyric_mean_words_sentence)), 
               color = '#FF0000', linetype = 'dashed', size = 0.6)


###### lyric_n_sentences ###### 

summary(spotify_fusion$lyric_n_sentences)

spotify_fusion |> 
  mutate(lyric_n_sentences = round(lyric_n_sentences, 1)) |>
  count(lyric_n_sentences, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(lyric_n_sentences)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(lyric_n_sentences)), 
               color = '#FF0000', linetype = 'dashed', size = 0.6)


###### lyric_n_words ###### 

summary(spotify_fusion$lyric_n_words)

spotify_fusion |> 
  mutate(lyric_n_words = round(lyric_n_words, 1)) |>
  count(lyric_n_words, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(lyric_n_words)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(lyric_n_words)), 
               color = '#FF0000', linetype = 'dashed', size = 0.6)


###### lyric_sentence_similarity ###### 

summary(spotify_fusion$lyric_sentence_similarity)

spotify_fusion |> 
  mutate(lyric_sentence_similarity = round(lyric_sentence_similarity, 1)) |>
  count(lyric_sentence_similarity, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(lyric_sentence_similarity)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(lyric_sentence_similarity)), 
               color = '#FF0000', linetype = 'dashed', size = 0.6)


###### lyric_vocabulary_wealth ###### 

summary(spotify_fusion$lyric_vocabulary_wealth)

spotify_fusion |> 
  mutate(lyric_vocabulary_wealth = round(lyric_vocabulary_wealth, 1)) |>
  count(lyric_vocabulary_wealth, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(lyric_vocabulary_wealth)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(lyric_vocabulary_wealth)), 
               color = '#FF0000', linetype = 'dashed', size = 0.6)


##### Colinealidad #####

cor_matrix <- spotify_fusion |> select(where(is.numeric)) |> na.omit() |> cor() |> round(2)

cor_matrix |>
  corrplot(method = "square", tl.cex = 0.65, number.cex = 0.7, type = "lower")

# Encontrar las variables con una correlación mayor a 0.7
corr_vars <- findCorrelation(cor_matrix, cutoff = 0.8)

# Obtener los nombres de las variables más correlacionadas
corr_var_names <- colnames(cor_matrix)[corr_vars] |> print()


##### Variables fecha ##### 

###### album_release_date ###### 

spotify_fusion |> 
  count(album_release_date, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

summary(spotify_fusion$album_release_date)

ggplot(spotify_fusion, aes(album_release_date)) +
    geom_density(color = "#17A54A", fill = "#71EB9D") +
    geom_vline(aes(xintercept = mean(album_release_date)), 
               color = '#FF0000', linetype = 'dashed', size = 0.6)

limites_años <- as.Date(c("1926-01-01", "2000-01-01", "2022-01-01"))
spotify_fusion$album_release_centurie <- cut(spotify_fusion$album_release_date, 
                                                    breaks = limites_años, 
                                                    labels = c("S_XX", "S_XXI"))

spotify_fusion |> 
  count(album_release_centurie, sort = FALSE) |> 
  mutate(porc = 100*n/sum(n))

spotify_fusion <- spotify_fusion |> 
  mutate(album_release_year = year(album_release_date))

spotify_fusion |> 
  count(album_release_year, sort = TRUE) |> 
  mutate(porc = 100*n/sum(n))

ggplot(spotify_fusion, aes(album_release_year)) +
    geom_bar(color = "#17A54A", fill = "#71EB9D") +
    theme_minimal()

 
#### Modificación estructural ####

spotify_final1 <- 
  spotify_fusion |> 
# Eliminamos variables
  dplyr::select(-c(track_name, album_name, artist_name, track_id, 
            album_id, artist_id, track_available_markets, 
            artist_genres, track_lyrics, 
            track_duration_ms, track_playlist,
            album_release_date))

spotify_final1$track_country<-factor(spotify_final1$track_country)
spotify_final1$album_type<-factor(spotify_final1$album_type)
spotify_final1$playlist_mean<-factor(spotify_final1$playlist_mean)
spotify_final1$artist_followers_categories<-factor(spotify_final1$artist_followers_categories)
spotify_final1$album_release_centurie<-factor(spotify_final1$album_release_centurie)
spotify_final1$market_Africa<-factor(spotify_final1$market_Africa)
spotify_final1$market_America<-factor(spotify_final1$market_America)
spotify_final1$market_Asia<-factor(spotify_final1$market_Asia)
spotify_final1$market_Europa<-factor(spotify_final1$market_Europa)
spotify_final1$market_Oceania<-factor(spotify_final1$market_Oceania)
spotify_final1$genre_rap<-factor(spotify_final1$genre_rap)
spotify_final1$genre_pop<-factor(spotify_final1$genre_pop)
spotify_final1$genre_rock<-factor(spotify_final1$genre_rock)
spotify_final1$genre_house<-factor(spotify_final1$genre_house)
spotify_final1$genre_indie<-factor(spotify_final1$genre_indie)
spotify_final1$genre_dance<-factor(spotify_final1$genre_dance)
spotify_final1$genre_folk<-factor(spotify_final1$genre_folk)
spotify_final1$genre_classical<-factor(spotify_final1$genre_classical)
spotify_final1$genre_hiphop<-factor(spotify_final1$genre_hiphop)
spotify_final1$genre_electro<-factor(spotify_final1$genre_electro)
spotify_final1$track_disc_number<-factor(spotify_final1$track_disc_number)
spotify_final1$track_mode<-factor(spotify_final1$track_mode)

skim(spotify_final1)


#### Exportación de ficheros #### 

write.csv(spotify_final, "spotify_final.csv")
write.csv(spotify_fusion, "spotify_fusion.csv")
write.csv(spotify_final1, "spotify_final1.csv")
