library(spotifyr)
library(dplyr)
library(dendextend)

Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXX')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXX')
access_token <- get_spotify_access_token()

b_songs <- spotifyr::get_artist_audio_features("blink-182")

albums_to_compare <- c('Cheshire Cat', 'Dude Ranch', 
                       'Enema Of The State', 'Take Off Your Pants And Jacket',
                       'blink-182', 'Neighborhoods', 'California')

# prepare the dataset for distance calculations
b_songs <- b_songs %>% 
  filter(album_name %in% albums_to_compare$album &
           artist_name %in% 'blink-182') %>%
  select(c("danceability", "energy", "loudness", "speechiness", "acousticness", 
           "instrumentalness", "liveness", "valence", "tempo",
           "duration_ms", "key_mode", "track_popularity","album_popularity",
           "artist_name", "track_name", "album_name")) %>%
  mutate(location = 1:n(),
         key_mode = as.numeric(as.factor(key_mode)))

str(b_songs)

# create a distance matrix
b_dist <- b_songs %>%
  select(-c("artist_name", "track_name", "album_name", "location")) %>%
  as.matrix() %>%
  scale(center = TRUE, scale = TRUE) %>% # scale
  dist(method = "euclidean")

# cluster the songs
b_hclust <- hclust(b_dist, method = 'average') 

# create the dendrogram
dend <- b_hclust %>% as.dendrogram() 

# assign a color to each album
my_colors <- b_songs$album_name %>% 
  recode('California' = 'darkorchid4', 'Neighborhoods' = 'grey',
         'blink-182' = 'deeppink','Cheshire Cat' = 'burlywood2','Dude Ranch' = 'red',
         'Enema Of The State'= 'cyan1', 'Take Off Your Pants And Jacket' = 'black')
my_colors <- my_colors[order.dendrogram(dend)]

# add labels to the dendrogram
labels(dend) <- b_songs$track_name[order.dendrogram(dend)]

# plot the dendrogram
par(mar = c(2,2,2,12))
par(family = 'Avenir')
dend %>% 
  set("branches_col", "gray30") %>% 
  set("labels_col", "gray30") %>%
  set("labels_cex", 0.6) %>%
  set("leaves_pch", 15) %>%
  set("leaves_col", my_colors) %>%
  set("nodes_cex", 0.85) %>% 
  plot(horiz = TRUE, main = list("blink-182 Song Similarity", cex = 1.5))
legend("topleft", cex = 0.5, title = 'Album',
       legend = c('Cheshire Cat (1995)', 'Dude Ranch (1997)', 'Enema Of The State (1999)', 
                  'Take Off Your Pants And Jacket (2001)','blink-182 (2003)', 
                  'Neighborhoods (2011)', 'California (2016'), 
       fill = c('burlywood2', 'red', 'cyan1', 'black', 'deeppink', 'grey', 'darkorchid4'))


# dist between albums
b_album_avg <- b_dist %>% as.matrix() %>% as.data.frame() %>%
  mutate(album_name = b_songs$album_name) %>%
  reshape2::melt(id = 'album_name') %>%
  mutate(variable = as.numeric(as.character(variable))) %>%
  left_join(b_songs[,c('location', 'album_name')], by = c('variable' = 'location')) %>%
  group_by(album_name.x, album_name.y) %>%
  summarise(distance = median(value)) %>%
  reshape2::dcast(album_name.x ~ album_name.y, value.var = 'distance')

# cluster the distances
dend_album <- b_album_avg %>% 
  select(-album_name.x) %>% 
  as.dist() %>% 
  hclust(method = 'average') %>%
  as.dendrogram()

# plot the dendrogram
album_colors <- b_songs$album_name %>% unique() %>%
  recode('California' = 'darkorchid4', 'Neighborhoods' = 'grey',
         'blink-182' = 'deeppink','Cheshire Cat' = 'burlywood2','Dude Ranch' = 'red',
         'Enema Of The State'= 'cyan1', 'Take Off Your Pants And Jacket' = 'black')

par(mar = c(5,2,5,14))
dend_album %>%
  set("nodes_cex", 0.85) %>%
  set("leaves_pch", 19) %>%
  set("leaves_col", c('deeppink', 'burlywood2', 'red', 'darkorchid4', 'grey', 'cyan1','black')) %>%
  plot(horiz = TRUE, main = list("blink-182 Album Similarity", cex = 1.5))


