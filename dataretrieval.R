# create data frame with columns: artistid, artist name, song name and songid. Contains the information about the popular songs
songsinfo <-read.csv("C:/Users/timon/Documents/Data Science/+Data Valorization/Kaggle Challenge/songsinfo.csv", stringsAsFactors = FALSE)
data <- read.csv("C:/Users/timon/Documents/Data Science/+Data Valorization/Kaggle Challenge/dataset.csv", stringsAsFactors = FALSE)
songsinfo <-read.csv("C:/Users/timon/Documents/Data Science/+Data Valorization/Kaggle Challenge/songsinfo.csv", stringsAsFactors = FALSE)
keys <- spotifyOAuth("spotify-kaggle-data-valorization","668f1b6ddcbb493584154d9e7126ad09","ac55b875163a4d2187b6d9a969d93156")


## In this code we look for the album of the popular songs, then explore the songs of the album and choose the song with the lowest popularity. The song cannot be an intro or interlude.
# The goal of getting the popular songs is to be able to compare the popular songs with the unpopular songs to we can ultimately learn what makes a song popular and predict what song is going to be popular.
# We chose to get the popular songs from the same artists and preferable from the same albums to make sure that the distribution of the songs are more or less the same. As 100 songs of rock will clearly differ in features with the top 100 songs. We wanted to have a similar melange of songs to get a similar distribution of popular and unpopular songs.


unpopularSongIds <- vector(mode="character",length = 10) # create vector with ids for the unpopular songs

countsame <- 0
for(z in 1:100){ #for every song in songsinfo
  getTrack <- getTrack(songsinfo[z,4],token=keys) # get the track information about popular song
  albumID <- getTrack[1,8] # get id of the album it appears on
  getAlbum<- getAlbum(albumID,token=keys) # get information about the album 
  getAlbum<- getAlbum("4a6DxkhmMvvEdPXxm4ergN",token=keys) # execute everything from here downwards to find the least popular song of a manually picked album
  ints <- list("interlude","intro","Intro","Interlude") # vector of strings that we don't want the song name to exist of
  nrowAlbum <- nrow(getAlbum) # number of songs in the album
  if(nrowAlbum==1)next # if there only exists one song in the album
  albumSongs <- data.frame(stringsAsFactors = FALSE) # create a data frame for the songs and it's popularity
  albumSongs <- c("id","popularity") # give the columns a name
  lowest <- 100 # start with 100 
  for(i in 1:nrowAlbum){ # for every song on the album
    songID <- getAlbum[i,1] # get song id of song in the album
    getTrack <- getTrack(songID,token=keys) # find information about that song
    songPopularity <- getTrack[1,4] # get populatiry
    print(getTrack[1,1])
    print(songPopularity)
    int <- 0
    for(a in 1:length(ints)){ # make sure songs are not intro/interlude
      if(length(grep(ints[a],getTrack[1,2]))==1){
        int <- int+1
      }
    }
    if(getTrack[1,4]<lowest){ # if this song is lower in popularity than the least popular song on the album so far
      lowest <- getTrack[1,4]
      lowestid <- getTrack[1,1]
    }
    if(int==0){ # check if it's a interlude or intro 
      albumSongs <- rbind(albumSongs,c(as.character(getTrack[1,1]), as.integer(getTrack[1,4])))
    }
    
  } 
  print("klaar")
  print(lowest) # popularity of the least popular song
  print(lowestid) # id of the least popular song
  if(lowestid == as.integer(getTrack[1,1])){
    countsame <- countsame +1
  }
  unpopularSongIds[z] <- as.character(lowestid) # put least popular song in vector
  #print(countsame)
}

# a big amount of the songs were not published in an album but were a single. In those cases we manually looked for songs from the same artist with a low popularity.
searchArtist("WIEE",token=keys)
id <- "5ZsFI1h6hIdQRw2ti0hz81"
getAlbums(id, type = "album", token=keys)
getArtist(id)

searchTrack("Hearts Don't Break Around Here",token=keys)
getTrack("704woR1IhQrXDPTWsU6UY3",token=keys)
getAlbum("5amj9zNeZ3B2EdpBgXrOZ0",token=keys)
