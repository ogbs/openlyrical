# Load dependencies
dependencies <- c("RCurl", "jsonlite")
lapply(dependencies, library, character.only=T)

# From Genius API, get only links of a provided artist.
get_song_links <- function(artist) {
  r <- dynCurlReader() 
  curlPerform(
    url="http://genius-api.com/api/artistInfo/",
    postfields=paste0("name=",artist),
    writefunction = r$update
    )
  return(gsub("http://rapgenius.com","",
    fromJSON(r$value())$songs$link
    ))
}
# e.g.
## get_song_links(artist="GZA") 

# Given song link, return `lyrics' JSON object from Genius API 
get_lyrics <- function(link) {
  r <- dynCurlReader() 
  curlPerform(
    url="http://genius-api.com/api/lyricsInfo/",
    postfields=paste0("link=",link),
    writefunction = r$update
    )
  return(fromJSON(r$value())$lyrics)
}
# e.g.
## get_lyrics("http://genius.com/Gza-liquid-swords-lyrics")

# Given a JSON lyrics object and artist string, isolate to artist and tokenize.
tokenize_song <- function(lyrics, artist) {
  if(
      ( length(lyrics$producingArtists) > 1 && length(grep(artist, lyrics$producingArtists)) == 1) 
      || 
      ( length(lyrics$sections$name) > 1 && length(grep(artist, lyrics$producingArtists)) > 0 )) {
        test <- sapply(lyrics$sections$verses[grep(artist, lyrics$sections$name)],"[[",2)
    } else {
        test <- lyrics$sections$verses   
    }
  test <- tolower(gsub("\\n", " ",gsub("[[:punct:]]","",unlist(test))))
  return(unlist(strsplit(test," ")))
}
# e.g.
##  tokenize_song( get_lyrics(get_song_links(artist="GZA")[1]), "GZA" )

# High-level function.
# Given artist string, retrieve all songs, tokenize lyrics, using Genius API.
get_artist_lyrics <- function(artist) {
  songs <- get_song_links(artist)
  return(
    sapply(songs, function(x) tokenize_song(get_lyrics(x),artist))
    ) 
}
# e.g.
## gzaLyrics <- get_artist_lyrics("GZA")

# Given tokenized lyrics, return all valid words.
artist_list <- function(tokenized) {
    list <- unlist(tokenized)
    return(list[!grepl("[0-9]+",list)])
}
# e.g.
## unique( length(artist_list(gzaLyrics)) )
