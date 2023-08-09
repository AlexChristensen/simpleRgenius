#' @title Get Song Lyrics
#' 
#' @description Get song lyrics and parts from a song or many songs by
#' an artist
#' 
#' @param artist_name Character (length = 1).
#' Uses the artist name to get correct lyrics.
#' The "primary artist" will be used to identify the
#' appropriate artist
#' 
#' @param song_names Character (vector).
#' Name or names of songs to get lyrics for (narrows down by \code{artist_name})
#' 
#' @param urls Character (vector).
#' URLs for the songs (rather than using artist and song names).
#' Skips search for proper information and provides exactly the URL
#' to the lyrics desired
#' 
#' @param verbose Boolean (length = 1).
#' Whether progress on which song lyrics are being obtained
#' should be printed.
#' Defaults to \code{TRUE}
#' 
#' @return Returns a data frame with artist, song name,
#' part of the song (e.g., verse, chorus), and lyric for each part
#' 
#' @examples
#' \dontrun{
#' get_lyrics(
#'   artist_name = "The Beatles",
#'   song_names = c(
#'     "Lucy in the Sky with Diamonds",
#'     "Magical Mystery Tour"
#'   )
#' )}
#' 
#' @author Alberto Almuina and Alexander P. Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom magrittr %>%
#' 
#' @export
#' 
# Gets all lyrics ----
# Updated 09.08.2023
get_lyrics <- function(artist_name, song_names, urls = NULL, verbose = TRUE)
{
  
  # Check if URLs are already provided
  if(is.null(urls)){
    
    # Check for verbose
    if(verbose){message("Fetching URLs...", appendLF = FALSE)}
    
    # Loop over song names
    SONG_URLS <- sapply(
      song_names, function(song_name){
        
        # Get top hits
        top_hits <- search_information(song_name, type = "song")
        
        # Get top hits with artist name
        top_hits <- top_hits[top_hits$primary_artist == artist_name,]
        
        # Return top hit with artist information
        return(top_hits[which.max(top_hits$page_views), "url"])
      }
    )
    
    # Return done
    message("done")
    
  }else{SONG_URLS <- urls}
  
  # Set en-dash
  en_dash <- rawToChar(as.raw(c(0xE2, 0x80, 0x93)))
  
  # Check for verbose
  if(verbose){message("Getting lyrics for...")}
  
  # Get all lyrics
  all_lyrics <- lapply(SONG_URLS, function(URL){
    
    # Get HTML output
    HTML_output <- rvest::read_html(URL)
    
    # Get artist and song information
    ARTIST_SONG <- trimws(
      gsub("\\|.*", "", HTML_output %>% rvest::html_nodes('title') %>% rvest::html_text())
    )
    ARTIST <- trimws(gsub(paste0(en_dash, ".*"), "", ARTIST_SONG))
    SONG <- trimws(gsub("Lyrics", "", gsub(paste0(".*", en_dash), "", ARTIST_SONG)))
    
    # Print message about getting lyrics
    if(verbose){message(SONG)}
    
    # Extract lyrics
    LYRICS <- HTML_output %>% rvest::html_nodes(xpath = '//*[@data-lyrics-container="true"]') %>% rvest::html_text()
    
    # Get parts from inside brackets
    parts <- stringr::str_extract_all(LYRICS, "\\[.*?\\]") %>% unlist()
    
    # Remove instrumental parts
    parts <- parts[!grepl("Instrumental", parts)]
    parts <- parts[!grepl("Non-Lyrical Break", parts)]
    
    # Extract the text outside brackets
    outside_brackets <- stringr::str_split(LYRICS, "\\[.*?\\]") %>% unlist()
    outside_brackets <- outside_brackets[outside_brackets != ""]
    
    # Get lyrics that are split
    lyrics <- split_words(outside_brackets)
    
    # Remove "( " space
    lyrics <- gsub("\\( ", "\\(", lyrics)
    
    # Return data frame
    return(
      data.frame(
        Artist = ARTIST,
        Song = SONG,
        Part = parts,
        Lyric = lyrics
      )
    )
    
    
  })
  
  # Get all lyrics as a data frame
  all_lyrics <- do.call(rbind.data.frame, all_lyrics)
  row.names(all_lyrics) <- NULL
  
  # Trim all lyrics one last time
  all_lyrics$Lyric <- trimws(all_lyrics$Lyric)
  all_lyrics$Lyric <- gsub("  ", " ", all_lyrics$Lyric)
  
  # Return results as data frame
  return(all_lyrics)
  
}


