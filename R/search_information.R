#' @title Search By Artist or Song for Information
#' 
#' @description Get basic Genius information for an artist or song title
#' 
#' @param name Character (length = 1).
#' Either an artist or a song
#' 
#' @param type Character (length = 1).
#' Specify whether \code{name} is an artist or a song
#' 
#' @return
#' Returns a data frame with the artists, song names,
#' Genius ID, Genius URL, primary artist, primary artist Genius ID,
#' and primary artist Genius URL
#' 
#' @author Alberto Almuina and Alexander P. Christensen <alexpaulchristensen@gmail.com>
#' 
#' @examples
#' \dontrun{
#' # Search for "The Beatles"
#' search_information("The Beatles", type = "artist")}
#' 
#' @export
# Search information based on name
# Updated 08.08.2023
search_information <- function(name, type = c("artist", "song"))
{
  
  # Fetch content
  available_content <- httr::content(
    httr::RETRY(
      'GET', url = "https://api.genius.com/search",
      httr::user_agent("Mozilla/5.0"),
      query = list(
        q = name, page = 1, per_page = 50,
        access_token = Sys.getenv('GENIUS_API_TOKEN')
      ), quiet = TRUE
    )
  )
  
  # Check for failed search
  failed_search(available_content)
  
  # Return results
  return(get_information(available_content$response$hits, name, type))
  
}
