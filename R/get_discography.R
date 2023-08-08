#' @title Get Discography
#' 
#' @description Gets full information for an artist
#' 
#' @param name Character (length = 1).
#' An artist's name
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
#' get_discography("The Beatles")}
#' 
#' @export
# Search information based on name
# Updated 08.08.2023
get_discography <- function(name)
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
  
  # Get initial information
  initial_information <- get_information(
    available_content$response$hits, name, "artist"
  )
  
  # Get artist id
  ARTIST_ID <- initial_information$primary_artist_id[
    which.max(initial_information$primary_artist == name)
  ] 
  
  # Initialize search page
  page <- 1
  
  # Initialize result list
  result_list <- list()
  
  # Loop over search pages
  while(TRUE){
    
    # Fetch content
    available_content <- httr::content(
      httr::RETRY(
        'GET', url = paste0(
          "https://api.genius.com/artists/", ARTIST_ID, "/songs"
        ),
        httr::user_agent("Mozilla/5.0"),
        query = list(
          q = name, page = page, per_page = 50,
          access_token = Sys.getenv('GENIUS_API_TOKEN')
        ), quiet = TRUE
      )
    )
    
    # Check for failed search
    failed_search(available_content)
    
    # Get information
    result_list[[page]] <- get_information(
      available_content$response$songs, "artist"
    )
    
    # Check for next page
    if(is.null(available_content$response$next_page)){
      break
    }else{ 
      
      # Increase page
      page <- page + 1
      
      # Update page
      cat("\r Searching on page: ", page, rep(" ", 10))
      
    }
    
  }
  
  # Return unique results
  return( 
    unique(
      do.call(rbind.data.frame, result_list), MARGIN = 1
    )
  )
  
}





