#%%%%%%%%%%%%%%%%%%%%%%
# *APPLY FUNCTIONS ----
#%%%%%%%%%%%%%%%%%%%%%%

# These functions are merely wrappers over the `*apply`
# family to execute specific tasks that are often repeatedly
# done. Their purpose are to provide slightly cleaner code.

# These `*vapply` are specific versions of `vapply` that
# pre-specify the output. These versions are generally
# simpler (in function) and marginally faster than 
# the more commonly used `sapply`. They are more strict
# in that they require knowing the output format (e.g., numeric)

#' @noRd
# Character vector/matrix output apply ----
# Updated 06.07.2023
cvapply <- function(X, FUN, ..., LENGTH = 1, USE.NAMES = TRUE)
{
  return(vapply(X = X, FUN = FUN, FUN.VALUE = character(LENGTH), ..., USE.NAMES = USE.NAMES))
}

#' @noRd
# Logical vector/matrix output apply ----
# Updated 06.07.2023
lvapply <- function(X, FUN, ..., LENGTH = 1, USE.NAMES = TRUE)
{
  return(vapply(X = X, FUN = FUN, FUN.VALUE = logical(LENGTH), ..., USE.NAMES = USE.NAMES))
}

#' @noRd
# Numeric vector/matrix output apply ----
# Updated 06.07.2023
nvapply <- function(X, FUN, ..., LENGTH = 1, USE.NAMES = TRUE)
{
  return(vapply(X = X, FUN = FUN, FUN.VALUE = numeric(LENGTH), ..., USE.NAMES = USE.NAMES))
}

#' @noRd
# Unlist `lapply` ----
# Updated 14.07.2023
ulapply <- function(X, FUN, ..., recursive = TRUE)
{
  return(unlist(lapply(X, FUN, ...), recursive = recursive))
}


#%%%%%%%%%%%%%%%%%%
# WEB SCRAPING ----
#%%%%%%%%%%%%%%%%%%

#' @noRd
# General error in failed search ----
# Updated 08.08.2023
# Check for meta status
failed_search <- function(available_content){
  
  if(available_content$meta$status != 200){
    stop(
      paste0(
        "Information on could not be found.",
        "\nIs spelling, punctuation, and spacing correct?"
      ), call. = FALSE
    )
  }
  
}

#' @noRd
# Get information function
# Updated 08.08.2023
get_information <- function(information, name, type)
{
  
  # Check for "result" in list name
  if("result" %in% names(information[[1]])){
    information <- lapply(information, function(x){x$result})
  }else if("response" %in% names(information[[1]])){
    information <- lapply(information, function(x){x$response})
  }
  
  # Remove incomplete lyrics
  keep_information <- lvapply(information, function(x){
    x$lyrics_state == "complete"
  })
  
  # Keep only complete lyrics
  information <- information[keep_information]
  
  # Get information
  if(type == "artist"){
    
    # Get only primary artist
    index <- which(
      lvapply(
        information, function(x){
          x$primary_artist$name == name
        }
      )
    )
    
  }else if(type == "song"){
    
    # Otherwise, use all indices
    index <- seq_along(information)
    
  }
  
  # Loop over information to get values
  return( 
    unique( # Get only unique
      do.call(
        rbind.data.frame, # Make into a data frame
        lapply(information[index], function(x){
          
          return(
            data.frame(
              artists = x$artist_names,
              song = x$title,
              id = x$id, url = x$url,
              primary_artist = x$primary_artist$name,
              primary_artist_id = x$primary_artist$id,
              primary_artist_url = x$primary_artist$url,
              page_views = ifelse(
                "pageviews" %in% names(x$stats),
                x$stats$pageviews, 0
              )
            )
          )
          
        })
      ), MARGIN = 1
    )
  )
  
}

#' @noRd
# Function to split words from GPT-4
# Updated 08.08.2023
split_words <- function(text) {
  
  # Replace capital letters with a space and the capital letter
  text <- stringr::str_replace_all(text, "([A-Z])", " \\1")
  
  # Replace punctuation followed by a non-space character with the punctuation and a space,
  # but exclude contractions with 's or 't, and exclude dashes
  text <- stringr::str_replace_all(text, "(\\p{P}(?<![''-])(?<![''](s|t)))([^\\s])", "\\1 \\2")
  
  # Remove extra spaces that may have been added at the beginning
  text <- stringr::str_replace(text, "^\\s+", "")
  
  # Return text
  return(text)
  
}
