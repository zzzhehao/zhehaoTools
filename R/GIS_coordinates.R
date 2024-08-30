#' Transform DMS coordinates in decimal format
#' 
#' @author Zhehao Hu
#' 
#' @details
#' Vectorized funciton. Input can either be a single character string or a vector of character. DMS coordinates should be separated by `°` or `'` to be able to be recognized. 
#' @param dms Latitude/Longitude coordinates in DMS format.
#' @param with_letter Is the coordinates indicated with letter (N, S, W, E)? Default set to "AUTO", also accept `TRUE` or `FALSE`. 
#' @return A `dbl` or a vector of `dbl`.
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @export
dms_to_decimal <- function(dms, with_letter = "AUTO") {
  # browser()
  as.vector(dms)
  ifelse(with_letter == "AUTO", {
    # lk_tb <- data.frame(c("N", "S", "W", "E"), c())
    with_letter <- str_detect(dms, regex("N|S|W|E", ignore_case = T))
  }, {})
  ifelse(with_letter, {
    neg <- grepl(regex("S|W", ignore_case = T), dms)
    dms <- gsub(regex('[[:alpha:]]', ignore_case = T), "", dms)
  }, {})

  # Split the DMS string into components
  parts <- stringr::str_split(dms, "°|'", simplify = TRUE)
  
  # Extract degrees, minutes, and seconds
  degrees <- as.numeric(parts[,1])
  minutes <- as.numeric(parts[,2]) %>% ifelse(is.na(.), 0, .)
  seconds <- as.numeric(parts[,3]) %>% ifelse(is.na(.), 0, .)
  
  # Calculate decimal degrees
  decimal <- round(degrees + (minutes / 60) + (seconds / 3600), digits = 7)
  decimal <- ifelse(neg, -decimal, decimal)
  
  return(decimal)
}