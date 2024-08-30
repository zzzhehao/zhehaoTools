#' Retrieve Metadata of a Notion Page
#' 
#' GET a Notion page with its metadata.
#' 
#' @author Zhehao Hu
#' @return Named character vector or character.
#' 
#' @param id ID of the Notion page.
#' @param secret Notion API token.
#' @param title_only If TRUE, only the page title will return.
#' 
#' @importFrom httr content
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @export
getPage <- function(id, secret, title_only = FALSE){
  headers = c("Authorization" = paste0("Bearer ", secret), "Notion-Version" = '2022-06-28')
  res <- httr::content(httr::GET(
    url = paste0("https://api.notion.com/v1/pages/", id),
    httr::add_headers(.headers = headers)))
  res <- res %>% unlist() 
  if (title_only) {
    return(as.character(res[["properties.Name.title.plain_text"]]))
  } else {
    return(res)
  }
}
