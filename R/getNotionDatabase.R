# Origin: https://github.com/Eflores89/notionR/blob/main/R/getNotionDatabase.R

#' Returns a database as a data.frame
#'
#' Query a database in Notion with desired filters and get a database as a data.frame in R or download the entire database (all pages).
#'
#' @details This is actually a POST request as per Notions API: https://developers.notion.com/reference/post-database-query
#'
#' @author Eduardo Flores
#' @return data.frame
#'
#' @param secret Notion API token
#' @param database_id Notion database ID. Use normalizeChromaPageIds if using directly from browser.
#' @param filters A list built with filter operators (see filters) to query database. If NULL will query everything.
#' @param show_progress show prints of progress?
#' @param all_pages download all pages (loop thru paginations)?
#' @param cover_icon also include cover and icon metadata?
#'
#'
#' @importFrom httr POST
#' @importFrom httr content
#' @importFrom tibble enframe
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr bind_rows
#' @export

getNotionDatabase <- function(secret, database_id, filters = NULL, show_progress = FALSE, all_pages = TRUE, cover_icon = FALSE){
  options(dplyr.summarise.inform = FALSE) # to supress all the grouping warnings!

  # +++++++++ construct headers
  headers = c(`Authorization` = secret, `Notion-Version` = '2022-06-28', `Content-Type` = 'application/json' )

  if(all_pages){
    if(show_progress){ print(paste0("++++ PAGINATING CALLS: ")) }

    # if using all_pages, I will run all of the pagination available.
    new_cursor <- TRUE
    dd <- NULL
    cursor <- NULL

    while( new_cursor ){
      if(show_progress){ print(paste0("- cursor: ", cursor, " / new_cursor: ", new_cursor )) }

      r <- callAPI(id = database_id, secret = secret, filters = filters, cursor = cursor)

      new_cursor <- r$has_more
      cursor <- r$next_cursor

      # stack the data.frames together
      tmp <- getItemsAndFlattenIntoDataFrame( r$results )

      dd <- dplyr::bind_rows(dd, tmp)

      if(show_progress){ print(paste0("- nrow of downloads: ", nrow(dd) )) }
    }
  }else{
    # no pagination, just the top 100
    if(show_progress){ print(paste0("++++ NO PAGINATION: ")) }
    cursor <- NULL
    r <- callAPI(id = database_id, secret = secret, filters = filters, cursor = cursor)

    dd <- getItemsAndFlattenIntoDataFrame( r$results )
  }

  return(dd)
}
