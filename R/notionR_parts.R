#' Call API
#' 
#' This function is divieded and modified from [getNotionDatabase()] and is utilized in other functions.
#' 
#' @author Eduardo Flores
#' 
#' @param id ID of the Notion page or database.
#' @param secret Notion API token.
#' @param filters A list built with filter operators (see filters) to query database. If NULL will query everything.

callAPI <- function(id, secret, filters, cursor = NULL){
  headers = c(`Authorization` = secret, `Notion-Version` = '2022-06-28', `Content-Type` = 'application/json' )
  res <- httr::POST(url = paste0('https://api.notion.com/v1/databases/', id, '/query'),
                    httr::add_headers(.headers = headers),
                    body = list("filter" = filters,
                                "start_cursor" = cursor),
                    # start_cursor = cursor,
                    encode = "json")
  return( httr::content(res) )
}

#' Call (POST) Notion API
#' 
#' Query a database in Notion with desired filters
#' 
#' @details
#' This function is divieded and modified from [getNotionDatabase()].
#' 
#' @author Eduardo Flores, Zhehao Hu
#' @return as_tibble
#' 
#' @param id ID of the Notion page or database.
#' @param secret Notion API token.
#' @param filters A list built with filter operators (see filters) to query database. If NULL will query everything.
#' 
#' @importFrom httr POST
#' @importFrom httr content
#' @importFrom tibble enframe
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr bind_rows
#' @importFrom tidyr as_tibble
#' @export
postAPI <- function(secret, id, filters = NULL) {
  new_cursor <- TRUE
  dd <- NULL
  cursor <- NULL
  
  while( new_cursor ){
  
    r <- callAPI(id = id,
                 secret = secret,
                 filters = filters,
                 cursor = cursor)
  
    new_cursor <- r$has_more
    cursor <- r$next_cursor
  
    # stack the data.frames together
    tmp <- getItemsAndFlattenIntoDataFrame( r$results )
  
    dd <- dplyr::bind_rows(dd, tmp)
  }
  
  return(tidyr::as_tibble(dd))
}

#' Flatten the results into a usable as_tibble with 1 row per page
#' 
#' @details
#' This function is divieded and modified from [getNotionDatabase()] that performes primary transformation of the query result from a POST response.
#' 
#' @author Eduardo Flores, Zhehao Hu
#' @return as_tibble
#' 
#' @param results POST results.
#' @param cover_and_icon also include cover and icon metadata?
#' 
#' @importFrom tibble enframe
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr bind_rows
#' @importFrom tidyr as_tibble
getItemsAndFlattenIntoDataFrame <- function(results, cover_and_icon = FALSE){

  # adding error-catching when the results are none (i.e., a filter w no results)
  if(length(results) < 1 ){
    dd <- data.frame("results" = "none")
    warning("There are no results from the query. Check your filters. A data.frame will still be exported, with col_name = results and row1 = none")

  }else{
    # the results (i.e., rows) are extracted into a simple data.frame with value being a list of each item's properties and id's
    items <- tibble::enframe(results)

    # now, for each item, we will extract a tidy data.frame where we have all of the columns
    dd <- NULL
    for(i in 1:nrow(items)){
      ## before we tidy up,
      ## add NA's if there is no cover or icon AND we want to based on the option in the parameters of the function

      if (cover_and_icon) {
        if(is.null(  items[[2]][[i]][["cover"]] )){
          items[[2]][[i]][["cover"]] <- as.logical("FALSE")
        }
        if(is.null( items[[2]][[i]][["icon"]] )){
          items[[2]][[i]][["icon"]] <- as.logical("FALSE")
        }
      }

      # this is a tidy dataset with column 1 = name (i.e., value.object.type, etc) and col2 = value (i.e,. d3f0ee76-fc3b-426c-8d23-cff84800b0d6)
      tmp <- tibble::enframe(unlist(items[[i, 2]]))

      # to avoid duplicates, (such as two relationships tied to a page) I will condense them into 1 separated by a pipe
      tmp <- tmp %>%
        group_by(name) %>%
        summarise("value" = paste(value, collapse = " | "))

      # now, I want to keep this as 1 row in a big data set, so I will pivot_wider
      tmp <- tidyr::pivot_wider(tmp)

      # now, I will create one big dataset, I will use dplyr in case columns are not exactly the same, which could be the case if one or various of the properties are missing
      dd <- dplyr::bind_rows(dd, tmp)
    }
  }
  return(as_tibble(dd))
}

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
#' @param clean Do primary clean process?
#'
#' @importFrom httr POST
#' @importFrom httr content
#' @importFrom tibble enframe
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr all_of
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr map_chr
#' @importFrom stringr regex
#' @importFrom stringr str_detect
#' @export

getNotionDatabase <- function(secret, database_id, filters = NULL, show_progress = FALSE, all_pages = TRUE, cover_icon = FALSE, clean = TRUE){
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

      dd <- as_tibble(dplyr::bind_rows(dd, tmp))

      if(show_progress){ print(paste0("- nrow of downloads: ", nrow(dd) )) }
    }
  }else{
    # no pagination, just the top 100
    if(show_progress){ print(paste0("++++ NO PAGINATION: ")) }
    cursor <- NULL
    r <- callAPI(id = database_id, secret = secret, filters = filters, cursor = cursor)

    dd <- getItemsAndFlattenIntoDataFrame( r$results )
  }

  if (clean) {
    sel <- str_detect(names(dd), regex("annotations"), negate = T) & 
      str_detect(names(dd), regex("text\\.content$"), negate = T) &
      str_detect(names(dd), regex("\\.type$|\\.id$"), negate = T)
    sel <- names(dd[, sel])
    dd <- dd %>% dplyr::select(dplyr::all_of(sel))
  }

  return(dd)
}
