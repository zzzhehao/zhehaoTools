% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/notionR_parts.R
\name{getItemsAndFlattenIntoDataFrame}
\alias{getItemsAndFlattenIntoDataFrame}
\title{Flatten the results into a usable as_tibble with 1 row per page}
\usage{
getItemsAndFlattenIntoDataFrame(results, cover_and_icon = FALSE)
}
\arguments{
\item{results}{POST results.}

\item{cover_and_icon}{also include cover and icon metadata?}
}
\value{
as_tibble
}
\description{
Flatten the results into a usable as_tibble with 1 row per page
}
\details{
This function is divieded and modified from \code{\link[=getNotionDatabase]{getNotionDatabase()}} that performes primary transformation of the query result from a POST response.
}
\author{
Eduardo Flores, Zhehao Hu
}
