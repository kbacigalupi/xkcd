
#' @title Retrieve metadata about an xkcd comic
#'
#' @description
#' Given an xkcd comic number, this function retrieves a JSON object
#' describing that comic from the official xkcd API.
#' @importFrom jsonlite read_json
#' @export
xkcd <- function(number) {
  url <- file.path("https://xkcd.com", number, "info.0.json")
  results <- jsonlite::read_json(url)
  x <- new_xkcd(results)
  x <- validate_xkcd(x)
  return(x)
}

new_xkcd <- function(x) {

  stopifnot(is.list(x))

  structure(x,
            class = "skcd")
}

validate_xkcd <- function(x) {
  required_fields <- c("month", "num", "link", "year", "news", "safe_title",
                       "transcript", "alt", "img", "title", "day")

  if(!all(required_fields %in% names(x))) {
    stop ("xkcd object is missing some fields")
  }
}
