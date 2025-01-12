library(rvest)
library(dplyr)
library(stringr)
library(polite)
library(purrr)
library(cli)


#' Fetch Reviews from The Guardian Film Website
#'
#' @param pages Numeric vector. The page numbers to fetch reviews from (e.g., 1:5 for pages 1 to 5).
#' @return A tibble containing review details for films.
#' @import polite rvest dplyr purrr cli
#' @export
get_reviews <- function(pages = 1) {
  initiation_message(pages)

  session <- bow("https://www.theguardian.com/film+tone/reviews", force = TRUE)
  review_links <- get_urls(session, pages)

  reviews_table <- map(review_links, ~ get_review_data(session, .x)) |>
    list_rbind()

  check_empty_data(reviews_table, "https://www.theguardian.com/film+tone/reviews")

  return(reviews_table)
}


#' Get URLs of Reviews
#'
#' @param session A polite session object for the target website.
#' @param pages Numeric vector. The page numbers to fetch review URLs from.
#' @return A character vector of review URLs.
#' @import polite rvest purrr
#' @export
get_urls <- function(session, pages) {
  responses <- map(pages, ~ scrape(session, query = list(page = .x)))
  tags <- c("a.u-faux-block-link__overlay", "href")

  review_urls <- map(responses, ~ get_attributes(.x, tags)) |> reduce(c)

  return(review_urls)
}


#' Fetch Data for a Single Review
#'
#' @param session A polite session object for the target website.
#' @param url Character. The URL of a single review.
#' @return A tibble containing details of the review.
#' @import polite rvest dplyr cli
#' @export
get_review_data <- function(session, url) {
  current_session <- nod(session, path = url)
  html_content <- scrape(current_session)
  validate_html(html_content, url)
  validate_element(html_content, "div.dcr-1djovmt h1", "review title")
  standfirst <- html_elements(html_content, "div.dcr-1yi1cnj")

  if_else(
    length(html_elements(standfirst, "svg")) == 0,
    stars <- get_stars(html_content, c("figure div.dcr-15zexom svg path", "fill")),
    stars <- get_stars(standfirst, c("svg path", "fill"))
  )

  data <- get_data_table(html_content, standfirst, url, stars) |> get_date()

  film_review_download_status_message(data)

  return(data)
}


#' Extract Text from HTML
#'
#' @param html_content An HTML document object.
#' @param tags Character. A CSS selector to identify the HTML element containing the text.
#' @return A character string of extracted text.
#' @import rvest
#' @export
get_text <- function(html_content, tags) {
  text <- html_content |>
    html_element(tags) |>
    html_text()

  return(text)
}


#' Extract Multiple Texts from HTML
#'
#' @param html_content An HTML document object.
#' @param tags Character. A CSS selector to identify the HTML elements containing the text.
#' @return A character string combining all extracted texts.
#' @import rvest
#' @export
get_texts <- function(html_content, tags) {
  texts <- html_content |>
    html_elements(tags) |>
    html_text() |>
    paste(collapse = "\n\n")

  return(texts)
}


#' Extract Attributes from HTML Elements
#'
#' @param html_content An HTML document object.
#' @param tags Character vector of length 2. The first element is a CSS selector for the element,
#' and the second is the attribute to extract.
#' @return A character vector of extracted attribute values.
#' @import rvest
#' @export
get_attributes <- function(html_content, tags) {
  element_tags <- tags[[1]]
  attribute_tags <- tags[[2]]

  attributes <- html_content |>
    html_elements(element_tags) |>
    html_attr(attribute_tags)

  if (is.null(attributes)) {
    warning(glue::glue("No attributes found for tag {element_tags}."))
    return(character(0))
  }

  return(attributes)
}


#' Create a Data Table for a Review
#'
#' @param html_content An HTML document object for the review page.
#' @param standfirst An HTML node object for the standfirst section.
#' @param url Character. The review URL.
#' @param stars Numeric. The star rating for the film.
#' @return A tibble containing review details.
#' @import rvest dplyr
#' @export
get_data_table <- function(html_content, standfirst, url, stars) {
  data_table <- tibble(
    review_title = get_text(html_content, "div.dcr-1djovmt h1"),
    film_title = word(review_title, 1, sep = fixed(" – ")) |> str_remove(" review"),
    author = get_text(html_content, "address a"),
    review_url = url,
    star_rating = stars,
    review_summary = get_text(standfirst, "p"),
    review_body = get_texts(html_content, "div#maincontent p")
  )

  return(data_table)
}


#' Add Date Columns to the Data Table
#'
#' @param data A tibble containing review details.
#' @return A tibble with added date columns.
#' @import dplyr
#' @export
get_date <- function(data) {
  data_with_date <- data |>
    mutate(
      year = word(review_url, 5, sep = fixed("/")),
      month = word(review_url, 6, sep = fixed("/")),
      day = word(review_url, 7, sep = fixed("/")),
      date = as.Date(paste0(year, month, day), "%Y%b%d")
    ) |>
    select(-c(year, month, day))

  return(data_with_date)
}


#' Extract Star Ratings from HTML
#'
#' @param html_content An HTML document object.
#' @param tags Character vector of length 2. The first element is a CSS selector for the element,
#' and the second is the attribute to extract.
#' @return Numeric. The number of stars.
#' @import rvest
#' @export
get_stars <- function(html_content, tags) {
  stars <- get_attributes(html_content, tags)
  stars_num <- length(stars[stars == "#121212"])

  return(stars_num)
}


#' Display an Initiation Message
#'
#' @param pages Numeric vector. The page numbers to fetch reviews from.
#' @return None. Prints a message to the console.
#' @import cli
#' @export
initiation_message <- function(pages) {
  pages_num <- if_else(
    length(pages) == 1,
    "a page",
    paste0(length(pages), " pages")
  )

  message <- cli_alert(
    "{.href [Politely](https://dmi3kno.github.io/polite/)} downloading {pages_num} of film reviews from {.href [The Guardian Film website](https://www.theguardian.com/uk/film)}."
  )

  return(message)
}


#' Display Download Status Message for a Review
#'
#' @param data A tibble containing review details.
#' @return None. Prints a message to the console.
#' @import cli
#' @export
film_review_download_status_message <- function(data) {
  author <- data$author
  url <- data$review_url
  title <- data$film_title

  message <- cli_alert_success(
    "Downloaded {author}'s review of {.href [{title}]({url})}."
  )

  return(message)
}


validate_html <- function(html_content, url) {
  if (is.null(html_content) || length(html_content) == 0) {
    stop(glue::glue("Failed to load content from {url}."))
  }
}


validate_element <- function(html_content, tag, element_name) {
  element <- html_element(html_content, tag)
  if (is.null(element)) {
    stop(glue::glue("Missing {element_name} element for the given review."))
  }
}


check_empty_data <- function(data, url) {
  if (nrow(data) == 0) {
    warning(glue::glue("Empty data returned for {url}. Check the structure of the HTML."))
  }
}
