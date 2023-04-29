library(rvest)
library(dplyr)
library(stringr)

url <- "https://www.theguardian.com/film/2023/apr/23/the-three-musketeers-dartagnan-eva-green-shines-as-milady-in-flamboyant-french-adaptation"

html_content <- read_html(url)

author <- html_content |> 
  html_element("address a") |> 
  html_text()

review_title <- html_content |> 
  html_element("div.dcr-1djovmt h1") |> 
  html_text()

film_title <- word(review_title, 1, sep = fixed(" – ")) |> 
  str_remove(" review")

standfirst <- html_content |> 
  html_elements("div.dcr-1yi1cnj")

review_summary <- standfirst |> 
  html_element("p") |> 
  html_text()

stars <- standfirst |> 
  html_elements("svg path") |> 
  html_attr("fill")

if(length(stars) == 0) {
  stars <- html_content |> 
    html_elements("figure div.dcr-15zexom svg path") |> 
    html_attr("fill")
}

stars_num <- length(stars[stars=="#121212"])

review_body <- html_content |> 
  html_elements("div#maincontent p") |> 
  html_text() |> 
  paste(collapse = "\n\n")

data <- tibble(
  film_title = film_title,
  review_title = review_title,
  author = author,
  review_url = url,
  star_rating = stars_num,
  review_summary = review_summary,
  review_body = review_body
)

data_with_date <- data |> 
  mutate(
    year = word(url, 5, sep = fixed("/")),
    month = word(url, 6, sep = fixed("/")),
    day = word(url, 7, sep = fixed("/")),
    date = as.Date(paste0(year, month, day), "%Y%b%d")
  ) |> 
  select(-c(year, month, day))
