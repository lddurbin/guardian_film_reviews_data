# Guardian Film Reviews Scraper

This repository contains an R script that scrapes film reviews from [The Guardian Film website](https://www.theguardian.com/film+tone/reviews). Using packages like rvest, polite, and dplyr, it extracts review details such as titles, authors, star ratings, summaries, and full review texts into a structured table format.

## Features
* Retrieve Film Reviews: Scrape multiple pages of film reviews from The Guardian.
* Structured Output: Extracted data is returned as a tidy tibble for easy analysis.
* Star Rating Extraction: Includes parsing of visual star ratings from the HTML.
* Polite Web Scraping: Utilises the polite package to ensure compliance with web scraping best practices.

## Requirements

The script requires the following R packages, which can be installed by running renv::restore() after you clone the repo:
* rvest - Web scraping.
* dplyr - Data manipulation.
* stringr - String operations.
* polite - Polite web scraping.
* purrr - Functional programming tools.
* cli - Command-line interface messages.

Install them with:

## Usage
1. Clone this repository:

git clone https://github.com/yourusername/guardian-film-reviews-scraper.git
cd guardian-film-reviews-scraper


2.	Run the script in R:

### Source the script (if saved as `guardian_reviews.R`)
source("guardian_reviews.R")

### Fetch reviews for the first 2 pages
reviews <- get_reviews(pages = 1:2)

### View the results
print(reviews)


3.	The get_reviews function will return a tibble containing the following columns:
* review_title: Title of the review.
* film_title: Title of the film.
* author: Author of the review.
* review_url: URL of the review.
* star_rating: Star rating (numeric, 0–5).
* review_summary: Summary of the review.
* review_body: Full text of the review.
* date: Publication date.

## Functions Overview

get_reviews(pages)

Fetches reviews for the specified page numbers.

get_urls(session, pages)

Extracts review URLs from the specified pages.

get_review_data(session, url)

Scrapes detailed data for a single review.

get_text(html_content, tags)

Extracts text from a specific HTML element.

get_texts(html_content, tags)

Extracts and combines multiple texts from specific HTML elements.

get_attributes(html_content, tags)

Extracts attributes (e.g., URLs) from HTML elements.

get_data_table(html_content, standfirst, url, stars)

Creates a tibble containing details of a review.

get_date(data)

Parses the publication date from the review URL and adds it to the data.

get_stars(html_content, tags)

Extracts the number of stars based on SVG attributes.

initiation_message(pages)

Displays a message indicating the scraping process initiation.

film_review_download_status_message(data)

Displays a success message for each downloaded review.

## Notes
* Respect Website Terms: Always ensure your scraping activities comply with the website’s terms of service.
* Rate Limiting: Adjust your scraping frequency if necessary to avoid overloading the website. The polite package should handle this for you.

## License

This project is licensed under the MIT License. See the LICENSE file for details.

## Contributions

Contributions, issues, and feature requests are welcome! Feel free to fork this repository and submit a pull request.

## Acknowledgements

Special thanks to the developers of the R packages that made this project possible:
* rvest
* polite
* dplyr
* purrr
* cli

Happy scraping! 🎥🍿