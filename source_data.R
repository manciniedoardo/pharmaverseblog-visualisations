# Pharmaverseblog location ---
html_link <- "https://pharmaverse.github.io/blog/"

# Harvest pharmaverseblog ----
html <- rvest::read_html(html_link)

row <- html_elements(html, ".quarto-post, .image-right")

raw_posts <- tibble(
  title = row %>% html_element(".listing-title") %>% html_text2(),
  description = row %>% html_element(".listing-description") %>% html_text2(),
  date = row %>% html_element(".listing-date") %>% html_text2() %>% lubridate::mdy(),
  authors = row %>% html_element(".listing-author") %>% html_text2(),
  categories = row %>% html_element(".listing-categories") %>% html_text2(),
  link = row %>% html_element(".metadata,a") %>% html_attr("href") %>% str_replace("./", html_link),
) 
  
# Tidy up data ----
posts <- raw_posts %>% 
  # Add link to posts
  mutate(title = paste0("<a href='", link, "'>", title,"</a>")) %>% 
  select(-link)


# Summary stats -----

## Posts ----
unique_posts <- nrow(posts)

earliest_post_date <- posts %>% 
  arrange(date) %>% 
  filter(row_number() == 1) %>% 
  select(date) %>% 
  pull()

latest_post_date <- posts %>% 
  arrange(desc(date)) %>% 
  filter(row_number() == 1) %>% 
  select(date) %>% 
  pull()

days_since_last_post <- difftime(Sys.Date(), as.Date(latest_post_date))

## Authors ----
post_authors <- posts %>%
  separate_longer_delim(
    cols = authors,
    delim = ","
  ) %>%
  mutate(authors = str_trim(authors)) %>%
  count(authors)

post_authors$authors <- factor(
  post_authors$authors, 
  levels = post_authors$authors[order(post_authors$n, decreasing = TRUE)]
)

unique_authors <- post_authors %>% 
  select(authors) %>% 
  distinct() %>% 
  nrow()

## Categories ----
post_categories <- posts %>%
  separate_longer_delim(
    cols = categories,
    delim = "\n"
  ) %>%
  mutate(categories = str_trim(categories)) %>%
  count(categories) %>%
  arrange(desc(n))

post_categories$categories <- factor(
  post_categories$categories, 
  levels = post_categories$categories[order(post_categories$n, decreasing = TRUE)]
)

## Posts by month ----

# Extracting month and year
posts$month_year <- format(posts$date, "%Y-%m")

# Creating a complete dataset with all months within the range of data
all_months <- data.frame(month_year = seq.Date(min(posts$date), max(posts$date), by = "month"))
all_months$month_year <- format(all_months$month_year, "%Y-%m")

# Merging the complete dataset with the counts of posts per month
posts_per_month <- all_months %>%
  full_join(posts %>% count(month_year), by = "month_year") %>%
  replace_na(list(n = 0))
