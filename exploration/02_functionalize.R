# functionalize script from 01_one_week
library(RCurl)
library(tidyverse)
library(lubridate)
library(tidytext)
library(wordcloud)

get_ecolog <- function(url) {
    data <- RCurl::getURL(url)
    
    data_split <- stringr::str_split(data, "\n")[[1]]
    
    title <- data_split[stringr::str_detect(data_split, "\\d lines")]
    from <- data_split[stringr::str_detect(data_split, "<b>From:")]
    datetime <- data_split[stringr::str_detect(data_split, "<b>Date:")]
    
    data_clean <- tibble(date = stringr::str_replace_all(datetime, ".*</b> \\D{3}, | \\d\\d:\\d\\d:\\d\\d.*", ""),
                         subject = stringr::str_replace_all(title, '.*\\d\">|</a>.*', ''),
                         contact = stringr::str_replace_all(from, ".*</b> | &lt.*", ""),
                         lines = stringr::str_replace_all(title, '.*<i>\\(| lines.*', ''),
                         url = stringr::str_replace_all(title, '.*href=\"|\">.*', '')) %>% 
        mutate(lines = as.numeric(lines),
               date = lubridate::dmy(date))
    
    return(data_clean)
    print(data_clean$date[1])
}

urls <- c("https://listserv.umd.edu/cgi-bin/wa?A1=ind1803c&L=ecolog-l",
          "https://listserv.umd.edu/cgi-bin/wa?A1=ind1803b&L=ecolog-l",
          "https://listserv.umd.edu/cgi-bin/wa?A1=ind1803a&L=ecolog-l",
          "https://listserv.umd.edu/cgi-bin/wa?A1=ind1802d&L=ecolog-l",
          "https://listserv.umd.edu/cgi-bin/wa?A1=ind1802c&L=ecolog-l",
          "https://listserv.umd.edu/cgi-bin/wa?A1=ind1802b&L=ecolog-l",
          "https://listserv.umd.edu/cgi-bin/wa?A1=ind1802a&L=ecolog-l",
          "https://listserv.umd.edu/cgi-bin/wa?A1=ind1801e&L=ecolog-l",
          "https://listserv.umd.edu/cgi-bin/wa?A1=ind1801d&L=ecolog-l",
          "https://listserv.umd.edu/cgi-bin/wa?A1=ind1801c&L=ecolog-l",
          "https://listserv.umd.edu/cgi-bin/wa?A1=ind1801b&L=ecolog-l",
          "https://listserv.umd.edu/cgi-bin/wa?A1=ind1801a&L=ecolog-l")

ecolog_2018 <- lapply(urls, get_ecolog)

ecolog_2018_df <- do.call(rbind, ecolog_2018)

write.csv(ecolog_2018_df, "data/ecolog_2018.csv")

# get entire archive -------------

home_page <- getURL("https://listserv.umd.edu/archives/ecolog-l.html")

ecolog_urls <- home_page %>% 
    str_extract_all("href=.*>") %>% 
    unlist() %>% 
    as_tibble() %>% 
    filter(str_detect(value, "week")) %>% 
    separate(value, c("href", "url", "week"), '\"') %>% 
    select(url) %>% 
    mutate(url = paste0("https://listserv.umd.edu", url)) 

#use for-loop to better evaluate break-points:
ecolog_all <- vector("list", length(ecolog_urls$url))
for(i in 267:1081){
    ecolog_all[[i]] <- get_ecolog(ecolog_urls$url[i])
}

# get_ecolog("https://listserv.umd.edu/cgi-bin/wa?A1=ind1310a&L=ecolog-l")

ecolog_all_df <- do.call(rbind, ecolog_all)
write.csv(ecolog_all_df, "data/ecolog_all.csv", row.names = FALSE)

# tidy text, 2018 --------------

ecolog_2018_count <- ecolog_2018_df %>% 
    mutate(number = 1:nrow(ecolog_2018_df)) %>% 
    select(number, subject) %>% 
    unnest_tokens(word, subject) %>% 
    anti_join(stop_words) %>% 
    mutate(word = str_replace(word, "s$", "")) %>% 
    count(word, sort = TRUE) %>% 
    arrange(desc(n))

write.csv(ecolog_2018_count, "data/ecolog_2018_count.csv", row.names = FALSE)

ecolog_2018_count %>% 
    filter(n > 30) %>% 
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()

ecolog_2018_count %>% 
    with(wordcloud(word, n, max.words = 100))

# tidy text, all -------------

ecolog_all_count <- ecolog_all_df %>% 
    mutate(number = 1:nrow(ecolog_all_df)) %>% 
    select(number, subject) %>% 
    unnest_tokens(word, subject) %>% 
    anti_join(stop_words) %>% 
    mutate(word = str_replace(word, "s$", "")) %>% 
    count(word, sort = TRUE) %>% 
    arrange(desc(n))

write.csv(ecolog_all_count, "data/ecolog_all_count.csv", row.names = FALSE)

ecolog_all_count %>% 
    filter(n > 1000) %>% 
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()

ecolog_all_count %>% 
    with(wordcloud(word, n, max.words = 100))
