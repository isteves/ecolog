# functionalize script from 01_one_week
library(RCurl)
library(tidyverse)
library(lubridate)
library(tidytext)

get_ecolog <- function(url) {
    data <- getURL(url)
    
    data_split <- str_split(data, "\n")[[1]]
    
    title <- data_split[str_detect(data_split, " lines")]
    from <- data_split[str_detect(data_split, "From:")]
    datetime <- data_split[str_detect(data_split, "Date:")]
    
    data_clean <- tibble(date = str_replace_all(datetime, ".*</b> \\D{3}, | \\d\\d:\\d\\d:\\d\\d.*", ""),
                         subject = str_replace_all(title, '.*\\d\">|</a>.*', ''),
                         contact = str_replace_all(from, ".*</b> | &lt.*", ""),
                         lines = str_replace_all(title, '.*<i>\\(| lines.*', ''),
                         url = str_replace_all(title, '.*href=\"|\">.*', '')) %>% 
        mutate(lines = as.numeric(lines),
               date = dmy(date))
    
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

# tidy text --------------

ecolog_2018_count <- ecolog_2018_df %>% 
    mutate(number = 1:nrow(ecolog_2018_df)) %>% 
    select(number, subject) %>% 
    unnest_tokens(word, subject) %>% 
    anti_join(stop_words) %>% 
    count(word, sort = TRUE) %>% 
    arrange(desc(n))

write.csv(ecolog_2018_count, "data/ecolog_2018_count.csv")

ecolog_2018_count %>% 
    filter(n > 30) %>% 
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()

library(wordcloud)

ecolog_2018_count %>% 
    with(wordcloud(word, n, max.words = 100))
