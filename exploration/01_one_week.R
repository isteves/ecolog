library(RCurl)
library(tidyverse)
library(lubridate)

#start by trying out cleaning/processing on one week's worth of data...

# Clean data -----------------

url <- "https://listserv.umd.edu/cgi-bin/wa?A1=ind1803b&L=ecolog-l"
data <- getURL(url)

data_split <- str_split(data, "\n")[[1]]

title <- data_split[str_detect(data_split, "lines")]
from <- data_split[str_detect(data_split, "From:")]
datetime <- data_split[str_detect(data_split, "Date:")]

#title: title, url, lines
lines <- str_replace_all(title, '.*<i>\\(| lines.*', '')
subject <- str_replace_all(title, '.*\\d\">|</a>.*', '')
url <- str_replace_all(title, '.*href=\"|\">.*', '') 

#from
contact <- str_replace_all(from, ".*</b> | &lt.*", "")

#date...time doesn't really matter
#time zone could be interesting... (but ignore for now)
date <- str_replace_all(datetime, ".*</b> \\D{3}, | \\d\\d:\\d\\d:\\d\\d.*", "")

data_clean <- tibble(date = str_replace_all(datetime, ".*</b> \\D{3}, | \\d\\d:\\d\\d:\\d\\d.*", ""),
                     subject = str_replace_all(title, '.*\\d\">|</a>.*', ''),
                     contact = str_replace_all(from, ".*</b> | &lt.*", ""),
                     lines = str_replace_all(title, '.*<i>\\(| lines.*', ''),
                     url = str_replace_all(title, '.*href=\"|\">.*', '')) %>% 
    mutate(lines = as.numeric(lines),
           date = dmy(date))

# Organize for text ----------------

library(tidytext)

data_clean %>% 
    mutate(number = 1:nrow(data_clean)) %>% 
    select(number, subject) %>% 
    unnest_tokens(word, subject) %>% 
    anti_join(stop_words) %>% 
    count(word, sort = TRUE) %>% 
    filter(n > 5) %>% 
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()
