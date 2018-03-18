#topic modeling

library(tidytext)
library(topicmodels)

#convert to document matrix
        # each row represents one document (such as a book or article),
        # each column represents one term, and
        # each value (typically) contains the number of appearances of that term in that document

ecolog_2018_dfm <- ecolog_2018_df %>% 
    mutate(number = 1:nrow(ecolog_2018_df)) %>% 
    select(number, subject) %>% 
    unnest_tokens(word, subject) %>% 
    anti_join(stop_words) %>% 
    group_by(number) %>% 
    count(word, sort = TRUE) %>% 
    cast_dfm(number, word, n)

el_lda <- LDA(ecolog_2018_dfm, k = 2, control = list(seed = 1234))

el_top_terms <- el_lda %>% 
    tidy() %>% 
    group_by(topic) %>% 
    top_n(10, beta) %>% 
    ungroup() %>% 
    arrange(topic, -beta)

el_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()

#beta = probability per topic per word

beta_spread <- el_lda %>% 
    tidy() %>%
    mutate(topic = paste0("topic", topic)) %>%
    spread(topic, beta) %>%
    filter(topic1 > .001 | topic2 > .001) %>%
    mutate(log_ratio = log2(topic2 / topic1)) 

beta_spread %>% 
    top_n(20, abs(log_ratio)) %>%
    mutate(term = reorder(term, log_ratio)) %>%
    ggplot(aes(term, log_ratio)) +
    geom_col(show.legend = FALSE) +
    coord_flip()

#gamma = probability per-document-per-topic

el_posts <- tidy(el_lda, matrix = "gamma") 


# try with more than 2 topics -------------

el_lda <- LDA(ecolog_2018_dfm, k = 10, control = list(seed = 1234))
tidy(el_lda, matrix = "gamma") %>% 
    top_n(20, abs(gamma)) %>%
    mutate(term = reorder(term, log_ratio)) %>%
    ggplot(aes(term, log_ratio)) +
    geom_col(show.legend = FALSE) +
    coord_flip()

# nvm...let's just go for classifying based on logical categories...
