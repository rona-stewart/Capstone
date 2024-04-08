# Load the relevant libraries
library(tokenizers); library(dplyr); library(quanteda.textstats); library(data.table)

# Convert corpus to a dataframe including lines
data <- data.frame(text = sapply(allclean, as.character), stringsAsFactors = FALSE)
data <- unlist(data) %>%
        tokens(what = "word", remove_punct = TRUE) %>%
        tokens_remove(stopwords("english"))

# Manipulate the dataset to tokens and create ordered objects for each of uni, tri and quadgrams
unigrams <- data %>%
        dfm() %>%
        textstat_frequency()

unigramsdt <- data.table(ngram = unigrams$feature, frequency = unigrams$frequency, 
                     probability = unigrams$frequency / sum(unigrams$frequency))

rm(unigrams)

bigrams <- data %>% 
        tokens_ngrams(n = 2) %>%
        dfm() %>%
        textstat_frequency()

bigramsdt <- data.table(ngram = bigrams$feature, frequency = bigrams$frequency, 
                     probability = bigrams$frequency / sum(bigrams$frequency))

rm(bigrams)

trigrams <- data %>% 
        tokens_ngrams(n = 3) %>%
        dfm() %>%
        textstat_frequency()

trigramsdt <- data.table(ngram = trigrams$feature, frequency = trigrams$frequency, 
                     probability = trigrams$frequency / sum(trigrams$frequency))

rm(trigrams)

quadgrams <- data %>% 
        tokens_ngrams(n = 4) %>%
        dfm() %>%
        textstat_frequency()

quadgramsdt <- data.table(ngram = quadgrams$feature, frequency = quadgrams$frequency, 
                     probability = quadgrams$frequency / sum(quadgrams$frequency))

rm(quadgrams)
