#Load the necessary packages
library(quanteda.textstats); library(data.table); library(dplyr); library(textcat)

# Create a table showing the number of lines in the full data set for each file, 
# and the number taken in the training datasets

sumLines <- tibble(Source_file = c("Blogs","News","Twitter"),
                   Total_lines = c(len1, len2, len3), 
                   Sample_lines = c(length(blogssub), length(newssub),length(twitsub)))

# Manipulate the training datasets to tokens
blogssub <- tokens(blogssub, remove_punct = TRUE)
newssub <- tokens(newssub, remove_punct = TRUE)
twitsub <- tokens(twitsub, remove_punct = TRUE)

# Create tables of the most frequent word for each source
dfmat_blogs <- blogssub %>% tokens_remove(stopwords("english")) %>% dfm()
dfmat_news <- newssub %>% tokens_remove(stopwords("english")) %>% dfm()
dfmat_tweets <- twitsub %>% tokens_remove(stopwords("english")) %>% dfm()

# Create a table summarising the lines and words in each training dataset
sumSamples <- tibble(Sample = c("Blogs","News","Twitter"),
                     Total_lines = c(length(blogssub), length(newssub), length(twitsub)),
                     Total_words = c(sum(ntoken(blogssub)),sum(ntoken(newssub)),sum(ntoken(twitsub))),
                     Max_words = c(max(ntoken(blogssub)), max(ntoken(newssub)),max(ntoken(twitsub))),
                     Ave_words = c(round(mean(ntoken(blogssub)),0), round(mean(ntoken(newssub)),1),
                                   round(mean(ntoken(twitsub)),1)),
                     Most_freq_word = c(textstat_frequency(dfmat_blogs,n=1)[1,1],
                                        textstat_frequency(dfmat_news,n=1)[1,1],
                                        textstat_frequency(dfmat_tweets,n=1)[1,1]))

# Create some plots showing features of the data
par(mfrow = c(1,3))
hist(ntoken(blogssub), col = "skyblue", main = "Blogs", xlab = "No. words"); 
hist(ntoken(newssub), col = "seagreen", main = "News", xlab = "No. words"); 
hist(ntoken(twitsub), col = "salmon", main = "Twitter", xlab = "No. words") 

# Consider most frequent bigrams and trigrams from each dataset
blogs_bi <- blogssub %>% tokens_remove(stopwords("english")) %>% tokens_ngrams(n = 2) %>% dfm()
blogs_tri <- blogssub %>% tokens_remove(stopwords("english")) %>% tokens_ngrams(n = 3) %>% dfm()
news_bi <- newssub %>% tokens_remove(stopwords("english")) %>% tokens_ngrams(n = 2) %>% dfm()
news_tri <- newssub %>% tokens_remove(stopwords("english")) %>% tokens_ngrams(n = 3) %>% dfm()
twit_bi <- twitsub %>% tokens_remove(stopwords("english")) %>% tokens_ngrams(n = 2) %>% dfm()
twit_tri <- twitsub %>% tokens_remove(stopwords("english")) %>% tokens_ngrams(n = 3) %>% dfm()

# Consider how many foreign words occur in the dataset
frequency <- mutate(frequency, language = textcat(word))
languages <- summarise(frequency, language)

# Consider how many unique words are needed in a frequency sorted dictionary to 
# cover 50% of all word instances in the language? 90%?

dfmat_all <- rbind(dfmat_blogs, dfmat_news, dfmat_tweets)
all_ranked <- textstat_frequency(dfmat_tweets)
all_ranked <- all_ranked %>%
        mutate(cumfreq = cumsum(frequency)) %>%
        mutate(prop = cumfreq/(sum(frequency)))

# Note that the first 559 words provide 50% coverage, while 13,636 words provides 90% coverage