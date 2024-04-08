# Clean the train data (blogssub, newssub, twitsub)
alldata <- c(blogssub, newssub, twitsub)
temp_corpus <- Corpus(VectorSource(alldata))
        
# Set a function to replace unwanted characters / patterns with space
contospace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

# Remove any URLs (identified by ftp(s) or http(s)://)
allclean <- tm_map(temp_corpus, contospace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
        
# Remove any email addresses (identified by ___@___.___)
allclean <- tm_map(allclean, contospace, "^([A-Za-z]|[0-9]).*@(([A-Za-z]|[0-9]).*).(([A-Za-z]|[0-9]).*)")
        
# Remove any Twitter handles (identified by @____)
allclean <- tm_map(allclean, contospace, "^@(([A-Za-z]|[0-9]).+)")
        
# Convert all characters to lower case
allclean <- tm_map(allclean, tolower)
        
# Remove punctuation (given propensity of emojis, <3 etc. in tweets in particular)
allclean <- tm_map(allclean, removePunctuation)
        
# Remove numbers
allclean <- tm_map(allclean, removeNumbers)

# Remove profanities / slang words (note only first c.7,000 words considered for c.80% coverage) 
# Set a vector of words which should be removed
wordstoremove <- readLines("./rem_words.txt", skipNul = TRUE)
allclean <- tm_map(allclean, removeWords, wordstoremove)

# Clean memory space
rm(blogssub, newssub, twitsub, alldata, temp_corpus, contospace, wordstoremove)