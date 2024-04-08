# Load relevant libraries
library(tokenizers); library(data.table); library(stringr)
                
# Identify all observed examples of the predictor text
getobsquad <- function(predictor, data = quadgramsdt){
        regex <- sprintf("%s%s%s", "^", predictor, "_")
        relindices <- grep (regex, quadgramsdt$ngram)
        if(length(relindices) >0){
                obsquad <- quadgramsdt[relindices,]
        }
        else {obsquad <- quadgramsdt[0,]}
        return(obsquad)
}
        
getobstri <- function(predictor, data = trigramsdt){
        relpred <- sub("^[a-z]+_", "", predictor)
        regex <- sprintf("%s%s%s", "^", relpred, "_")
        relindices <- grep (regex, trigramsdt$ngram)
        if(length(relindices) >0){
                obstri <- trigramsdt[relindices,]
        }
        else {obstri <- trigramsdt[0,]}
        return(obstri)
}
        
getobsbi <- function(predictor, data = bigramsdt){
        relpred <- sub("^[a-z]+_[a-z]+_", "", predictor)
        regex <- sprintf("%s%s%s", "^", relpred, "_")
        relindices <- grep (regex, bigramsdt$ngram)
        if(length(relindices) >0){
                obsbi <- bigramsdt[relindices,]
        }
        else {obsbi <- bigramsdt[0,]}
        return(obsbi)
}


# Using the unigram, bigram and trigram counts previously calculated, assess the likelihood of the next word
        
## TO TROUBLESHOOT: all weighted probabilities seem to be the same?!

prediction <- function(predictor, d1 = 0.5, d2 = 0.7, d3 = 0.9){
        obsquad <- getobsquad(predictor = predictor)
        obstri <- getobstri(predictor = predictor)
        obsbi <- getobsbi(predictor = predictor)
        
        nextword <- rbind(obsquad, obstri, obsbi)
        
        nextword$prediction <- sub(".*_","",nextword$ngram)
        nextword$source <- str_count(nextword$ngram, "_") + 1
        nextword$multiplier <- ifelse(nextword$source == 4, 1,
                                ifelse(nextword$source == 3, d3,
                                    ifelse(nextword$source == 2, d2, d1)))
        nextword$DiscProb <- nextword$probability * nextword$multiplier
        
        groupedword <- nextword %>%
                group_by(prediction) %>%
                summarise(weightedprob = sum(DiscProb)) %>%
                arrange(desc(weightedprob))
                          
        return(groupedword)
        }        

### TO DO: FIGURE OUT HOW TO BUILD IN PROBABILITY OF UNOBSERVED COMBINATIONS AND CALCULATE DISCOUNT FACTORS 



components <- tail(strsplit(nextword$ngram, "_")[[1]],1)

# Extract the last word (i.e., the last element of the components vector)
last_word <- tail(components, 1)



, getobsquad = getobsquad(predictor = predictor), 
getobstri = getobstri(predictor = predictor), 
getobsbi = getobsbi(predictor = predictor)


obstrigrams <- function(trigramsdt, bigramsdt, predictor, d2 = 0.1) {
        if(nrow(obstrigrams) < 1) return(NULL)
        obsCount <- filter(bigramsdt, ngram==predictor)$frequency[1]
        obsTrigProbs <- mutate(obstrigams, frequency=((frequency - d2) / obsCount))
        colnames(obsTrigProbs) <- c("ngram", "prob")
                
        return(obsTrigProbs)
        }        
        
        unigramsdt

        
        
# EXAMPLE CODE:
        kats_backoff <- function(word, unigram_prob, bigram_prob, lambda = 0.5) {
                if (word %in% names(bigram_prob)) {
                        prob <- (1 - lambda) * bigram_prob[word] + lambda * unigram_prob[word[2]]
                } else {
                        prob <- lambda * unigram_prob[word[2]]
                }
                return(prob)
        }
        
        
# Assign discount values