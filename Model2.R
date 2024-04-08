# Load relevant libraries
library(tokenizers); library(data.table); library(stringr)

quadgramsdt <- quadgramsdt %>%
        filter(discfact >= 0.0000000001) %>%
        mutate(weightedprob = probability*discfact) %>%
        select(ngram, finalword, weightedprob) %>%
        mutate(reltri = sub("(^[a-z]+_)?","",ngram))
        
trigramsdt <- trigramsdt %>%
        filter(discfact >= 0.0000000001) %>%
        mutate(weightedprob = probability*discfact) %>%
        select(ngram, finalword, weightedprob) %>%
        mutate(relbi = sub("(^[a-z]+_)?","",ngram))
        
bigramsdt <- bigramsdt %>%
        filter(discfact >= 0.0000000001) %>%
        mutate(weightedprob = probability*discfact) %>%
        select(ngram, finalword, weightedprob) %>%
        mutate(reluni = sub("(^[a-z]+_)?","",ngram))

unigramsdt <- unigramsdt %>%
        mutate(weightedprob = probability*discfact) %>%
        select(ngram, finalword, weightedprob)


# FOR SHINY APP, ENSURE ANYTHING ABOVE THIS IS STORED IN FILE? 
# LOOK AT HOW I CAN CHANGE THE NEXT WORD PREDICTED?!? (especially when no examples of observed)

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
        obsquad <- getobsquad(predictor = predictor)
        data <- data[!(data$ngram %in% obsquad$reltri),]
        relpred <- sub("^[a-z]+_", "", predictor)
        regex <- sprintf("%s%s%s", "^", relpred, "_")
        relindices <- grep (regex, data$ngram)
        if(length(relindices) >0){
                obstri <- data[relindices,]
        }
        else {obstri <- data[0,]}
        return(obstri)
}

getobsbi <- function(predictor, data = bigramsdt){
        obstri <- getobstri(predictor = predictor)
        data <- data[!(data$ngram %in% obstri$relbi),]
        relpred <- sub("^[a-z]+_[a-z]+_", "", predictor)
        regex <- sprintf("%s%s%s", "^", relpred, "_")
        relindices <- grep (regex, bigramsdt$ngram)
        if(length(relindices) >0){
                obsbi <- bigramsdt[relindices,]
        }
        else {obsbi <- bigramsdt[0,]}
        return(obsbi)
}

getunobs <- function(predictor, data = unigramsdt){
        obsquad <- getobsquad(predictor = predictor)
        obstri <- getobstri(predictor = predictor)
        obsbi <- getobsbi(predictor = predictor)
        all_obs <- rbind(obsquad, obstri, obsbi)
        unobs_words <- data[!(data$ngram %in% all_obs$finalword),]
        
        return(unobs_words)
        
}

prediction <- function(predictor, d1 = 0.5, d2 = 0.7, d3 =0.9){
        obsquad <- getobsquad(predictor = predictor)
        obstri <- getobstri(predictor = predictor)
        obsbi <- getobsbi(predictor = predictor)
        unobs_words <- getunobs(predictor = predictor)
        
        nextword <- rbind(obsquad, obstri, obsbi, unobs_words)
        
        nextword$multiplier <- ifelse(nextword$source == 4, 1,
                                      ifelse(nextword$source == 3, d3,
                                             ifelse(nextword$source == 2, d2, d1)))
        
        nextword$DiscProb <- nextword$weightedprob * nextword$multiplier
        
        groupedword <- nextword %>%
                group_by(finalword) %>%
                summarise(finalprob = sum(DiscProb)) %>%
                arrange(desc(finalprob))
        
        return(groupedword)
}