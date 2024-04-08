##### TO DO:
# 3. Can I make it faster?

#
# This is the server logic of a Shiny web application.
# This is where the predictor is analysed, and a prediction is formed
#

library(shiny); library(shinybusy); library(stringr); library(dplyr)

# Define server logic required to predict the next word based on the predictor phrase
function(input, output) {
        
        ## Load the data        
        quadgramsdt <- read.table("quadformodel.csv", header = TRUE, sep = " ")
        trigramsdt <- read.table("triformodel.csv", header = TRUE, sep = " ")
        bigramsdt <- read.table("biformodel.csv", header = TRUE, sep = " ")
        unigramsdt <- read.table("uniformodel.csv", header = TRUE, sep = " ")        
        
        
        
        ## Define the input phrase as a reactive object, string        
        string <- reactive ({
                ui <- unlist(str_split(input$predictor, " "))
                trig <- paste(ui[length(ui)-2], ui[length(ui)-1], ui[length(ui)])
                string <- gsub(" ", "_", tolower(trig))
                return(string)
        })
        
        ## Set the functions for use in the prediction model
        getobsquad <- function(predictor = string(), data = quadgramsdt, discount = 1){
                data$DiscProb <- discount*data$weightedprob
                regex <- sprintf("%s%s%s", "^", predictor, "_")
                relindices <- grep (regex, data$ngram)
                if(length(relindices) >0){
                        obsquad <- data[relindices,]
                }
                else {obsquad <- data[0,]}
                return(obsquad)
        }
        
        getobstri <- function(predictor = string(), data = trigramsdt, discount = 0.1){
                obsquad <- getobsquad()
                data <- data[!(data$ngram %in% obsquad$n1gram),]
                data$DiscProb <- discount*data$weightedprob
                relpred <- sub("^[a-z]+_", "", predictor)
                regex <- sprintf("%s%s%s", "^", relpred, "_")
                relindices <- grep (regex, data$ngram)
                if(length(relindices) >0){
                        obstri <- data[relindices,]
                }
                else {obstri <- data[0,]}
                return(obstri)
        }
        
        getobsbi <- function(predictor = string(), data = bigramsdt, discount = 0.0005){
                obsquad <- getobsquad()
                obstri <- getobstri()
                data <- data[!(data$ngram %in% obsquad$n1gram & data$ngram %in% obstri$n1gram),]
                data$DiscProb <- discount*data$weightedprob
                relpred <- sub("^[a-z]+_[a-z]+_", "", predictor)
                regex <- sprintf("%s%s%s", "^", relpred, "_")
                relindices <- grep (regex, data$ngram)
                if(length(relindices) >0){
                        obsbi <- data[relindices,]
                }
                else {obsbi <- data[0,]}
                return(obsbi)
        }
        
        getunobs <- function(predictor, data = unigramsdt, discount = 0.00001){
                obsquad <- getobsquad()
                obstri <- getobstri()
                obsbi <- getobsbi()
                all_obs <- c(obsquad$finalword, obstri$finalword, obsbi$finalword)
                unobs_words <- data[!(data$ngram %in% all_obs),]
                unobs_words$DiscProb <- discount*unobs_words$weightedprob
                
                return(unobs_words)
                
        }
        
        getpredict <- function(predictor){
                nextword <- rbind(getobsquad(), getobstri(), 
                                  getobsbi(), getunobs())
                
                nextword <- nextword %>%
                        group_by(finalword) %>%
                        summarise(finalprob = sum(DiscProb)) %>%
                        arrange(desc(finalprob))
                
                ans <- sample(nextword$finalword[nextword$finalprob == max(nextword$finalprob)],1)
                return(ans) 
        }
        
        observeEvent(submitButton, {
                Sys.sleep(1)
                
                ## Identify the next word prediction
                output$prediction <- renderText(
                        ifelse(input$predictor =="", " ", getpredict())
                           )
        })
}