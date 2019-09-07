suppressPackageStartupMessages(c(
    library(shiny),
    library(tm),
    library(stringr)))



# Loading Bigram, Trigram and Quadgram Data frame files

quadgram <- readRDS("quadgram.RData");
trigram  <- readRDS("trigram.RData");
bigram   <- readRDS("bigram.RData");

mesg <<- ""

# Cleaning user Input before start Prediction of next word

Predict <- function(x) {
    xclean <- removeNumbers(removePunctuation(tolower(x)))
    xs <- strsplit(xclean, " ")[[1]]
    # Predict the next term of the user input sentence
    # 1. For prediction of the next word, Quadgram is used first
    # 2. If no Quadgram is found, then the Trigram is used
    # 3. If no Trigram is found, then the Bigram is used
    # 4. If no Bigram is found, then the word with highest frequency is returned
    
    
    if (length(xs)>= 3) {
        xs <- tail(xs,3)
        if (identical(character(0),head(quadgram[quadgram$unigram == xs[1] & quadgram$bigram == xs[2] & quadgram$trigram == xs[3], 4],1))){
            Predict(paste(xs[2],xs[3],sep=" "))
        }
        else {mesg <<- "Next word is predicted using 4-gram."; head(quadgram[quadgram$unigram == xs[1] & quadgram$bigram == xs[2] & quadgram$trigram == xs[3], 4],1)}
    }
    else if (length(xs) == 2){
        xs <- tail(xs,2)
        if (identical(character(0),head(trigram[trigram$unigram == xs[1] & trigram$bigram == xs[2], 3],1))) {
            Predict(xs[2])
        }
        else {mesg<<- "Next word is predicted using 3-gram."; head(trigram[trigram$unigram == xs[1] & trigram$bigram == xs[2], 3],1)}
    }
    else if (length(xs) == 1){
        xs <- tail(xs,1)
        if (identical(character(0),head(bigram[bigram$unigram == xs[1], 2],1))) {mesg<<-"No match found. Most common word 'the' is returned."; head("the",1)}
        else {mesg <<- "Next word is predicted using 2-gram."; head(bigram[bigram$unigram == xs[1],2],1)}
    }
}

shinyServer(function(input, output) {
    
    output$prediction <- renderPrint({
        result <- Predict(input$inputString)
        output$text2 <- renderText({mesg})
        result
    });
    
    output$text1 <- renderText({
        input$inputString});
}
)