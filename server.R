#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# variables for controlling results
filename <- "Apple.csv"
searchterm <- "@Apple"
stocksymbol <- "AAPL"

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    #output$to1 <- renderText({
        #input$company
    #    print(filename)
    #})
    observeEvent(input$company, {
        if(input$company=="Apple") {
            filename <- "Apple.csv"
            searchterm <- "@Apple"
            stocksymbol <- "AAPL"
        } else {
            if(input$company=="Boeing") {
                filename <- "Boeing.csv"
                searchterm <- "@Boeing"
                stocksymbol <- "BA"
            } else {
                if(input$company=="Levis") {
                    filename <- "Levis.csv"
                    searchterm <- "@Levis"
                    stocksymbol <- "LEVI"
                } else {
                    if(input$company=="Tesla") {
                        filename <- "Tesla.csv"
                        searchterm <- "@Tesla"
                        stocksymbol <- "TSLA"
                    } else {
                        filename <- ""
                        searchterm <- ""
                        stocksymbol <- ""
                    }
                    
                }
            }
        }
        
        #output$to1 <- renderText(filename)
        
        #################################################################
        # Model Code Starts Here                                        #
        #################################################################
        if(filename != "") {
            library(twitteR)
            
            # uncomment the following to download fresh data
            #setup_twitter_oauth ***removed for security purposes***
            #myTweets <- searchTwitter(searchterm, lang="en", retryOnRateLimit = 5, n=10000)
            #length(myTweets)
            #
            # convert tweets to a dataframe
            #tweets.df <- twListToDF(myTweets)
            #dim(tweets.df)
            
            
            # code for use with local files
            tweets.df <- read.csv(filename)
            numRows <- nrow(tweets.df) #used later to determine cutomm of number of terms
            #output$to1 <- renderText(numRows)
            
            # if importing from csv, convert columns to correct type
            tweets.df$text <- as.character(tweets.df$text)
            tweets.df$created <- as.POSIXct(tweets.df$created)
            
            # strip any non-UTF-8 characters, required for shiny.io
            Encoding(tweets.df$text) <- "latin1"
            x <- iconv(tweets.df$text, "latin1", "UTF-8", sub='')
            x <- NULL
            
            
            # convert dataframe to a corpus
            library(tm)
            myCorpus <- Corpus(VectorSource(tweets.df$text))
            
            # remove punctuation
            myCorpus <- tm_map(myCorpus, removePunctuation)
            # remove numbers
            myCorpus <- tm_map(myCorpus, removeNumbers)
            # remove http*
            toSpace = content_transformer( function(x, pattern) gsub(pattern,"",x) )
            myCorpus = tm_map( myCorpus, toSpace, "https.*")
            # remove stopwords
            library(stopwords)
            myStopwords <- c(stopwords(language="en", source="smart"))
            
            myCorpus <- tm_map(myCorpus, stripWhitespace)
            myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
            
            #this fails on shiny.io
            #if (!require("pacman")) install.packages("pacman")
            #pacman::p_load_gh("trinker/textstem")
            #pacman::p_load(textstem, dplyr)
            
            #this errors... not sure why
            #myCorpus <- tm_map(myCorpus, lemmatize_words)
            
            
            #Building a Document-Term Matrix
            myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
            #inspect(myDtm)
            
            rowTotals <- apply(myDtm , 1, sum) #Find the sum of words in each Document
            myDtm <- myDtm[rowTotals > 0, ] #remove all docs without words
            
            
            #Frequent Terms and Association
            freq.terms <- findFreqTerms(myDtm, lowfreq=(numRows / 20))
            
            term.freq <- rowSums(as.matrix(myDtm))
            term.freq <- subset(term.freq, term.freq >= (numRows / 20))
            df <- data.frame(term = names(term.freq), freq = term.freq)
            
            
            
            library(ggplot2)
            output$barchart <- renderPlot(ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
                                              xlab("Terms") + ylab("Count") + coord_flip()
                                          ) 
            
            # Word cloud setup
            library(wordcloud)
            m <- as.matrix(myDtm)
            # calculate the frequency of words and sort it by frequency
            word.freq <- sort(rowSums(m), decreasing = T)
            # colors
            pal <- brewer.pal(9, "BuGn")
            pal <- pal[-(1:4)]
            
            # plot word cloud
            output$wordcloud <- renderPlot(wordcloud(words = names(word.freq), freq = word.freq, min.freq = (numRows / 100),
                                                     random.order = F, colors = pal)) 
            
            
            
            # Sentiment analysis
            library(data.table)
            require(devtools)
            #install_github("sentiment140", "okugami79")
            
            library(sentimentr)
            sentiments <- sentiment_by(tweets.df$text)
            sentiments <- as.data.frame(sentiments)
            
            # sentiment plot
            colnames(sentiments)=c("score")
            sentiments$date <- as.IDate(tweets.df$created)
            result <- aggregate(score ~ date, data = sentiments, sum)
            #plot(result, type = "l")
            
            
            # Get quantmod
            if (!require("quantmod")) {
                install.packages("quantmod")
                library(quantmod)
            }
            
            # request stock price data from source "yahoo"
            library(quantmod)
            start <- as.Date(result[1,1])
            end <- as.Date(result[nrow(result),1] + 1)
            stockprices <- getSymbols(stocksymbol, src = "yahoo", from = start, to = end, auto.assign = FALSE)
            stockprices <- data.frame(date=index(stockprices), coredata(stockprices))
            names(stockprices) <- c("date","Open","High","Low","Close","Volume","Adjusted")
            
            # set up a data frame for plotting the Close price
            stockline <- stockprices[,c("date","Close")]
            #plot(result,type="l",col="red")
            #lines(stockline,col="green")
            
            # normalize then replot
            normresult <- result
            normresult$score <- (normresult$score - min(result$score)) / (max(result$score) - min(result$score))
            
            normstock <- stockline
            normstock$Close <- (normstock$Close - min(stockline$Close)) / (max(stockline$Close) - min(stockline$Close))
            
            output$lineplots <- renderPlot({plot(normresult,type="l",col="red")
                                           lines(normstock,col="green")
                                           legend("right", c("Sentiment", "Stock"), col = c("red","green"), lty = 1:3, bg="grey95")#, cex = 0.5)
                                           title(main = "Normalized Sentiment vs. Stock Price Changes by Day")}) 
            
            
            # Correlation
            cordf <- merge(result, stockprices[,c("date","Close")], by = "date")
            output$correlationlabel <- renderText("Correlation estimate:")
            if(nrow(cordf)>2) {
                res <- cor.test(cordf$score, cordf$Close, 
                                method = "pearson")
                output$correlation <- renderUI({
                    HTML(res[[4]])
                })
                
            } else {
                output$correlation <- renderUI({
                    HTML("***Too few days worth of Tweets to produce a correlation result***")
                })
            }
            
            
            
        }
    })
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

})
