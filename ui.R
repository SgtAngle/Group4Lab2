#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    theme = shinytheme("superhero"),
    # Application title
    titlePanel("Does Twitter Sentiment Affect/Reflect Stock Price?"),
    h3("CSDA1040 - Group 4"),
    h5("Shawn Mills, Steven Too Heng Kwee"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("company", "Select company:", 
                        choices=c("","Apple","Boeing","Levis","Tesla")),
            hr(),
            helpText("Sample data from four predetermined companies ending June 14, 2019")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Normalized Plot", 
                         plotOutput("lineplots"),
                         textOutput("correlationlabel"),
                         htmlOutput("correlation")
                ),
                tabPanel("Word Cloud", 
                         plotOutput("wordcloud")
                ),
                tabPanel("Word Count Bar Chart", 
                         plotOutput("barchart"))
            )
            #"This is the main panel",
            #plotOutput("barchart"),
            #plotOutput("lineplots"),
            #textOutput("correlationlabel"),
            #htmlOutput("correlation"),
            #textOutput("to1")
            #plotOutput("distPlot")
            #plotOutput(outputId = "main_plot", height = "300px")
        )
    )
))
