

install.packages("tau")
install.packages("XML")

# ---- Load Libraries to use the string funcions and XML functions ----
library(shiny)
library(dplyr)
library(ggplot2)
library(XML)
library(tau)

# Import the data into a Data-Frame
setwd("/Users/gandara/Documents/master data science paper/UvA")
getwd()
xmldataframe <- xmlToDataFrame("data-science-A.xml")

ger <- "hola"
myFactor1<-c("1", "2", "3")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("Mining Words!"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      radioButtons("documentNo",
                   label = "Select a Document to Mine:",
                   choices = myFactor1),
      
      # Input: Slider for the number of document ----
      sliderInput(inputId = "topN",
                  label = "Top N.:",
                  min = 1,
                  max = 15,
                  value = 5),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
    )
  )
)

red.bold.italic.text <- element_text(face = "bold", color = "black", size = 12)
blue.bold.italic.16.text <- element_text(face = "bold.italic", color = "red", size = 16)



# Define server logic required to draw a histogram ----
server <- function(input, output) {
  datasetInput <- reactive({
    
    #take the selected TEXT for the Selected Document
    oz <- xmldataframe$text[as.numeric(input$documentNo)]
    #convert the TEXT to lower case to find the word no matter the uppercase
    oz.counts = textcnt(oz, n=1, method="string", tolower=T)
    #create a data frame with all the words and the number of instances
    oz.counts.df = data.frame(word = names(oz.counts), count = c(oz.counts))
    
    dataset1 <- oz.counts.df %>% top_n(as.numeric(input$topN), count)
    dataset2 <- oz.counts.df %>% top_n(as.numeric(input$topN), count)
    dataset3 <- oz.counts.df %>% top_n(as.numeric(input$topN), count)
    
    switch(input$documentNo,
           "1" = dataset1,
           "2" = dataset2,
           "3" = dataset3)
  })
  
  output$distPlot <- renderPlot({
    ggplot(data = datasetInput()) +
      geom_bar(colour="black", mapping = aes(x = word, y = count), fill = 5555, stat = "identity", 
               position=position_dodge(),
               size=.3) +                        # Thinner lines
                xlab(paste("TOP ", input$topN)) + ylab("Number of Instances of the WORD") + # Set axis labels
      ggtitle(paste("Document:", xmldataframe$docno[as.numeric(input$documentNo)], " ", 
              xmldataframe$section[as.numeric(input$documentNo)])) +
      theme(title = blue.bold.italic.16.text, axis.title = red.bold.italic.text)
  })
}


# Run app
shinyApp(ui,server)
