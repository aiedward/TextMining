# ui.R

library(shiny)
library(reshape)


shinyUI(
  pageWithSidebar(
  headerPanel('LDA Helper'),
  sidebarPanel(
    textInput("inputSql", 
              "Input Data SQL:",
              "Insert SQL!!"),
    textInput("projectName",
              "Project Name:"),
    sliderInput("sparse",
                "Sparse Ratio:",
                min = 0.9,  max = 1, value = 0.99, step=0.001),
    
    numericInput('clusters', 'Cluster count', 10,
                 min = 2, max = 50),
    
    numericInput('terms', 'Term count', 50,
                 min = 10, max = 100),

    checkboxInput("visual", "Visualization", value = TRUE),
    
    checkboxInput("MSC", "Manual Spam Check", value = FALSE),
    
    submitButton("Execute LDA")
    
  ),
  mainPanel(
    fluidRow(
      column(5,h4("Console")),
      column(5,textOutput("visual")),
      column(2,downloadButton('downloadData', 'Download'))
    ),
    verbatimTextOutput("console"),
    plotOutput("network"),
    tableOutput("lda")
  )
))