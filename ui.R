
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Controlling the False Discovery Rate"),

  # Sidebar with a slider input for number of bins
  
  fluidRow(
    column(3, 
      sliderInput("mm",
                  "Number of hypotheses:",
                  min = 1,
                  max = 50,
                  value = 16),
      sliderInput("reps", 
                  "Number of replications",
                  min = 1,
                  max = 10000, value=600, step=100),
      sliderInput("pp", 
                  "Percentage of true hypotheses",
                  min = 0.1,
                  max = 0.9, value=0.5, step=0.05),
      numericInput("LL",
                   "Maximal true effect",
                   value=5)
    ),
    column(6,  # Show a plot of the generated distribution
           mainPanel(
             tabsetPanel(
               tabPanel("Comparisons"),
               tabPanel("Summary", plotOutput("powerPlot")),
               tabPanel("Data",
                        textOutput("activeReplicationText"),
                        dataTableOutput("DataTable"))
             )
           )),
    column(3,   uiOutput("replicationSelector"),
                tableOutput("compTable"))
  )

    


  
))
