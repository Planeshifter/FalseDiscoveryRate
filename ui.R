
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
                  max = 100,
                  value = 40),
      sliderInput("reps", 
                  "Number of replications",
                  min = 1,
                  max = 5000, value=500, step=50),
      sliderInput("pp", 
                  "Percentage of true hypotheses",
                  min = 0.1,
                  max = 0.9, value=0.5, step=0.05),
      numericInput("LL",
                   "Maximal true effect",
                   value=5),
      radioButtons("FUN",label = "Non-zero expectation groups", c("decreasing",
                                                                  "equal",
                                                                  "increasing"), selected="equal")
    ),
    column(6,  # Show a plot of the generated distribution
             tabsetPanel(
               tabPanel("Plots", plotOutput("powerPlot", height=1200)),
               tabPanel("Data",
                        textOutput("activeReplicationText"),
                        dataTableOutput("DataTable"))
             )
           ),
    column(3,   h4("Averages"),
                tableOutput("compTableAll"),
                h4("Data of Single Repitition"),
                uiOutput("replicationSelector"),
                tableOutput("compTable"))
  )

    


  
))
