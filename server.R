library(shiny)
library(ggplot2)
library(reshape2)

source("fdr-sim-1.R")

shinyServer(function(input, output) {
  
  data <- reactiveValues()
  generate_data <- reactive(
    data$expResults <- replicate(input$reps, run.exp(input$mm, input$pp, input$LL, f.flat,.05,.05),
                                 simplify = FALSE)
  )

  output$replicationSelector <- renderUI({
    numericInput("active_run", "Select run:", min = 1, max = input$reps, step = 1, value = 1)
  })
  
  output$activeReplicationText <- renderText({
    paste(c("Data of the ", input$active_run, "run:"), sep = " ")
  })
  
  output$powerPlot <- renderPlot({
    generate_data()
    powers = lapply(data$expResults, function(x){x$table[1,]})
    powersDF = as.data.frame(do.call("rbind", powers))
    powersDF$run = 1:input$reps
    powersDF_melt = melt(powersDF, id.vars = "run")
    
    p = ggplot(data = powersDF_melt, aes(x=variable, y=value, fill=variable)) + geom_boxplot() + 
      labs(x="Procedure", y="Power", title="Power of Tests")
    print(p)
  })
  
  output$compTable <- renderTable({
    generate_data()
    print(data$expResults$table)
    data$expResults[[input$active_run]]$table
  })
  
  output$DataTable <- renderDataTable({
    generate_data()
    data$expResults[[input$active_run]]$full.dat               
  })

})
