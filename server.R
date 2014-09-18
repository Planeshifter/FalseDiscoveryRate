library(shiny)
library(ggplot2)
theme_set(new = theme_grey(base_size = 16))
library(reshape2)
library(grid)
library(plyr)
library(gridExtra)
library(scales)

source("fdr-sim-1.R")
source("summarySE.R")

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
    generate_data()
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
    
    
    pcer = lapply(data$expResults, function(x){ x$table[2, ]})
    pcerDF = as.data.frame(do.call("rbind", pcer))
    pcerDF$run = 1:input$reps
    pcerDF_melt = melt(pcerDF, id.vars = "run")
    pcerDF_summary = summarySE(pcerDF_melt, "value", "variable")
    
    q = ggplot(data = pcerDF_summary, aes(x=variable, y=value, colour=variable)) + 
      geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.1) +
      geom_point()+labs(x="Procedure", y="Error Rate", title="Pairwise-comparison error rate (PCER)")
    
    fwer = lapply(data$expResults, function(x){ x$table[3, ]})
    fwerDF = as.data.frame(do.call("rbind", fwer))
    fwerDF$run = 1:input$reps
    fwerDF_melt = melt(fwerDF, id.vars = "run")
    fwerDF_summary = summarySE(fwerDF_melt, "value", "variable")
    
    r = ggplot(data = fwerDF_summary, aes(x=variable, y=value, colour=variable)) + 
      geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.1) +
      geom_point()+labs(x="Procedure", y="Error Rate", title="Family-wise error rate (FWER)")
    
    fdr = lapply(data$expResults, function(x){ x$table[4, ]})
    fdrDF = as.data.frame(do.call("rbind", fdr))
    fdrDF$run = 1:input$reps
    fdrDF_melt = melt(fdrDF, id.vars = "run")
    fdrDF_summary = summarySE(fdrDF_melt, "value", "variable")
    
    s = ggplot(data = fdrDF_summary, aes(x=variable, y=value, colour=variable)) + 
      geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.1) +
      geom_point()+labs(x="Procedure", y="Error Rate", title="False discovery rate (FDR)")
    
    print(grid.arrange(p,q,r,s, nrow=2, ncol=2))
  },  height = 400, width = 1000)
  
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
