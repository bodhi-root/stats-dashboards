# File: app.R
# Simple shiny app that lets the user input a univariate data set and produces
# summary statistics and plots
#

library(shiny)
library(stringr)

source("funcs/univariate.R")

#' Parses the input text.  This can be either comma-delimited
#' or whitespace-delimited.
#' 
parse_input_data <- function(txt) {
  #print(txt)
  if (grepl(",", txt)) {
    x_txt <- str_split(txt, ",")[[1]]
  } else {
    x_txt <- str_split(txt, "\\s+")[[1]]
  }
  
  x <- as.numeric(str_trim(x_txt))
  x[!is.na(x)]
}

server = function(input, output, session) {
  
  # place to store input vector in a way that charts will update
  values <- reactiveValues(x=c())
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$SUBMIT, {
    values$x <- parse_input_data(input$INPUT_DATA)
    values$stats <- proc_univariate(values$x)
    
    # locality tests
    
    t_result <- t.test(x)
    s_result <- sign.test(x)
    w_result <- wilcox.test(x)
    
    values$locality_test <- list(
      t_statistic = t_result$statistic,
      t_pvalue = ifelse(t_result$p.value < .0001, "<.0001", t_result$p.value),
      
      s_statistic = s_result$statistic,
      s_pvalue = ifelse(s_result$p.value < .0001, "<.0001", s_result$p.value),
      
      w_statistic = w_result$statistic,
      w_pvalue = ifelse(w_result$p.value < .0001, "<.0001", w_result$p.value)
    )
  })
  
  output$PLOT_HISTOGRAM <- renderPlot({
    x    <- values$x
    if(length(x) == 0) {
      return(NULL)
    }
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, xlab="", main="") #, breaks = bins)
  })
  
  output$PLOT_BOX <- renderPlot({
    x <- values$x
    if (length(x) == 0) {
      return(NULL)
    }
    
    boxplot(x, horizontal = T, frame=F)
  })
  
  output$STATS_N               <- renderText(values$stats$moments["N"])
  output$STATS_MEAN            <- renderText(values$stats$moments["Mean"])
  output$STATS_STD_DEVIATION   <- renderText(values$stats$moments["Std Deviation"])
  output$STATS_SKEWNESS        <- renderText(values$stats$moments["Skewness"])
  output$STATS_UNCORRECTED_SS  <- renderText(values$stats$moments["Uncorrected SS"])
  output$STATS_COEFF_VARIATION <- renderText(values$stats$moments["Coeff Variation"])
  
  output$STATS_SUM             <- renderText(values$stats$moments["Sum"])
  output$STATS_VARIANCE        <- renderText(values$stats$moments["Variance"])
  output$STATS_KURTOSIS        <- renderText(values$stats$moments["Kurtosis"])
  output$STATS_CORRECTED_SS    <- renderText(values$stats$moments["Corrected SS"])
  output$STATS_STD_ERROR_MEAN  <- renderText(values$stats$moments["Std Error Mean"])
  
  output$STATS_LOCATION_MEAN   <- renderText(values$stats$location["Mean"])
  output$STATS_LOCATION_MEDIAN <- renderText(values$stats$location["Median"])
  output$STATS_LOCATION_MODE   <- renderText(values$stats$location["Mode"])
  
  output$STATS_VARIABILITY_STD_DEVIATION       <- renderText(values$stats$variability["Std Deviation"])
  output$STATS_VARIABILITY_VARIANCE            <- renderText(values$stats$variability["Variance"])
  output$STATS_VARIABILITY_RANGE               <- renderText(values$stats$variability["Range"])
  output$STATS_VARIABILITY_INTERQUARTILE_RANGE <- renderText(values$stats$variability["Interquartile Range"])
  
  output$STATS_Q100 <- renderText(values$stats$quantiles["100%"])
  output$STATS_Q99  <- renderText(values$stats$quantiles["99%"])
  output$STATS_Q95  <- renderText(values$stats$quantiles["95%"])
  output$STATS_Q90  <- renderText(values$stats$quantiles["90%"])
  output$STATS_Q75  <- renderText(values$stats$quantiles["75%"])
  output$STATS_Q50  <- renderText(values$stats$quantiles["50%"])
  output$STATS_Q25  <- renderText(values$stats$quantiles["25%"])
  output$STATS_Q10  <- renderText(values$stats$quantiles["10%"])
  output$STATS_Q5   <- renderText(values$stats$quantiles["5%"])
  output$STATS_Q1   <- renderText(values$stats$quantiles["1%"])
  output$STATS_Q0   <- renderText(values$stats$quantiles["0%"])
  
  output$LOCALITY_TEST_T_STATISTIC <- renderText(values$locality_test$t_statistic)
  output$LOCALITY_TEST_T_PVALUE    <- renderText(values$locality_test$t_pvalue)
  output$LOCALITY_TEST_S_STATISTIC <- renderText(values$locality_test$s_statistic)
  output$LOCALITY_TEST_S_PVALUE    <- renderText(values$locality_test$s_pvalue)
  output$LOCALITY_TEST_W_STATISTIC <- renderText(values$locality_test$w_statistic)
  output$LOCALITY_TEST_W_PVALUE    <- renderText(values$locality_test$w_pvalue)
}

#shinyApp(ui, server)
shinyApp(ui = htmlTemplate("www/index.html"), server)