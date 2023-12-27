########## codeGenerator.R #############
#
# Reactives and functions for generating
# non-reactive R code that reproduces
# the analyses of the shiny script.
#
########################################
# David Allbritton
# December 2023
# v.0.9.5 2023.12.26
########################################

## Print out initial static code that does not depend on app user input selections:
####### beginning of static code block
initialCode <- '

### nonreactive R code generated within the app for reproducibility

##############  Edit this part by hand as needed ######################
## data file name; change as needed.  It can be .xlsx, .xls, or .csv ##
input_file <- "originalData.xlsx"       # Change this to your file path
#######################################################################

## source the "HelperFunctions.R" file that is used to reformat the input data, etc.
source("HelperFunctions.R")  # Functions from this file that are used:
                             # reformat.df()
                             # is_outlier()
                             # priorposteriorlikelihood.ggplot()
                             # tauprior.ggplot
                             # robustness

## load libraries
# get  needed libraries from ui.R
# commenting out the ones that are shiny-specific
library(purrr)
library(metafor)
library(readxl)
library(writexl)
library(tools)
# library(shiny)
library(bayesmeta)
library(cowplot)
library(dplyr)
library(DT)
library(data.table)
library(esc)
library(ggplot2)
library(MAd)
library(readr)
library(R.rsp)
# library(shinyBS)
# library(shinycssloaders)
# library(shinythemes)
library(stringr)
library(tidyr)
library(xtable)
# library(shinyalert)
# library(shinyjs)
# library(shinymeta)

## Function to read data based on file extension
read_data <- function(file_path) {
  file_extension <- tools::file_ext(file_path)
  #
  if (file_extension %in% c("xlsx", "xls")) {
    df <- read_excel(file_path) %>% as.data.frame()
  } else if (file_extension == "csv") {
    df <- read_csv(file_path, show_col_types = FALSE) %>% as.data.frame()
  } else {
    stop("File format not supported. Must be .xlsx, .xls, or .csv.")
  }
  #
  return(df)
}

## Read and process the data from the input file
df_as_uploaded <- read_data(input_file)
newrvs <- reformat.df(df_as_uploaded)
df <- newrvs$df  # reformatted for use in the analyses

#'
####### end of static code block


################### Generate MA ########
## Generate non-reactive R code to create MA object
#
observeEvent(MA(), {
  # Initialize the code with the static content from above
  code_for_MA <- paste0(initialCode)
  #
  ##########
  ## Generate the code for the required selection fields based on current inputs
  code_for_MA.inputs <- sprintf("
## R code to create MA object that contains selected data for all analyses
#
Variable.Factor.Names <- %s
Variable.Numeric.Names <- %s
Design <- %s
Publication.Year <- c(%s, %s)
N_Intervention <- c(%s, %s)
included <- %s
aggregation <- '%s'
#",
                                paste("c(", paste(dQuote(myrvs$Variable.Factor.Names), collapse = ", "), ")", sep=""),
                                paste("c(", paste(dQuote(myrvs$Variable.Numeric.Names), collapse = ", "), ")", sep=""),
                                paste("c(", paste(dQuote(input$Design), collapse = ", "), ")", sep=""),
                                input$Publication.Year[1], input$Publication.Year[2],
                                input$N_Intervention[1],  input$N_Intervention[2],
                                paste("c(", paste(dQuote(enc2utf8(input$included)), collapse = ", "), ")", sep=""),
                                input$aggregation

  )
  # Add that to the string that contains the non-reactive R code that will be output
  code_for_MA <- paste0(code_for_MA, code_for_MA.inputs)
  #
  ##############
  ## Write the code for filtering based on user-defined variable selection factors
  someCode <- sprintf("
Variable.Factors.selected <- list() ")
  code_for_MA <- paste0(code_for_MA, someCode)
  #
  # loop over the variable factor names to write code
  for (varName in myrvs$Variable.Factor.Names)  {
    keepValues <- input[[varName]]
    someCode <- sprintf("
Variable.Factors.selected[[%s]] <- c(%s) ",
                        dQuote(varName),
                        paste(dQuote(keepValues), collapse = ", ")
    )
    code_for_MA <- paste0(code_for_MA, someCode)
  }
  # Assign the generated code to output
  output$MAcodeOutput <- renderText({ code_for_MA })
  #
  #
  ##############
  ## Write the code for filtering based on user-defined variable selection numerics
  someCode <- sprintf("
Variable.Numerics.selected <- list() ")
  code_for_MA <- paste0(code_for_MA, someCode)
  #
  # loop over the variable numeric names to write code
  for (varName in myrvs$Variable.Numeric.Names)  {
    keepValues <- input[[varName]]
    someCode <- sprintf("
Variable.Numerics.selected[[%s]] <- c(%s) ",
                        dQuote(varName),
                        paste(dQuote(keepValues), collapse = ", ")
    )
    code_for_MA <- paste0(code_for_MA, someCode)
  }
  #
  ################## More code that does not depend on UI selections in the app
  ## Create the MA object; More static code to copy without changing:
  code_for_MA_function <- '

  ## function to create MA data object for all analyses
createMA.nonReactive <- function(df, Variable.Factor.Names, Variable.Numeric.Names,
                                 Variable.Factors.selected, Variable.Numerics.selected,
                                 Design.include, Publication.Year.include, N_Intervention.include,
                                 included, aggregation, method = "BHHR", cor = 0.5) {
  # Create subset based on chosen inclusion criteria
  df_sub <- df %>% filter(Design %in% Design.include,
                          Publication.Year >= Publication.Year.include[1],
                          Publication.Year <= Publication.Year.include[2],
                          N_Intervention >= N_Intervention.include[1],
                          N_Intervention <= N_Intervention.include[2],
                          Paper.and.Exp %in% included)

  # Subset based on selection factors
  for (varName in Variable.Factor.Names) {
    keepValues <- Variable.Factors.selected[[varName]]
    df_sub <- df_sub[df_sub[,varName] %in% keepValues, ]
  }

  # Subset based on selection numerics
  for (varName in Variable.Numeric.Names) {
    df_sub <- df_sub[df_sub[,varName] >= Variable.Numerics.selected[[varName]][1], ]
    df_sub <- df_sub[df_sub[,varName] <= Variable.Numerics.selected[[varName]][2], ]
  }

  # Replace ID with Paper.Number if aggregating over papers
  if (aggregation == "Papers") {
    df_sub$ID <- df_sub$Paper.Number
    df_sub$study <- df_sub$Paper
  }

  # Aggregate effect sizes
  aggES <- agg(id = ID,
               es = yi,
               var = vi,
               data = df_sub,
               cor = cor,
               method = method)

  # Merging aggregated ES with original dataframe
  MAnr <- merge(x = aggES, y = df_sub, by.x = "id", by.y = "ID")
  MAnr <- unique(setDT(MAnr)[sort.list(id)], by = "id")
  MAnr <- with(MAnr, MAnr[order(MAnr$es)])

  return(MAnr)
}

## Create MA, using the inputs recorded from the shiny app UI
MA <- createMA.nonReactive(df, Variable.Factor.Names, Variable.Numeric.Names,
                           Variable.Factors.selected, Variable.Numerics.selected,
                           Design, Publication.Year, N_Intervention,
                           included, aggregation)

  '
  ########### End of this static code section
  #
  code_for_MA <- paste0(code_for_MA, code_for_MA_function)  # adding that static code

################### end of Generate MA ########


####### Get the code to record the priors ######
#
someCode <- sprintf('
## Record the priors for Bayesian analyses ##
tauprior    <-  %s
mupriorsd   <-  %s
scaletau    <-  %s
mupriormean <-  %s
#
                ',
                    paste(dQuote(input$tauprior), collapse = ", "),
                    paste(dQuote(input$mupriorsd), collapse = ", "),
                    paste(dQuote(input$scaletau), collapse = ", "),
                    paste(dQuote(input$mupriormean), collapse = ", ")
        )

  code_for_bma <- paste0(someCode)   # start a new block of code
  
  ## Reformat the priors
  someCode <- '
## reformat the priors to work for bayesmeta
if (mupriorsd == "") mupriorsd <- NULL else mupriorsd <- as.numeric(mupriorsd)
if (scaletau == "") scaletau <- NULL else scaletau <- as.numeric(scaletau)
if (mupriormean == "") mupriormean <- NULL else mupriormean <- as.numeric(mupriormean)
#
'
  code_for_bma <- paste0(code_for_bma, someCode)   # update the block of code
  ####### End of code to record the priors ######
  


  ################### Generate bma ###############################
  ## Generate non-reactive R code to create bma bayesmeta object
  #

  someCode <- '

#### Create bma object for Bayesian functions ####

# Function to Create bma bayesmeta object needed for all outputs
createMA.nonReactive <- function(MA, tauprior, mupriorsd, scaletau, mupriormean) {
    ## Generate bayesmeta-object "bma" depending on tau prior chosen
        if (tauprior == "Half cauchy") {
        bma <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study,
                         tau.prior = function(t) dhalfcauchy(t, scale = scaletau),
                         mu.prior = c("mean" = mupriormean, "sd" = mupriorsd))
      } else if (tauprior == "Half student t") {
        bma <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study,
                         tau.prior = function(t) dhalfnormal(t, scale = scaletau),
                         mu.prior = c("mean" = mupriormean, "sd" = mupriorsd))
      } else {
        bma <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study,
                         tau.prior = tauprior,
                         mu.prior = c("mean" = mupriormean, "sd" = mupriorsd))
      }
  bma
    }  #### end of createMA.nonReactive function

# calculate bma bayesmeta object
calculate_bma <- "Yes"  ## Change to "no" if you want to skip the slow Bayesian calculations
if(calculate_bma == "Yes") {
  bma <- createMA.nonReactive(MA, tauprior, mupriorsd, scaletau, mupriormean)
}

'

  code_for_bma <- paste0(code_for_bma, someCode)   # update code block
  #
  ############### End of  Generate bma ###############################
  
  

  ################### Create code_for_plots ###############################
  ## Generate non-reactive R code for the main panel outputs
  #
  code_for_plots <- '
  
## Create a list called "output" to make it easier to adapt the shiny code
output <- list()

######### Create the plots etc. from the results panels ####################

# Outliers panel
create_boxplot <- function(MA) {
  MAo <- MA %>% tibble::rownames_to_column(var = "outlier") %>% mutate(is_outlier=ifelse(is_outlier(es), es, as.numeric(NA)))
  MAo$study[which(is.na(MAo$is_outlier))] <- as.numeric(NA)
  ggplot(MAo, aes(x = factor(0), es)) +
    geom_boxplot(outlier.size = 3.5, outlier.colour = "#D55E00", outlier.shape = 18, fill = "lightgrey") +
    geom_text(aes(label=study),na.rm = T, nudge_y = 0.02, nudge_x = 0.05) +
    stat_boxplot(geom="errorbar", width = 0.05) +
    scale_x_discrete(breaks = NULL) +
    xlab(NULL) + ylab("Hedges g") +
    theme_minimal_hgrid(12)
}
#
output$boxplot <- create_boxplot(MA)
output$boxplot  


########## frequentist analyses

# Create model for frequentist meta-analysis
fma <- rma(MA$es, MA$var, slab=MA$study)
  
### Frequentist Forest Plot
create_freq_forest <- function(fma) {
  model <- fma 
  # Increase bottom margin to make space for the text
  par(mar = c(5, 4, 4, 2) + 0.1)  # Adjust the bottom margin (the first value)
  # Generate the forest plot
  plot <- metafor::forest.rma(x = model, showweights = TRUE, addfit = TRUE,
                              order = "obs", xlab = "Hedges g", 
                              addpred = TRUE, 
                              efac = 0,
                              col = "red",
                              border = "red")
  # Add Cochrans Q, its p-value, and I² statistic as text
  # Position the text below the plot
  mtext(side = 1, line = 4, 
        text = paste0("Cochrans Q = ", round(model$QE, 2), 
                      " (p = ",  format(round(model$QEp, 4), nsmall = 4), ")\n",
                      "I² = ", round(model$I2, 2), "%"),
        adj = 0, cex = 0.8)
  # Return the plot
  plot
}
#
output$freq_forest <- create_freq_forest(fma)
#  output$freq_forest

# Funnel plot (frequentist)
output$freq_funnel <- funnel(fma, xlab = "Observed outcome")
#  output$freq_funnel


######### bayesian analyses:

# Forest Plot panel
output$forest <-  forestplot.bayesmeta(bma, xlab = "Hedges g")
# output$forest

# Funnel Plot panel
output$funnel <-  funnel.bayesmeta(bma, main = "")
# output$funnel

# Statistics panel
output$statistics_panel <-  capture.output({
  cat("Statistics for Bayesian analysis")
  cat("\n\n")
  cat("Bayes Factors:")
  cat("\n")
  print(bma$bayesfactor[1,])
  cat("\n\n")
  cat("Marginal posterior summary:")
  cat("\n")
  print( bma$summary)
  cat("\n")
  cat("Maximum-likelihood:")
  cat("\n")
  print(bma$ML)
  cat("\n\n")
  cat("Joint maximum a-posteriori:")
  cat("\n")
  print( bma$MAP[1,])
})
cat(output$statistics_panel, sep = "\n")


# Additional plots panel
output$evupdate <- priorposteriorlikelihood.ggplot(bma, lowerbound = 0 - (mupriormean + 1) * 1.5, upperbound = 0 + (mupriormean + 1) * 1.5)
output$taupriorplot <-   tauprior.ggplot(bma)
#
print("Prior, posterior, & likelihood")
output$evupdate 
print("Joint posterior density")
output$joint <- plot.bayesmeta(bma, which=2, main = "Joint posterior density of heterogeneity Tau and effect mu")
print("Tau prior distribution")
output$taupriorplot

# Bayes factor robustness plot panel
#
create_robustplot <- function(tauprior, mupriorsd, scaletau){
  robust = "Yes"  ##Change to "no" if you want to skip this very time consuming section
  robustggplot <- NULL
  if (robust == "Yes" & tauprior == "Half cauchy") {
    robustggplot <- robustness(MA,SD = mupriorsd, tauprior = function(t) dhalfcauchy(t, scale = scaletau))
  } else if (robust == "Yes" & tauprior == "Half student t") {
    robustggplot <- robustness(MA,SD = mupriorsd, tauprior = function(t) dhalfnormal(t, scale = scaletau))
  }
  robustggplot
}
#
output$robustplot <- create_robustplot(tauprior, mupriorsd, scaletau)

print("Bayes Factors over a variety of prior standard deviations:")
output$robustplot 
'
  ########## End of  Create code_for_plots ###############################

  

  ####### Wrapping up and saving the code for display and downloading ###########
  # Put together all the R code
  code_for_R_script <- paste0(code_for_MA, code_for_bma, code_for_plots)
  
  # Create headers and footers for R markdown file
  
  markdown_header <- paste(
    '---',
    'title: "Untitled"',
    'output: html_document',
    '---',
    '',
    '```{r setup, include=FALSE}',
    'knitr::opts_chunk$set(echo = TRUE)',
    '```',
    '',
    '## R Markdown',
    '',
    'This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.',
    '',
    'When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document.',
    '',
    '```{r rcode}',
    '',
    sep = "\n"
  )
  
  markdown_footer <- "```"
  
  # Put the R code into markdown form
  code_for_R_markdown  <- paste(markdown_header, code_for_R_script, markdown_footer, sep = "\n")
  

  # Assign the generated code and markdown to output
  output$R_code_Output <- renderText({ code_for_R_script })
  output$R_markdown_Output <- renderText({ code_for_R_markdown })
  

  ## save code to a file.               # debugging
  # writeLines(code_for_R_script, con = "nonReactiveVersion_part_1_utf8.R", useBytes = TRUE) # "write" could not handle the non-asci characters
  
   ## Download handler for the R code file
   output$downloadCode <- downloadHandler(
     filename = function() {
       "R_code_script.R"
     },
     content = function(file) {
       writeLines(enc2utf8(code_for_R_script), file)
     }
   )
   
   ## Download handler for the R markdown file
   output$downloadMarkdown <- downloadHandler(
     filename = function() {
       "R_code_markdown.Rmd"
     },
     content = function(file) {
       writeLines(enc2utf8(code_for_R_markdown), file)
     }
   )
   
   ## Download file for R code -- done in server.R
   ## Download file for R markdown -- done in  server.R
   
  
})  # end of observeEvent(MA())
