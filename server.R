#######################################################################################
################### A General Tool for BAYESIAN META-ANALYSIS #################
#######################################################################################

################### Shiny App v.0.9.5 2023.12.26 SERVER ###################################
# David Allbritton
# https://github.com/davidallbritton/Breathing_life_into_meta-analysis
#
# Portions derived and adapted from https://vinzentwolf.shinyapps.io/taVNSHRVmeta/
# as described in https://doi.org/10.1111/psyp.13933
#
###################################################################################


# Define server logic
server <- function(input, output, session) {
  
  # Include code from other files.  Note that the "source" command
  # must be inside the "server" function and "local = T" must be
  # included so that the code is evaluated within the server envirnoment
  # rather than in the user's workspace (global environment)
  #
  source("codeGenerator.R", local = T)  # for generating non-reactive R code to download for reproducibility
  
  # increase the allowable file size for uploads:
  options(shiny.maxRequestSize = 100 * 1024^2)
  
  # list of the tabs that require Bayesian calculations:
  bayestabs <- c(
    "bayesian_forest_plot",
    "bayesian_funnel_plot",
    "bayesian_statistics",
    "bayesian_additional_plots" ## "bayesian_robustness" must be treated separately
  )

  ## Initialize with stored data, which will be replaced when a data file is uploaded
  ## by the user
  myrvs <- reactiveValues(currentInputFile = NULL)   
  
  ## initialize reactive value to indicate whether the app has finished loading
  myrvs$uiRendered  <- FALSE
  
  ###  initialize $previousPlots if the file exists on the server
  myrvs$previousPlots <- list()
  if (file.exists("data/defaultPrecalculatedPlots.RDS")) {
    myrvs$previousPlots <- readRDS("data/defaultPrecalculatedPlots.RDS")
  }
  
  ###  initialize $previousModels if the file exists on the server
  myrvs$previousModels <- list()
  if (file.exists("data/defaultPrecalculatedModels.RDS")) {
    myrvs$previousModels <- readRDS("data/defaultPrecalculatedModels.RDS")
  }

  ### a reactive value that gets updated whenever myrvs$previousModels changes:
  #   This version of MA() without factor variables is necessary because
  #   factor levels are recorded differently on different platforms, making
  #   it impossible to use precalculated bma()s from one computer on another computer
  #   This is needed to be make the pre-loaded bma() models work on the shinyapps.io server.
  previousMAsNoFactors <- reactiveValues(MAs = list())
  #
  observe ({
    prevModels <- myrvs$previousModels
    isolate ({
      if(length(myrvs$previousModels)) {
        for (i in 1:length(prevModels)) {
          nofactors <- prevModels[[i]]$MA
          nofactors <- nofactors %>% mutate_if(is.factor, as.character)
          previousMAsNoFactors$MAs[[i]] <- nofactors
        }
      }
    })
  })
    
  
  myrvs$nfiles <- 0
    observe({
    if(is.null(input$DataFileUp)){   #this trigger works because input$DataFileUp gets initialized to null when the app first loads, which triggers the observer
      isolate({                   #isolate so that changes in myrvs do not trigger the observer
        newrvs <- reformat.df(df)
        myrvs$df.reactive <- newrvs$df          # for manipulating and calculating; gets altered
        myrvs$df.original <- newrvs$df.original # as originally loaded or uploaded; does not change
        myrvs$df.updated <- myrvs$df.original   # original data plus any data points input by the user
        myrvs$Variable.Factor.Names <- newrvs$Variable.Factor.Names
        myrvs$Variable.Numeric.Names <- newrvs$Variable.Numeric.Names
        myrvs$na.warning <- newrvs$na.warning
        myrvs$recalculatedSinceUpload <- 0
        myrvs$dataErrorMessage <- ""
        myrvs$defaultYear <- thisYear
        myrvs$ma_num <- 1
        myrvs$triggerBma <- FALSE
        myrvs$triggerBmaRobust <- FALSE
      })
    }
  })
  
   
    ## When the user uploads a data file, replace the existing data and update the UI
    observeEvent(input$DataFileUp, {
      # Read the data from the uploaded file:
      file_path <- input$DataFileUp$datapath
      file_extension <- tools::file_ext(file_path)
      #
      output$inputFileError <- renderUI({  
        if (!is.null(input$DataFileUp)) {
          if (!(file_extension %in% c("xlsx", "xls", "csv"))) {
            p(style = "color:red", "***File format not supported. Must be .xlsx, .xls, or .csv.***")
          }
        }
      })
      #
      validate(need(file_extension %in% c("xlsx", "xls", "csv"), "Please upload an Excel (.xlsx/.xls) or CSV (.csv) file"))
      #
      if (file_extension %in% c("xlsx", "xls")) {
        df <- readxl::read_excel(file_path) %>% as.data.frame()
        # make sure the numeric columns are of the correct type
        start_col <- which(names(df) == "Begin.Selection.Numerics")
        end_col <- which(names(df) == "End.Selection.Numerics")
        # Check if both columns are found
        if (length(start_col) == 0 || length(end_col) == 0) {
          # Instead of stop(), use a Shiny-friendly method to show an error
          showNotification("Start or end column not found in the dataframe", type = "error")
          return()
        }
        # Convert columns from start to end into numeric
        df[, start_col:end_col] <- lapply(df[, start_col:end_col], function(x) as.numeric(as.character(x)))
        #
        # List of column names to potentially convert to numeric
        columns_to_convert <- c("Publication.Year", "N_Intervention", "N_Control", "N_Total")
        # Loop through each column name
        for (col in columns_to_convert) {
          # Check if the column exists in the dataframe
          if (col %in% names(df)) {
            # Convert the column to numeric
            df[[col]] <- as.numeric(as.character(df[[col]]))
          }
        }
        #
      } else if (file_extension == "csv") {
        # The .csv should use UTF-8 encoding if there are non-ASCII characters
        # otherwise they can cause the app to crash.
        df <- readr::read_csv(file_path, show_col_types = FALSE)  %>% as.data.frame()
        # make sure the numeric columns are of the correct type
        start_col <- which(names(df) == "Begin.Selection.Numerics")
        end_col <- which(names(df) == "End.Selection.Numerics")
        # Check if both columns are found
        if (length(start_col) == 0 || length(end_col) == 0) {
          # Instead of stop(), use a Shiny-friendly method to show an error
          showNotification("Start or end column not found in the dataframe", type = "error")
          return()
        }
        # Convert columns from start to end into numeric
        df[, start_col:end_col] <- lapply(df[, start_col:end_col], function(x) as.numeric(as.character(x)))
        #
        # List of column names to potentially convert to numeric
        columns_to_convert <- c("Publication.Year", "N_Intervention", "N_Control", "N_Total")
        # Loop through each column name
        for (col in columns_to_convert) {
          # Check if the column exists in the dataframe
          if (col %in% names(df)) {
            # Convert the column to numeric
            df[[col]] <- as.numeric(as.character(df[[col]]))
          }
        }
      }
      #
      newrvs <- reformat.df(df)
      myrvs$df.reactive <- newrvs$df
      myrvs$df.original <- newrvs$df.original
      myrvs$df.updated <- myrvs$df.original 
      myrvs$Variable.Factor.Names <- newrvs$Variable.Factor.Names
      myrvs$Variable.Numeric.Names <- newrvs$Variable.Numeric.Names
      myrvs$na.warning <- newrvs$na.warning
      myrvs$nfiles <- myrvs$nfiles + 1
      myrvs$recalculatedSinceUpload <- 0
      myrvs$currentInputFile <- input$DataFileUp$name
      myrvs$uiRendered  <- FALSE    # to keep recalculate button inactive until UI loaded
      output$inputFileError <- renderUI(NULL) # remove error message if file uploaded successfully
    })
  
  observeEvent(input$recalculateButton, {
    output$currentDataFile <- renderUI({
      isolate({
        myrvs$recalculatedSinceUpload <- 1
        if (myrvs$nfiles > 0) {
          p(
            "The currently displayed results are from ", 
            span(style = "white-space: nowrap", myrvs$currentInputFile)
          )
        } else {
          p("The currently displayed results are from Vasilev et al., 2018 plus updates as of 2023")
        }
      })
    })
  })
  
  #################### 
  ##   tabPanel("Study criteria",    ## creating UI content for this tabPanel ##
  output$studyCriteria <- renderUI({
    ## read in the reactive values to use in creating the UI tabPanel entries:
    df <- myrvs$df.reactive
    Variable.Factor.Names <- myrvs$Variable.Factor.Names 
    Variable.Numeric.Names <- myrvs$Variable.Numeric.Names 
    na.warning <- myrvs$na.warning
    
    tagList(    
             br(), 
             radioButtons(inputId = "aggregation", label = p("Aggregate over", style="color:#333333",
                                                             tags$style(type = "text/css", "#q18 {vertical-align: top;}"),
                                                             bsButton("q118", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),    
                          choices = c(ID = "ID", Papers = "Papers")),
             bsPopover(id="q118", title = "Aggregation.",
                       content = paste0("<p>Aggregate effect sizes over ID (default) or over papers.",
                                        "<p>Selecting Papers will compute a single aggregated effect size for each paper. Selecting ID will aggregate based on the numbers in the ID column in the data file.  If you want no aggregation, make sure the data file has a unique ID number for each line.",
                                        "<p>Default: ID."),
                       placement = "right", 
                       trigger = "click",
                       options = list(container = "body")),
             checkboxGroupInput(inputId = "Design", label = p("Study design",style="color:#333333",
                                                              tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                                              bsButton("q1", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")), 
                                choices = levels(df$Design), selected = levels(df$Design)),
             bsPopover(id="q1", title = "Study design.",
                       content = paste0("<p>Choose to include effect sizes calculated within subjects, between subjects, or both.",
                                        "<p>Default: both."),
                       placement = "right", 
                       trigger = "click",
                       options = list(container = "body")),
             
             sliderInput(inputId = "Publication.Year", label = p("Publication year",style="color:#333333",
                                                        tags$style(type = "text/css", "#q8 {vertical-align: top;}"),
                                                        bsButton("q8", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")), 
                         min = min(df$Publication.Year), max = max(df$Publication.Year), value = c(min(df$Publication.Year), max(df$Publication.Year)), step = 1, sep = "", ticks = F),
             bsPopover(id="q8", title = "Publication year.",
                       content = paste0("<p>Choose to include effect sizes from studies with a certain range of publication years.",
                                        "<p>Default: all."),
                       placement = "right", 
                       trigger = "click",
                       options = list(container = "body")),
             sliderInput(inputId = "N_Intervention", label = p("N for Intervention Group",style="color:#333333"),
                         min = min(df$N_Intervention), max = max(df$N_Intervention), value = c(min(df$N_Intervention), max(df$N_Intervention)), step = 1, sep = "", ticks = F),
             if(!is.na(na.warning["N_Intervention"])) p(na.warning["N_Intervention"]) else "",
             
             ### loop over the variable numeric selection columns
                                           lapply(Variable.Numeric.Names, function(varName) {
                                             pp <- if(!is.na(na.warning[varName])) p(na.warning[varName]) else ""
                                             ss <- sliderInput(inputId = varName, label = p(varName ,style="color:#333333"), 
                                                         min = min(df[,varName], na.rm = T), max = max(df[,varName], na.rm = T), value = c(min(df[,varName], na.rm = T), max(df[,varName], na.rm = T)), ticks = F)
                                             list(ss, pp)
                                           }), 
             
             ## loop over the variable factor columns
             lapply(Variable.Factor.Names, function(varName) {                             
               checkboxGroupInput(inputId = varName, label = p(varName,style="color:#333333"), 
                                  choices = levels(df[,varName]), selected = levels(df[,varName]))
             }),
             
        checkboxGroupInput(
          inputId = "included", label = p("Include/exclude specific studies",style="color:#333333",
                                           tags$style(type = "text/css", "#q9 {vertical-align: top;}"),
                                           bsButton("q9", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")), 
           choices = levels(df$Paper.and.Exp), selected = levels(df$Paper.and.Exp)
        ),
        bsPopover(id="q9", title = "Include/exclude specific studies.",
                  content = paste0("<p>Exclude specific studies by removing the tick mark.",
                                   "<p>This selection is hierarchically below the other inclusion/exclusion criteria.",
                                   "<p>If a study is excluded by one of the selected criteria above, this study will not be included in the analysis, even though it is still ticked here.",
                                   "<p>However, if a study is selected for inclusion by the criteria above, but is unticked here, the study will not be included."),
                  placement = "right", 
                  trigger = "click",
                  options = list(container = "body"))
      )
#    myrvs$uiRendered <- TRUE
  })
  #################### End of study criteria panel 
  
  

  ##  addStudies
  #################### 
  ##   tabPanel   addStudies   ## creating UI content for this tabPanel ##
  #
  # Create the UI radio button for choosing whether to choose an existing paper or add a new one
  output$studyAskType <- renderUI({
    myrvs$df.reactive  # reactive trigger
    # Determine whether the new data point goes with an existing paper:
    radioButtons(inputId = "existingPaper", 
                 label = 'Is the new effect size from a paper that exists in the "Current data"?', 
                 choices = c("No","Yes"), selected = "No")
  })
  #
  # Choose an existing paper or add a new one
  output$setNewPaper <- renderUI({    #myrvs$ID_add
    df <- myrvs$df.reactive   
    req(input$existingPaper)
    if(input$existingPaper == "Yes") {
      selectInput(inputId = "Paper_add", 
                  label = "Select the paper to enter an additional effect size for",
                  choices = unique(sort(df$Paper)),    
                  selected = df$Paper[length(df$Paper)]
                  )
    }
    else {
      textInput(inputId = "Paper_add", label = "Paper to add (citation)", 
                paste0(as.character(max(df$ID) +1), "_author (year)"))
    }
  })
  #
  Paper.Number_add <- reactive ({
    df <- myrvs$df.reactive
    req(input$existingPaper)
    if (input$existingPaper == "Yes") {
      if (is.na(input$Paper_add)) max(df$ID) +1
      else if (input$Paper_add %in% df$Paper) { #this "if" prevents a temporary error message
        df[df$Paper == input$Paper_add, "Paper.Number"][[1]]
        } 
      }
    else {max(df$ID) +1}
  })
  #
  # set the default year for when the user enters a new data point:
  observeEvent(input$Paper_add, {   # update myrvs$defaultYear whenever the Paper_add input field changes
    myrvs$defaultYear <- thisYear   # reset to the default year, then change it if adding an e.s. for an existing paper
    papernum <- Paper.Number_add()
    if (input$existingPaper == "Yes") {
      if (isTruthy(df[df$Paper.Number == papernum, "Publication.Year"][1])) {
        myrvs$defaultYear <- df[df$Paper.Number == Paper.Number_add(), "Publication.Year"][1]
      }
    }
  })
  #
  # Create part 1 of the rest of the tabPanel for adding studies (always presented)
  output$addStudies1 <- renderUI({
    ## read in the reactive values to use in creating the UI tabPanel entries:
    df <- myrvs$df.reactive
    myrvs$ID_add <-  max(df$ID) +1
    #
    tagList(
      p("ID = ", myrvs$ID_add),
      p("Paper.Number = ", Paper.Number_add()),
      numericInput(inputId = "Publication.Year_add",  label = "Publication Year", value = myrvs$defaultYear),
      numericInput(inputId = "Experiment.Number_add",  label = "Experiment.Number (within paper)", value =1),
      numericInput(inputId = "Effect.Size.Number_add",  label = "Effect.Size.Number (within experiment)", value =1),
      radioButtons(inputId = "Design_add", label = p("Study design"), 
                   choices = c("between", "within")
      ),
      hr(),
      p(strong("Effect size information"),"to calculate Hedges' g.  You can enter g and g_var directly,",
        "or let g be calculated from d or from group means and standard deviations.",
        "If you choose", '"enter everything" then means will only be used if d is not provided,',
        "and d will only be used if g is not provided."),
      radioButtons(inputId = "useGorD", label = p("How would you like to enter the information for the new effect size you are adding?"),
        choiceNames =   c("Just input g and g_var", 
                    "Just input d, d_var, and N", 
                    "Just input means, SDs, and N",
                    "Let me enter everything I have"), 
        choiceValues = c("g", "d", "means", "all"),
        selected = "g"
      ),
    )
  })
  #
  # Create section 2 of the tabPanel for adding studies, conditional on having values for g
  output$addStudies2 <- renderUI({
    myrvs$df.reactive  # referencing df.reactive causes the form to be regenerated after each submission
    gtags <-  ""
    dtags <-  ""
    ntags <-  ""
    msdtags <-  ""
    rtags <-  ""
    req(input$useGorD) #need the "req" to prevent temporary error message for zero-length useGorD in the "if"
    if  ((input$useGorD == "g") | (input$useGorD == "all")) {  # display g inputs
      gtags <- tagList( hr(),
        numericInput(inputId = "g_add",  label = "effect size (Hedges' g)", value =""),
        numericInput(inputId = "g_var_add",  label = "variance of g (g_var)", value ="")
      )
    } 
    if (input$useGorD != "g") {  # display N inputs
      ntags <- tagList( hr(),
        p("Number of subjects in each group:"),
        numericInput(inputId = "N_Total_add",  label = "Total N", value = NA),
        numericInput(inputId = "N_Intervention_add",  label = "Intervention N", value = NA),
        numericInput(inputId = "N_Control_add",  label = "Control N", value = NA)
      )
    }
    if ((input$useGorD == "d") | (input$useGorD == "all")) {  # display d inputs
      # display the input boxes for d, d_var
      dtags <- tagList( hr(),
        p("Cohen's d and its variance:"),
        numericInput(inputId = "d_add",  label = "effect size (Cohen's d)", value =""),
        numericInput(inputId = "d_var_add",  label = "variance of d", value ="")
      )
    }
    if ((input$useGorD == "means") | (input$useGorD == "all")) {  # display g inputs
      req(input$Design) #need? the "req" to prevent temporary error message for zero-length useGorD in the "if"
      msdtags <- tagList( hr(),
        # get means and sds
        p("Group means and variabilities:"),
        numericInput(inputId = "mean_E_add",  label = "Intervention mean", value =NULL), 
        numericInput(inputId = "mean_C_add",  label = "Control mean", value =""),
        radioButtons(inputId = "reverseCode_add", choiceNames = c("No. (More is better, e.g., % correct)", "Yes. (More is worse, e.g., % errors, RT)"),
                     choiceValues = c("No", "Yes"),
                     selected = "No", label = "Means require reverse coding?"),
        numericInput(inputId = "var_E_add",  label = "Intervention variability (as SD, SE, or variance)", value =""),
        numericInput(inputId = "var_C_add",  label = "Control variability (as SD, SE, or variance)", value =""),
        radioButtons(inputId = "var_type_add", 
                     choiceNames = c("Standard deviation (SD)", "Variance", "Standard error (SE)"), 
                     choiceValues = c("Standard deviation", "Variance", "Standard error"), 
                     selected = "Standard deviation", label = "Variance type")
      )
      # if within, get r
      if (input$Design_add == "within"){
        rtags <- tagList(
          numericInput(inputId = "r_add",  
                      label = paste('Within-study outcome correlation (for "within" designs).',  
                                     "If blank, the default estimate of r=0.74 from Vasilev et al. (2018) will be used"), 
                       value = NULL,  min = 0, max = 1, step = 0.01)
        )
      }
    
      
    }
    tagList(gtags,  dtags,  ntags, msdtags, rtags, hr() )
  })
  #
  #
  # Create the rest of the tabPanel for adding studies: optional selection variables 
  output$addStudies3 <- renderUI({
    ## read in the reactive values to use in creating the UI tabPanel entries:
    df <- myrvs$df.reactive
    Variable.Factor.Names <- myrvs$Variable.Factor.Names 
    Variable.Numeric.Names <- myrvs$Variable.Numeric.Names 
    na.warning <- myrvs$na.warning
          tagList(   
            hr(),
            p(strong("Optional variables"), '- for use in "Study criteria" panel selections'),
            hr(),
            #   
            ## loop over the variable factor columns
            lapply(Variable.Factor.Names, function(varName) {  
              varName_add <- paste0(varName, "_add")
              radioButtons(inputId = varName_add, label = p(varName), 
                           choices = c(levels(df[,varName]), "Other (not listed)"), selected = "")
            }),
            #
            ### loop over the variable numeric selection columns
            lapply(Variable.Numeric.Names, function(varName) {
              varName_add <- paste0(varName, "_add")
              numericInput(inputId = varName_add, label = varName, value = "")
            }), 
            hr(),
            #
            actionButton(inputId = "add1study","Add this study", icon("sync"), style = "color: green; background-color: white"),
            p(myrvs$dataErrorMessage)
          )
  })
  #################### End of add studies panel 
  
  #### reset the new-study submit error message
  resetErrorMessage <- observe({
    myrvs$df.reactive      # remove the error message when new data is uploaded or input
    isolate(myrvs$dataErrorMessage <- "")
  })
  #  I'm not sure why doing these both in one observer did not work before MA() exists...
  resetErrorMessage2 <- observe({
    MA()                   # remove the error message when the "recalculate" button is pressed
    isolate(myrvs$dataErrorMessage <- "")
  })
  resetErrorMessage3 <- observe({ # remove error message if any of these change:
    input$useGorD
    input$g_add
    input$g_var_add
    input$d_add
    input$d_var_add
    input$var_C_add
    input$mean_C_add
    input$mean_E_add
    input$N_Control_add
    input$N_Intervention_add
    input$N_Total_add
    input$r_add
    #
    isolate(myrvs$dataErrorMessage <- "")
  })
  #### end of resetting the new study submit error message
  
  
  ########### Adding a study that was input by the user
  observeEvent(input$add1study, {     #when a study is input to add, do this:
    myrvs$recalculatedSinceUpload <- 0
    other.Names_add <- c("Publication.Year_add",  "Experiment.Number_add", 
                         "Effect.Size.Number_add", "Design_add", 
                         "r_add",
                         "N_Total_add", "N_Intervention_add", "N_Control_add", 
                         "mean_E_add",  "mean_C_add", 
                         "reverseCode_add",
                         "var_E_add",   "var_C_add",   "var_type_add",
                         "g_add",  "g_var_add",  "d_add",  "d_var_add"
    )
    # change it to just the fields that are being input, based on useGorD
    if(isTruthy(input$useGorD)) {
      if (input$useGorD == "g") {
        other.Names_add <- c("Publication.Year_add",  "Experiment.Number_add", 
                             "Effect.Size.Number_add", "Design_add", 
                             "g_add",  "g_var_add"
        )
      }
      if (input$useGorD == "d") {
        other.Names_add <- c("Publication.Year_add",  "Experiment.Number_add", 
                             "Effect.Size.Number_add", "Design_add", 
                             "N_Total_add", "N_Intervention_add", "N_Control_add", 
                             "d_add",  "d_var_add"
        )
      }
      if (input$useGorD == "means") {
        other.Names_add <- c("Publication.Year_add",  "Experiment.Number_add", 
                             "Effect.Size.Number_add", "Design_add", 
                             "N_Total_add", "N_Intervention_add", "N_Control_add", 
                             "mean_E_add",  "mean_C_add", 
                             "reverseCode_add",
                             "var_E_add",   "var_C_add",   "var_type_add"
        )
        if (input$Design_add == "within") {
          other.Names_add <- c(other.Names_add, "r_add")
        }
      }
    }
    other.Names <- str_replace(other.Names_add, "_add", "")
    # The inputfields and calculatedfields will need to be added to the dataset
    calculatedfields <- c("Paper.and.Exp", "yi", "vi")
    inputfields <- c(myrvs$Variable.Factor.Names, myrvs$Variable.Numeric.Names, other.Names)
    #
    # Change the paper name if it was entered as a "new" paper but duplicates the name of an existing paper
    #  (it would be better to catch it at input and prompt the user to fix it, but for now this will
    #   at least prevent errors when inputting more additional studies)
    if (!isTruthy(input$Paper_add)) {
      myrvs$dataErrorMessage <- "Error: Must choose a paper"
      return("Error: Must choose a paper...")
    }
    Paper <- input$Paper_add
    if ((df[df$Paper == input$Paper_add, "Paper"] %>% length() > 0) & (input$existingPaper == "No")) {
      message ("oops, duplicate name for a 'new' paper!!")
      Paper <- paste0(Paper, "_x")
    }
    #
    # Create the study label: Paper.and.Exp
    Paper.and.Exp <- paste0(Paper, " Exp. ", input$Experiment.Number_add, " ES ",input$Effect.Size.Number_add)
    #
    # set the value of r for within-subjects designs:
    r <- r_estimate
    if (isTruthy(input$r_add)) r <- as.numeric(input$r_add)  
    #
    ######################### Get the values for yi and vi
    #
    # First check to make sure the right input exists
    gInputsOK <- if (
      isTruthy(input$g_add) & 
      isTruthy(input$g_var_add) 
    ) {TRUE} else {FALSE}
    dInputsOK <- if (
      isTruthy(input$d_add) & 
      isTruthy(input$d_var_add) & 
      isTruthy(input$N_Total_add) & 
      isTruthy(input$N_Intervention_add) & 
      isTruthy(input$N_Control_add) & 
      isTruthy(input$Design_add) 
    ) {TRUE} else {FALSE}
    meansInputsOK <- if (
      isTruthy(input$mean_E_add) & 
      isTruthy(input$mean_C_add) & 
      isTruthy(input$N_Total_add) & 
      isTruthy(input$N_Intervention_add) & 
      isTruthy(input$N_Control_add) & 
      isTruthy(input$Design_add) & 
      isTruthy(input$var_C_add)
    ) {TRUE} else {FALSE}
    #
    ### call the function and pass it arguments from the user input; assign yi and vi values
    #
    if (input$useGorD == "all") {
      if (!isTruthy(c(meansInputsOK, dInputsOK, gInputsOK))){  
        myrvs$dataErrorMessage <- "Error: Some required inputs were missing"
        return("Error: Some required inputs were missing")
      }
      gstats <- getEffectSize (g = as.numeric(input$g_add), 
                               g_var = as.numeric(input$g_var_add), 
                               d = as.numeric(input$d_add), 
                               d_var = as.numeric(input$d_var_add), 
                               mean_E = as.numeric(input$mean_E_add), 
                               mean_C = as.numeric(input$mean_C_add),
                               var_E = as.numeric(input$var_E_add), 
                               var_C = as.numeric(input$var_C_add), 
                               var_type = input$var_type_add, 
                               N_Total = as.numeric(input$N_Total_add), 
                               N_Intervention = as.numeric(input$N_Intervention_add), 
                               N_Control = as.numeric(input$N_Control_add), 
                               Design = input$Design_add, 
                               r = r,
                               reverseCode = input$reverseCode_add
      )
    }
    if (input$useGorD == "means") {
      if (input$Design_add == "between") {r = NA}
      if (!meansInputsOK){
        myrvs$dataErrorMessage <- "Error: Must input means, N's, and control-group variability (SD, SE, or variance)"
        return("Error: Must input means, N's, and control-group variability (SD, SE, or variance)...")
      }
      gstats <- getEffectSize (mean_E = as.numeric(input$mean_E_add), 
                               mean_C = as.numeric(input$mean_C_add),
                               var_E = as.numeric(input$var_E_add), 
                               var_C = as.numeric(input$var_C_add), 
                               var_type = input$var_type_add, 
                               N_Total = as.numeric(input$N_Total_add), 
                               N_Intervention = as.numeric(input$N_Intervention_add), 
                               N_Control = as.numeric(input$N_Control_add), 
                               Design = input$Design_add, 
                               r = r,
                               reverseCode = input$reverseCode_add
      )
    }
    if (input$useGorD == "d") {
      if (!dInputsOK) {
        myrvs$dataErrorMessage <- "Error: Must input Design, N's, d, and d_var"
        return("Error: Must input Ns, d, and d_var...")
      }
      gstats <- getEffectSize (d = as.numeric(input$d_add), 
                               d_var = as.numeric(input$d_var_add), 
                               N_Total = as.numeric(input$N_Total_add), 
                               N_Intervention = as.numeric(input$N_Intervention_add), 
                               N_Control = as.numeric(input$N_Control_add), 
                               Design = input$Design_add, 
      )
    }
    if (input$useGorD == "g") {
      if (!gInputsOK){
        myrvs$dataErrorMessage <- "Error: Must input g and g_var"
        return("Error: Must input g and g_var...")
      }
      gstats <- getEffectSize (g = as.numeric(input$g_add), 
                               g_var = as.numeric(input$g_var_add), 
      )
    }
    #
    # create a 1-row dataframe for the values that were input by the user and the calculated effect size:
    newstudy <- data.frame(yi = gstats$yi, vi = gstats$vi, ID = myrvs$ID_add, 
                           Paper = Paper,
                           Paper.and.Exp = Paper.and.Exp, 
                           Paper.Number = Paper.Number_add())
    # add all the values that were input by the user
    for (n in inputfields){
      nn <- paste0(n, "_add")
      newstudy[n] <- input[[nn]]
    }
    newstudy$FromUserInput <- "Yes"
    #
    # add the new row of data to the current data:
    myrvs$df.updated <- bind_rows(myrvs$df.updated, newstudy)
    newdatalist <- reformat.df(myrvs$df.updated) 
    req(newdatalist)  # make sure the new data was successfully processed before resetting the input fields
    # update the active data file, which triggers lots of reactive stuff:
    myrvs$df.reactive <- newdatalist$df
    # update the warnings about NAs in the study selection panel
    myrvs$na.warning <- newdatalist$na.warning
    # change focus to the Study criteria tab after successful data input:
    updateTabsetPanel(session, "dataSetupPanel", selected = "Study criteria")
    #
  })  #### end of adding a study
  
  
  
  # Create MA reactive for all outputs
  MA <- eventReactive(input$recalculateButton, {
    myrvs$triggerBma <- FALSE  ## reset the trigger for calculating bma()  
    myrvs$triggerBmaRobust <- FALSE  ## reset the trigger for robustness plots  
    # import the reactive version of the data and the relevant column names
    df <- myrvs$df.reactive
    Variable.Factor.Names <- myrvs$Variable.Factor.Names 
    Variable.Numeric.Names <- myrvs$Variable.Numeric.Names 
    ## Create subset based on chosen inclusion criteria
    df_sub <- df %>% filter(Design %in% input$Design,
                            Publication.Year >= input$Publication.Year[1], 
                            Publication.Year <= input$Publication.Year[2],
                            N_Intervention >= input$N_Intervention[1],
                            N_Intervention <= input$N_Intervention[2],
                            Paper.and.Exp %in% input$included)
    #
    ## Create subset based on the above plus input-file defined selection factors
    for (varName in Variable.Factor.Names)  {
      keepValues <- input[[varName]]
      df_sub <- df_sub[df_sub[,varName] %in% keepValues, ]
    }
    #
    ## Create subset based on the above plus input-file defined selection numerics
    for (varName in Variable.Numeric.Names)  {
      df_sub <- df_sub[df_sub[,varName] >= input[[varName]][1], ]
      df_sub <- df_sub[df_sub[,varName] <= input[[varName]][2], ]
    }
    ### need to check to make sure nstudies is not zero; throw error message
    validate(
      need(nrow(df_sub) > 0, "That returns zero studies. Please change your selection criteria and Recalculate.")
    )
    #
    # replace ID with Paper.Number if aggregating over papers:
    if (input$aggregation == "Papers") {
      df_sub$ID <- df_sub$Paper.Number
      df_sub$study <- df_sub$Paper
    }
    #
    ## Aggregate effect sizes
    aggES <- agg(id     = ID,
                 es     = yi,
                 var    = vi,
                 data   = df_sub,
                 cor = .5,
                 method = "BHHR")
    ## Merging aggregated ES with original dataframe 
    MA <- merge(x = aggES, y = df_sub, by.x = "id", by.y = "ID") 
    MA <- unique(setDT(MA) [sort.list(id)], by = "id")
    MA <- with(MA, MA[order(MA$es)])
    myrvs$ma_num <- myrvs$ma_num + 1
    MA
  })
  
  
  ## modal to warn when Bayesian model update is requested
  observe({
    # Reactively depend on MA()
    MA()
    # Trigger only when specific tabs that require bma() are selected
    req(input$mainTabset %in% bayestabs)   # defined at the top of the server function
    #
    ##  repeating code from bma() that checks for cached bma models:
    isolate({    old_bma <- FALSE
    # copy reactive values for use in this block.  Use as arguments to functions.
    MA          <-  MA()
    tauprior    <-  input$tauprior
    mupriorsd   <-  input$mupriorsd
    scaletau    <-  input$scaletau
    mupriormean <-  input$mupriormean
    # end of reactive values that are copied.
    old_bma <- checkOldModels(myrvs$previousModels, MA=MA, tauprior=tauprior, mupriorsd=mupriorsd, scaletau=scaletau, mupriormean=mupriormean, prevMAsNoFactors = previousMAsNoFactors$MAs)
    })
    ##
    #
    if(!isTruthy(old_bma))  { # skip the shinyalert if that bma is already cached
      #
      # Display the shinyalert  
      shinyalert(
        title = "Are you sure you want to do this new Bayesian analysis?  It could take a long time.",
        type = "warning",
        showCancelButton = TRUE,
        confirmButtonText = "Yes, continue!",
        cancelButtonText = "No, not right now.",
        callbackR = function(value) {
          myrvs$triggerBma <- value
        }
      )}
    else  myrvs$triggerBma <- TRUE  # do trigger "recalculation" of bma without the shinyalert if that bma is already cached.  It will be retrieved from the cache.
  })
  

  # Create bma reactive needed for all outputs
  bma <- reactive({
    #req(MA())           # trigger to update bma; MA() gets updated only when "recalculate" button is pressed
    req(myrvs$triggerBma)  # Ensure this block runs only after user confirmation
    isolate({           # so that changes in priors do not trigger bma() update before "recalculate" button is pressed
      ## Generate bayesmeta-object "bma" depending on tau prior chosen
      old_bma <- FALSE
      # copy reactive values for use in this block.  Use as arguments to functions.
      MA          <-  MA()
      tauprior    <-  input$tauprior
      mupriorsd   <-  input$mupriorsd
      scaletau    <-  input$scaletau
      mupriormean <-  input$mupriormean
      # end of reactive values that are copied.
      old_bma <- checkOldModels(myrvs$previousModels, MA=MA, tauprior=tauprior, mupriorsd=mupriorsd, scaletau=scaletau, mupriormean=mupriormean, prevMAsNoFactors = previousMAsNoFactors$MAs)
      if(isTruthy(old_bma)) bma <- old_bma  # retrieve previously calculated bma model
      else { #### if there is no prevously calculated bma model to retrieve, calculate a new one
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
        ## store the new model 
        updateModels(MA=MA, tauprior=tauprior, mupriorsd=mupriorsd, scaletau=scaletau, mupriormean=mupriormean, bma=bma)
        bma
      }  ####
    })  # end of isolate()
  })   # end of bma() definition
  
 
  
  
  # function to update the list of previous models when bma() changes; for use in the bma() block only
  # used for its side effect of updating myrvs only; returned value not used.
  updateModels <- function(MA, tauprior, mupriorsd, scaletau, mupriormean, bma) {
    newrow <- list(MA=MA, tauprior=tauprior, mupriorsd=mupriorsd, scaletau=scaletau, mupriormean=mupriormean, bma=bma) 
    myrvs$previousModels[length(myrvs$previousModels) + 1] <- list(newrow)
    length(myrvs$previousModels)  # unused return value
  }
  
  # function to update the list of previous plots; for use in the robustggplot block only
  # used for its side effect of updating myrvs only; returned value not used.
  updatePlots <- function(MA, tauprior, mupriorsd, scaletau, robust, robustggplot) {
    newrow <- list(MA=MA, tauprior=tauprior, mupriorsd=mupriorsd, scaletau=scaletau, robust=robust, robustggplot=robustggplot) 
    myrvs$previousPlots[length(myrvs$previousPlots) + 1] <- list(newrow)
    length(myrvs$previousPlots)  # unused return value
  }

  
  # Study overview panel  
  output$studies <- DT::renderDataTable({
    MAs <- as.data.frame(MA())
    MAclean <-  mutate(MAs, "Included Studies" = study) %>% 
      select("Included Studies", Publication.Year)
    DT::datatable(MAclean,
                  options = list(pageLength = nrow(MAclean)))
  })
  
  
  # Current data panel  
  output$currentData.display <- DT::renderDataTable({
    current.data <- as.data.frame(myrvs$df.updated)
    DT::datatable(current.data,
                  options = list(pageLength = nrow(current.data)))
  })
  
 
  ## Warning message if 3 or less studies are included
    output$warning <- renderPrint({
      MAs <- as.data.frame(MA())
      if (nrow(MAs) < 4) {print('WARNING: With the chosen inclusion criteria, 3 or fewer studies will be included in the analysis.')}
  })
    
  # Outliers panel
  output$boxplot <- renderPlot({
    MAo <- MA() %>% tibble::rownames_to_column(var = "outlier") %>% mutate(is_outlier=ifelse(is_outlier(es), es, as.numeric(NA)))
    MAo$study[which(is.na(MAo$is_outlier))] <- as.numeric(NA)
    ggplot(MAo, aes(x = factor(0), es)) +
      geom_boxplot(outlier.size = 3.5, outlier.colour = "#D55E00", outlier.shape = 18, fill = "lightgrey") +
      geom_text(aes(label=study),na.rm = T, nudge_y = 0.02, nudge_x = 0.05) +
      stat_boxplot(geom="errorbar", width = 0.05) +
      scale_x_discrete(breaks = NULL) +
      xlab(NULL) + ylab("Hedges' g") +
      theme_minimal_hgrid(12)
  }, width = 600, height = 600)
  
  # Forest plot panel height
  forest_height <- reactive(length(bma()$y) * 25 + 200)
  
  # Forest Plot panel
  output$forest <- renderPlot({
    forestplot.bayesmeta(bma(), xlab = "Hedges' g")
  }, height = forest_height)
  
  # Funnel Plot panel
  output$funnel <- renderPlot({
    funnel.bayesmeta(bma(), main = "")
  })
  # Statistics panel
  output$bf <- renderPrint ({
    bma()$bayesfactor[1,]
  })
  output$summary <- renderPrint({
    bma()$summary
  })
  output$ML <- renderPrint({
    bma()$ML
  })
  output$MAP <- renderPrint({
    bma()$MAP[1,]
  })
  

  # Additional plots panel
  output$evupdate <- renderPlot({
    priorposteriorlikelihood.ggplot(bma(), lowerbound = 0 - (input$mupriormean + 1) * 1.5, upperbound = 0 + (input$mupriormean + 1) * 1.5)
  }, width = 800)
  output$joint <- renderPlot({
    plot.bayesmeta(bma(), which=2, main = "")
  }, width = 800)
  output$taupriorplot <- renderPlot({
    tauprior.ggplot(bma())
  }, width = 800)


  ## modal to warn when Bayesian robust plot pane update is requested
  observe({
    # Reactively depend on MA()
    MA()
    # Trigger only when the robustness tab is selected
    req(input$mainTabset %in% c("bayesian_robustness"))   # for that panel only
    req(input$robust == "Yes")   # only if yes is checked in the priors panal
    #
    ##  repeating code  that checks for cached robustness plots:
    isolate({    old_plot <- FALSE
    # copy reactive values for use in this block.  Use as arguments to functions.
    MA <- MA()
    MA_nofactors <- as.data.frame(MA)
    MA_nofactors <-  MA_nofactors %>% mutate_if(is.factor, as.character)
    robust <- input$robust
    tauprior <- input$tauprior
    mupriorsd <- input$mupriorsd
    scaletau <- input$scaletau
    old_plot <- checkOldPlots(myrvs$previousPlots, MA=MA_nofactors, tauprior=tauprior, mupriorsd=mupriorsd, scaletau=scaletau, robust=robust)
    if(!isTruthy(old_plot))  { # skip the shinyalert if that plot is already cached
      #
      # Display the shinyalert
      shinyalert(
        title = "Are you sure you want to do this new Bayesian robustness analysis?  It could take a VERY long time.",
        type = "warning",
        showCancelButton = TRUE,
        confirmButtonText = "Yes, continue!",
        cancelButtonText = "No, not right now.",
        callbackR = function(value) {
          myrvs$triggerBmaRobust <- value
        }
      )}
    else  myrvs$triggerBmaRobust <- TRUE
    })
  })

  
  # Bayes factor robustness plot panel
  output$warning2 <- renderPrint({
      print("WARNING: Plot will not be computed, because an improper τ prior was chosen. Proper τ priors are 'Half student t' and 'Half cauchy'.")
  })
  #
  output$robustplot <- renderPlot({
    MA <- MA()  #trigger recalculation
    MA_nofactors <- as.data.frame(MA) 
    MA_nofactors <-  MA_nofactors %>% mutate_if(is.factor, as.character)
    robust <- input$robust
    tauprior <- input$tauprior
    mupriorsd <- input$mupriorsd
    scaletau <- input$scaletau
    old_plot <- checkOldPlots(myrvs$previousPlots, MA=MA_nofactors, tauprior=tauprior, mupriorsd=mupriorsd, scaletau=scaletau, robust=robust)
    if(isTruthy(old_plot)) robustggplot <- old_plot  # retrieve previously calculated plot
    else {
      robustggplot <- NULL
      if (robust == "Yes" & tauprior == "Half cauchy" & isTruthy(myrvs$triggerBmaRobust)) {
        robustggplot <- robustness(MA,SD = mupriorsd, tauprior = function(t) dhalfcauchy(t, scale = scaletau))
      } else if (robust == "Yes" & tauprior == "Half student t" & isTruthy(myrvs$triggerBmaRobust)) {
        robustggplot <- robustness(MA,SD = mupriorsd, tauprior = function(t) dhalfnormal(t, scale = scaletau))
      }
      ## store the new plot in the list of old plots; this function only has a side effect, no return value
      if (isTruthy(robustggplot)) {
        updated <- updatePlots(MA=MA_nofactors, tauprior=tauprior, mupriorsd=mupriorsd, scaletau=scaletau, robust=robust, robustggplot=robustggplot)
      }
    }
    robustggplot
  }, width = 800)
  
  
  
  ###### Managing "cached" bayesmeta models:
  #
  #### observer to clear the Saved Models when the button is pressed
  observeEvent(input$ClearModels, {
    myrvs$previousModels <- list()
  })
  #
  #### section for uploading saved models from a file
  #### for reading .RDS file containing previously calculated bayesmeta models
  observeEvent(input$SavedModelsUp, {
    # Read the data from the RDS file the user uploaded:
    fileExtension <- tools::file_ext(input$SavedModelsUp$datapath)
    output$inputFileErrorBup <- renderUI({  # create error message in case file not uploaded successfully
      if (!is.null(input$SavedModelsUp)) p(style = "color:red", "***File was not read***")
    })
    validate(need(fileExtension == "RDS" | fileExtension == "rds" | fileExtension == "Rds" , "Please upload an RDS file"))
    newrows_bma <- readRDS(input$SavedModelsUp$datapath)
    oldrows_bma <- myrvs$previousModels
    allrows_bma <- c(newrows_bma, oldrows_bma)
    myrvs$previousModels <- allrows_bma
    output$inputFileErrorBup <- renderUI(NULL) # remove error message if file uploaded successfully
  })
  #
  #### For downloading saved bayesmeta models (can be uploaded in this format)
  output$rds_file.bma <- downloadHandler(   
    filename = function() {
      "bayesmeta_models.RDS" 
    },
    content = function (file) {
      saveRDS(myrvs$previousModels, file)
    }
  )
  #
  
  
  
  ###### Managing "cached" BF robustness plots:
  #
  #### observer to clear the Saved plots when the button is pressed
  observeEvent(input$ClearPlots, {
    myrvs$previousPlots <- list()
  })
  #
  #### section for uploading saved Plots from a file
  #### for reading .RDS file containing previously created BF robustness plots 
  observeEvent(input$SavedPlotsUp, {
    # Read the data from the RDS file the user uploaded:
    fileExtension <- tools::file_ext(input$SavedPlotsUp$datapath)
    output$inputFileErrorBup <- renderUI({  # create error message in case file not uploaded successfully
      if (!is.null(input$SavedPlotsUp)) p(style = "color:red", "***File was not read***")
    })
    validate(need(fileExtension == "RDS" | fileExtension == "rds" | fileExtension == "Rds" , "Please upload an RDS file"))
    newrows_bma <- readRDS(input$SavedPlotsUp$datapath)
    oldrows_bma <- myrvs$previousPlots
    allrows_bma <- c(newrows_bma, oldrows_bma)
    myrvs$previousPlots <- allrows_bma
    output$inputFileErrorBup <- renderUI(NULL) # remove error message if file uploaded successfully
  })
  #
  #### For downloading saved BF robustness plots (can be uploaded in this format)
  output$rds_file.robustnessPlots <- downloadHandler(   
    filename = function() {
      "BF_robustness_plots.RDS" 
    },
    content = function (file) {
      saveRDS(myrvs$previousPlots, file)
    }
  )

  
  ########### Downloads panel
  #
  ##### create download buttons to display in UI
  output$downloadButtons <- renderUI({
    if (myrvs$recalculatedSinceUpload > 0){
      tagList(
        p(),
        downloadButton("originalData", "Data as originally uploaded"),
        p(),
        downloadButton("currentData.down", "Data as currently in use (primarily for debugging)"),
        p(),
        downloadButton("listInputs", "List of all selected Study Criteria and Prior Specifications"),
        p(),
        downloadButton("bayesmetaCall", "Function call, parameters, and selected data for the current analysis")
      ) 
    }
    else(p("Must (Re)Calculate first...."))
  })
  #
  ##### create things for the UI to download here....
  output$originalData <- downloadHandler(   
    filename = function() {
      "originalData.xlsx" 
    },
    content = function (file) {
      writexl::write_xlsx(myrvs$df.original, file)
    }
  )
  #  
  output$currentData.down <- downloadHandler(
    filename = function() {
      "currentData.xlsx" 
    },
    content = function (file) {
      if (is.null(myrvs$currentInputFile)) {Source <- "Vasilev et al., 2018"} else Source <- myrvs$currentInputFile
      Source <- req(as.data.frame(Source))
      sheetList <- list(current_Data = myrvs$df.updated,
                        current_df = myrvs$df.reactive, 
                        selected_data = req(as.data.frame(MA())), 
                        original_Data = myrvs$df.original, 
                        Source = Source )
      writexl::write_xlsx(sheetList, file)
    }
  )
  #
  output$bayesmetaCall <- downloadHandler(
    filename = function() {
      "bayesmeta_call.xlsx" 
    },
    content = function (file) {
      if (is.null(myrvs$currentInputFile)) {Source <- "Vasilev et al., 2018 plus 2023 updates"} else Source <- myrvs$currentInputFile
      Source <- req(as.data.frame(Source))
      Bayesmeta.Summary <- req(as.data.frame(bma()$summary))
      Bayesmeta.Summary$statistic <-   row.names(Bayesmeta.Summary) 
      Bayesmeta.Summary <- select(Bayesmeta.Summary, statistic, tau, mu, theta)
      Bayesmeta.Call <- capture.output(bma()$call) %>% paste(collapse = "") %>% as.data.frame() 
        names(Bayesmeta.Call) <- "bayesmeta analysis command"
      mupriormean <- req(as.data.frame(input$mupriormean))
      mupriorsd <- req(as.data.frame(input$mupriorsd))
      tauprior <- req(as.data.frame(input$tauprior))
      scaletau <- req(as.data.frame(input$scaletau))
      robust <- req(as.data.frame(input$robust))
      sheetList <- list(Bayesmeta.Summary = Bayesmeta.Summary, 
                        Bayesmeta.Call = Bayesmeta.Call, 
                        mupriormean = mupriormean,
                        mupriorsd = mupriorsd,
                        tauprior = tauprior,
                        scaletau = scaletau,
                        MA.selected.data = req(as.data.frame(MA())), 
                        Source = Source 
                        )
      writexl::write_xlsx(sheetList, file)
    }
  )
  #
  output$listInputs <- downloadHandler(
    filename = function() {
      "currentInputSelections.xlsx" 
    },
    content = function (file) {
      inputslist <- reactiveValuesToList(input)
      ns <- names(inputslist)
      skipnames1 <- c("website","q8","q18","q118", "recalculateButton","q16","q19", "q1","q17", "q20" ,"email1", "q9"  )
      skipnames2 <- c( "studies_cells_selected" ,  "studies_rows_all"    ,     "studies_rows_selected"  , 
                       "studies_state"     ,       "studies_search"       ,    "studies_cell_clicked"  ,  
                       "studies_columns_selected", "studies_rows_current")
      skipnames3 <- c("currentData.display_state", "currentData.display_cell_clicked",
                     "currentData.display_cells_selected" ,  "currentData.display_rows_selected",
                      "currentData.display_rows_all"    ,     "currentData.display_search"   ,       
                       "currentData.display_rows_current"  ,   "currentData.display_columns_selected", "dataSetupPanel")
      skipnames4 <- c("ClearModels", "SavedPlotsUp",
                      "mainTabset",
                      "ClearPlots",
                      "SavedModelsUp"
      )
      skipnames <- c(skipnames1, skipnames2, skipnames3, skipnames4)
      namestolist <- ns[! ns %in% skipnames]
      orderednames <- c("mupriormean", "mupriorsd", "tauprior", "scaletau", "robust", "DataFileUp", "aggregation", "Design", "Publication.Year","included" )
      extranames <- namestolist[! namestolist %in% orderednames]
      allnames <- c(orderednames, extranames)
      ilist <- inputslist[allnames]
      ilist <- purrr::discard(ilist, is.list) # just in case any elements are lists and would crash the next line of code
      ilist2 <- lapply(ilist, function(x) as.data.frame(x))
      writexl::write_xlsx(ilist2, col_names = F, file)
    }
  )

  ##### This section added for frequentist analyses, for large datasets ####
  #
  # Create model for frequentist meta-analysis
  fma <- reactive({
    MA()  # must reference MA() first so its check for (nrows > 0) happens
    #
    rma(MA()$es, MA()$var, slab=MA()$study)})
  
  # Forest plot panel height
  freq_forest_height <- reactive(length(fma()$yi) * 12 + 200)
  
  ### Frequentist Forest Plot
  output$freq_forest <- renderPlot({
    model <- fma()  
    # Increase bottom margin to make space for the text
    par(mar = c(5, 4, 4, 2) + 0.1)  # Adjust the bottom margin (the first value)
    # Generate the forest plot
    plot <- metafor::forest.rma(x = model, showweights = TRUE, addfit = TRUE,
                                order = "obs", xlab = "Hedges' g", 
                                addpred = TRUE, 
                                efac = 0,
                                col = "red",
                                border = "red")
    # Add Cochran's Q, its p-value, and I² statistic as text
    # Position the text below the plot
    mtext(side = 1, line = 4, 
          text = paste0("Cochran's Q = ", round(model$QE, 2), 
                       " (p = ",  format(round(model$QEp, 4), nsmall = 4), ")\n",
                       "I² = ", round(model$I2, 2), "%"),
          adj = 0, cex = 0.8)
    # Return the plot
    plot
  }, height = freq_forest_height)
  

  # Funnel plot (frequentist)
  output$freq_funnel <- renderPlot({
    funnel(fma(), xlab = "Observed outcome")
  })

  
  ## observer to signal when the "recalculate" button can be activated
  observe({
    # Check if the necessary reactive values are populated
    if (!is.null(myrvs$df.reactive) && 
        !is.null(myrvs$Variable.Factor.Names) && 
        !is.null(myrvs$Variable.Numeric.Names) && 
        !is.null(myrvs$na.warning)) {
        # If the condition is met, set the flag to TRUE
      myrvs$uiRendered <- TRUE
    }
  })
  
  ## activate the recalculate button after the UI finishes rendering
  # Observe changes in myrvs$uiRendered
  observe({
    if (myrvs$uiRendered) {
      # Enable the recalculate button
      shinyjs::enable("recalculateButton")
    }
  })
 
  # Download handler for the HelperFunctions.R file
  output$download_HelperFunctions <- downloadHandler(
    filename = function() {
      "HelperFunctions.R"
    },
    content = function(file) {
      file.copy("HelperFunctions.R", file)
    }
  )
  
}  # end of server
