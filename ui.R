#######################################################################################
################### A General Tool for Living Meta-Analysis #######################
#######################################################################################
# v.0.9.6 2024.03.06

###################   Shiny App UI ###################################

# Define UI
ui <- fluidPage(theme = shinytheme("cosmo"),
                tags$head(
                  tags$meta(charset = "UTF-8")
                ),
                useShinyjs(),
                titlePanel(title = div("A General Tool for Living Meta-Analysis"),
                           windowTitle = "A General Tool for Living Meta-Analysis"),
                sidebarLayout(
                  sidebarPanel(fluidRow(
                    uiOutput("currentDataFile"),
                    
                    tabsetPanel(id = "dataSetupPanel",
                      tabPanel("Study criteria",    
                               actionButton("recalculateButton","(Re)Calculate Meta-Analysis", icon("sync"), 
                                            style = "color: yellow; background-color: green",
                                            disabled = TRUE
                                            ),
                               fileInput("DataFileUp", label = "Upload your data file (.csv .xls or .xlsx)", accept = c(".xls", ".xlsx", ".csv")),
                               uiOutput("inputFileError"),
                               
                               ##########  The study selection panel is created in the server:
                               uiOutput("studyCriteria"),
                               ##########
                               tags$a(href="#currentDataFile", class="btn btn-default", "Go to top")
                               ),

                      tabPanel("Prior specifications",
                               br(), 
                               numericInput(inputId = "mupriormean", 
                                            label = p("µ prior mean",style="color:#333333",
                                                      tags$style(type = "text/css", "#q16 {vertical-align: top;}"),
                                                      bsButton("q16", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),   
                                            value = 0, step = 0.1),
                               bsPopover(id="q16", title = "µ prior mean.",
                                         content = paste0("<p>Set the mean of your µ prior (effect).",
                                                          "<p>Note that the results and their interpretability are drastically influenced by prior choices.",
                                                          "<p>Default: 0"),
                                         placement = "right", 
                                         trigger = "click",
                                         options = list(container = "body")),
                               numericInput(inputId = "mupriorsd", 
                                            label = p("µ prior standard deviation", style="color:#333333",
                                                      tags$style(type = "text/css", "#q17 {vertical-align: top;}"),
                                                      bsButton("q17", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),    
                                            value = 1.5, step = 0.1, min = 0),
                               bsPopover(id="q17", title = "µ prior standard deviation.",
                                            content = paste0("<p>Set the standard deviation of your µ prior (effect).",
                                                             "<p>Note that the results and their interpretability are drastically influenced by prior choices.",
                                                             "<p>Default: 1.5"),
                                            placement = "right", 
                                            trigger = "click",
                                            options = list(container = "body")),
                               radioButtons(inputId = "robust", 
                                            label = p("µ Bayes Factor robustness check", style="color:#333333",
                                                      tags$style(type = "text/css", "#q18 {vertical-align: top;}"),
                                                      bsButton("q18", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),    
                                            choices = c(No = "No", Yes = "Yes")),
                               bsPopover(id="q18", title = "µ Bayes Factor robustness check.",
                                            content = paste0("<p>Bayes Factors over a variety of prior standard deviations will be plotted.",
                                                             "<p>Note that selecting Yes will lead to an increase in computation time and that the plot will only be computed if priors for τ and μ are proper.",
                                                             "<p>Default: No."),
                                            placement = "right", 
                                            trigger = "click",
                                            options = list(container = "body")),
                               radioButtons(inputId = "tauprior", 
                                            label = p("τ prior", style="color:#333333",
                                                      tags$style(type = "text/css", "#q19 {vertical-align: top;}"),
                                                      bsButton("q19", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),    
                                            choices = c("Half cauchy", "Half student t","uniform", "sqrt", "Jeffreys", "BergerDeely", "conventional", "DuMouchel", "shrinkage", "I2")),
                               bsPopover(id="q19", title = "τ prior.",
                                         content = paste0("<p>Choose your τ prior.",
                                                          "<p>Note that the results and their interpretability are drastically influenced by prior choices.",
                                                          "<p>Default: Half cauchy."),
                                         placement = "right", 
                                         trigger = "click",
                                         options = list(container = "body")),
                               numericInput(inputId="scaletau", 
                                            label = p("τ prior scale (for half cauchy or half student t)",style="color:#333333",
                                                      tags$style(type = "text/css", "#q20 {vertical-align: top;}"),
                                                      bsButton("q20", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),    
                                            value=0.5, step=0.05),
                               bsPopover(id="q20", title = "τ prior scale.",
                                         content = paste0("<p>Set the scale of your τ prior (if a half cauchy or a half student t prior is selected).",
                                                          "<p>Note that the results and their interpretability are drastically influenced by prior choices.",
                                                          "<p>Default: 0.5"),
                                         placement = "right", 
                                         trigger = "click",
                                         options = list(container = "body")),
                               a("Further information on choosing an appropriate τ prior.", href="https://cran.r-project.org/web/packages/bayesmeta/bayesmeta.pdf", target = "_blank")),
                      
                      
                      
                      
                      tabPanel("Add a Study",   
                               p("Add new studies here.  Each effect size from a multi-experiment or multi-measurement",
                                 "paper should be a separate entry.",
                                 'An alternative method for adding studies is to download the "Current data" as a .xlsx file,',
                                 "add new rows of data, then upload the new .xlsx file for analysis."),
                               hr(),
                               
                               ##########  The rest of the study selection panel is created in the server:
                               uiOutput("studyAskType"),
                               uiOutput("setNewPaper"),
                               uiOutput("addStudies1"),
                               uiOutput("addStudies2"),
                               uiOutput("addStudies3"),
                               ##########
                      ),
                      
                      
                      
                      
                      
                      
                      
                      
                      ############ "Saved Bayesian Models" panel
                      tabPanel("Saved Plots and Models",   
                               p("The models created by bayesmeta can take a long time to compute.",
                                 "To save time",
                                 "you can download models and plots from the current session",
                                 "and then upload them in a future session."
                               ),
                               hr(),
                               #
                               p(tags$strong("Bayesmeta models")),
                               
                               #
                               downloadButton("rds_file.bma", "Download Saved Models"),
                               fileInput("SavedModelsUp", label = "Upload Saved Models", accept = c(".rds", ".RDS", ".Rds")),
                               #
                               actionButton("ClearModels", "Delete Saved Models"),
                               #
                               #
                               hr(),
                               #
                               p(tags$strong("Bayes Factor Robustness Plots")),
                               
                               #
                               downloadButton("rds_file.robustnessPlots", "Download Saved Plots"),
                               fileInput("SavedPlotsUp", label = "Upload Saved Plots", accept = c(".rds", ".RDS", ".Rds")),
                               #
                               actionButton("ClearPlots", "Delete Saved Plots"),
                               
                      ),
                      # 
                      hr()
                    ))),
                  mainPanel(
                    fluidRow(
                      tabsetPanel(id = "mainTabset",
                        tabPanel("Explanation", 
                                 printButton,
                                 h3("Welcome to the interactive living meta-analysis tool!"), 
                                 h4("Purpose:"),
                                 p("The goals of this project are to make it easier to conduct an updatable meta-analysis", 
                                   "and to encourage meta-analysts to present their work as living, evolving states of", 
                                   "knowledge rather than as fixed snapshots of the literature at a single point in time", 
                                   "and from a single point of view.  Meta-analysts can upload data and interactively set", 
                                   "selection criteria and analysis parameters to produce either a frequentist or Bayesian", 
                                   " meta-analytic estimate of an effect size along with standard statistics and graphs.  ", 
                                   "Other researchers can alter the parameters of the analysis or change which studies are ", 
                                   "included and observe the results.", "For reproducibility, R code or R markdown can be",
                                   "downloaded for the selected analysis."), 
                                 h4("Updating Results:"),
                                 p('Except for the "Current data" tab, all results tabs are only updated when the "Re-calculate Meta-Analysis" button is pressed.', 
                                   "It is not necessary to re-calculate for each tab, however.  All tabs are updated after each recalculation."), 
                                 
                                 h4("Recommended use:"),
                                 p('Because the calculations for Bayesian models can take', 
                                   "a long time for large datasets (and the results of Bayesian and frequentist methods are likely to be similar for large datasets), you may want to first use the frequentist",
                                   "panels before deciding which Bayesian models to run.",
                                   "The Bayes Factor Robustness Check panel requires multiple",
                                   "recomputations of the Bayesian model and is VERY slow as a result.",
                                   "You may only want to run it once, on your final selected model.",
                                   "For Bayesian analyses of large datasets, it is recommended that you download the Shiny app from Github (see below) and run it locally on your own machine for better performance.",
                                   " For demonstration purposes, however, the following are precomputed for the default dataset and cached to load quickly for Bayesian models and robustness plots:
",
                                 tags$ul(
                                   tags$li("All studies (default settings), with mu prior mean 0 and SD 1.5 (default priors)"),
                                   tags$li("All studies (default settings), with mu prior mean and SD empty (resulting in uniform priors) [no robustness plot]"),
                                   tags$li("Original Vasilev et al. 2018 studies only, with mu prior mean 0 and SD 1.5 (default priors)"),
                                   tags$li("Original Vasilev et al. 2018 studies only, with mu prior mean and SD empty (uniform priors; similar to the analysis in Vasilev et al. 2018) [no robustness plot]"),
                                   tags$li("All studies, aggregated over papers, with mu prior mean 0 and SD 1.5 (default priors)")
                                 )
                                   ), 
                                 
                                 
                                 
                                 
                                 h4("Adding new studies:"),
                                 p("You can add new effect sizes one at a time using the",
                                   '"Add a Study" tab. You can save the updated data file',
                                   "that includes the added studies in the", '"Downloads" tab.'), 
                                 p('You can also upload an entirely new data file in the "Study criteria" tab.',
                                   "This will replace the current data entirely, including anything that was",
                                   "previously input through the", '"Add a Study" tab.',
                                   "The file to upload should be .csv (with UTF-8 encoding if it contains non-ASCII characters), .xls or .xlsx format, and should contain the following columns:"),
                                 tags$ul(
                                   tags$li(tags$b("yi"), "- a standardized effect size (Hedges g)"),
                                   tags$li(tags$b("vi"), "- the variance of g"),
                                   tags$li(tags$b("ID"), "- a unique number for each effect size"),
                                   tags$li(tags$b("Paper.Number"), "- a unique number for each paper (study)"),
                                   tags$li(tags$b("Experiment.Number"), "- within each paper"),
                                   tags$li(tags$b("Effect.Size.Number"), "- within each experiment"),
                                   tags$li(tags$b("Paper"), "- citation such as: Author (year)"),
                                   tags$li(tags$b("Paper.and.Exp"), "- citation plus Experiment and/or Effect Size number"),
                                   tags$li(tags$b("Publication.Year"), "- appears as a slider input in the",
                                           '"Study criteria" panel for selecting which studies to include.'
                                           ),
                                   tags$li(tags$b("Design"), '- "between" or "within", lower case, no quotes.',
                                           "Appears as a checkbox input in the",
                                           '"Study criteria" panel for selecting which studies to include.'
                                           ),
                                   tags$li(tags$b("N_Intervention"), "- number of subjects in the intervention condition"),
                                   tags$li(tags$b("N_Control"), "- number of subjects in the baseline condition"),
                                   tags$li(tags$b("N_Total"), "- sum of N_Intervention and N_Control for between-subjects designs; equal to N_Intervention for within-subjects designs"),
                                   tags$li(tags$b("Begin.Selection.Factors"), "and", tags$b("End.Selection.Factors"), 
                                           "- all columns between these two columns will appear as checkbox inputs in the",
                                           '"Study criteria" panel for selecting which studies to include.',
                                           ),
                                   tags$li(tags$b("Begin.Selection.Numerics"), "and", tags$b("End.Selection.Numerics"), 
                                           "- all columns between these two columns will appear as numeric slider inputs in the",
                                           '"Study criteria" panel for selecting which studies to include.',
                                           ),
                                   tags$li(tags$b("r"), "(optional) - within-study correlation of outcome measures for within-subjects designs."),
                                   tags$li("Any number of additional columns (that will be ignored)")
                                 ),
                                 p("This", 
                                 a("template file", href = "dataFileTemplate.csv", download = "dataFileTemplate.csv"),
                                 "can be used to help prepare your data for upload."),
                          #       br(),
                                 h4("Calculation methods:"),
                                 p("Meta-analysis calculations use the R", '"metafor" and "bayesmeta" packages, along with',
                                  "functions adapted from", paste0(first_shiny_meta_paper, " and ", noise_meta_paper, "."),
                                  " Input data for meta-analysis are Hedges' g and the variance of g (yi and vi).",
                                  "When yi and vi are calculated from user input, the following are used:"),
                                 tags$ul(
                                   tags$li("g and g_var input by user: copied directly from user input"),
                                   tags$li("g and g_var computed from d: using functions from", noise_meta_paper),
                                   tags$li("g and g_var computed from means: using R metafor::escalc, measure = "),
                                   tags$ul(
                                     tags$li("SMD - using M1, M2, SD1 and SD2 from a between-subject design"),
                                     tags$li("SMD1 - using M1, M2, and SD2 from a between-subject design"),
                                     tags$li("SMCC - using M1, M2, SD1 and SD2 from a within-subject design"),
                                     tags$li("SMCR - using M1, M2, and SD2 from a within-subject design")
                                   )
                                 ),
                                 h4("Paper:"),
                                 ("This app accompanies the following "),
                                 a("paper", href="https://doi.org/", target = "_blank"), 
                                 h4("Code and data:"),
                                 ("This app's R Code and sample dataset can be found "),
                                 a("here", href="https://github.com/davidallbritton/Breathing_life_into_meta-analysis", target = "_blank"),
                                 ("on GitHub."),
                                 h4("Server:"),
                                 ("This app is currently hosted at "),
                                 a("https://dallbrit.shinyapps.io/Breathing_Life_into_MetaAnalysis/", href="https://dallbrit.shinyapps.io/Breathing_Life_into_MetaAnalysis/", target="_blank"),
                                 br(), 
                                 h4("Contact:"),
                                 p("The app is maintained by David Allbritton"),
                                 p("Contact/Visit us:"),
                                   a(shiny::actionButton(inputId = "david.allbritton@depaul.edu", 
                                                         label = "Mail", 
                                                         icon = icon("envelope", lib = "font-awesome")),
                                     href="mailto:david.allbritton@depaul.edu"),
                                   a(shiny::actionButton(inputId = "website", 
                                                         label = "Web", 
                                                         icon = icon("globe", lib = "font-awesome")),
                                     href="https://github.com/davidallbritton"),
                                 br(),br(),
                                 h5("Acknowledgments:"),
                                 p("This app adapts and extends the work of",
                                   first_shiny_meta_paper, 
                                   br(),
                                   a(first_shiny_meta_paper_doi, href=first_shiny_meta_paper_doi, target="_blank"),
                                   br(),
                                   a(first_shiny_meta_paper_app, href=first_shiny_meta_paper_app, target="_blank"),
                                 )
                                 ),
                        tabPanel("Included Studies", printButton,
                          h4('This table lists all the studies included by the current selected criteria'),
                          p('(updated only when "Re-Calculate Meta-Analysis" button on the left-side of the screen is pressed
                            )'),
                          textOutput("warning"), br(),
                          DT::dataTableOutput("studies") %>% withSpinner(type = 6, color = "#3498DB"), br()
                          ),
                        tabPanel("Current data", printButton,
                                 h4('This table displays the current data file plus any added studies'), br(),
                                 DT::dataTableOutput("currentData.display") %>% withSpinner(type = 6, color = "#3498DB"), br()
                        ),
                        tabPanel("Outlier check", 
                                 printButton, 
                                 h4("Boxplot graph:"), 
                                 plotOutput("boxplot") %>% withSpinner(type = 6, color = "#3498DB")),
                        # insert frequentist tabs here
                        tabPanel("Forest plot", 
                                 printButton, 
                                 h4("Frequentist Forest plot with study weights and 95% confidence intervals:"),
                                 plotOutput("freq_forest") %>% withSpinner(type = 6, color = "#3498DB")
                        ),
                        # tabPanel("Funnel plot",
                        #          printButton,
                        #          h4("Frequentist Funnel plot"),
                        #          plotOutput("freq_funnel") %>% withSpinner(type = 6, color = "#3498DB")
                        # ),
                        
                        tabPanel("Funnel plot",
                                 printButton,
                                 h4("Frequentist Funnel plot"),
                                 plotOutput("freq_funnel") %>% withSpinner(type = 6, color = "#3498DB"),
                                 verbatimTextOutput("funnel_code")  # Add this line to display the code
                        ),
                        
                        tabPanel("Bayesian Forest plot",  value = "bayesian_forest_plot",
                                 printButton, 
                                 h4("Forest plot with 95% credible intervals:"),
                                 plotOutput("forest") %>% withSpinner(type = 6, color = "#3498DB")
                                 ),
                        tabPanel("Bayesian Funnel plot", value = "bayesian_funnel_plot",
                                 printButton,
                                 h4("Funnel plot to assess publication bias:"),
                                 plotOutput("funnel")  %>% withSpinner(type = 6, color = "#3498DB")
                                 ),
                        tabPanel("Bayesian Statistics", value = "bayesian_statistics",
                                 printButton,
                                 h4("Parameters:"),
                                 p("τ (tau): posterior distribution of heterogeneity."),
                                 p("μ (mu): posterior distribution of effect."),
                                 p("θ (theta): 'predictive distribution, that expresses the posterior knowledge about a future observation, i.e., an additional draw θk+1 from the underlying population of studies.' (Röver, 2020, p. 16)."),
                                 ("Statistics are calculated/estimated using the bayesmeta package "), 
                                 a("(Röver, 2020).", href="http://dx.doi.org/10.18637/jss.v093.i06", target = "_blank"),
                                 ("The corresponding github page can be found "),
                                 a("here.", href = "https://github.com/cran/bayesmeta", target = "_blank"), br(),
                                 h4("Bayes factors:"), verbatimTextOutput("bf") %>% withSpinner(type = 6, color = "#3498DB"),
                                 p("Bayes factors are only computed if the priors for τ and μ are proper."), br(),
                                 h4("Marginal posterior summary:"), verbatimTextOutput("summary") %>% withSpinner(type = 6, color = "#3498DB"), br(),
                                 h4("Maximum-likelihood:"), verbatimTextOutput("ML") %>% withSpinner(type = 6, color = "#3498DB"), br(),
                                 h4("Joint maximum-a-posteriori:"), verbatimTextOutput("MAP") %>% withSpinner(type = 6, color = "#3498DB")
                                 ),
                        tabPanel("Bayesian Additional plots", value = "bayesian_additional_plots",
                                 printButton,
                                 h4("Joint posterior density of heterogeneity τ and effect μ:"), plotOutput("joint") %>%  withSpinner(type = 6, color = "#3498DB"),
                                 p("Darker shading corresponds to higher probability density."),
                                 p("Red lines indicate (approximate) 2-dimensional credible regions,"),
                                 p("green lines show marginal posterior medians and 95% credible intervals,"),
                                 p("blue lines show conditional posterior mean effect as a function of the heterogeneity along with a 95% interval."),
                                 p("Red cross (+): posterior mode"),
                                 p("Pink cross (x): ML estimate"), br(),
                                 h4("Prior, posterior, & likelihood:"), plotOutput("evupdate") %>% withSpinner(type = 6, color = "#3498DB"), br(),
                                 h4("τ prior distribution:"), plotOutput("taupriorplot") %>% withSpinner(type = 6, color = "#3498DB")
                                 ),
                        tabPanel("Bayes factor robustness check", value = "bayesian_robustness",
                                 printButton,
                                 h4("Bayes Factors over a variety of prior standard deviations:"),
                                 p("Will only be computed if 'Yes' is selected for 'µ Bayes Factor robustness check' and the priors for τ and μ are proper.",),br(),
                                 p("NOTE: The robustness check requires computing multiple bayesmeta models",
                                   "and therefore takes a long time for all but the smallest datasets.",
                                   "You may want to delay this step until after you have decided on your final",
                                   "selections for study criteria and priors."),
                                 conditionalPanel(condition = "input.tauprior == 'uniform' | 
                                                               input.tauprior == 'sqrt' |
                                                               input.tauprior == 'Jeffreys' |
                                                               input.tauprior == 'BergerDeely' | 
                                                               input.tauprior == 'conventional' | 
                                                               input.tauprior == 'DuMouchel' | 
                                                               input.tauprior == 'shrinkage' | 
                                                               input.tauprior == 'I2'",
                                                  textOutput("warning2")),
                                 conditionalPanel(condition = "input.robust == 'Yes'", 
                                                  plotOutput("robustplot") %>% withSpinner(type = 6, color = "#3498DB"),
                                 p("Default: 1.5 (orange horizontal line)"),
                                 p("Narrow: user selected standard deviation / 2"),
                                 p("User: user selected standard deviation"),
                                 p("Wide: user selected standard deviation + 1"),
                                 p("Ultrawide: user selected standard deviation + 2"),
                                 p("Interpretations of Bayes factors are based on Jeffreys (1961) with slight modifications by Lee and Wagenmakers (2013) and should be considered with caution."))
                                 ),
                        tabPanel("Downloads", br(),
                          h4("Download Data and Results"),
                          p("From the currently loaded data file and most recent calculations"), 
                          uiOutput("downloadButtons")
                        ), #end of Downloads tab
                         #    
                        tabPanel("R Code", br(),
                                 p("Download an R script (.R file) or R markdown document (.Rmd file",
                                   "that will reproduce the analyses and plots as currently calculated",
                                   "by the app. The HelperFunctions file is also required, and the ",
                                   "data file must also be in the same directory.  The default data file ",
                                   "name is ",
                                   '"originalData.xlsx"', "(which can be downloaded from the Downloads tab)",
                                   "but the R code can be changed after downloading to accept any ",
                                   ".xlsx, .xls, or .csv filename.",
                                   "The R code that will be downloaded is displayed below.  If it is",
                                   "not displayed or is not what you expected, you may need to click Re-Calculate."
                                   ),
                                 downloadButton("downloadCode", "Download R Code"),
                                 downloadButton("downloadMarkdown", "Download R Markdown"),
                                 downloadButton("download_HelperFunctions", "Download required helper functions"),
                                 verbatimTextOutput("R_code_Output")
                                 ),
                        hr()
                          ) #end of main tabs panel
                        )
                      )
                    )
                )                               
                
