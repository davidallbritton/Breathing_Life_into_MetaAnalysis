################### A General Tool for Living Meta-Analysis #################
# v.0.9.6 2024.03.06
#----
# default data file for initial display, before user uploads their own data file:
load(file = "data/2023updatedData.Rda")             # loads a dataframe called "df"

####################################################################################

# Load required packages and source helper functions #----
library(purrr)
library(metafor)
library(readxl)
library(writexl)
library(tools)
library(shiny)
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
library(shinyBS)
library(shinycssloaders)
library(shinythemes)
library(stringr)
library(tidyr)
library(xtable)
library(shinyalert)
library(shinyjs)
# library(shinymeta)

options(useFancyQuotes = F)

################### load external R source files ################################
source("HelperFunctions.R")
source("effect_sizes.R")


################## Constants #######################################################
printButton <- HTML('<p  style="text-align:right; font-size: 8px;"><button  onClick="window.print()">PRINT</button></p>')
r_estimate <- 0.74326344959  # Vasilev et al.'s estimate of the correlation between outcomes in a single study (for "within" designs)
thisYear <- 2024    # default for entering new data points
first_shiny_meta_paper_full <- "Wolf, V., Kühnel, A., Teckentrup, V., Koenig, J., & Kroemer, N. B. (2021). Does transcutaneous auricular vagus nerve stimulation affect vagally mediated heart rate variability? A living and interactive Bayesian meta‐analysis. Psychophysiology, 58(11), e13933."
first_shiny_meta_paper <- "Wolf, et al. (2021)"
first_shiny_meta_paper_doi <- "https://doi.org/10.1111/psyp.13933"
first_shiny_meta_paper_app <- "https://vinzentwolf.shinyapps.io/taVNSHRVmeta"
noise_meta_paper_full <- "Vasilev, M. R., Kirkby, J. A., & Angele, B. (2018). Auditory distraction during reading: A Bayesian meta-analysis of a continuing controversy. Perspectives on Psychological Science, 13(5), 567-597."
noise_meta_paper <- "Vasilev, et al. (2018)"
