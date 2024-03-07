#######################################################################################
################### A General Tool for BAYESIAN META-ANALYSIS #######################
#######################################################################################
# v.0.9.6 2024.03.06

################### Helper functions #################################################

##################  Functions   ####################################################

# Reformat the data.  returns a list
reformat.df <- function(df.input) {
  df.original <- df.input
  df <- df.original
  ################## Get list of variable factors from input data file ##############
  Variable.Factor.Names <-  colnames(select(df, Begin.Selection.Factors:End.Selection.Factors & !c(Begin.Selection.Factors, End.Selection.Factors)))
  ################## Get list of numeric selection variables from input data file ##############
  Variable.Numeric.Names <-  colnames(select(df, Begin.Selection.Numerics:End.Selection.Numerics & !c(Begin.Selection.Numerics, End.Selection.Numerics)))
  #
  # Add a column to mark the end of the original data file columns
  # if it does not already exist
  if(!("End.Original.Data" %in% colnames(df))) {df$"End.Original.Data" <- ""}    
  ################## Replace NAs in variable factors with "NA (missing)" string
  df <- dplyr::mutate_at(df, vars(Begin.Selection.Factors:End.Selection.Factors), as.character)
  for (vName in Variable.Factor.Names)  {
    df[is.na(df[,vName]), vName]  <- "NA (missing)"
  }
  ###### Change some columns to factors in case they were not already.  
  # Required columns that should be factors: Paper.and.Exp, Paper, Design
  # variable "Selection.Factors" should be factors as well
  df <- dplyr::mutate_at(df, vars(Paper.and.Exp, Paper, Design, Begin.Selection.Factors:End.Selection.Factors), as.factor)
  #
  ################# Create warnings for NAs in the variable numeric selection columns ####
  na.warning <- ""
  na.warning <- sapply(c(Variable.Numeric.Names, "N_Intervention"), function(nvname){
    if(nna <- sum(is.na(df[,nvname]))) {
      na.warning <- paste0(na.warning, "**WARNING!** ",'"', nvname,'"', " contained ", nna, " NAs (missing values), which have now been recoded as zeroes.")
    }
  })
  ################ Recode NAs in the variable numeric selection columns as zeroes #####
  for (vName in c(Variable.Numeric.Names, "N_Intervention"))  {
    df[is.na(df[,vName]), vName]  <- 0
  }
  #
  #  The input data file should contain columns for Experiment.Number and Effect.Size.Number
  #  for use in aggregating effects within each paper.
  #  The following lines are just in case an input file does not have those columns, perhaps
  #  because the papers each had only one experiment and one measurement.
  #
  if(!("Experiment.Number" %in% colnames(df))) {df$"Experiment.Number" <- 1}    # experiment number within a paper
  if(!("Effect.Size.Number" %in% colnames(df))) {df$"Effect.Size.Number" <- 1}  # effect size number within an experiment
  #
  # copy the "Paper.and.Exp" column to a "study" column.  
  # the study column is used by the aggregation functions
  df$study <- df$Paper.and.Exp
  #
  ###  The calculation functions expect yi and vi instead of g and g_var, so add those columns if needed:
  if(!("yi" %in% colnames(df))) {df$yi <- df$g}
  if(!("vi" %in% colnames(df))) {df$vi <- df$g_var}
  #
  # the function returns this list:
  dfout <- list(
    df.original = df.original,
    df = df, 
    Variable.Factor.Names = Variable.Factor.Names, 
    Variable.Numeric.Names =Variable.Numeric.Names, 
    na.warning = na.warning
    )
  dfout
}


# Generate function "priorposteriorlikelihood.ggplot"
## Plots prior, posterior and likelihood distribution
priorposteriorlikelihood.ggplot <- function(bma, lowerbound, upperbound) {
  effect <- seq(lowerbound, upperbound, length = 200)
  colors <- c("posterior" = "#D55E00", "prior" = "#009E73", "likelihood" = "#0072B2")
  devAskNewPage(ask = FALSE)
  ggplot(data = NULL, aes(x=effect,y=bma$dposterior(mu = effect))) + 
    geom_line(aes(x = effect, y = bma$likelihood(mu = effect), col = "likelihood")) +
    geom_line(aes(x = effect, y = bma$dposterior(mu = effect), col = "posterior")) + 
    geom_line(aes(x = effect, y = bma$dprior(mu = effect), col = "prior")) + 
    geom_vline(xintercept = 0, col = "gray") + 
    theme_minimal_hgrid(12) + 
    labs(x = "effect μ", y = "probability density", color = "legend") + 
    scale_color_manual(values = colors)
}

# Generate function "tauprior.ggplot"
## Plots tau prior distribution
tauprior.ggplot <- function(bma) {
  effect <- seq(0, 2, length = 200)
  devAskNewPage(ask = FALSE)
  ggplot(data = NULL, aes(x=effect,y=bma$dprior(tau = effect))) + 
    geom_line(aes(x = effect, y = bma$dprior(tau = effect))) + 
    geom_vline(xintercept = 0, col = "gray") + 
    theme_minimal_hgrid(12) + 
    labs(x = "heterogeneity τ", y = "probability density") 
}

# Generate function "robustness"
## Plots Bayes factor robustness BF10 or BF01 depending on which is > 1
robustness  <- function(MA,SD, tauprior) {
  narrow <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study, 
                      tau.prior = tauprior, 
                      mu.prior = c("mean" = 0, "sd" = (SD/2)))
  default <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study, 
                       tau.prior = tauprior, 
                       mu.prior = c("mean" = 0, "sd" = 1.5))
  user <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study, 
                    tau.prior = tauprior, 
                    mu.prior = c("mean" = 0, "sd" = SD))
  wide <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study, 
                    tau.prior = tauprior, 
                    mu.prior = c("mean" = 0, "sd" = SD+1))
  ultrawide <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study, 
                         tau.prior = tauprior, 
                         mu.prior = c("mean" = 0, "sd" = SD+2))
  BFS <- if(user$bayesfactor[1,2]  < 1) {  #choosing between plotting BF01 vs. BF10
    defaultBFS <- 1/default$bayesfactor[1,2]
    yLabel <- "BF10"
    firstHypothesis <- "H1"
    secondHypothesis <- "H0"
    c(1/narrow$bayesfactor[1,2], 1/user$bayesfactor[1,2], 1/wide$bayesfactor[1,2], 1/ultrawide$bayesfactor[1,2])
  } else {
    defaultBFS <- default$bayesfactor[1,2]
    yLabel <- "BF01"
    firstHypothesis <- "H0"
    secondHypothesis <- "H1"
    c(narrow$bayesfactor[1,2], user$bayesfactor[1,2], wide$bayesfactor[1,2], ultrawide$bayesfactor[1,2])
  }
  SDS <- c(SD/2,SD,SD+1,SD+2)
  names <- c("Narrow", "User","Wide","Ultrawide")
  deflabel <- data.frame(Ref = "Default", val = 1.5, stringsAsFactors = F)
  ggplot(data = NULL, aes(SDS,BFS)) +
    geom_hline(yintercept = 1, color = "grey", size = 0.3, alpha = .6) + 
    geom_hline(yintercept = 3, color = "grey", size = 0.3, alpha = .6) + 
    geom_hline(yintercept = 10, color = "grey", size = 0.3, alpha = .6) + 
    geom_hline(yintercept = 30, color = "grey", size = 0.3, alpha = .6) + 
    geom_vline(xintercept = 1.5, color = "#D55E00", size = 0.3, alpha = .4) +
    geom_point(aes(SDS,BFS), alpha = .75) + 
    geom_text(aes(label = round(BFS, digits = 2)), hjust = -0.75) +
    geom_point(aes(1.5, defaultBFS), alpha = .75) +
    scale_x_continuous(breaks = c(SDS, 1.5), limits = c(SDS[1]-.5,SDS[4]+3)) +
    scale_y_continuous(limits = c(0,max(BFS)+10)) +
    geom_line(aes(SDS,BFS), alpha = .4) +
    geom_text(aes(label = names),vjust = -1) +
    geom_text(mapping = aes(x = val, y = 0, label = Ref, hjust = -.2, vjust = -1), data = deflabel, color = "#D55E00") +
    labs(x = "standard deviations", y = yLabel) +
    theme_cowplot(12) +
    annotate("text", label = paste("Moderate evidence for", firstHypothesis), x = SDS[4] + 2, y = 6.5) + #3-10
    annotate("text", label = paste("Strong evidence for", firstHypothesis), x = SDS[4] + 2, y = 20) + #10-30
    annotate("text", label = paste("Very strong evidence for", firstHypothesis), x = SDS[4] + 2, y = (BFS[4]+10+30)/2) + #30-100
    annotate("text", label = paste("Anecdotal evidence for", firstHypothesis), x = SDS[4] + 2, y = 2) + #1-3
    annotate("text", label = paste("Anecdotal evidence for", secondHypothesis), x = SDS[4] + 2, y = 0) #0-1
}

# Generate function "is_outlier"
## Check for outliers
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


################### Helper functions for frequentist analysis ########################

# Generate function "myMareg
## Meta analysis regression with summary, forest and funnel plot
myMareg <- function(formula = es~1,
                    var = var,
                    data,
                    addfit_forest = T) {
  model <- mareg(formula = formula,
                 var = var,
                 data = data,
                 slab = data$study)
  
  result <- list(summary(model),
                 confint(model),
                 metafor::forest.rma(x = model, showweights = T, addfit = addfit_forest,
                                     order = "obs",
                                     xlim=c(-10,8)),
                 funnel(model, xlab = "Observed outcome"))
  listnames <- c("Modelsummary", "Model-Fit", "Forestplot", "Funnelplot")
  names(result) <- listnames
  return(result)
  
}

# Generate function "myCoef"
## Coefficients table
myCoef <- function(coef){
  coef<-round(coef,digits = 3)
  DT::datatable(coef,
                rownames=c("Intercept"),
                colnames=c("b","S.E.","z","lower CI","upper CI","p"))
}

# Generate function "myFit"
## Model fit
myFit <- function(fit){
  fit<-round(fit,digits = 3)
  DT::datatable(fit)
}

# Generate function "myUni"
## Model uniqueness
myUni<-function(rand){
  uni<-as.data.frame(rand)
  uni$estimate<-round(uni$estimate,digits = 2)
  uni$ci.lb<-round(uni$ci.lb,digits = 2)
  uni$ci.ub<-round(uni$ci.ub,digits = 2)
  DT::datatable(uni,
                colnames = c("estimate","lower CI","upper CI"))
}



########################### Functions for users adding a study ##########
# calculate effect size from user input
##
isok <- function(x){
  # returns true if x exists and is truthy; false otherwise
  # useful for validating input fields before using them
  xname <- as.list(sys.call())[[2]]
  if(exists(xname)){isTruthy(x)} else {FALSE}
}
#
getEffectSize <- function(g=NA, g_var=NA, d=NA, d_var=NA, mean_E=NA, mean_C=NA,
                  var_E=NA, var_C=NA, var_type=NA, N_Total=NA, N_Intervention=NA, 
                  N_Control=NA, Design="between", r = r_estimate,
                  reverseCode="No") {
  if  (!isTruthy(g)  |  !isTruthy(g_var)) {
    if (!isTruthy(d) | !isTruthy(d_var) ) {  
      if (!isTruthy(N_Control) |  !isTruthy(N_Intervention)) {
      } #throw an error: must specify Ns
      if (!isTruthy(mean_E) |  !isTruthy(mean_C)) {
        message("Add throwing an error here for means"); return(NA)}   #throw an error: must specify control group SD
      type <- "E-C"
      if (reverseCode =="Yes") {
        type <- "C-E"
        mean_E <- -mean_E
        mean_C <- -mean_C
      }
      if (!isTruthy(var_C)) {
        message("Add throwing an error here; no control SD provided***"); return(NA)} #throw an error: must specify control group SD
      if (var_type == "Standard deviation"){
        sd2i <- var_C
        sd1i <- var_E
      } else if (var_type == "Variance"){
        sd2i <- sqrt(var_C)
        sd1i <- sqrt(var_E)
      } else if (var_type == "Standard error") {
        sd2i <- var_C * sqrt(N_Control)
        sd1i <- var_E * sqrt(N_Intervention)
        }
      #
      # calculate g for a between design from group means and SDs
      if (Design == "between"){
        if (!isTruthy(var_E)) { # using control SD only; SDM1 for between groups design
          gcalc <- metafor::escalc (measure = "SMD1", vtype = "LS2",
                           m1i = mean_E, m2i = mean_C,
                           sd2i = sd2i, n1i = N_Intervention, n2i = N_Control)
        } else { # using both control SD and intervention SD;  SMD for between groups design
          gcalc <- metafor::escalc (measure = "SMD", vtype = "LS2",
                           m1i = mean_E, m2i = mean_C, 
                           sd1i = sd1i, sd2i = sd2i, n1i = N_Intervention, n2i = N_Control)   
        }
      }
      else { # for within designs (anything other than "between")
        if (!isTruthy(var_E)) { # using control SD only; SMCR for within group design
          gcalc <- metafor::escalc (measure = "SMCR", vtype = "LS2",
                           m1i = mean_E, m2i = mean_C,
                           sd1i = sd2i, ni = N_Total, ri = r)   # using SD of the control condition sd2i
        } else { # using both control SD and intervention SD; SMCC for within group design
          gcalc <- metafor::escalc (measure = "SMCC", vtype = "LS2",
                           m1i = mean_E, m2i = mean_C, 
                           sd1i = sd1i, sd2i = sd2i, ni = N_Total, ri = r)
        }
      }
      g <- gcalc$yi[[1]]
      g_var <- gcalc$vi
    } else {  # if d and d_var were provided by the user
      # calculate g from d, using the functions from Vasilev et al., 2022 
      g <- Hedges_g(d = d, design = Design, N_C = N_Control, N_E = N_Intervention, N = N_Total)
      g_var <- Hedges_g_var(d_var = d_var, design = Design, N_C = N_Control, N_E = N_Intervention, N = N_Total)
    }
  } # should have g and g_var at this point to return in a list
  req(g)
  req(g_var)
  list(yi = g, vi = g_var)
}
###########  End of function for calculating g and g_var from user input #############

########## Function for checking for previously calculated bma models ###########
checkOldModels <- function(listPrevious, MA, tauprior, mupriorsd, scaletau, mupriormean, prevMAsNoFactors) {
  return_bma <- FALSE
  if(length(listPrevious)) {
    MA <- as.data.frame(MA) 
    MA <-  MA %>% mutate_if(is.factor, as.character)
    for (i in seq_along(listPrevious)) {
      ma_previous <- as.data.frame(prevMAsNoFactors[[i]]) 
      if (identical(ma_previous, MA) && 
          identical(listPrevious[[i]]$tauprior, tauprior) && 
          identical(listPrevious[[i]]$mupriorsd, mupriorsd) && 
          identical(listPrevious[[i]]$scaletau, scaletau) && 
          identical(listPrevious[[i]]$mupriormean, mupriormean)
      )  {
        return_bma <- listPrevious[[i]]$bma
        break
      }
    }
  }
  if(isTruthy(return_bma)) return_bma else FALSE
}

########## Function for checking for previously created BF robust plots ###########
checkOldPlots <- function(listPrevious, MA, tauprior, mupriorsd, scaletau, robust) {
  return_robustggplot <- FALSE
  if(length(listPrevious)) {
    MA <- as.data.frame(MA) 
    MA <-  MA %>% mutate_if(is.factor, as.character)
    for (i in seq_along(listPrevious)) {
      ma_previous <- listPrevious[[i]]$MA 
      if (identical(ma_previous, MA) && 
          identical(listPrevious[[i]]$mupriorsd , mupriorsd) && 
          identical(listPrevious[[i]]$scaletau , scaletau) && 
          identical(listPrevious[[i]]$robust , robust)
          )  {
        return_robustggplot <- listPrevious[[i]]$robustggplot
        break
      }
    }
  }
  if(isTruthy(return_robustggplot)) return_robustggplot else FALSE
}

########## Functions for generating R code

removeBadCharacters <- function(df) { # remove problematic characters, like " \r \n 
  #    from a dataframe
  # Define the characters to remove: carriage returns, newlines, tab, form feed
  chars_to_remove <- c("\\r", "\\n", "\\t",  "\\f")
  pattern <- paste(chars_to_remove, collapse = "|")
  #
  # Apply the removal operation only to character and factor columns
  df_clean <- as.data.frame(lapply(df, function(column) {
    # Store the original type for later use
    original_type <- class(column)
    #
    # Convert factor to character to process it
    column_as_char <- as.character(column)
    # Remove the unwanted characters
    column_cleaned <- gsub(pattern, "", column_as_char)
    #
    # If the original column was a factor, convert it back
    if (original_type == "factor") {
      column_cleaned <- factor(column_cleaned)
    }
    #
    # Return the cleaned column
    return(column_cleaned)
  }), stringsAsFactors = FALSE) # Keeps strings as characters unless explicitly converted back to factors
  #
  return(df_clean)
}

removeBadCharactersFromString <- function(inputString) {  # remove problematic characters, like " \r \n
  # Define the characters to remove: carriage returns, newlines, tabs, and form feeds
  chars_to_remove <- c("\r", "\n", "\t",  "\f")
  pattern <- paste(chars_to_remove, collapse = "|")
  #
  # Remove the defined characters from the input string
  cleanedString <- gsub(pattern, "", inputString)
  #
  return(cleanedString)
}

replaceBadCharacters  <- function(df) { # replace problematic characters, like " \r \n with a space
  #    in a dataframe
  # Define the characters to remove: carriage returns, newlines, tab, form feed
  chars_to_remove <- c("\\r", "\\n", "\\t", "\\f")
  pattern <- paste(chars_to_remove, collapse = "|")
  #
  # Apply the removal operation only to character and factor columns
  df_clean <- as.data.frame(lapply(df, function(column) {
    # Store the original type for later use
    original_type <- class(column)
    #
    # Convert factor to character to process it
    column_as_char <- as.character(column)
    # Remove the unwanted characters
    column_cleaned <- gsub(pattern, " ", column_as_char)
    #
    # If the original column was a factor, convert it back
    if (original_type == "factor") {
      column_cleaned <- factor(column_cleaned)
    }
    #
    # Return the cleaned column
    return(column_cleaned)
  }), stringsAsFactors = FALSE) # Keeps strings as characters unless explicitly converted back to factors
  #
  return(df_clean)
}

replaceBadCharactersFromString <- function(inputString) {  # replace problematic characters, like " \r \n with a space
  # Define the characters to remove: carriage returns, newlines, tabs, and form feeds
  chars_to_remove <- c("\r", "\n", "\t",  "\f")
  pattern <- paste(chars_to_remove, collapse = "|")
  #
  # Remove the defined characters from the input string
  cleanedString <- gsub(pattern, " ", inputString)
  #
  return(cleanedString)
}

check_for_bad_chars <- function(df) {   # check for newlines and other problematic characters
  # Define the pattern to search for bad characters
  # \r - carriage return, \n - newline, \t - tab, \f - form feed
  pattern <- "[\r\n\t\f]"
  #
  # Use apply to check each element of the dataframe
  contains_special_chars <- apply(df, c(1, 2), function(x) {
    # Check if the current element matches the pattern
    return(grepl(pattern, x))
  })
  #
  # If any element contains special (bad) characters, return TRUE
  if (any(contains_special_chars)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
