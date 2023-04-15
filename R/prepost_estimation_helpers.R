#' Create Descriptive Statistics for Small Area Estimation Report
#'
#' This function estimates the coefficient of variation at level specified, basic
#' statistics such number of units, regions and target areas as well as the
#' threshold on which SAE is applied and the outcome indicator of interest
#' (i.e. poverty line and poverty rate). These indicators are all expressed for
#' the census and survey
#'
#' @param ebp_object the EBP object produced from by EMDI from unit model estimation
#' the object is of class "ebp emdi"
#' @param smp_weights the sample weight variable in the household dataset
#' (i.e. the training data), include column of 1s (DO NOT LEAVE UNSPECIFIED)
#' @param pop_weights the population weight variable in the census (or synthetic)
#' census dataset (i.e. the test data), include column of 1s (NO NOT LEAVE
#' UNSPECIFIED)
#' @param repvar the variable level at which Coefficient of Variation should be
#' computed
#' @param welfare the welfare aggregate variable or outcome variable of interest
#' @param smp_data the survey/training data
#' @param pop_data the population/census/training data
#' @param threshold the poverty line or threshold specified
#' @param pop_domains the target area variable within `pop_data`
#' @param smp_domains the target area variable within `smp_data`
#'
#' @export
#'

ebp_reportdescriptives <- function(ebp_object,
                                   smp_weights,
                                   pop_weights,
                                   repvar,
                                   welfare,
                                   smp_data,
                                   pop_data,
                                   threshold,
                                   pop_domains,
                                   smp_domains){
  
  ####get list of variables
  hh_varlist <- colnames(ebp_object$framework$smp_data)
  pop_varlist <- hh_varlist[!(hh_varlist %in% c(welfare, smp_weights))]
  
  ### subset the survey and census data
  smp_df <- smp_data[complete.cases(hh_varlist),
                     c(hh_varlist, repvar, smp_weights)]
  pop_df <- pop_data[complete.cases(pop_varlist),
                     c(pop_varlist, repvar, pop_weights)]
  
  ### ---------- Estimate CV for census and survey at repvar level --------- ###
  
  ##### create a dataset with headcounts, MSEs for survey and census information
  smp_doms <- unique(smp_df[[smp_domains]])
  
  ##### quickly rename column names in the MSE section of the EBP object
  colnames(ebp_object$MSE)[!grepl("Domain",
                                  colnames(ebp_object$MSE))] <-
    paste0("MSE_", colnames(ebp_object$MSE)[!grepl("Domain",
                                                   colnames(ebp_object$MSE))])
  
  df <- merge(x = ebp_object$MSE[, c("Domain", "MSE_Head_Count")],
              y = ebp_object$ind[, c("Domain", "Head_Count")],
              by = "Domain")
  
  
  
  df$in_sample <- ifelse(df$Domain %in% smp_doms, 1, 0)
  
  df$Domain <- as.integer(as.character(df$Domain))
  
  add_df <-
    data.frame(Domain = as.integer(names(tapply(smp_df[[smp_weights]],
                                                smp_df[[smp_domains]],
                                                sum,
                                                na.rm = TRUE))),
               smp_weights = tapply(smp_df[[smp_weights]],
                                    smp_df[[smp_domains]],
                                    sum,
                                    na.rm = TRUE))
  
  add_df <- rbind(add_df,
                  data.frame(Domain = as.integer(df$Domain[!(df$Domain %in%
                                                               add_df$Domain)]),
                             smp_weights = NA))
  
  df <- merge(x = df, y = add_df, by = "Domain")
  
  add_df <-
    data.frame(Domain = as.integer(names(tapply(pop_df[[pop_weights]],
                                                pop_df[[pop_domains]],
                                                sum,
                                                na.rm = TRUE))),
               pop_weights = tapply(pop_df[[pop_weights]],
                                    pop_df[[pop_domains]],
                                    sum,
                                    na.rm = TRUE))
  
  df <- merge(x = df, y = add_df, by = "Domain")
  
  ### add the repvar variable to df as well
  pop_df$Domain <- pop_df[[pop_domains]]
  
  add_df <- unique(pop_df[, c("Domain", repvar)])
  
  df <- merge(x = df,
              y = add_df[, c("Domain", repvar)],
              by = "Domain")
  
  df$CV <- df$MSE_Head_Count / df$Head_Count
  
  ### compute the cvs for census and survey at repvar level
  add_df <- data.frame(unique(df[[repvar]]))
  
  colnames(add_df) <- repvar
  
  add_df$sum_smp_weights <- tapply(X = df$smp_weights,
                                   INDEX = df[[repvar]],
                                   FUN = sum,
                                   na.rm = TRUE)
  add_df$sum_pop_weights <- tapply(X = df$pop_weights,
                                   INDEX = df[[repvar]],
                                   FUN = sum,
                                   na.rm = TRUE)
  df <- merge(x = df, y = add_df, by = repvar)
  
  
  df$smp_weights <- df$smp_weights / df$sum_smp_weights
  df$pop_weights <- df$pop_weights / df$sum_pop_weights
  
  cv_df <-
    data.frame(indicator = paste0("CV for Area ", unique(df[[repvar]])),
               census = tapply(X = df$CV * df$pop_weights,
                               INDEX = df[[repvar]],
                               FUN = sum,
                               na.rm = TRUE),
               survey = tapply(X = df$CV * df$smp_weights,
                               INDEX = df[[repvar]],
                               FUN = sum,
                               na.rm = TRUE))
  
  #### ----------------- add other elements of the table ----------------- ####
  ##### compute number of households in census and survey
  basic_df <-
    data.frame(indicator = c("Number of Units", "Number of Regions",
                             "Number of Target Areas"),
               census = c(round(sum(pop_df[[pop_weights]], na.rm = TRUE)),
                          length(unique(pop_df[[repvar]][is.na(pop_df[[repvar]]) == FALSE])),
                          length(unique(pop_df[[pop_domains]][is.na(pop_df[[smp_domains]]) == FALSE]))),
               survey = c(ebp_object$framework$N_smp,
                          length(unique(smp_df[[repvar]][is.na(smp_df[[repvar]]) == FALSE])),
                          length(unique(smp_df[[smp_domains]][is.na(smp_df[[smp_domains]]) == FALSE]))))
  
  basic_df$census <- as.integer(basic_df$census)
  basic_df$survey <- as.integer(basic_df$survey)
  
  ##### compute poverty numbers
  smp_data$poor <- ifelse(smp_data[[welfare]] < threshold, 1, 0)
  
  smp_data[[smp_weights]] <-
    smp_data[[smp_weights]] / sum(smp_data[[smp_weights]],
                                  na.rm = TRUE)
  
  pov_df <-
    data.frame(indicator = c("National Poverty Rate", "National Poverty Line"),
               census = c(NA, NA),
               survey = c(sum(smp_data$poor * smp_data[[smp_weights]]),
                          threshold))
  
  return(list(cv_table = cv_df,
              basicinfo_df = basic_df,
              poverty_df = format(pov_df, scientific = FALSE)))
  
  
}

#' Perform test for difference between survey and census means
#'
#' This function computes weighted means of the same set of variables within the
#' census and the survey. A test for difference of the means are performed for
#' each variable with two-tailed p-values returned.
#'
#' @param smp_data the survey/training data
#' @param pop_data the population/census/training data
#' @param varlist the set of variables of interest
#' @param smp_weights the sample weight variable in the household dataset
#' (i.e. the training data), include column of 1s (DO NOT LEAVE UNSPECIFIED)
#' @param pop_weights the population weight variable in the census (or synthetic)
#' census dataset (i.e. the test data), include column of 1s (NO NOT LEAVE
#' UNSPECIFIED)
#'
#' @export

ebp_test_means <- function(smp_data,
                           pop_data,
                           varlist,
                           smp_weights,
                           pop_weights){
  
  ### get the set of complete cases of the variables as
  ### would have been used in model estimation
  smp_df <- smp_data[complete.cases(c(varlist, smp_weights)),
                     c(varlist, smp_weights)]
  pop_df <- pop_data[complete.cases(c(varlist, pop_weights)),
                     c(varlist, pop_weights)]
  
  weighted.sd <- function(x, w){
    
    delta_sq <- (x - mean(x))^2 ##square deviation of the xs
    
    nzero_w <- (length(w[w > 0]) - 1) / length(w[w > 0])
    
    result <- sqrt(sum(w * (delta_sq)) / (nzero_w * sum(w)))
    
    return(result)
  }
  
  smp_means_df <- data.frame(smp_means = apply(X = smp_df[,varlist],
                                               MARGIN = 2,
                                               FUN = weighted.mean,
                                               w = smp_df[[smp_weights]]),
                             smp_sd = apply(X = smp_df[,varlist],
                                            MARGIN = 2,
                                            FUN = weighted.sd,
                                            w = smp_df[[smp_weights]]),
                             variable = varlist)
  
  pop_means_df <- data.frame(pop_means = apply(X = pop_df[,varlist],
                                               MARGIN = 2,
                                               FUN = weighted.mean,
                                               w = pop_df[[pop_weights]]),
                             pop_sd = apply(X = pop_df[,varlist],
                                            MARGIN = 2,
                                            FUN = weighted.sd,
                                            w = pop_df[[pop_weights]]),
                             variable = varlist)
  
  means_df <- merge(smp_means_df, pop_means_df, by = "variable")
  
  means_df$diff_sd <- sqrt((means_df$smp_sd)^2 + (means_df$pop_sd)^2)
  
  means_df$diff <- means_df$pop_means - means_df$smp_means
  
  means_df$zscore <- means_df$diff / means_df$diff_sd
  
  means_df$pvalue <- 2 * (1 - pnorm(abs(means_df$zscore)))
  
  return(means_df[, c("variable", "smp_means", "pop_means", "diff", "pvalue")])
  
}

#' Produce coefficient table for reporting
#'
#' This function takes the object of class 'ebp' to present the regression
#' model results having specified the number of decimal places.
#'
#' @param ebp_object the EBP object produced from by EMDI from unit model estimation
#' the object is of class "ebp emdi"
#' @param decimals the number of decimals to report on coefficient estimates
#'
#' @export


ebp_reportcoef_table <- function(ebp_object,
                                 decimals = 3) {
  
  specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k)) ### round to decimal place
  
  options(scipen = 999) ##drop the scientific notation
  
  varname_dt <- as.data.frame(rownames(coef(summary(ebp_object$model))))
  
  colnames(varname_dt) <- "Variable"
  
  coef_dt <- as.data.frame(coef(summary(ebp_object$model)))
  
  coef_dt <- cbind(varname_dt, coef_dt)
  
  coef_dt$Sig <- ifelse(coef_dt$`p-value` < 0.001, "***",
                        ifelse(coef_dt$`p-value` < 0.05 &
                                 coef_dt$`p-value` >= 0.001, "**",
                               ifelse(coef_dt$`p-value` < 0.01 &
                                        coef_dt$`p-value` >= 0.05, "*", "")))
  
  # coef_dt[,Sig := ifelse(`p-value` < 0.001, "***",
  #                        ifelse(`p-value` < 0.05 & `p-value` >= 0.001, "**",
  #                               ifelse(`p-value` < 0.01 & `p-value` >= 0.05, "*", "")))]
  
  coef_dt$Value <- ifelse(coef_dt$Value < abs(0.0004999999),
                          signif(coef_dt$Value, 2),
                          specify_decimal(coef_dt$Value, decimals))
  
  # coef_dt[,Value := ifelse(Value < abs(0.0004999999),
  #                          signif(Value, 2),
  #                          specify_decimal(Value, decimals))]
  
  coef_dt$StdError <- ifelse(coef_dt$Std.Error < abs(0.0004999999),
                             signif(coef_dt$Std.Error, 2),
                             specify_decimal(coef_dt$Std.Error, decimals))
  
  # coef_dt[,StdError := ifelse(Std.Error < abs(0.0004999999),
  #                              signif(Std.Error, 2),
  #                              specify_decimal(Std.Error, decimals))]
  
  coef_dt$Value <- paste0(coef_dt$Value, coef_dt$Sig)
  
  ### quick relabellings
  colnames(coef_dt)[colnames(coef_dt) %in% c("Value", "StdError")] <-
    c("coeff", "std_error")
  
  rownames(coef_dt) <- seq(nrow(coef_dt))
  
  return(coef_dt[, c("Variable", "coeff", "std_error")])
  
}

#' Produce EBP Head Count Population/Rate by Rank
#'
#' This function combines the ebp object with the census data to produce report
#' tables that rank head count estimates either by population of poor or the
#' head count rates themselves in descending order. The function allows the user
#' to select the first/last "x" number of areas by name as well.
#'
#' @param ebp_object the EBP object produced from by EMDI from unit model estimation
#' the object is of class "ebp emdi"
#' @param pop_data the population/census/training data
#' @param pop_domains the target area variable within `pop_data`
#' @param pop_domnames the population domain names
#' @param pop_weights the population weight variable in the census
#' @param byrank_indicator if argument is "count", the function ranks the product
#' of Head_Count (from object of class `ebp`) and `pop_weights`, otherwise it
#' the function simply ranks Head_Count output within `ebp` object
#' @param number_to_list an integer, the first `number_to_list` number of
#' target areas to produce from `byrank_indicator` ordering.
#' @param head a logical, if `TRUE` the top `number_to_list` results will be returned
#' and if `FALSE` the bottom `number_to_list` will be returned
#'
#' @export


ebp_report_byrank <- function(ebp_object,
                              pop_data,
                              pop_domains,
                              pop_domnames,
                              pop_weights,
                              byrank_indicator = "count",
                              number_to_list = NULL,
                              head = TRUE){
  
  ### compute population totals
  pop_data <- pop_data[, c(pop_domains, pop_domnames, pop_weights)]
  
  result_dt <- tapply(X = pop_data[[pop_weights]],
                      INDEX = pop_data[[pop_domains]],
                      FUN = sum,
                      na.rm = TRUE)
  
  result_dt <- as.data.frame(result_dt)
  
  result_dt <- data.frame(domain = rownames(result_dt),
                          population = result_dt[[1]])
  
  pop_data[[pop_domains]] <- as.character(pop_data[[pop_domains]])
  
  result_dt <- merge(x = result_dt,
                     y = unique(pop_data[, c(pop_domains,
                                             pop_domnames)]),
                     by.x = "domain",
                     by.y = pop_domains)
  
  ### include the EBP Head_Count
  
  result_dt <- merge(x = result_dt,
                     y = ebp_object$ind[, c("Domain", "Head_Count")],
                     by.x = "domain",
                     by.y = "Domain")
  
  result_dt$poor_count <- result_dt$Head_Count * result_dt$population
  
  ### rank order the table as requested
  if (byrank_indicator == "count") {
    
    result_dt <- result_dt[order(-result_dt$poor_count),]
    
  } else {
    
    result_dt <- result_dt[order(-result_dt$Head_Count),]
    
  }
  
  if (is.null(number_to_list)){
    
    number_to_list <- nrow(result_dt)
    
  }
  
  if (head == TRUE) {
    
    result_dt <- head(result_dt, number_to_list)
    
  } else if (head == FALSE) {
    
    result_dt <- tail(result_dt, number_to_list)
  }
  
  
  return(result_dt)
  
}



ebp_compute_cv <- function(ebp_obj,
                           yvar,
                           domainvar,
                           designvar = NULL,
                           weights,
                           calibmatrix,
                           threshold,
                           cluster_id,
                           reweights){
  
  ## ******************** Direct Estimate : Mean and CV ************************
  
  ## computing direct estimate using calibrated bootstrapping (EMDI + LAEKEN) - direct CV1
  direct_obj <- emdi::direct(y = yvar,
                             smp_data = ebp_obj$framework$smp_data,
                             smp_domains = domainvar,
                             weights = weights,
                             design = designvar,
                             threshold = threshold,
                             var = TRUE,
                             boot_type = "calibrate",
                             X_calib = calibmatrix,
                             totals = NULL,
                             na.rm = TRUE)
  
  direct_obj$ind$Direct_Head_Count_CV <- sqrt(direct_obj$MSE$Head_Count) / direct_obj$ind$Head_Count
  
  ## computing direct estimate using the Horowitz Thompson (HT) indicator - direct CV2
  ### first compute poverty rates
  poor <- as.integer(ebp_obj$framework$smp_data[[yvar]] < threshold)
  
  domsize_dt <- as.data.frame(tapply(ebp_obj$model$data[[weights]],
                                     ebp_obj$model$data[[domainvar]],
                                     sum,
                                     na.rm = TRUE))
  
  colnames(domsize_dt) <- "popsize"
  domsize_dt$Domain <- rownames(domsize_dt)
  
  domsize_dt <- domsize_dt[is.na(domsize_dt$popsize) == FALSE,]
  
  domsize_dt <- domsize_dt[,c("Domain", "popsize")]
  
  ### HT estimator CV for direct estimate
  directht_dt <- sae::direct(y = poor,
                             dom = ebp_obj$model$data[[domainvar]],
                             sweight = ebp_obj$model$data[[weights]],
                             domsize = domsize_dt)
  
  directht_dt$Domain <- direct_obj$ind$Domain
  ## Compute design effect controlled direct estimates and CVs. (direct CV3)
  #### first estimate naive bootstrap
  #### compute design effect
  #### include psu list into the ebp data object
  ebp_obj$model$data$cluster_id <- as.factor(cluster_id)
  
  ebp_obj$model$data$domainvar <- ebp_obj$model$data[[domainvar]]
  
  ebp_obj$model$data$poor <- as.integer(ebp_obj$model$data[[yvar]] > log(threshold))
  
  ebp_obj$model$data$weights <- reweights
  
  ebpobj_svy <- survey::svydesign(ids = ~1,
                                  weights = ~weights,
                                  strata = designvar,
                                  survey.lonely.psu = "adjust",
                                  data = ebp_obj$model$data)
  
  deff_adjust <- survey::svymean(x = ~poor, ebpobj_svy, na = TRUE, deff = TRUE)
  deff_adjust <- attr(deff_adjust, "deff")[1,1]
  
  ### multiple design effect with naive calibration
  naivevar_dt <- direct(y = yvar,
                        smp_data = ebp_obj$framework$smp_data,
                        smp_domains = domainvar,
                        design = designvar,
                        weights = weights,
                        threshold = threshold,
                        var = TRUE)
  
  naivevar_dt$ind$deff_CV <- sqrt(deff_adjust) * (sqrt(naivevar_dt$MSE$Head_Count) / naivevar_dt$ind$Head_Count)
  
  ## ************************ SAE Model Estimates and CV Estimation ****************************
  
  ## compute standard CV using the EMDI package estimator function
  emdi_dt <- emdi::estimators(object = ebp_obj,
                              indicator = "Head_Count",
                              MSE = FALSE,
                              CV = TRUE)
  
  result_dt <- emdi_dt$ind
  
  colnames(result_dt)[colnames(result_dt) %in% c("Head_Count", 
                                                 "Head_Count_CV")] <- 
    c("EBP_Head_Count", "EBP_Head_Count_CV")
  
  direct_dt <- direct_obj$ind[, c("Domain", "Head_Count", "Direct_Head_Count_CV")]
  
  colnames(direct_dt)[colnames(direct_dt) %in% c("Head_Count", 
                                                 "Direct_Head_Count_CV")] <- 
    c("Direct_Head_Count", "CB_Head_Count_CV")
  
  result_dt <- merge(result_dt, 
                     direct_dt,
                     on = "Domain")
  
  direct_dt <- directht_dt[, c("Domain", "CV")]
  
  direct_dt$CV <- direct_dt$CV / 100
  
  direct_dt$Domain <- as.factor(direct_dt$Domain)
  
  colnames(direct_dt)[colnames(direct_dt) %in% "CV"] <- "HT_Head_Count_CV"
  
  result_dt <- merge(result_dt,
                     direct_dt,
                     on = "Domain")
  
  direct_dt <- naivevar_dt$ind[, c("Domain", "deff_CV")]
  
  colnames(direct_dt)[colnames(direct_dt) %in% "deff_CV"] <- "DesignEffect_CV"
  
  result_dt <- merge(result_dt,
                     direct_dt,
                     on = "Domain")
  
  result_dt <- result_dt[,c("Domain", "Direct_Head_Count", "EBP_Head_Count", "HT_Head_Count_CV",
                            "CB_Head_Count_CV", "DesignEffect_CV", "EBP_Head_Count_CV")]
  
  
  return(result_dt)
  
}


create_calibmatrix <- function(x){
  
  
  unique_obs <- unique(x)
  
  result <- 
  lapply(unique_obs,
         function(y) {
           
           z <- as.integer(y == x)
           
           return(z)
           
         })
  
  result <- do.call(cbind, result)
  
  colnames(result) <- unique_obs
  
  return(result)

}










