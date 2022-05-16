# Internal documentation -------------------------------------------------------

# The function notation defines the notational framework for the EBP approach
# e.g. number of households in population or sample (per domain), distinction
# between in-sample and out-of-sample
# see Molina and Rao (2003) p.370-371


framework_ebp <- function(fixed, pop_data, pop_domains, smp_data, smp_domains, 
                          threshold, custom_indicator = NULL, na.rm, weights = NULL,
                          pop_weights = NULL, rescale_weights, rescale_popweights,
                          use_lmewgts, use_emdiwgts, lmecontrol_maxiter,
                          lmecontrol_option) {

  # Reduction of number of variables
  mod_vars <- all.vars(fixed)
  mod_vars <- mod_vars[mod_vars != as.character(fixed[2])]
  smp_vars <- c(as.character(fixed[2]), mod_vars, smp_domains, weights)
  pop_vars <- c(mod_vars, pop_domains, pop_weights)
  smp_data <- smp_data[, smp_vars]
  weights  <- weights
  pop_weights <- pop_weights
  fw_check1(pop_data = pop_data, mod_vars = mod_vars, pop_domains = pop_domains, 
            smp_data = smp_data, fixed = fixed, smp_domains = smp_domains, 
            threshold = threshold, weights = weights)
 

  pop_data <- pop_data[, pop_vars]
  
  lmecontrol_maxiter <- lmecontrol_maxiter
  lmecontrol_option  <- lmecontrol_option

  # Deletion of NA
  if (na.rm == TRUE) {
    pop_data <- na.omit(pop_data)
    smp_data <- na.omit(smp_data)
  } else if (any(is.na(pop_data)) || any(is.na(smp_data))){
    stop('EBP does not work with missing values. Set na.rm = TRUE in function 
          ebp.')
  }
  

  if (is.null(pop_weights) == FALSE & is.null(weights) == TRUE){
    
    message('Missing sample weights, vector of 1s created')
    pop_data$weights <- rep(1, nrow(smp_data))
    
  }
  
  # Rescale the weights
  if(rescale_weights == TRUE){
    
    smp_data[,weights] <- scaler(smp_data[,weights])
    
  }
  
  if(rescale_popweights == TRUE){
    
    pop_data[,pop_weights] <- scaler(pop_data[,pop_weights])
  }
  
  # Order of domains
  pop_data <- pop_data[order(pop_data[[pop_domains]]),]
  pop_data[[pop_domains]] <- factor(pop_data[[pop_domains]], 
                                    levels = unique(pop_data[[pop_domains]]))
  pop_domains_vec <- pop_data[[pop_domains]]

  smp_data <- smp_data[order(smp_data[[smp_domains]]),]
  smp_data[[smp_domains]] <- factor(smp_data[[smp_domains]], 
                                    levels = unique(pop_data[[pop_domains]]))
  smp_domains_vec <- smp_data[[smp_domains]]
  smp_domains_vec <- droplevels(smp_domains_vec)
  
  
  fw_check2(pop_domains = pop_domains, pop_domains_vec = pop_domains_vec, 
            smp_domains = smp_domains, smp_domains_vec = smp_domains_vec)


  # Number of households in population
  N_pop <- length(pop_domains_vec)
  # Number of households in sample
  N_smp <- length(smp_domains_vec)
  # Number of out-of-sample households
  N_unobs <- N_pop - N_smp
  # Number of domains in the population
  N_dom_pop <- length(unique(pop_domains_vec))
  # Number of domains in the sample
  N_dom_smp <- length(unique(smp_domains_vec))
  # Number of out-of-sample domains
  N_dom_unobs <- N_dom_pop - N_dom_smp
  # Number of households in population per domain
  n_pop <- as.vector(table(pop_domains_vec))
  # Number of households in sample per domain
  smp_domains_vec_tmp <- as.numeric(smp_domains_vec)
  n_smp <- as.vector(table(smp_domains_vec_tmp))
  
  # Indicator variables that indicate if domain is in- or out-of-sample
  obs_dom <- pop_domains_vec %in% unique(smp_domains_vec)
  dist_obs_dom <- unique(pop_domains_vec) %in% unique(smp_domains_vec)
  
  fw_check3(obs_dom = obs_dom, dist_obs_dom = dist_obs_dom, pop_domains = pop_domains, 
            smp_domains = smp_domains)

  indicator_list <- list(
    fast_mean = function(y, threshold) {t(weighted.mean(y[1:(length(y)/2)],
                                                        y[(1+length(y)/2):length(y)]))},
    hcr = function(y, threshold) {t(weighted.mean(y[1:(length(y)/2)] < threshold,
                                                  y[(1+length(y)/2):length(y)]))},
    pgap = function(y, threshold) {
      # t(mean((y < threshold) * (threshold - y) / threshold))
      dt <- data.frame(x = y[1:(length(y)/2)],
                       w = y[(1+length(y)/2):length(y)])
      dt$poor <- (dt$x < threshold)
      weight_pov <- dt$w[dt$poor]
      weight_total <- sum(dt$w)
      d <- (1 - (dt$x[dt$poor] / threshold))
      fgt1 <- sum(d * weight_pov) / weight_total
      return(fgt1)
      },
    gini = function(y, threshold) {
      
      n <- length(y[1:(length(y)/2)]) 
      dt <- data.frame(x = y[1:(length(y)/2)],
                       w = y[(1+length(y)/2):length(y)])
      dt <- dt[order(dt$x),]
      # Compute weighted welfare
      y <- dt$x * dt$w
      y_lag <- collapse::flag(y, fill = 0)
      
      # Compute area under the curve using
      # Area of trapezoid = Base * Average height
      v <- (cumsum(y_lag) + (y / 2)) * dt$w
      auc <- sum(v) # Area Under the Curve
      
      # Compute Area Under the Lorenz Curve
      # Normalize auc so it is always between 0 and 0.5
      auc <- (auc / sum(dt$w)) / sum(y)
      
      # Compute Gini
      gini <- 1 - (2 * auc)
      
      return(gini)
    }
    ,
    qsr = function(y, threshold) {   
      weights <- y[(1+length(y)/2):length(y)]
      quant14 <- wtd.quantile(x = y[1:(length(y)/2)], 
                              weights = weights,
                              probs = c(0.2, 0.8))
    
      iq1 <- y[1:(length(y)/2)] <= quant14[1]
      iq4 <- y[1:(length(y)/2)] > quant14[2]
      t((sum(weights[iq4] * y[iq4])/sum(weights[iq4]))/
          (sum(weights[iq1] * y[iq1])/sum(weights[iq1])))
      },
    quants = function(y, threshold) {t(quantile(y[1:(length(y)/2)], probs = c(.10,.25, .5, .75, .9)))}
  )
  
  indicator_names <- c("Mean",
                      "Head_Count",
                      "Poverty_Gap",
                      "Gini",
                      "Quintile_Share",
                      "Quantile_10",
                      "Quantile_25",
                      "Median",
                      "Quantile_75",
                      "Quantile_90"
                      )


  if(!is.null(custom_indicator) && length(custom_indicator) > 0){
    indicator_list <- c(indicator_list, custom_indicator)
    indicator_names <- c(indicator_names, names(custom_indicator))
  }

  if (is.null(threshold)) {
    threshold <- 0.6 * median(smp_data[[paste(fixed[2])]])
    message("The threshold for the HCR and the PG is automatically set to 60% of 
          the median of the dependent variable and equals",threshold, "\n")
  }
  
  

  return(list(pop_data           = pop_data,
              pop_domains_vec    = pop_domains_vec,
              smp_data           = smp_data,
              smp_domains_vec    = smp_domains_vec,
              smp_domains        = smp_domains,
              N_pop              = N_pop,
              N_smp              = N_smp,
              N_unobs            = N_unobs,
              N_dom_pop          = N_dom_pop,
              N_dom_smp          = N_dom_smp,
              N_dom_unobs        = N_dom_unobs,
              n_pop              = n_pop,
              n_smp              = n_smp,
              obs_dom            = obs_dom,
              dist_obs_dom       = dist_obs_dom,
              indicator_list     = indicator_list,
              indicator_names    = indicator_names,
              threshold          = threshold,
              weights            = weights,
              pop_weights        = pop_weights,
              use_lmewgts        = use_lmewgts,
              use_emdiwgts       = use_emdiwgts,
              lmecontrol_maxiter = lmecontrol_maxiter,
              lmecontrol_option  = lmecontrol_option)
         )
}



