#### -------------------------- prepost-estimation  internals -------------------------------- ####

compute_gini <- function(welfare) {
  
  
  welfare <- welfare[1:length(welfare)/2]
  weight <- welfare[(length(welfare)/2) + 1 : length(welfare)]
  
  if (is.null(weight)){
    weight <- rep(1, length(welfare))
  }
  
  df <- data.frame(welfare = welfare,
                   weight = weight)
  
  df <- df[complete.cases(df),]
  
  welfare <- df$welfare
  weight <- df$weight
  
  weight <- weight/sum(weight)
  
  order <- order(welfare)
  welfare <- welfare[order]
  weight <- weight[order]
  p <- cumsum(weight)
  nu <- cumsum(weight * welfare)
  n <- length(nu)
  nu <- nu/nu[n]
  gini <- sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])
  
  return(gini)  
}

compute_gap <- function(welfare, threshold){
  
  welfare <- welfare[1:length(welfare)/2]
  weight <- welfare[(length(welfare)/2) + 1 : length(welfare)]
  
  df <- data.frame(welfare = welfare,
                   weight = weight)
  
  df <- df[complete.cases(df),]
  
  welfare <- df$welfare
  weight <- df$weight
  
  
  pov_status <- (welfare < threshold)
  
  relative_distance <- (1 - (welfare[pov_status] / threshold))
  
  weight_pov <- weight[pov_status]
  
  weight_total <- sum(weight)
  
  fgt1 <- sum(relative_distance * weight_pov) / weight_total
  
  return(fgt1)
  
}

compute_headcount <- function(welfare, threshold){
  
  welfare <- welfare[1:length(welfare)/2]
  weight <- welfare[(length(welfare)/2) + 1 : length(welfare)]
  
  df <- data.frame(welfare = welfare,
                   weight = weight)
  
  df <- df[complete.cases(df),]
  
  welfare <- df$welfare
  weight <- df$weight
  
  pov_status <- as.integer(welfare < threshold)
  
  fgt0 <- sum(pov_status * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE)
  
  return(fgt0)
  
}

present_modelselect <- function(dt,
                                y,
                                xvars,
                                weights,
                                cluster_id){
  
  dt <- as.data.table(dt)
  
  if(is.null(weights) == FALSE){
    
    dt <- na.omit(dt[,c(y, xvars, weights, cluster_id), with = F])
    
    weights <- dt[, weights, with = F]
    
    #weights <- scale(weights)
    
  } else {
    
    dt <- na.omit(dt[,c(y, xvars, cluster_id), with = F])
    
    weights <- 1
    
  }
  
  xset <- dt[, xvars, with = F]
  
  y <- dt[, y, with = F]
  
  
  
  
  
}