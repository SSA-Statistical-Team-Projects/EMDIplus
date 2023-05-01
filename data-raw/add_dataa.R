# usethis::use_data(eusilcA_smp, overwrite = TRUE)
# usethis::use_data(eusilcA_pop, overwrite = TRUE)
# 

### include a district code and region code to the data
N <- length(unique(eusilcA_pop$state))

## starting with state
area_df <- data.frame(state = unique(eusilcA_pop$state),
                      state_code = paste0(sprintf(paste0("%0", 
                                                         nchar(as.character(N)), 
                                                         "d"),
                                                  1:N)))

## then district
area_df <- merge(x = unique(eusilcA_pop[,c("state", "district")]),
                 y = area_df)

N <- length(unique(eusilcA_pop$district))
district_code <- tapply(area_df$district,
                        area_df$state,
                        FUN = function(xx) {
                                  
                        y <- paste0(sprintf(paste0("%0", 
                                            nchar(as.character(N)), 
                                            "d"),
                                    1:length(xx)))
                                  
                                })

area_df$district_code <- paste0(area_df$state_code, unlist(district_code))


eusilcA_pop2 <- merge(x = eusilcA_pop, y = area_df)

eusilcA_smp2 <- merge(x = eusilcA_smp, y = area_df)

eusilcA_pop2$state_code <- as.integer(eusilcA_pop2$state_code)
eusilcA_pop2$district_code <- as.integer(eusilcA_pop2$district_code)


#### include household and population weights
set.seed(123)

assign_weights <- function(vector,
                           popsize){
  
  set.seed(123)
  
  n <- length(vector)
  
  rand_nums <- runif(n - 1, 0, popsize)
  
  rand_nums <- sort(rand_nums)
  
  diffs <- c(rand_nums[1], diff(rand_nums), popsize - rand_nums[n])
  
  vec <- sample(diffs, n)
  
  vec <- c(vec[is.na(vec) == FALSE],
           popsize - sum(vec, na.rm = TRUE))
  
  return(vec)
  
  
  
}

eusilcA_pop2$popweights <- assign_weights(vector = eusilcA_pop2$district,
                                           popsize = 8.956*10^6)


area_pops <- tapply(X = eusilcA_pop2$popweights,
                    INDEX = eusilcA_pop2$state,
                    FUN = sum,
                    na.rm = TRUE)

eusilcA_smp2$weight <- 
unlist(mapply(FUN = assign_weights,
              vector = split(eusilcA_smp2$eqIncome, 
                             eusilcA_smp2$state),
              popsize = area_pops))




usethis::use_data(eusilcA_pop2, overwrite = TRUE)
usethis::use_data(eusilcA_smp2, overwrite = TRUE)

