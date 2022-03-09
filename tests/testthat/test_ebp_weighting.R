## test if the new ebp function works with the different weighting systems

# Load needed data
load("EBP/incomedata.RData")
load("EBP/incomedata_woTeruel.RData")
load("EBP/Xoutsamp_AuxVar.RData")

##add fake population weights
Xoutsamp_AuxVar$population <- (99038.46/713301)*abs(rnorm(nrow(Xoutsamp_AuxVar)))


## testing the different weighting regimes
#### testing the application of both LME weights + parameter weighting + population weights (allwgtsystem)
ebp_wgt1 <- ebp(fixed = income ~ educ1,
                pop_data = Xoutsamp_AuxVar,
                pop_domains = "domain",
                smp_data = incomedata, 
                smp_domains = "prov",
                L = 2, threshold = 10000,
                transformation = "log",
                weights = "weight",
                pop_weights = "population",
                MSE = TRUE,
                B = 2,
                use_lmewgts = TRUE,
                use_emdiwgts = TRUE) ##result looks right to me

#### testing the application of LME weights + population weights - parameter weighting
ebp_wgt2 <- ebp(fixed = income ~ educ1,
                pop_data = Xoutsamp_AuxVar,
                pop_domains = "domain",
                smp_data = incomedata, 
                smp_domains = "prov",
                L = 2, threshold = 10000,
                transformation = "log",
                weights = "weight",
                pop_weights = "population",
                MSE = TRUE,
                B = 2,
                use_lmewgts = TRUE,
                use_emdiwgts = FALSE) ##result looks right to me

#### EMDI outta the box : parameter weighting (No LME weights, No population weights)
ebp_wgt3 <- emdi::ebp(fixed = income ~ educ1,
                      pop_data = Xoutsamp_AuxVar,
                      pop_domains = "domain",
                      smp_data = incomedata, 
                      smp_domains = "prov",
                      L = 2, threshold = 10000,
                      transformation = "log",
                      weights = "weight",
                      MSE = TRUE,
                      B = 2)


### no weights at all except the population weights
ebp_wgt4 <- ebp(fixed = income ~ educ1,
                pop_data = Xoutsamp_AuxVar,
                pop_domains = "domain",
                smp_data = incomedata, 
                smp_domains = "prov",
                L = 2, threshold = 10000,
                transformation = "log",
                weights = "weight",
                pop_weights = "population",
                MSE = TRUE,
                B = 2,
                use_lmewgts = FALSE,
                use_emdiwgts = FALSE) ##result looks right to me

### replicate EMDI results
Xoutsamp_AuxVar$population1 <- 1
ebp_wgt5 <- ebp(fixed = income ~ educ1,
                pop_data = Xoutsamp_AuxVar,
                pop_domains = "domain",
                smp_data = incomedata, 
                smp_domains = "prov",
                L = 2, threshold = 10000,
                transformation = "log",
                weights = "weight",
                pop_weights = "population1",
                MSE = TRUE,
                B = 2,
                use_lmewgts = FALSE,
                use_emdiwgts = TRUE) ##result looks right to me




