## test if the new ebp function works with the different weighting systems

# Load needed data
load("EBP/incomedata.RData")
load("EBP/incomedata_woTeruel.RData")
load("EBP/Xoutsamp_AuxVar.RData")

##add fake population weights
Xoutsamp_AuxVar$population <- 99038.46/713301

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
                B = 2) ##result looks right to me


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
                only_lmewgts = TRUE) ##result looks right to me


