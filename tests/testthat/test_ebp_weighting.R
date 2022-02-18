## test if the new ebp function works with the different weighting systems

# Load needed data
load("EBP/incomedata.RData")
load("EBP/incomedata_woTeruel.RData")
load("EBP/Xoutsamp_AuxVar.RData")


ebp_wgt1 <- ebp(fixed = income ~ educ1,
                pop_data = Xoutsamp_AuxVar,
                pop_domains = "domain",
                smp_data = incomedata, 
                smp_domains = "prov",
                L = 2, threshold = 10000,
                transformation = "log",
                weights = "weight",
                MSE = TRUE,
                B = 2) ##result looks right to me

ebp_wgt2 <- ebp(fixed = income ~ educ1,
                pop_data = Xoutsamp_AuxVar,
                pop_domains = "domain",
                smp_data = incomedata, 
                smp_domains = "prov",
                L = 2, threshold = 10000,
                transformation = "ordernorm",
                weights = "weight",
                MSE = TRUE,
                B = 2) ##result looks right to me as well

ebp_wgt1 <- ebp(fixed = income ~ educ1,
                pop_data = Xoutsamp_AuxVar,
                pop_domains = "domain",
                smp_data = incomedata, 
                smp_domains = "prov",
                L = 2, threshold = 10000,
                transformation = "log",
                weights = "weight") ##result looks right to me

### test weighting the populations as well with pop_weight == 1
Xoutsamp_AuxVar$population <- 1

ebp_wgt3 <- ebp(fixed = income ~ educ1,
                pop_data = Xoutsamp_AuxVar,
                pop_domains = "domain",
                smp_data = incomedata, 
                smp_domains = "prov",
                L = 2, 
                threshold = 10000,
                transformation = "ordernorm",
                weights = "weight",
                pop_weights = "population",
                MSE = TRUE,
                B = 2) ##not working here

ebp_wgt4 <- ebp(fixed = income ~ educ1,
                pop_data = Xoutsamp_AuxVar,
                pop_domains = "domain",
                smp_data = incomedata, 
                smp_domains = "prov",
                L = 2, threshold = 10000,
                transformation = "log",
                weights = "weight",
                pop_weights = "population") ##more outlandish results

##something is wrong with the weighting



