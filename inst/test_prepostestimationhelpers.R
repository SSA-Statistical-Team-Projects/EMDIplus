
data("eusilcA_pop2")
data("eusilcA_smp2")
#### set of variables used in model estimation
variables <- c("gender", "eqsize", "cash", "self_empl",
              "unempl_ben", "age_ben", "surv_ben",
              "sick_ben", "dis_ben", "rent", "fam_allow",
              "house_allow", "cap_inv", "tax_adj")

### estimate a unit model
emdi_model <- emdiplus::ebp(fixed = as.formula(paste("eqIncome ~ ", paste(variables,
                                                                         collapse= "+"))),
                           pop_data = eusilcA_pop2,
                           pop_domains = "district",
                           smp_data = eusilcA_smp2,
                           smp_domains = "district",
                           na.rm = TRUE,
                           weights = "weight",
                           pop_weights = "popweights",
                           MSE = TRUE,
                           threshold = 11000,
                           B = 2,
                           L = 2)

### model estimation
ebp_reportdescriptives(ebp_object = emdi_model,
                      smp_weights = "weight",
                      pop_weights = "popweights",
                      repvar = "state",
                      welfare = "eqIncome",
                      smp_data = eusilcA_smp2,
                      pop_data = eusilcA_pop2,
                      threshold = 11000,
                      pop_domains = "district",
                      smp_domains = "district")
