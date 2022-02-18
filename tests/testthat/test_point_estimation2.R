# Test if point estimation runs on its own and returns the same values
# Testing to see if the new changes will also work for both old systems transformations and the new ones like
# arcsin and backtransformation


# Load needed data
load("EBP/incomedata.RData")
load("EBP/incomedata_woTeruel.RData")
load("EBP/Xoutsamp_AuxVar.RData")

## generate fake smp and pop weights
set.seed(100)
Xoutsamp_AuxVar$population <- 10*abs(rnorm(nrow(Xoutsamp_AuxVar)))

## test that including sampling and population weighting works
test_that("Does monte_carlo function give benchmark results?", {
  suppressWarnings(RNGversion("3.5.0"))
  # Single elements needed in monte_carlo()
  framework <- framework_ebp(income ~ educ1,
                             Xoutsamp_AuxVar, 
                             "provlab", 
                             incomedata,
                             "provlab",
                             4282.081,
                             custom_indicator = NULL,
                             na.rm = TRUE,
                             weights = "weight",
                             pop_weights = "population")
  # Fixed optimal parameter and shift (benchmark values)
  ebp_optpar_bc <- read.csv2("EBP/ebp_optpar_bc.csv", sep = ",", 
                             stringsAsFactors = TRUE)  
  ebp_shift_bc  <- read.csv2("EBP/ebp_shift_bc.csv", sep = ",", 
                             stringsAsFactors = TRUE)
  
  lambda <- as.numeric(as.character(ebp_optpar_bc[,"Optpar"]))
  shift  <- as.numeric(as.character(ebp_shift_bc))
  
  # Conduct transformation using the optimal parameter
  transformation_par <- data_transformation(fixed          = income ~ educ1,
                                            smp_data       = framework$smp_data,
                                            transformation = "box.cox",
                                            lambda         = lambda
  )
  
  # Conduct regression using transformed data
  # mixed_model <- lme(fixed  = income~educ1,
  #                    data   = transformation_par$transformed_data ,
  #                    random = as.formula(paste0("~ 1 | as.factor(", framework$smp_domains, ")")),
  #                    method = "REML")
  mixed_model <- lme(fixed  = income~educ1, 
                     data   = transformation_par$transformed_data ,
                     random = as.formula(paste0("~ 1 | as.factor(", 
                                                framework$smp_domains, ")")),
                     method = "ML",
                     control = nlme::lmeControl(maxiter=100,opt="nlminb"),
                     weights = varComb(varIdent(as.formula(paste0("~ 1 | as.factor(", 
                                                                  framework$smp_domains, ")"))),
                                       varFixed(as.formula(paste0("~1/", 
                                                                  framework$weights)))))
  # Get model parameter
  est_par <- model_par(mixed_model        = mixed_model,
                       framework          = framework,
                       transformation_par = transformation_par,
                       fixed              = income~educ1
  )
  
  # Get parameter for the generating model
  gen_par <- gen_model(model_par   = est_par,
                       fixed       = income~educ1,
                       framework   = framework
  )
  
  
  
  set.seed(100) 
  point <- monte_carlo(transformation = "box.cox",
                       L = 2,
                       framework = framework,
                       lambda = lambda,
                       shift = shift,
                       model_par = est_par,
                       gen_model = gen_par
  )
  
  # Load benchmark point estimates
  ebp_point_bc <- read.csv2("EBP/ebp_point_bc.csv", sep = ",", 
                            stringsAsFactors = TRUE)
  
  # # compare 10% quantile
  # expect_equal(point[,"Quantile_10"],
  #              as.numeric(as.character(ebp_point_bc[,"quant10"])))
  # # compare HCR
  # expect_equal(point[,"Head_Count"],
  #              as.numeric(as.character(ebp_point_bc[,"hcr"])),
  #              tolerance = 2e-1)
  
  
})

