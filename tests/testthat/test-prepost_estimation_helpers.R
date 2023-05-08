# #### set up to test the functions by running the models
# variables <- c("eqsize", "cash", "self_empl",
#                "unempl_ben", "age_ben", "surv_ben",
#                "sick_ben", "dis_ben", "rent", "fam_allow",
#                "house_allow", "cap_inv", "tax_adj")
# 
# 
# #### check that result values make sense
# 
# 
# emdi_model <- emdiplus::ebp(fixed = as.formula(paste("eqIncome ~ ", paste(variables, 
#                                                                 collapse= "+"))),
#                             pop_data = eusilcA_pop2, 
#                             pop_domains = "district", 
#                             smp_data = eusilcA_smp2, 
#                             smp_domains = "district",
#                             na.rm = TRUE,
#                             weights = "weight",
#                             pop_weights = "popweights",
#                             MSE = TRUE,
#                             threshold = 11000,
#                             B = 2,
#                             L = 2)
# 
#   report_list <-
#     ebp_reportdescriptives(ebp_object = emdi_model,
#                            smp_weights = "weight",
#                            pop_weights = "popweights",
#                            repvar = "state",
#                            welfare = "eqIncome",
#                            smp_data = eusilcA_smp2,
#                            pop_data = eusilcA_pop2,
#                            threshold = 11000,
#                            pop_domains = "district",
#                            smp_domains = "district")
# 
# 
# 
# result2 <- 
# ebp_test_means(varlist = variables,
#                smp_weights = "weight",
#                pop_weights = "popweights",
#                smp_data = eusilcA_smp2,
#                pop_data = eusilcA_pop2)
# 
# result3 <- 
# ebp_reportcoef_table(emdi_model, 4)
# 
# ### top 10 highest rate below threshold by rank (in ascending order)
# result4 <- 
# ebp_report_byrank(ebp_object = emdi_model,
#                  pop_data = eusilcA_pop2,
#                  pop_domains = "district",
#                  pop_weights = "popweights",
#                  number_to_list = 10,
#                  head = FALSE)
# 
# ### full data of highest rate below threshold by rank (descending order)
# result5 <-
# ebp_report_byrank(ebp_object = emdi_model,
#                  pop_data = eusilcA_pop2,
#                  pop_domains = "district",
#                  pop_weights = "popweights",
#                  byrank_indicator = "rate")
# 
# result6 <-
# ebp_report_byrank(ebp_object = emdi_model,
#                   pop_data = eusilcA_pop2,
#                   pop_domains = "district",
#                   pop_weights = "popweights")
# 
# result7 <- 
# ebp_compute_cv(ebp_object = emdi_model,
#                smp_data = eusilcA_smp2,
#                welfare = "eqIncome",
#                calibvar = "state",
#                domainvar = "district",
#                boot_type = "calibrate",
#                designvar = NULL,
#                threshold = 11000,
#                smp_weights = "weight")
# 
# result8 <- 
# aggregate_saedirect(ebp_object = emdi_model,
#                     smp_data = eusilcA_smp2,
#                     pop_data = eusilcA_pop2, 
#                     welfare = "eqIncome",
#                     smp_domains = "district",
#                     pop_domains = "district",
#                     pop_weights = "popweights",
#                     pop_regions = "state",
#                     smp_weights = "weight",
#                     threshold = 11000,
#                     indicator = "Head_Count")
# 


  
 




































