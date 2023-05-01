### read in the zanzibar pop and smp data

pop <- readRDS("tests/testthat/EBP/pop.RDS")
smp <- readRDS("tests/testthat/EBP/smp.RDS")

g_list <- colnames(smp)[grepl("ntl_", colnames(smp)) | 
                        grepl("cp_", colnames(smp))|
                        grepl("bu_", colnames(smp))|
                        grepl("inf_", colnames(smp))|
                        grepl("region_", colnames(smp))]

x <- as.matrix(smp[,g_list])
y <- as.matrix(smp$AECONS)
#Ify - please see if glmnet specification should be improved
glmnet1<-glmnet::cv.glmnet(x=x,y=y,weights=smp$wt,type.measure='mse',nfolds=10,alpha=1) #LASSO regression
c<-coef(glmnet1,s='lambda.min',exact=TRUE)
inds<-which(as.matrix(c!=0))
variables<-row.names(c)[inds]
`%notin%` <- Negate(`%in%`)
variables<-variables[variables %notin% '(Intercept)']
coef(glmnet1, s = "lambda.min")
x <- smp[,c(variables)]
ols <-  lm(y ~., data = x, weights=smp$wt)
r2 <- summary(ols)$adj.r.squared #save r-squared to get a sense of a model fit
#now save formula which can be used later
fmla <- as.formula(paste("AECONS ~ ", paste(variables, collapse= "+")))

### add district names to the smp and pop data real quick

smp$DNAMES <- paste0("district", smp$DISTRICT)
pop$DNAMES <- paste0("district", pop$DISTRICT)

emdi_model <- ebp(fixed = fmla, 
                  pop_data = as.data.frame(pop),
                  pop_domains = "DISTRICT", 
                  smp_data = as.data.frame(smp), 
                  smp_domains = "DISTRICT",
                  threshold = 66312.63,
                  transformation = "log",
                  na.rm = TRUE,
                  MSE = TRUE,
                  weights = "weight",
                  pop_weights = "popweight",
                  L = 10,
                  B = 10)


test_cv <- ebp_compute_cv(ebp_obj = emdi_model,
                          welfare = "AECONS",
                          domainvar = "DISTRICT",
                          smp_weights = "weight",
                          calibvar = "REGION",
                          threshold = 66312.63,
                          smp_data = as.data.frame(smp),
                          boot_type = "naive")


aggregate_saedirect(ebp_object = emdi_model,
                    smp_data = as.data.frame(smp),
                    pop_data = as.data.frame(pop),
                    pop_domains = "DISTRICT",
                    pop_weights = "weight",
                    regionvar = "REGION",
                    indicator = "Head_Count")
























