* stata wrapper for EMDIplus 
* depends on package rcall 
cap program drop Remdiplus 
program define Remdiplus 
syntax namelist, smp_data(string) pop_data(string) weights(string) pop_weight(string) smp_domains(string) pop_domains(string) threshold(real)  [l(int 100) b(int 100) mse(string) transformation(string) cpus(int 1) seed(int 1234) savexls(string) saveobject(string) interval(string) scale_bootvar(string)] 

* set defaults 
if "`MSE'"=="" {
local MSE="TRUE"
}
if "`transformation'"=="" {
local transformation "no"
}
if "`savexls'"=="" {
local saving "EMDI_results.xlsx"
}
if "`saveobject'"=="" {
local saveobject "EMDI"
}
if "`scale_bootvar'"=="" {
local scale_bootvar "NULL"
}



local pop_data : subinstr local pop_data "\" "/", all 
local smp_data : subinstr local smp_data "\" "/", all 
local savexls : subinstr local savexls "\" "/", all 
local saveobject : subinstr local saveobject "\" "/", all 

preserve
use `smp_data', replace 
* output model to `modelfile', a textfile R can read 
tempname modelfile  
gettoken yvar xvars : namelist   
local formula "`yvar' ~ "
foreach var of varlist `xvars' {
local formula "`formula' + `var'"
}
local formula : subinstr local formula "+ " ""
dis "`formula'"

cap file close R 
file open R using "`modelfile'.txt", write replace 
file write R "`formula'" _n
file close R
local working_dir : pwd
local working_dir : subinstr local working_dir "\" "/", all
restore 


rcall debug: library("emdiplus"); ///
	library("haven"); ///
	library("openxlsx"); ///
	library("nlme"); ///
	library("R.utils"); ///
	povline <- `threshold'; ///
	setwd("`working_dir'"); ///
	pop <- as.data.frame(read_dta("`pop_data'")); ///
	smp <- as.data.frame(read_dta("`smp_data'")); ///
	model <- read.delim("`modelfile'.txt", header = FALSE, sep = "\t"); ///
	model <- as.formula(as.character(model[1,1])); ///
	ebp_results <- ebp(fixed = model,pop_data = pop, ///
            pop_domains = "`pop_domains'", smp_data = smp, smp_domains = "`smp_domains'", ///
            threshold = `threshold', L = `l', B = `b', MSE = `mse', transformation = "`transformation'", ///
            na.rm = TRUE, cpus = `cpus', seed=`seed', weights = "`weights'", pop_weights = "`pop_weight'"); ///
	write.excel(ebp_results, file = "`savexls'", indicator = "Poverty", ///
            MSE = `mse', CV = `mse', split = FALSE); ///
	save(ebp_results,file="`saveobject'")
	
end
  