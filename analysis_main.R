##analysis##

#clear global environment
rm(list=ls())

#load necessary packages
packages <- c("sampleSelection","dplyr", "sjmisc")
lapply(packages, require, character.only = TRUE)
rm(packages)

#loading data
panel_df <- read.csv("panel_df.csv")
panel_df <- panel_df[, -which(names(panel_df) %in% c("X"))]

#name all the variables that are categorical
categorical_vars <- c("gor_dv",
                      "hiqual_dv",
                      "jbiindb_dv",
                      "generation",
                      "maedqf",
                      "paedqf",
                      "ethn_dv",
                      "sex_dv",
                      "health",
                      "ukborn",
                      "jbft_dv",
                      "j1semp",
                      "pidp",
                      "wave")

## first stage probit set-up ##
#convert all the categorical variables into factors for dummy variable creation  
panel_df[,categorical_vars] <- lapply(panel_df[,categorical_vars] , factor)

#assigning a vector of strings for the indep. variables used in first stage
probit_variables <- c("britid", 
                      "finnow",
                      "finfut",
                      "jbsat",
                      "gor_dv",
                      "nchild_dv",
                      "hiqual_dv",
                      "jbiindb_dv",
                      "ethn_dv",
                      "dvage",
                      "age.sq",
                      "wave")


#get forumla for first stage probit
f_semp <- as.formula(paste('jbsemp ~', paste(probit_variables, collapse='+')))
f_emp <- as.formula(paste('!(jbsemp) ~', paste(probit_variables, collapse='+')))


## set up for self-employed second stage model ##
heckit_model_variables <- c("generation", 
                            "health",
                            "jbiindb_dv",
                            # variable_indices$ukborn$index,
                            "hiqual_dv",
                            "jbft_dv",
                            "sex_dv",
                            "ethn_dv",
                            "dvage",
                            "age.sq",
                            "wave")


#assign the formula for the second stage model
f_heckit <- as.formula(paste('asinh(fimnlabgrs_dv) ~', paste(heckit_model_variables, collapse='+')))


##run main model##

waveFE_results_semp  <- heckit2fit(selection = f_semp, outcome = f_heckit,  data = panel_df)
waveFE_results_emp <- heckit2fit(selection = f_emp, outcome = f_heckit,  data = panel_df)


## run model with niche vars ##
parent_vars <- c("maedqf", "paedqf")
remaining_var <- c("j1semp") 

## run model with parent controls##
#subset for variables related to parents
panel_parent_df <-panel_df[, -(which(names(panel_df) %in% remaining_var))]
panel_parent_df <- na.omit(panel_parent_df)

#define model spec
probit_variables_parents <- c(probit_variables, parent_vars)
f_semp_parent <- as.formula(paste('jbsemp ~', paste(probit_variables_parents, collapse='+')))
f_emp_parent <- as.formula(paste('!(jbsemp) ~', paste(probit_variables_parents, collapse='+')))

#run model with variables related to parents
waveFE_results_semp_parent <- heckit2fit(selection = f_semp_parent, outcome = f_heckit,  data = panel_parent_df)
waveFE_results_emp_parent <- heckit2fit(selection = f_emp_parent, outcome = f_heckit,  data = panel_parent_df)

## run model with j1semp control##
#subset for 'j1semp'
panel_j1semp_df <-panel_df[, -(which(names(panel_df) %in% parent_vars))]
panel_j1semp_df <- na.omit(panel_j1semp_df)

#define model spec
probit_variables_j1semp <- c(probit_variables, remaining_var)
f_semp_j1semp <- as.formula(paste('jbsemp ~', paste(probit_variables_j1semp, collapse='+')))
f_emp_j1semp <- as.formula(paste('!(jbsemp) ~', paste(probit_variables_j1semp, collapse='+')))

#run model with 'j1semp'
waveFE_results_semp_j1semp <- heckit2fit(selection = f_semp_j1semp, outcome = f_heckit,  data = panel_j1semp_df)
waveFE_results_emp_j1semp <- heckit2fit(selection = f_emp_j1semp, outcome = f_heckit,  data = panel_j1semp_df)

## run model with all control##
#subset for all niche variables
panel_all_df <- na.omit(panel_df)

#define model spec
probit_variables_all <- c(probit_variables,parent_vars ,remaining_var)
f_semp_all <- as.formula(paste('jbsemp ~', paste(probit_variables_all, collapse='+')))
f_emp_all <- as.formula(paste('!(jbsemp) ~', paste(probit_variables_all, collapse='+')))

#run model with all niche variables
waveFE_results_semp_all <- heckit2fit(selection = f_semp_all, outcome = f_heckit,  data = panel_all_df)
waveFE_results_emp_all <- heckit2fit(selection = f_emp_all, outcome = f_heckit,  data = panel_all_df)

##save the fitted models as .rds files##
saveRDS(waveFE_results_emp, "main_fit_emp.rds")
saveRDS(waveFE_results_semp, "main_fit_semp.rds")
saveRDS(waveFE_results_emp_parent, "parent_fit_emp.rds")
saveRDS(waveFE_results_semp_parent, "parent_fit_semp.rds")
saveRDS(waveFE_results_emp_j1semp, "j1semp_fit_emp.rds")
saveRDS(waveFE_results_semp_j1semp, "j1semp_fit_semp.rds")
saveRDS(waveFE_results_emp_all, "all_fit_emp.rds")
saveRDS(waveFE_results_semp_all, "all_fit_semp.rds")
