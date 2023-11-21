##Create Regression Tables##
rm(list = ls())
#load necessary packages
packages <- c("stargazer", "dplyr")
lapply(packages, require, character.only = TRUE)
rm(packages)


##create list to contain all the model fits##
modelFits <- vector(mode = "list", 4)
names(modelFits) <- c("main", "parent", "j1semp", "all")

for (i in 1:length(modelFits)){
  modelFits[[i]] <- vector(mode = "list", 3)
  names(modelFits[[i]]) <- c("emp", "semp", "vcov")
}

#load various model fits
modelFits[['main']][['emp']] <- readRDS("main_fit_emp.rds")
modelFits[['main']][['semp']] <- readRDS("main_fit_semp.rds")
modelFits[['parent']][['emp']] <- readRDS("parent_fit_emp.rds")
modelFits[['parent']][['semp']] <- readRDS("parent_fit_semp.rds")
modelFits[['j1semp']][['emp']] <- readRDS("j1semp_fit_emp.rds")
modelFits[['j1semp']][['semp']] <- readRDS("j1semp_fit_semp.rds")
modelFits[['all']][['emp']] <- readRDS("all_fit_emp.rds")
modelFits[['all']][['semp']] <- readRDS("all_fit_semp.rds")

#create a list of covariance matrices
for (i in 1:length(modelFits)){
  modelFits[[i]]$vcov <- vector(mode = "list", 3)
  names(modelFits[[i]][['vcov']]) <- c("probit", "heckit_emp", "heckit_semp")
}

#identify the index for specific subset of covariance matrix
find_vcov_index <- function(fit){
  vcov <- fit[['emp']]$vcov
  heckit_first_covariate <- which(rownames(vcov) %in% c("(Intercept)"))[2]
  index_list <- list(probit = 1:(heckit_first_covariate-1), 
                     heckit = heckit_first_covariate:(nrow(vcov)-2))
  return(index_list)
}

vcov_index <- lapply(modelFits, find_vcov_index)

#load the covariance matrix with the correct standard errors into the list
for (i in 1:length(modelFits)){
  probit_index_range <- vcov_index[[i]][['probit']]
  probit_subset <- modelFits[[i]][['emp']]$vcov[probit_index_range, probit_index_range]
  heckit_index_range <- vcov_index[[i]][['heckit']]
  heckit_emp_subset <- modelFits[[i]][['emp']]$vcov[heckit_index_range, heckit_index_range]
  heckit_semp_subset <- modelFits[[i]][['semp']]$vcov[heckit_index_range, heckit_index_range]
  
  modelFits[[i]][['vcov']][['probit']] <- probit_subset
  modelFits[[i]][['vcov']][['heckit_emp']] <- heckit_emp_subset
  modelFits[[i]][['vcov']][['heckit_semp']] <- heckit_semp_subset
}

#call the correct SE for the each of the fits
main_probit_SE <- sqrt(diag(modelFits[['main']][['vcov']][['probit']]))
main_emp_SE <- sqrt(diag(modelFits[['main']][['vcov']][['heckit_emp']]))
main_semp_SE <- sqrt(diag(modelFits[['main']][['vcov']][['heckit_semp']]))
parent_probit_SE <- sqrt(diag(modelFits[['parent']][['vcov']][['probit']]))
parent_emp_SE <- sqrt(diag(modelFits[['parent']][['vcov']][['heckit_emp']]))
parent_semp_SE <- sqrt(diag(modelFits[['parent']][['vcov']][['heckit_semp']]))
j1semp_probit_SE <- sqrt(diag(modelFits[['j1semp']][['vcov']][['probit']]))
j1semp_emp_SE <- sqrt(diag(modelFits[['j1semp']][['vcov']][['heckit_emp']]))
j1semp_semp_SE <- sqrt(diag(modelFits[['j1semp']][['vcov']][['heckit_semp']]))
all_probit_SE <- sqrt(diag(modelFits[['all']][['vcov']][['probit']]))
all_emp_SE <- sqrt(diag(modelFits[['all']][['vcov']][['heckit_emp']]))
all_semp_SE <- sqrt(diag(modelFits[['all']][['vcov']][['heckit_semp']]))



## assign the various arguments for the table of first stage results
#list of standard errors to input
SE_list <- list(main_probit_SE,
                parent_probit_SE,
                j1semp_probit_SE,
                all_probit_SE)

SE_list <- lapply(SE_list, unname)

#vector of dependent variable labels 
probit_dep_var_label <- c('Pr(self-employed)')

#specify which covariates to keep in the table
temp_lm_names <- names(modelFits$all$semp$probit$estimate)
probit_to_keep_vars <- c("XS(Intercept)",
                         "XSbritid", 
                         "XSfinnow",
                         "XSfinfut",
                         "XSjbsat",
                         "XSnchild_dv",
                         "XSdvage",
                         "XSage.sq",
                         "XSj1semp2")

probit_to_keep_index <- sapply(probit_to_keep_vars, match, table = temp_lm_names)

#specify the labels of the covariates that will be included
probit_covariate_label <- c("(Intercept)",
                            "British Identity",
                            "Present Fin. Status",
                            "Future Fin. Status",
                            "Job Satisfaction",
                            "Num. of Children", 
                            "Age",
                            "$Age^2$",
                            "First-Job Self-Emp.")

#specify the dummies to be omitted and add label for them
probit_to_omit_vars <- c("XSwave3", "XShiqual_dv2", "XSethn_dv2", "XSjbiindb_dv3", "XSgor_dv2", "maedqf")

probit_to_omit_label <- c("Time Fixed Effects?",
                          "Education Qual. Dummies?",
                          "Ethnicity Dummies?",
                          "Job Industry Dummies?",
                          "Region Dummies?",
                          "Parent Education Controls?")


#get the regression table output via stargazer
stargazer(modelFits$main$semp$probit,
          modelFits$parent$semp$probit,
          modelFits$j1semp$semp$probit,
          modelFits$all$semp$probit,
          column.sep.width = "1.5pt",
          covariate.labels = probit_covariate_label,
          dep.var.labels = probit_dep_var_label,
          dep.var.labels.include = TRUE,
          digits = 3,
          df = FALSE,
          font.size = "small",
          keep = probit_to_keep_index,
          omit = probit_to_omit_vars,
          omit.labels = probit_to_omit_label,
          model.numbers = TRUE)

#// NEED TO MANUALLY CHANGE THE PARENT CONTROL YES/NO VALUES DUE TO STARGAZER BUG //#

## assign the various arguments for the table of second stage results for employed
#list of standard errors to input
emp_SE_list <- list(main_emp_SE,
                    parent_emp_SE,
                    j1semp_emp_SE,
                    all_emp_SE)

emp_SE_list <- lapply(emp_SE_list, unname)

#vector of dependent variable labels 
emp_dep_var_label <- c("$IHS(y_{emp})$")


#specify which covariates to keep in the table
temp_lm_names <- names(modelFits$main$emp$lm$coefficients)
emp_to_keep_vars <- c("XO(Intercept)", 
                      "XOgeneration2",
                      "XOgeneration3",
                      "XOgeneration4",
                      "XOhealth2",
                      "XOsex_dv2",
                      "XOdvage",
                      "XOage.sq",
                      "XOjbft_dv2",
                      "imrData$IMR1")

emp_to_keep_index <- sapply(emp_to_keep_vars, match, table = temp_lm_names)


#specify the dummies to be omitted and add label for them
emp_to_omit_vars <- c("XOwave3", "XOhiqual_dv2", "XOethn_dv2", "XOjbiindb_dv3")

emp_to_omit_labels <- c("Time Fixed Effects?",
                        "Education Qual. Dummies?",
                        "Ethnicity Dummies?",
                        "Job Industry Dummies?")


#specify the labels of the covariates that will be included
emp_covariate_label <- c("(Intercept)",
                         "2nd Generation",
                         "3rd Generation",
                         "4th Generation",
                         "Healthy", 
                         "Female",
                         "Age",
                         "$Age^2$",
                         "Part-Time Worker",
                         "Inverse Mills Ratio")


#get the regression table output via stargazer
stargazer(modelFits$main$emp$lm,
          modelFits$parent$emp$lm,
          modelFits$j1semp$emp$lm,
          modelFits$all$emp$lm,
          se = emp_SE_list,
          column.sep.width = "1.5pt",
          covariate.labels = emp_covariate_label,
          dep.var.labels = emp_dep_var_label,
          dep.var.labels.include = TRUE,
          digits = 3,
          df = FALSE,
          font.size = "small",
          keep = emp_to_keep_index,
          omit = emp_to_omit_vars,
          omit.labels = emp_to_omit_labels,
          model.numbers = TRUE,
          omit.stat = c("f"))



## assign the various arguments for the table of second stage results for self-employed
#list of standard errors to input
semp_SE_list <- list(main_semp_SE,
                     parent_semp_SE,
                     j1semp_semp_SE,
                     all_semp_SE)

semp_SE_list <- lapply(semp_SE_list, unname)

#vector of dependent variable labels 
semp_dep_var_label <- c("$IHS(y_{semp})$")


#specify which covariates to keep in the table
temp_lm_names <- names(modelFits$main$semp$lm$coefficients)
semp_to_keep_vars <- c("XO(Intercept)", 
                       "XOgeneration2",
                       "XOgeneration3",
                       "XOgeneration4",
                       "XOhealth2",
                       "XOsex_dv2",
                       "XOdvage",
                       "XOage.sq",
                       "XOjbft_dv2",
                       "imrData$IMR1")

semp_to_keep_index <- sapply(semp_to_keep_vars, match, table = temp_lm_names)


#specify the dummies to be omitted and add label for them
semp_to_omit_vars <- c("XOwave3", "XOhiqual_dv2", "XOethn_dv2", "XOjbiindb_dv3")

semp_to_omit_labels <- c("Time Fixed Effects?",
                         "Education Qual. Dummies?",
                         "Ethnicity Dummies?",
                         "Job Industry Dummies?")


#specify the labels of the covariates that will be included
semp_covariate_label <- c("(Intercept)",
                          "2nd Generation",
                          "3rd Generation",
                          "4th Generation",
                          "Healthy", 
                          "Female",
                          "Age",
                          "$Age^2$",
                          "Part-Time Worker",
                          "Inverse Mills Ratio")


#get the regression table output via stargazer
stargazer(modelFits$main$semp$lm,
          modelFits$parent$semp$lm,
          modelFits$j1semp$semp$lm,
          modelFits$all$semp$lm,
          se = semp_SE_list,
          column.sep.width = "1.5pt",
          covariate.labels = semp_covariate_label,
          dep.var.labels = semp_dep_var_label,
          dep.var.labels.include = TRUE,
          digits = 3,
          df = FALSE,
          font.size = "small",
          keep = semp_to_keep_index,
          omit = semp_to_omit_vars,
          omit.labels = semp_to_omit_labels,
          model.numbers = TRUE,
          omit.stat = c("f"))


