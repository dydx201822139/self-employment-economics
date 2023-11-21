##panel data preparation##

#clear global environment
rm(list=ls())

#load necessary packages
packages <- c("dplyr", "sjmisc")
lapply(packages, require, character.only = TRUE)
rm(packages)

#loading data
pre_panel_df <- read.csv("pre_panel_df.csv")
pre_panel_df <- pre_panel_df[, -which(names(pre_panel_df) %in% c("X"))]

#rename wave dummies
pre_panel_df$wave[pre_panel_df$wave == 3] <- 6
pre_panel_df$wave[pre_panel_df$wave == 2] <- 3

#register as panel data
# panel_df <- pdata.frame(pre_panel_df,index = c("pidp", "wave"))
panel_df <- pre_panel_df

#change jbsemp values to 0 and 1
panel_df$jbsemp <- ifelse(panel_df$jbsemp == 2, TRUE, FALSE)

#remove resp. who are ambiguous in their lineage/migration background
panel_df <- panel_df[panel_df$generation != 6 & panel_df$generation != 5,]

#remove resp. who are UK born to avoid multicolinearity issue for ukborn variable
panel_df[panel_df$ukborn != 5,]$ukborn <- TRUE
panel_df[panel_df$ukborn ==5,]$ukborn <- FALSE

#include a second order polynomial term for age
panel_df$age.sq <- (panel_df$dvage)**2

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

#remove levels from the factor jbiindb_dv which would become a constant after subsetting for employment status
tally <- table(panel_df[panel_df$jbsemp == 1,]$jbiindb_dv)

to_remove <- c()
for (i in 1:length(tally)){
  if(tally[[i]] == 0){
    to_remove <- c(to_remove, names(tally)[i])
  }
}
parent_vars <- c("maedqf", "paedqf")
j1semp_df <- panel_df[, -(which(names(panel_df) %in% parent_vars))]
j1semp_df <- na.omit(j1semp_df)
tally <- table(j1semp_df[j1semp_df$jbsemp == 1,]$jbiindb_dv)
for (i in 1:length(tally)){
  if(tally[[i]] == 0){
    to_remove <- c(to_remove, names(tally)[i])
  }
}
parent_df <- panel_df[, -(which(names(panel_df) %in% c("j1semp")))]
parent_df <- na.omit(parent_df)
tally <- table(parent_df[parent_df$jbsemp == 1,]$jbiindb_dv)
for (i in 1:length(tally)){
  if(tally[[i]] == 0){
    to_remove <- c(to_remove, names(tally)[i])
  }
}
all_df <- na.omit(parent_df)
tally <- table(all_df[all_df$jbsemp == 1,]$jbiindb_dv)
for (i in 1:length(tally)){
  if(tally[[i]] == 0){
    to_remove <- c(to_remove, names(tally)[i])
  }
}
to_remove <- as.integer(to_remove)


#subset the levels we want to remove from the dataframe
panel_df <- filter(panel_df, !(jbiindb_dv %in% to_remove))

write.csv(panel_df,"panel_df.csv")
