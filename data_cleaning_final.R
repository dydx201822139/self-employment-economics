##Data Cleaning##

#load necessary packages
packages <- c("foreign", "haven", "sjlabelled", "sjmisc", "ramify", "dplyr")
lapply(packages, require, character.only = TRUE)
rm(packages)

#load functions
source("functions.R")

#load the necessary datasets from .dta files
ukhls_waves <- c("ukhls statfiles/ukhls_w1.dta","ukhls statfiles/ukhls_w3.dta","ukhls statfiles/ukhls_w6.dta")
df <- lapply(ukhls_waves, read_dta)
rm(ukhls_waves)

#load cross-wave variables dataset
cross_wave <- read_dta("ukhls statfiles/xwavedat.dta")

#load in the names of the variables we want from .csv file
variables <- read.csv("essentials.csv")


#extract the vector variables that we want to include
wanted_var <- c(variables$essentials,variables$niche) #combining variables into one vector
temp <- length(variables$niche[variables$niche==""]) #finding the number of empty cells
wanted_var <- head(wanted_var, -temp) #deleting ""


#subset dataset to include only the wanted variables using custom functions
indices_list <- lapply(df, get_indices, to_compare = wanted_var)



#save data into a new list
final_df <- vector("list", length = length(df))
for(k in 1:length(df)){
  final_df[[k]] <- df[[k]][,indices_list[[k]]] 
}

#unify all the variable names except for pidp using custom functions
final_df <- lapply(final_df, unify_names)

#add a new categorical variable to identify which wave obs. was made
for(i in 1:length(final_df)){
  new_col <- rep(i,dim(final_df[[i]])[1])
  final_df[[i]]$wave <- new_col
}


#finding intersection of variables among the dataset of every wave
temp_list <- vector(mode = "list",length = length(final_df))
for(i in 1:length(final_df)){
  temp_list[[i]] <- names(final_df[[i]])
}
common_var <- Reduce(intersect,temp_list)

#concatenation using 'bind_rows' from dplyr
agg_df <- bind_rows(lapply(final_df,remove_all_labels)) #removing all the labels on the variables
agg_df <- agg_df[common_var] #subsetting the dataframe by common variables

#removing redundant vars we don't need
removed_var<-c("ppsex",
               "pn1sex",
               "pn2sex",
               "pns1sex",
               "pns2sex",
               #"pjulk4wk",
               #"dcsex",
               #"whorusex",
               "sex",
               #"nch14resp",
               #"nxtjbhrs",
               #"pdvage",
               #"jbsempchk",
               #"racel_dv",
               #"vote3",
               "dvage2uk",
               "yr2uk4"
               #"ff_jbsemp",
               #"jbstat",
               #"qualoc",
               #"ff_jbstat",
               #"qfhigh_dv",
               #"nqfhigh_dv",
               #"nhiqual_dv")
)

agg_df <- agg_df[,!(names(agg_df) %in% removed_var)] 
rm(removed_var)

#extract the cross wave variables that we want to include
temp <- length(variables$cross[variables$cross==""]) #finding the number of empty cells
cross_var <- head(variables$cross, -temp) #deleting ""

#subset the cross_wave dataset
cols_indices <- which(names(cross_wave) %in% c(c("pidp"), cross_var))#find indices for the cross wave variables we want
rows_logical <- cross_wave$pidp %in% unique(agg_df$pidp) #find logical for the necessary pidp we need
common_cross <- cross_wave[rows_logical,cols_indices]
common_cross <- bind_rows(lapply(common_cross,remove_all_labels)) #remove variable labels

#merge the cross_wave dataset onto the main dataset
agg_df <- merge(agg_df, common_cross, by.x = "pidp")

rm(common_cross)




# # subset dataframe for only individuals who changed employment status
# agg_df_ukhls <- agg_df_ukhls[agg_df_ukhls$jbstat == 1 | agg_df_ukhls$jbstat == 2,]
# agg_df_ukhls <- agg_df_ukhls[duplicated(agg_df_ukhls$pidp) | duplicated(agg_df_ukhls$pidp, fromLast=TRUE),]
# agg_df_ukhls <- agg_df_ukhls[with(agg_df_ukhls, order(wave,pidp)),]
# 
# num_row <- dim(agg_df_ukhls)[1]
# logical_vec <- rep(FALSE, num_row/2)
# 
# for (i in 1:(num_row/2)){
#   emp_stat_w6 <- agg_df_ukhls$jbstat[i]
#   emp_stat_w9 <- agg_df_ukhls$jbstat[i+(num_row/2)]
#   if (abs(emp_stat_w6 - emp_stat_w9) == 1){
#     logical_vec[i] <- TRUE
#     logical_vec[i+(num_row/2)] <- TRUE
#   }
# }
# 
# agg_df_ukhls <- agg_df_ukhls[logical_vec,]

#removing second job and refusals
agg_df <- agg_df[!(agg_df$j2has == 1 | agg_df$j2has == 8),]
agg_df <- agg_df[, -which(names(agg_df) %in% c("j2has"))]

#removing obs. who did not respond to treatment variable
agg_df <- agg_df[(agg_df$britid>=0),]

#removing obs. who did not give response to their employment status
agg_df <- agg_df[agg_df$jbsemp > 0,]



#concatenate into a list that sorts variables by the degree they vary over time
time_variability <- list(variant = c("dvage",
                                    "fimnlabgrs_dv",
                                    "health",
                                    "finnow",
                                    "finfut",
                                    "jbft_dv",
                                    "jbiindb_dv",
                                    "britid",
                                    "jbsat"),
                         
                         partial = c("gor_dv",
                                     "hiqual_dv",
                                     "nchild_dv"),
                         
                         invariant = c("sex_dv",
                                       "ethn_dv",
                                       "ukborn",
                                       "generation",
                                       "maedqf",
                                       "paedqf",
                                       "j1semp")
                         )

#remove all the missing values/negative values for the time variant variables
for(i in 1:length(time_variability[["variant"]])){
  if (time_variability[["variant"]][i] != "fimnlabgrs_dv"){
  agg_df <- subset(agg_df,get(time_variability[["variant"]][i]) >= 0)
  }
}


agg_df <- agg_df[order(agg_df$pidp),] #ordering by pidp (ascending)

#replacing negative values for time invariant variable
for(i in 1:length(time_variability[["invariant"]])){
  temp_col <- get(time_variability[["invariant"]][i],agg_df) 
  temp_arr <- tapply(temp_col,agg_df$pidp,t_invariant_func) #use of custom function
  temp_lst <- flatten(temp_arr) #flatten array to a list
  temp_vec <- unlist(temp_lst, use.names = FALSE) #convert list into vector
  temp_index <- which(colnames(agg_df) == time_variability[["invariant"]][i]) #call only the columns that are time invariant
  agg_df[,temp_index] <- temp_vec #overwrite the old column in the dataframe with updated column
}

#replace negative values for partially variant variables if not updated
for(i in 1:length(time_variability[["partial"]])){
  temp_col <- get(time_variability[["partial"]][i],agg_df) 
  temp_arr <- tapply(temp_col,agg_df$pidp,t_partial_func) #use of custom function
  temp_lst <- flatten(temp_arr) #flatten array to a list
  temp_vec <- unlist(temp_lst, use.names = FALSE) #convert list into vector
  temp_index <- which(colnames(agg_df) == time_variability[["partial"]][i])  #call only the columns that are partially time variant
  agg_df[,temp_index] <- temp_vec #overwrite the old column in the dataframe with updated column
}

#identify the variables that are considered niche and do not need to have NaNs removed
niche_vars <- c("maedqf", "paedqf", "j1semp")
to_remove_NaN <- setdiff(names(agg_df),niche_vars) #identify all the remaining variables

#remove all the rows with remaining NaNs
# agg_df <- na.omit(agg_df)
agg_df <- del_NaN_from_cols(agg_df, to_remove_NaN)

#panel dataset
pre_panel_df <- agg_df[duplicated(agg_df$pidp)|duplicated(agg_df$pidp,fromLast = TRUE),]

#saving cleaned dataset to working directory
write.csv(agg_df,"agg_df.csv")
write.csv(pre_panel_df,"pre_panel_df.csv")



  