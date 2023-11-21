library(foreign)
library(haven)
library(sjlabelled)
library(ramify)
source("functions.R")
# bhps_waves <- c("ukhls statfiles/bhps_w1.dta", 
#                 "ukhls statfiles/bhps_w2.dta",
#                 "ukhls statfiles/bhps_w3.dta",
#                 "ukhls statfiles/bhps_w4.dta",
#                 "ukhls statfiles/bhps_w5.dta",
#                 "ukhls statfiles/bhps_w6.dta",
#                 "ukhls statfiles/bhps_w7.dta",
#                 "ukhls statfiles/bhps_w8.dta",
#                 "ukhls statfiles/bhps_w9.dta",
#                 "ukhls statfiles/bhps_w10.dta",
#                 "ukhls statfiles/bhps_w11.dta",
#                 "ukhls statfiles/bhps_w12.dta",
#                 "ukhls statfiles/bhps_w13.dta",
#                 "ukhls statfiles/bhps_w14.dta",
#                 "ukhls statfiles/bhps_w15.dta",
#                 "ukhls statfiles/bhps_w16.dta",
#                 "ukhls statfiles/bhps_w17.dta",
#                 "ukhls statfiles/bhps_w18.dta")  #vector of bhps wave names

ukhls_waves <- c("ukhls statfiles/ukhls_w1.dta",
                 "ukhls statfiles/ukhls_w2.dta",
                 "ukhls statfiles/ukhls_w3.dta",
                 "ukhls statfiles/ukhls_w4.dta",
                 "ukhls statfiles/ukhls_w5.dta",
                 "ukhls statfiles/ukhls_w6.dta",
                 "ukhls statfiles/ukhls_w7.dta",
                 "ukhls statfiles/ukhls_w8.dta",
                 "ukhls statfiles/ukhls_w9.dta",
                 "ukhls statfiles/ukhls_w10.dta") #vector of ukhls wave names

#df_bhps <- vector(mode = "list",length = length(bhps_waves))
df_ukhls <- vector(mode = "list",length = length(ukhls_waves))

# df_bhps[[1]] <- read_dta("ukhls statfiles/bhps_w1.dta") 
# df_bhps[[2]] <- read_dta("ukhls statfiles/bhps_w2.dta")
# df_bhps[[3]] <- read_dta("ukhls statfiles/bhps_w3.dta")
# df_bhps[[4]] <- read_dta("ukhls statfiles/bhps_w4.dta")
# df_bhps[[5]]<- read_dta("ukhls statfiles/bhps_w5.dta")
# df_bhps[[6]] <- read_dta("ukhls statfiles/bhps_w6.dta")
# df_bhps[[7]] <- read_dta("ukhls statfiles/bhps_w7.dta")
# df_bhps[[8]] <- read_dta("ukhls statfiles/bhps_w8.dta")
# df_bhps[[9]] <- read.dta13("ukhls statfiles/bhps_w9.dta")
# df_bhps[[10]] <- read.dta13("ukhls statfiles/bhps_w10.dta")
# df_bhps[[11]] <- read.dta13("ukhls statfiles/bhps_w11.dta")
# df_bhps[[12]] <- read.dta13("ukhls statfiles/bhps_w12.dta")
# df_bhps[[13]] <- read.dta13("ukhls statfiles/bhps_w13.dta")
# df_bhps[[14]] <- read.dta13("ukhls statfiles/bhps_w14.dta")
# df_bhps[[15]] <- read.dta13("ukhls statfiles/bhps_w15.dta")
# df_bhps[[16]] <- read.dta13("ukhls statfiles/bhps_w16.dta")
# df_bhps[[17]] <- read.dta13("ukhls statfiles/bhps_w17.dta")
# df_bhps[[18]] <- read.dta13("ukhls statfiles/bhps_w18.dta")
df_ukhls[[1]] <- read_dta("ukhls statfiles/ukhls_w1.dta")
df_ukhls[[2]]<- read_dta("ukhls statfiles/ukhls_w2.dta")
df_ukhls[[3]]<- read_dta("ukhls statfiles/ukhls_w3.dta")
df_ukhls[[4]]<- read_dta("ukhls statfiles/ukhls_w4.dta")
df_ukhls[[5]]<- read_dta("ukhls statfiles/ukhls_w5.dta")
df_ukhls[[6]]<- read_dta("ukhls statfiles/ukhls_w6.dta")
df_ukhls[[7]]<- read_dta("ukhls statfiles/ukhls_w7.dta")
df_ukhls[[8]]<- read_dta("ukhls statfiles/ukhls_w8.dta")
df_ukhls[[9]]<- read_dta("ukhls statfiles/ukhls_w9.dta")
df_ukhls[[10]]<- read_dta("ukhls statfiles/ukhls_w10.dta")
                      
# for (i in 1:length(ukhls_waves)){
#   df_ukhls[[i]] <- read.dta(ukhls_waves[i])
# }
essentials <- read.csv("essentials - Sheet1.csv")

#for-loop applied to bhps waves
# for (i in 1:length(bhps_waves)){
#   temp_df <- df_bhps[[i]]
#   rows_num <- length(temp_df$var_names)
#   temp_df$var_names_unified <- as.array(replicate(rows_num, 0)) #create new column
#   temp_df$var_names_unified[1:2] <- temp_df$var_names[1:2] #first 2 values are same
#   for (j in 3:rows_num){
#     temp <- temp_df$var_names[j]
#     temp_df$var_names_unified[j] <- substring(temp, 4) #keep the 4th character and onwards
#   }
#   df_bhps[[i]]<- temp_df  #output the new dataframe
# }
# 
# rm(temp_df)

library(sjmisc) #for str_contains
wanted_var <- c(essentials$essentials,essentials$niche) #combining essentials into one vector
temp <- length(essentials$niche[essentials$niche==""]) #finding the number of empty cells
wanted_var <- head(wanted_var, -temp) #deleting ""
rm(temp)
# rm(length_diff)
# #bhps
# indices_list_bhps <- vector("list", length(df_bhps))
# for(k in 1:length(df_bhps)){
#   var  <- names(df_bhps[[k]]) #names in dataframe
#   indices  <- logical(length(var)) #creating vector of logicals
#   for (i in 1:length(wanted_var)) {
#     for (j in 1:length(var)) {
#       if(str_contains(var[j], wanted_var[i]) == TRUE) {
#         indices[j] <- TRUE #if the variable name contains the wanted variable then it will be true
#       }
#     } 
#   }
#   indices_list_bhps[[k]] <- indices #append vector to the list
# }
# 
# final_df_bhps <- vector("list", length = length(df_bhps))
# for(k in 1:length(df_bhps)){
#   final_df_bhps[[k]] <- df_bhps[[k]][,indices_list_bhps[[k]]] #making big dataframe
# }

#ukhls
indices_list_ukhls <- vector("list", length(df_ukhls))
for(k in 1:length(df_ukhls)){
  var  <- names(df_ukhls[[k]]) #names in dataframe
  indices  <- logical(length(var))
   for (j in 1:length(var)) {
     for (i in 1:length(wanted_var)) {
      if(str_contains(var[j], wanted_var[i]) == TRUE) {
        indices[j] <- TRUE
      }
     }
   } 
   
  indices_list_ukhls[[k]] <- indices
}
rm(i)
rm(j)
rm(k)

final_df_ukhls <- vector("list", length = length(df_ukhls))
for(k in 1:length(df_ukhls)){
  final_df_ukhls[[k]] <- df_ukhls[[k]][,indices_list_ukhls[[k]]] 
}
rm(k)

#name changing
for (i in 1:length(final_df_ukhls)){
  temp_df <- final_df_ukhls[[i]]
  rows_num <- length(names(temp_df))
  for (j in 2:rows_num){
    temp <- names(temp_df)[j]
    names(temp_df)[j] <- substring(temp, 3) #keep the 3rd character and onwards
  }
  final_df_ukhls[[i]]<- temp_df #output the new dataframe
}
rm(temp_df)
rm(temp)

#make wave var
for(i in 1:length(final_df_ukhls)){
  new_col <- rep(i,dim(final_df_ukhls[[i]])[1])
  final_df_ukhls[[i]]$wave <- new_col
}
rm(i)
#finding intersection
temp_list <- vector(mode = "list",length = length(final_df_ukhls))
for(i in 1:length(final_df_ukhls)){
  temp_list[[i]] <- names(final_df_ukhls[[i]])
}
common_var <- Reduce(intersect,temp_list)
rm(temp_list)
#concatenation
library(dplyr)
agg_df_ukhls <- bind_rows(lapply(final_df_ukhls,remove_all_labels))
agg_df_ukhls <- agg_df_ukhls[common_var]

removed_var<-c("ppsex","pn1sex", "pn2sex", "pns1sex" ,"pns2sex","pjulk4wk")
agg_df_ukhls <- agg_df_ukhls[,!(names(agg_df_ukhls) %in% removed_var)] #removing redundant vars we don't need

# #saving stuff
# string1 <- "~/Documents/Year 3/Econ Soc Research/agg_df_ukhls"
# string2 <- ".csv"
# file_name <- paste(string1,string2,sep="")
# write.csv(agg_df_ukhls,file_name)

agg_df_ukhls <- subset(agg_df_ukhls,!(j2has == 1 | j2has == 8)) #removing second job and refusals
agg_df_ukhls <- subset(agg_df_ukhls,natid1 >= 0| natid2 >=0 | natid3>=0 | natid4 >=0| natid5 >=0)
agg_df_ukhls$natid <- ifelse(agg_df_ukhls$natid1 == 0 & 
                             agg_df_ukhls$natid2 ==0 & 
                             agg_df_ukhls$natid3==0 & 
                             agg_df_ukhls$natid4 ==0& 
                             agg_df_ukhls$natid5 ==0,0,1)

#missing values

t_variant_var <- c("dvage",
                  "jbstat",
                   "jbhrs",
                   "jbsat",
                   # ,"julk4wk",
                   "fimnlabgrs_dv",
                   # "jbisco88_cc",
                   #"jlisco88_cc",
                   "health",
                   "finnow",
                   "finfut",
                   "jbft_dv"
                  )

for(i in 1:length(t_variant_var)){
  agg_df_ukhls <- subset(agg_df_ukhls,get(t_variant_var[i]) >= 0)
}

t_invariant_var <- c("sex",
                     "yr2uk4",
                     "plbornc",
                     "racel_dv",
                     "ethn_dv",
                     "ukborn",
                     "macob",
                     "pacob")

t_partial_var <- c("gor_dv",
                   "qfhigh_dv",
                   "hiqual_dv",
                   "qualoc",
                   # "mlstat",
                   "nchild_dv",
                   "nch14resp"
                   )

agg_df_ukhls <- agg_df_ukhls[order(agg_df_ukhls$pidp),] #ordering by pidp (ascending)

#time invariant variable replacing -ve values
for(i in 1:length(t_invariant_var)){
  temp_col <- get(t_invariant_var[i],agg_df_ukhls) 
  temp_arr <- tapply(temp_col,agg_df_ukhls$pidp,t_invariant_func) #use of custom function
  temp_lst <- flatten(temp_arr)
  temp_vec <- unlist(temp_lst, use.names = FALSE) 
  temp_index <- which(colnames(agg_df_ukhls) == t_invariant_var[i])
  agg_df_ukhls[,temp_index] <- temp_vec
}
rm(temp_lst)
rm(temp_vec)
rm(temp_arr)
rm(temp_col)
rm(temp_index)


for(i in 1:length(t_partial_var)){
  temp_col <- get(t_partial_var[i],agg_df_ukhls) 
  temp_arr <- tapply(temp_col,agg_df_ukhls$pidp,t_partial_func) #use of custom function
  temp_lst <- flatten(temp_arr)
  temp_vec <- unlist(temp_lst, use.names = FALSE) 
  temp_index <- which(colnames(agg_df_ukhls) == t_partial_var[i])
  agg_df_ukhls[,temp_index] <- temp_vec
}

keep <- c("plbornc","yr2uk4","qualoc")
to_remove_NaN <- setdiff(names(agg_df_ukhls),keep)
for(i in 1:length(to_remove_NaN)){
  temp_index <- which(colnames(agg_df_ukhls) == to_remove_NaN[i])
  agg_df_ukhls<- agg_df_ukhls[!is.nan(agg_df_ukhls[,temp_index]),]
}

#register as panel data
library(plm)
panel_df_ukhls <- pdata.frame(agg_df_ukhls,
                              index = c("pidp","wave"),
                              drop.index = FALSE
                              )





#saving stuff
string1 <- "~/Documents/Year 3/Econ Soc Research/agg_df_ukhls"
string2 <- ".csv"
file_name <- paste(string1,string2,sep="")
write.csv(agg_df_ukhls,file_name)



