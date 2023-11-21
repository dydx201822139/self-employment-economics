#Unification of Variable Names

bhps_waves <- c("variable-names_bhps_w1.csv", 
           "variable-names_bhps_w2.csv",
           "variable-names_bhps_w3.csv",
           "variable-names_bhps_w4.csv",
           "variable-names_bhps_w5.csv",
           "variable-names_bhps_w6.csv",
           "variable-names_bhps_w7.csv",
           "variable-names_bhps_w8.csv",
           "variable-names_bhps_w9.csv",
           "variable-names_bhps_w10.csv",
           "variable-names_bhps_w11.csv",
           "variable-names_bhps_w12.csv",
           "variable-names_bhps_w13.csv",
           "variable-names_bhps_w14.csv",
           "variable-names_bhps_w15.csv",
           "variable-names_bhps_w16.csv",
           "variable-names_bhps_w17.csv",
           "variable-names_bhps_w18.csv")  #vector of bhps wave names

ukhls_waves <- c("variable-names_ukhls_w1.csv",
                 "variable-names_ukhls_w2.csv",
                 "variable-names_ukhls_w3.csv",
                 "variable-names_ukhls_w4.csv",
                 "variable-names_ukhls_w5.csv",
                 "variable-names_ukhls_w6.csv",
                 "variable-names_ukhls_w7.csv",
                 "variable-names_ukhls_w8.csv",
                 "variable-names_ukhls_w9.csv",
                 "variable-names_ukhls_w10.csv") #vector of ukhls wave names


#for-loop applied to bhps waves
for (i in 1:length(bhps_waves)){
  df <- read.csv(bhps_waves[i])
  rows_num <- length(df$var_names)
  df$var_names_unified <- as.array(replicate(rows_num, 0)) #create new column
  df$var_names_unified[1:2] <- df$var_names[1:2] #first 2 values are same
  for (j in 3:rows_num){
    temp <- df$var_names[j]
    df$var_names_unified[j] <- substring(temp, 4) #keep the 4th character and onwards
  }
  write.csv(df[,2:4], bhps_waves[i]) #output the new dataframe
}

#ukhls wave 1
df <- read.csv(ukhls_waves[1])
rows_num <- length(df$var_names)
df$var_names_unified <- as.array(replicate(rows_num, 0)) #create new column
df$var_names_unified[1] <- df$var_names[1]
for (j in 2:rows_num){
  temp <- df$var_names[j]
  df$var_names_unified[j] <- substring(temp, 3) #keep the 3rd character and onwards
}
write.csv(df[,2:4], ukhls_waves[1]) #output the new dataframe



#for-loop applied to ukhls waves 2 - 10
for (i in 2:length(ukhls_waves)){
  df <- read.csv(ukhls_waves[i])
  rows_num <- length(df$var_names)
  df$var_names_unified <- as.array(replicate(rows_num, 0)) #create new column
  df$var_names_unified[1:2] <- df$var_names[1:2] #first 2 values are same
  for (j in 3:rows_num){
    temp <- df$var_names[j]
    df$var_names_unified[j] <- substring(temp, 3) #keep the 3rd character and onwards
  }
  write.csv(df[,2:4], ukhls_waves[i]) #output the new dataframe
}





