#######################################################
#This scrip summarizes the results of 3way IPFP in long-table
# basis of this table is the ratio of each size-type group cross age category 
# and ratio of each age-cat cross age_size

#By: Pariya Pourmohammadi
#######################################################

library(stringr)
WD <- readline(prompt="Enter working directory ")
setwd(WD)
# setwd("K:\\DataServices\\Projects\\Current_Projects\\Projections_2050\\Analysis\\AgeSizeType_clusters")
data <- read.csv("3waysIPFP_PUMSseed.csv")

#compute the ratio values based of taz values
generate_ratio <- function(taz){
  new_names <- str_replace_all(names(data),"NoChild", "No_child")
  names(data) <- new_names
  words <- c("Single","Child","No_child")
  words1 <- c("_1","_2","_3","_4")
  tmp <- expand.grid(words,words1)
  cols <- paste0(tmp$Var1, tmp$Var2)
  age_cat <- c("age1","age2","age3","age4")
  tmp_tbl <- as.data.frame(array(0, dim =c(length(age_cat), length(cols))))
  names(tmp_tbl) <- cols
  tmp_tbl <- cbind(age_cat, tmp_tbl)

  taz_data <- data[which(data$taz == taz),]

  for (i in (1:length(cols) )){
    for(j in (1: length(age_cat))){
      tmp_tbl[which(tmp_tbl$age_cat == age_cat[j]),cols[i]] <- taz_data[ ,which(str_detect(names(taz_data),cols[i]) & str_detect(names(taz_data),age_cat[j]))]
    }
  }
  
  tmp_tbl2 <- tmp_tbl
  
  for(k in 1:4){
    tmp_tbl[k,2:13] <- tmp_tbl[k,2:13] / sum(tmp_tbl[k, 2:13])
  }
  
  for(k in 2:13){
    tmp_tbl2[1:4,k] <- tmp_tbl2[1:4,k] / sum(tmp_tbl2[1:4, k])
  }
  
  words <- names(tmp_tbl)
  words <- words[2:length(words)] 
  words1 <- unique(age_cat)
  tmp <- expand.grid(words,words1)
  names <- paste(tmp$Var1, tmp$Var2, sep = "_")
  
  ratio_vector <- as.data.frame(array(0, dim =c(1, length(cols)*length(age_cat))))
  names(ratio_vector) <- names
  
  ratio_vector1 <- ratio_vector
  for (i in (1:length(cols) )){
    for(j in (1: length(age_cat))){
      ratio_vector[ ,which(str_detect(names(ratio_vector),cols[i]) & str_detect(names(ratio_vector),age_cat[j]))] <- tmp_tbl[which(tmp_tbl$age_cat == age_cat[j]),cols[i]]
    }
  }
  
  for (i in (1:length(cols) )){
    for(j in (1: length(age_cat))){
      ratio_vector1[ ,which(str_detect(names(ratio_vector1),cols[i]) & str_detect(names(ratio_vector1),age_cat[j]))] <- tmp_tbl2[which(tmp_tbl2$age_cat == age_cat[j]),cols[i]]
    }
  }
  ratio_vector <- cbind(taz ,ratio_vector)
  ratio_vector1 <<- cbind(taz ,ratio_vector1)
  
  return (ratio_vector)
}

# Convert the ratio table to a long table of taz-type-size-age-ratio
wideToLong = function(ratioTable){
  conv_table <- ratioTable
  names_ <- names(ratioTable)
  names_ <- str_replace(names_,"Single_" , "1")
  names_ <- str_replace(names_,"No_child_" , "2")
  names_ <- str_replace(names_,"Child_" , "3")
  names_ <- str_replace(names_,"_age1" , "1")
  names_ <- str_replace(names_,"_age2" , "2")
  names_ <- str_replace(names_,"_age3" , "3")
  names_ <- str_replace(names_,"_age4" , "4")
  names(conv_table) <- names_
  
  long_table <- melt(conv_table,id.vars = "TAZ_ID")
  long_table$HHtype <- substr(long_table$variable,1,1)
  long_table$HHsize <- substr(long_table$variable,2,2)
  long_table$age <- substr(long_table$variable,3,3)
  new_table <- data.frame(cbind(long_table$TAZ_ID, long_table$HHtype,long_table$HHsize,long_table$age,long_table$value))
  names(new_table ) <- c("TAZ_ID","HHtype","HHsize","age","com.wgt")
  return(new_table)
}

ratioTable <- as.data.frame(array(0, dim =c(dim(data)[1], ncol = 49)))
ratioTable1 <- as.data.frame(array(0, dim =c(0, ncol = 49)))

names_ <- c("TAZ_ID", "Single_1_age1",   "Child_1_age1", "No_child_1_age1", "Single_2_age1", "Child_2_age1", "No_child_2_age1", "Single_3_age1", "Child_3_age1", "No_child_3_age1",
            "Single_4_age1", "Child_4_age1", "No_child_4_age1", "Single_1_age2", "Child_1_age2", "No_child_1_age2", "Single_2_age2", "Child_2_age2", "No_child_2_age2",
            "Single_3_age2", "Child_3_age2", "No_child_3_age2", "Single_4_age2", "Child_4_age2", "No_child_4_age2", "Single_1_age3", "Child_1_age3", "No_child_1_age3",
            "Single_2_age3", "Child_2_age3", "No_child_2_age3", "Single_3_age3", "Child_3_age3", "No_child_3_age3", "Single_4_age3", "Child_4_age3", "No_child_4_age3",
            "Single_1_age4", "Child_1_age4", "No_child_1_age4", "Single_2_age4", "Child_2_age4", "No_child_2_age4", "Single_3_age4", "Child_3_age4", "No_child_3_age4",
            "Single_4_age4", "Child_4_age4", "No_child_4_age4")

names(ratioTable) <- names_
names(ratioTable1) <- names_

# Call generate_ratio function per taz and 
for(m in (1:dim(data)[1])){
  Tazs <- unique(data$taz)
  if(!is.na(data$taz == Tazs[m])[m]){
    # print(m)
    ratioTable[m,] <- generate_ratio(Tazs[m])
    ratioTable1 <- rbind(ratioTable1, ratio_vector1)
  }
}

ratioTable[is.na(ratioTable)] <- 0
ratioTable1[is.na(ratioTable1)] <- 0

write.csv(ratioTable,"ratioTable_crossAge.csv")
write.csv(ratioTable1,"ratioTable_crossTypeSize.csv")

names(ratioTable1) <- names(ratioTable)

new_tab <- wideToLong(ratioTable)
new_tab1 <- wideToLong(ratioTable1)

write.csv(new_tab, "LNGratioTable_cross_age.csv")
write.csv(new_tab1, "LNGratioTable_cross_size_type.csv")