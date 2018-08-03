######################################################
#This script uses Iterative proportional fitting procedure (IPFP) to assign 
#joint distributions, this file is based on 2ways ipfp on hhtype-size and
#assigning pop to each joint class (ipfp2 function), then based on PUMS data
#and age-pop another 2d ipfp (ipfp3 function) will be done to find age-type-size
#joint dist for pop data
#Thanks to Sarh Philbrik and Steve Gehrke AMAPC
#Data:  August, 3rd 2018
#Pariya Pourmohammadi
#@pariya_pm
######################################################

options(warning=2)

library(mipfp)
data_dir <- readline(prompt("Enter directory of the TAZ_Control data"))
# Data_directory <- "K:\\DataServices\\Projects\\Current_Projects\\Projections_2050\\Analysis\\AgentType_Clusters\\data\\tabular\\census\\processed"
# data <- read.table(paste0(Data_directory,"\\HHdsbyType_BG.csv"),header = TRUE,sep = ",")
data <- read.table(paste0(data_dir,"\\HHdsbyType_BG.csv"),header = TRUE,sep = ",")

#create the template for 2d IPFP family type-size results
join_dist <- data.frame(matrix(nrow = dim(data)[1] , ncol = 13))
names(join_dist) <- c("taz_id", "HHSingle_1","HHSingle_2","HHSingle_3","HHSingle_4",
                      "HHChild_1","HHChild_2","HHChild_3","HHChild_4",
                      "HHNoChild_1","HHNoChild_2","HHNoChild_3","HHNoChild_4")

#create the template for 2d IPFP family type-size age results with user-defined seeds
join_dist_pop <- data.frame(matrix(nrow = dim(data)[1] , ncol = 49))
names(join_dist_pop) <-  c("taz_id", "Single_1_age1","Single_1_age2","Single_1_age3","Single_1_age4",
                           "Single_2_age1","Single_2_age2","Single_2_age3","Single_2_age4",
                           "Single_3_age1","Single_3_age2","Single_3_age3","Single_3_age4",
                           "Single_4_age1","Single_4_age2","Single_4_age3","Single_4_age4",
                           "NoChild_1_age1","NoChild_1_age2","NoChild_1_age3","NoChild_1_age4",
                           "NoChild_2_age1","NoChild_2_age2","NoChild_2_age3","NoChild_2_age4",
                           "NoChild_3_age1","NoChild_3_age2","NoChild_3_age3","NoChild_3_age4",
                           "NoChild_4_age1","NoChild_4_age2","NoChild_4_age3","NoChild_4_age4",
                           "Child_1_age1","Child_1_age2","Child_1_age3","Child_1_age4",
                           "Child_2_age1","Child_2_age2","Child_2_age3","Child_2_age4",
                           "Child_3_age1","Child_3_age2","Child_3_age3","Child_3_age4",
                           "Child_4_age1","Child_4_age2","Child_4_age3","Child_4_age4")

#Create the template for 2d IPFP family type-size age results with PUMS data based seeds
join_dist_pop_seed <- data.frame(matrix(nrow = 0 , ncol = 49))
names(join_dist_pop_seed) <- c("taz_id", "Single_1_age1","Single_1_age2","Single_1_age3","Single_1_age4",
                                "Single_2_age1","Single_2_age2","Single_2_age3","Single_2_age4",
                                "Single_3_age1","Single_3_age2","Single_3_age3","Single_3_age4",
                                "Single_4_age1","Single_4_age2","Single_4_age3","Single_4_age4",
                                "NoChild_1_age1","NoChild_1_age2","NoChild_1_age3","NoChild_1_age4",
                                "NoChild_2_age1","NoChild_2_age2","NoChild_2_age3","NoChild_2_age4",
                                "NoChild_3_age1","NoChild_3_age2","NoChild_3_age3","NoChild_3_age4",
                                "NoChild_4_age1","NoChild_4_age2","NoChild_4_age3","NoChild_4_age4",
                                "Child_1_age1","Child_1_age2","Child_1_age3","Child_1_age4",
                                "Child_2_age1","Child_2_age2","Child_2_age3","Child_2_age4",
                                "Child_3_age1","Child_3_age2","Child_3_age3","Child_3_age4",
                                "Child_4_age1","Child_4_age2","Child_4_age3","Child_4_age4")


#Two way Iterative proportional fitting procedure (IPFP) on size & type
ipfp2 <- function(taz) {
  # seeds are set to 1 for all the HHtype-sizes except for the ones that are of type single and size>1
  taz_data <- data[data$ID==taz,] 
  seed_tbl <-  array(1,c(4, 3))
  seed_tbl[2:4,1] <- 0
  seed_tbl[1,2:3] <- 0
  
  #Get target data from TAZ data
  target_col <-  as.numeric(taz_data[,6:8])
  target_row <- as.numeric(taz_data[,2:5])
  target_data <- list(target_row, target_col)

  # Compute total population
  pop_total <- sum(taz_data[,9:length(taz_data)])
  
  # target list to show the order of target row/col
  target_list<- list(1,2)
  
  # Create join distribution of marginal dists
  m_sze <- Ipfp(seed_tbl, target_list, target_data, iter=50000, na.target=TRUE) 
  
  #generate pop data based on HH distribution in cross-class table
  #size 1,2,3 are multiplied by the class, remainder is uniformly distributed on 4+ class
  pop_dat_TAZ <- m_sze$x.hat
  pop_dat_TAZ[2,] <- pop_dat_TAZ[2,]*2
  pop_dat_TAZ[3,] <- pop_dat_TAZ[3,]*3
  pop_1_3 <- sum(pop_dat_TAZ[1:3,])
  mod_pop <- abs(pop_total- pop_1_3)
  
  pop_dat_TAZ[4,] <- pop_dat_TAZ[4,] * mod_pop/target_row[4]
  
  # function output is population in type/size vector
  pop_dat_TAZ <- as.data.frame(array(pop_dat_TAZ, dim=c(1,12)))
  names(pop_dat_TAZ) <- c("pop_Single_1","pop_Single_2","pop_Single_3","pop_Single_4",
                     "pop_NoChild_1","pop_NoChild_2","pop_NoChild_3","pop_NoChild_4",
                     "pop_Child_1","pop_Child_2","pop_Child_3","pop_Child_4")
  pop_dat_TAZ[is.na(pop_dat_TAZ)] <- 0 
  pop_dat_TAZ <- cbind(taz,pop_dat_TAZ)
  return(pop_dat_TAZ)
}


#this function does age vs size-type ipfp and returns a vector of age-type-size population
ipfp3 <- function(taz,seed_tbl) {
  # Generate ground truth data(control vectors)
  target_col <-  as.numeric(data[data$ID==taz,9:length(data)] )
  target_row <- as.numeric(as.matrix(join_dist[which(join_dist$taz_id==taz),2:length(join_dist)]))
  target_data <- list(target_col,target_row)
  target_list<- list(1,2)

  if(missing(seed_tbl)) {
    # If there is no seed passed to the function create it here
    seed_tbl <-  rbind(c(0,0,0,0,1,1,1,1,0,0,0,0),
                        c(0,0,0,0,1,1,1,1,0,0,0,0),
                        c(1,0,0,0,1,1,1,1,0,0,0,0),
                        c(1,0,0,0,1,1,1,1,1,1,1,1))
  }
  
  # Create join distribution of marginal dists
  m_sze <- Ipfp(seed_tbl, target_list, target_data, iter=50000, na.target=TRUE) 
  
  # create a vector of data
  pop_dat_TAZ <- as.data.frame(array(m_sze$x.hat, dim=c(1,dim(m_sze$x.hat)[1]*dim(m_sze$x.hat)[2])))
  names(pop_dat_TAZ) <- c("Single_1_age1","Single_1_age2","Single_1_age3","Single_1_age4",
                          "Single_2_age1","Single_2_age2","Single_2_age3","Single_2_age4",
                          "Single_3_age1","Single_3_age2","Single_3_age3","Single_3_age4",
                          "Single_4_age1","Single_4_age2","Single_4_age3","Single_4_age4",
                          "NoChild_1_age1","NoChild_1_age2","NoChild_1_age3","NoChild_1_age4",
                          "NoChild_2_age1","NoChild_2_age2","NoChild_2_age3","NoChild_2_age4",
                          "NoChild_3_age1","NoChild_3_age2","NoChild_3_age3","NoChild_3_age4",
                          "NoChild_4_age1","NoChild_4_age2","NoChild_4_age3","NoChild_4_age4",
                          "Child_1_age1","Child_1_age2","Child_1_age3","Child_1_age4",
                          "Child_2_age1","Child_2_age2","Child_2_age3","Child_2_age4",
                          "Child_3_age1","Child_3_age2","Child_3_age3","Child_3_age4",
                          "Child_4_age1","Child_4_age2","Child_4_age3","Child_4_age4")
  pop_dat_TAZ[is.na(pop_dat_TAZ)] <- 0 
  pop_dat_TAZ <- cbind(taz_id=taz, pop_dat_TAZ)
  return(pop_dat_TAZ)
}

# call the function per taz and generate a 2d table carrying population per type-size in TAZ
for(i in (1:dim(data)[1])){
  if(any(data$ID == unique(data$ID)[i]))
    join_dist[i,] <- ipfp2(unique(data$ID)[i])
}

#Get HHType-size in TAZ data
seed_base <- read.table("K:/DataServices/Projects/Current_Projects/Projections_2050/Data/Tabular/TAZ_assigned_dat_HHtype.csv", sep = ",",header = TRUE)



#####call the IPFP function with seeds from PUMS data
seed_tbl <-  array(0,c(4, 12))
Tazs <- unique(data$ID)


for(m in (1:dim(data)[1])){
  seed_check <- seed_base[seed_base$TAZID == Tazs[m],]
  d<- 1
  
##loop through each hhtype class, hhSize, age group and get the pums data of each type-size-age groups for each TAZ record
#place the value in seed_tbl
  for(k in (1:3)){
    for (j in (1:4)){
       for (i in (1:4)){
        if(length(seed_check$compWgt[seed_check$HHSize ==j & seed_check$HHtype ==k & seed_check$TAZ_ageC ==i]) > 0)
          seed_tbl[i,d] <- seed_check$compWgt[seed_check$HHSize ==j & seed_check$HHtype ==k & seed_check$TAZ_ageC == i]
       }
      d <- d+1
    }
  }
  
  #flag the records which have records in each class
  flag <- any(data$ID == Tazs[m]) && !any(data[which(data$ID == Tazs[m]),2:length(data)]== 0)
  
  if(flag)
    join_dist_pop_seed <- rbind(join_dist_pop_seed, ipfp3(Tazs[m],seed_tbl))
  
  #if not flagged pick the seed-tbl as the joint dist
  if(!flag){
    m_sze <- seed_tbl
    pop_dat_TAZ <- as.data.frame(array(m_sze, dim=c(1,dim(m_sze)[1]*dim(m_sze)[2])))
    names(pop_dat_TAZ) <- c("Single_1_age1","Single_1_age2","Single_1_age3","Single_1_age4",
                            "Single_2_age1","Single_2_age2","Single_2_age3","Single_2_age4",
                            "Single_3_age1","Single_3_age2","Single_3_age3","Single_3_age4",
                            "Single_4_age1","Single_4_age2","Single_4_age3","Single_4_age4",
                            "NoChild_1_age1","NoChild_1_age2","NoChild_1_age3","NoChild_1_age4",
                            "NoChild_2_age1","NoChild_2_age2","NoChild_2_age3","NoChild_2_age4",
                            "NoChild_3_age1","NoChild_3_age2","NoChild_3_age3","NoChild_3_age4",
                            "NoChild_4_age1","NoChild_4_age2","NoChild_4_age3","NoChild_4_age4",
                            "Child_1_age1","Child_1_age2","Child_1_age3","Child_1_age4",
                            "Child_2_age1","Child_2_age2","Child_2_age3","Child_2_age4",
                            "Child_3_age1","Child_3_age2","Child_3_age3","Child_3_age4",
                            "Child_4_age1","Child_4_age2","Child_4_age3","Child_4_age4")
    pop_dat_TAZ <- cbind(taz_id =Tazs[m],pop_dat_TAZ)
    join_dist_pop_seed <- rbind(join_dist_pop_seed, pop_dat_TAZ)
  }
}

#Write results to files
out_dir <- readline(prompt("Enter output files directory: "))
# setwd("K:\\DataServices\\Projects\\Current_Projects\\Projections_2050\\Analysis\\AgeSizeType_clusters")
setwd(out_dir)
write.csv(join_dist_pop_seed, "3waysIPFP_PUMSseed.csv")
write.csv(join_dist, "2way_sizeType_IPFP.csv")

######call ipfp3 function per TAZ without seeds from PUMS
# for(m in (1:dim(data)[1])){
#   if(any(data$ID == unique(data$ID)[m]))
#     join_dist_pop <- rbind(join_dist_pop,ipfp3(unique(data$ID)[m]))
# }
# write.csv(join_dist_pop, "3waysIPFP.csv")

