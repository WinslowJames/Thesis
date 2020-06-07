import_data <- function(x){
  if (x == "local"){
    thesis_data <- read.csv("~/THESIS/Data/ForRwithGender.csv",sep=",",header=TRUE)
  } else {
  thesis_data <- read.csv("//data.ucdenver.pvt/dept/SOM/ACCORDS/Projects/JRice_group/Winslow/Data/ForRwithGender.csv",sep=",",header=TRUE)  
  }
  thesis_data <<- thesis_data
}


#############

#############

#############

#############

#############

#############

#############

# # requires: 
# library(dplyr)
# library(reshape)
# # 

# Reshape the data to get one row per person and 
# one column : count of events (infections)
# one column : for each covariate 
# one column : for the maximum number of days to be the time variable
# one column : for the indicator of death 

clean_data <- function(thesis_data){ 
# rewrite more consisely #
# here we are just removing the variables irrelevant to reshaping the data #
change_data_one <- thesis_data[,-c(1, 3:9)]

# change the data into the desired form #
melt_thesis <- melt(change_data_one, id = c("MRN","start"))
summary(melt_thesis$variable)
cast_thesis <<- cast(melt_thesis, MRN~variable, sum)


subset_for_covs <<- thesis_data[,c(1:9)]
unique_subset_for_covs <<- unique(subset_for_covs)
# add max(days) column #

# merge the simple model columns with the covariates #
# this chunk isn't necesssary for the simple model simulation or likelihood #
Reshaped <<- merge(x = cast_thesis, y = unique_subset_for_covs, by = "MRN" , all.y = TRUE)
}


clean_up <- function(Reshaped){
  # rename columns #
Reshaped <<- `colnames<-`(Reshaped,c("id","time.to.death","n.events","dead","hospital","gender","race","organ","quarter","year","lt2","UngrpHospital")) 

# Since R will read year as continuous, we rescale to make the variable analagous to years since start time #

Reshaped$year <<- Reshaped$year - min(Reshaped$year) 


# Combine Race into White, Black, and Other #

Reshaped$race <<- recode(Reshaped$race , "White" = "White" , "Black" = "Black", "Asian" = "Other",
                        "Missing" = "Other")

}
