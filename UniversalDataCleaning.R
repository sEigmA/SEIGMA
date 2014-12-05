#######################################
## Title: Universal Data Cleaning      ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  12/05/2014         ##
## Date Modified: 12/05/2014         ##
#######################################

library(dplyr)

data_cleaning <- function(file_name = "ba002_01_5yr.sas7bdat", contents_file = "BA002_01_5yr_contents.csv",
                          variable = "VETERAN STATUS", id_cols = 1:5,
                          save_folder = "va", save_name = "vetstatusdata.csv"){
  if(file_name %in% list.files() == FALSE)
    stop("Please set working directory to folder with data to be cleaned")
  
  ## load data from SAS
  if(substring(file_name, last = 8) == "sas7bdat"){
    library(sas7bdat)
    sas_data <- read.sas7bdat(file_name)
  }
  
  ## load data from CSV
  if(substring(file_name, last = 3) == "csv"){
    dirty_data <- read.csv(file_name, na.strings = "N/A")
  }
  
  clean_df <- dirty_data
  
  ## give columns relevant titles
  column_titles <- read.csv(contents_file, skip=1) %>%
    arrange(X.)
  
  colnames(clean_df) <- as.character(column_titles$Label)
  
  ## Replace N/A's with "NA" to remove the slash.
  clean_df$Region <- replace(clean_df$Region, clean_df$Region=="N/A", NA)
  clean_df$County <- replace(clean_df$County, clean_df$County=="N/A", NA)
  clean_df$State <- replace(clean_df$State, clean_df$State=="N/A", NA)
  clean_df$Municipal <- replace(clean_df$Municipal, clean_df$Municipal=="N/A", NA)
  
  clean_df$Region  <- ifelse(!is.na(clean_df$Region), "United States", "MA")
  clean_df$Region  <- ifelse(!is.na(clean_df$County), paste(clean_df$County, "County"), clean_df$Region)
  clean_df$Region  <- ifelse(!is.na(clean_df$Municipal), as.character(clean_df$Municipal),
                             as.character(clean_df$Region))
  
  ## Add "County" to the end of each County name
  clean_df$County <- paste(clean_df$County, "County")
  
  ## grab only columns involving veterans status
  var_cols <- grep(variable, x = colnames(clean_df))
  clean_var_df <- clean_df %>%
    select(id_cols, var_cols) %>%
    arrange(Region)
  
  # Put US and MA at top of data set
  idx_edu_MA <- which(clean_var_df$Region == "MA")
  idx_edu_US <- which(clean_var_df$Region == "United States")
  final_df <- rbind.data.frame(clean_var_df[idx_edu_US,],
                               clean_var_df[idx_edu_MA,], 
                               clean_var_df[-c(idx_edu_MA, idx_edu_US),])
  
  # Save data
  write.csv(final_df, file=paste0(save_folder, "/", save_name))
}