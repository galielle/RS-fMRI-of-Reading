setwd("C:/Users/Gali/Dropbox/My Documents/JHU/Lab - Brenda's/1 Project/ROI analysis/RdN vs All/5nets_18ps_180113");

require(utils);
require(plyr);
library(lmerTest);
library(car);

require(data.table)
require(psych)
require(gdata)

### lists and data
area_id_list <- as.character(c('14','15','16','17','18','19','20','21','22','39','40','41','43','44','45','46','47','48','49','50','51','52',
                         '53','54','55','56','68','69','70','71','72','73','74','93','94','95','96','97','98','99','100','101'));  # list of area_ids

area_data <- read.table("area_data3.txt", header = TRUE)


### functions

# convert REST matrix to individual data file for participant
convert_participant_data <- function(participant, data, area_id_list = area_id_list) {

  participant_data <- data;
  colnames(participant_data)<- c(area_id_list) # change column names to be the area ids
  rownames(participant_data) <- c(area_id_list)

  participant_data[participant_data > 1] <- 1
  diag(participant_data)<-NA
  participant_data[lower.tri(participant_data)]<-NA # get only half
  ind_p_data<-as.data.table(unmatrix(participant_data)); # transform from matrix to vector
  
  colnames(ind_p_data) <- "corr"
  
  ind_p_data$participant <- participant # add col for participant - from function input
  stimpairs <- combn(area_id_list,2); # compute the pairwise names from the "area_id_list" list of col names
  pairnames<-expand.grid(area_id_list,area_id_list)
  pairnames<-paste0(pairnames[,2],'_',pairnames[,1]) 
  ind_p_data$area_id<-pairnames
  ind_p_data<-ind_p_data[complete.cases(ind_p_data),]
  
  return(ind_p_data)
}

# create one data file for all participants, based on participant list
create_all_ind <- function(area_id_list) {
  all_ind <- c();
  flist <- list.files(pattern = 'zFC*')
  for (i in 1:length(flist)) {
#    if (p < 10) { p_name <- paste("0", p, sep = "") } else { p_name <- p };
#    data <- as.matrix(read.table(paste("zFCMap_Subject", p_name, ".txt", sep = "")));
    data <- as.matrix(read.table(flist[i]));
    ind_data <- convert_participant_data(i, data, area_id_list);
    all_ind <- rbind(all_ind, ind_data)
  }
  return(all_ind)
}

# create unified data file with all relevant information
add_cols_all_ind <- function(ind_data, area_data) {
  all_ind <- ind_data;
  all_ind$abs_corr <- abs(all_ind$corr);
  area_data <- area_data;
  all_ind <- merge(all_ind, area_data, by = "area_id");
  return(all_ind)
}

### Create one data file - *RUN THIS*
ind_all <- add_cols_all_ind(create_all_ind(area_id_list), area_data);
write.table(ind_all, file = 'C:/Users/Gali/Dropbox/My Documents/JHU/Lab - Brenda\'s/1 Project/ROI analysis/RdN vs All/all_data_18p_opn.txt', sep = ",", quote = FALSE, col.names = TRUE);


