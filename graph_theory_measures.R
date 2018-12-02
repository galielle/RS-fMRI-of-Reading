setwd("C:/Users/Gali/Dropbox/My Documents/JHU/Lab - Brenda's/1 Project/ROI analysis/RdN vs All/Dendrograms");

library(ape);
library(FactoMineR);
library(cba);
library(dendextend);
library(pvclust);
require(graphics);

rois <- c('14',	'15',	'16',	'17',	'18',	'19',	'20',	'21',	'22',	'39',	'40',	'41',	'43',	'44',	'45',	'46',	'47',	'48',	'49',	'50',	'51',	'52',	'53',	'54',	'55',	'56',	'68',	'69',	'70',	'71',	'72',	'73',	'74',	'93',	'94',	'95',	'96',	'97',	'98',	'99',	'100',	'101');
RdN_rois <- c('93',	'94',	'95',	'96',	'97',	'98',	'99',	'100',	'101');
SpN_rois <- c('39',	'40',	'41',	'43',	'44',	'45',	'46',	'47');
DMN_rois <- c('14',	'15',	'16',	'17',	'18',	'19',	'20',	'21',	'22');
DAN_rois <- c('68',	'69',	'70',	'71',	'72',	'73',	'74');
SMN_rois <- c('48',	'49',	'50',	'51',	'52',	'53',	'54',	'55',	'56');
OPN_rois <- c('39',	'40',	'41',	'43',	'44',	'45',	'46',	'47', '93', '94', '96', '97', '100');
unique_rois <- c('14',	'15',	'16',	'17',	'18',	'19',	'20',	'21',	'22',	'39',	'40',	'41',	'43',	'44',	'45',	'46',	'47',	'48',	'49',	'52',	'53',	'54',	'55',	'56',	'68', '69',	'70',	'72',	'74',	'93',	'94',	'96',	'97',	'100')


read_mat_all <- function(uniq_rois = 1) {
  setwd("C:/Users/Gali/Dropbox/My Documents/JHU/Lab - Brenda's/1 Project/ROI analysis/RdN vs All/Dendrograms/individual");
  participants <- c(2,3,4,6,8,9,10,11);
  net_name <- "all";
  all_ps <- c();
  for(i in 1:8) {
    temp <- read.table(paste("p",participants[i],"_", net_name, ".txt", sep = ""));
    colnames(temp) <- rois;
    rownames(temp) <- rois;
    temp[temp == "Inf"] <- 0;
    temp <- abs(temp);
    if(uniq_rois == 1) { temp <- subset(temp, select = unique_rois); temp <- temp[unique_rois,]} 
    all_ps[[i]] <- temp;
  }
  return(all_ps)
}

avg_all <- function() {
  all_mat <- read_mat_all();
  avg_p_all <- rep(0,8);
  n_p_all <- rep(0,8);
  x <- 0
  for (i in 1:8) {
    for (j in 2:(length(all_mat[[i]][1,]))) {
      for(k in 1:(j-1)) {
        avg_p_all[i] <- avg_p_all[i] + all_mat[[i]][j,k];
      }
    }
  }
  return(avg_p_all/561)
} 

deg_all <- function(ind_cutoff = 1) {
  all_mats <- read_mat_all();
  if(ind_cutoff == 1) { all_avg <- avg_all() } else { all_avg <- rep(0.157, 8)}
  all_deg <- matrix(rep(0), ncol = nrow(all_mats[[1]]), nrow = 8)
  for (i in 1:8) {
    part_mat <- all_mats[[i]];
    part_avg <- all_avg[i];
    for (j in 1:ncol(all_deg)) {
      x <- part_mat[j,];
      all_deg[i,j] <- length(x[x>part_avg])
    }
  }
  return(all_deg)
}