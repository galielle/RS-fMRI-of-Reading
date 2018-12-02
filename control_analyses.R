require(utils);
require(plyr);
library(lmerTest);
library(car);

## All data: These data contain all the ROIs from the 7 networks (RdN, SpN, LCN, CCN, DMN, DAN, SMN) ##
ind_all <- read.table("cont_combo_151110.txt", header = TRUE);
ind_all <- subset(ind_all, WAS != 0); # remove same pairs
ind_all_no_shared <- subset(ind_all, dist > 0); # subset without autocorrelations
included_nets <- c(0:9,12,13,17, 19, 21)

ind_inc <- subset(ind_all, nets %in% included_nets)

coh <- subset(ind_inc, WAS == 1);

net <- subset(ind_inc, SMN != 0); ## Change network here 

## OPN data: These data contain OPN ROIs; shared ROIs between the SpN and RdN only appear once. ##
opn_ind_all <- read.table("snl_ind.txt", header = TRUE);
opn_ind_all <- subset(opn_ind_all, WAS != 0);  #remove same pairs
opn_ind_all <- subset(opn_ind_all, CCN == 0); #remove the CCN
opn_ind_all <- subset(opn_ind_all, LCN == 0); #remove the LCN
opn_ind_all_no_shared <- subset(opn_ind_all, dist > 0)

opn <- opn_ind_all
opn <- subset(opn, OPN != 0);

## Seed data: Comparing the data only to one seed ## 

seednum <- 22 ## 47 vwfa, 15 pcc, 50 M1, 70 R pIPS, 78 IFG, 95 VWFA in RdN
seed_net <- subset(ind_all_no_shared, area1num == seednum | area2num == seednum);
seed_net$nets <- factor(seed_net$nets);

#######

ind <- coh;   ## Change between net and coh here ##
ind$nets <- factor(ind$nets);

fit_cont <- lmer(abs(corr) ~ nets + (1 | participant), data = ind, control = lmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 20000000)));

fit_within_across <- lmer(abs(corr) ~ SMN + (1 | participant), data = ind, control = lmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 20000000)));

fit_seed <- lmer(abs(corr) ~ nets + (1 | participant), data = seed_net, control = lmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 20000000)));


fit_net <- function(net_name = "RdN", net_data = ind_all) {
  dat <- subset(net_data, net_data[, net_name] != 0);
  dat$nets <- factor(dat$nets);
  fit_net <- lmer(abs(corr) ~ nets + (1 | participant), data = dat, control = lmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 20000000)));
  return(summary(fit_net));
}

avg_ind <- ddply(net, .(nets), summarise,
                 avg_abs = mean(abs(corr)),
                 sd_abs = sd(abs(corr)),
                 avg_dist = mean(dist));

avg_within_across <- function(net_name = "RdN", net_data = ind_all) {
  dat <- net_data
  avg_net_within <- mean(abs(dat$corr[dat[,net_name]==-1]))
  avg_net_across <- mean(abs(dat$corr[dat[,net_name]==1]))
  return(c(avg_net_within, avg_net_across));
}

fit_no_seed <- function(net_name = "RdN", seed_num, net_data = ind_inc) {
  dat <- net_data;
  dat <- subset(dat, dat$area1num != seed_num);
  dat <- subset(dat, dat$area2num != seed_num);
  no_seed <- fit_net(net_name, net_data);
  return(no_seed)
}


combo_rois <- c(14, 17, 18, 19, 20, 21, 22, 39, 40, 41, 	43, 44, 45, 46, 47, 48, 49, 52, 53, 54, 55, 56, 68, 70, 72, 74, 93, 94, 96, 97, 100)
rand_net <- function(number_of_rois, dataset = ind_all) {
  dat <- dataset;
  n_rois <- number_of_rois;
  roi_set <- sample(combo_rois, n_rois);
  new_net <- subset(dat, area1num%in%roi_set & area2num%in%roi_set);
  net_abs_avg <- mean(abs(new_net$corr));
  return(net_abs_avg)
}

sample_net <- function(number_of_rois, number_of_samples, dataset = ind_all) {
  dat <- dataset;
  n_rois <- number_of_rois;
  n_samp <- number_of_samples;
  sampset <- rep(NA, n_samp);
  for(i in 1:n_samp) {
    sampset[i] <- rand_net(n_rois, dat);
  }
  return(sampset);
}

avg_net <- ddply(subset(ind_all, dist > 0), .(nets), summarise,
                 avg_abs = mean(abs(corr)),
                 sd_abs = sd(abs(corr)),
                 avg_dist = mean(dist))
