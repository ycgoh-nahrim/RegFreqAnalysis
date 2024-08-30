
# load packages
library(lmomRFA)
library(lmom)

library(tidyverse)
library(openxlsx)
#library(gghighlight)

library(sf)

######################################
# Ref

# Hosking, J. R. M., & Wallis, J. R. (1997). 
# Regional Frequency Analysis: An Approach Based on L-Moments. Cambridge University Press.

# Hydrologic Engineering Center, Index Flood Regionalization Pt III & IV
# https://youtu.be/hjdnlbxrje8?si=0ZYkBqe_tuNBxLg7
# https://www.youtube.com/watch?v=8HkztqnptIA


######################################
# INPUT DATA


# set strings as factors to false
options(stringsAsFactors = FALSE)

#set working directory
setwd('J:/Backup_main/2024/20211221_Banjir_2021/Analysis/Aquarius1h_202404_KLSel/RFA3')



# add stations data
stn_df = read.csv("J:/Backup_main/2024/20211221_Banjir_2021/Data/DATA RHN JPS KL SEL 12 April 2022/TIDEDA_RF_stn_KLSel2.csv", 
                    header = T, sep = ",")

# INPUT
area_name <- "Kuala Lumpur - Selangor"
a_name <- "KLSel"
data_source <- ""
source_abb <- "Aq"
font_family <- "Roboto"
dur <- 1 #in hour


#####
# combine data


#from rainfall database
## actual input needed to calculate L-moments
## columns: 
### Stn_no  : int  
### Year    : int  
### Datetime: chr  
### Depth   : num  
### Date    : chr  
### Depthmax: num  
### Duration: int

rain_df1 <- read.csv("J:/Backup_main/2024/20211221_Banjir_2021/Analysis/Aquarius1h_202403_Sel2/Aq_RF_max_all_Sel.csv", 
                     header = T, sep = ",")

rain_df2 <- read.csv("J:/Backup_main/2024/20211221_Banjir_2021/Analysis/Aquarius1h_202401_KL/Aq_RF_max_all_KL.csv", 
                     header = T, sep = ",")


#combine
raindata_df <- rbind(rain_df1, rain_df2)

str(raindata_df)


#select duration and years (before event)
raindata_df_sel <- raindata_df %>% 
  filter(Duration == dur, Year < 2021)



# split into list for regsalmu
raindata_list <- split(raindata_df_sel$Depthmax, raindata_df_sel$Stn_no)


######################################
# L-MOMENTS


# regsamlmu
# Computes the "unbiased" sample L-moments and L-moment ratios of multiple sets of data stored
# in a list or matrix. Following the paradigm of regional frequency analysis, we regard the data sets
# as coming from different measurement sites.
## returns regdata object


#name Character vector: site identifier.
#n Numeric vector: record length.
#mean (l) Numeric vector: sample mean.
#t Numeric vector: sample L-CV.
#t_3 Numeric vector: sample L-skewness.
#t_4 Numeric vector: sample L-kurtosis.
#t_5 Numeric vector: sample L-moment ratio t5.


# regsalmu
# L-moments for each station
raindata_lm <- regsamlmu(raindata_list)


# get regdata format
### name      : int  
### n         : int  
### l_1       : num  
### t         : num  
### t_3       : num  
### t_4       : num  
### t_5       : num 

## remove stations with NA
regdata_rain <- na.omit(raindata_lm)

str(regdata_rain)

## format column from character to numeric
regdata_rain$name <- as.integer(as.character(regdata_rain$name))



#####
# join station list

### output format:
### name: int  
### Lat : num  
### Long: num  
### Elev: num  
### n   : int  
### l_1 : num  
### t   : num  
### t_3 : num  
### t_4 : num  
### t_5 : num  


## join data with station list
rain_stn_reg <- regdata_rain %>%
  left_join(stn_df, by = c("name" = "Stn_no")) 

str(rain_stn_reg)


## select relevant columns only
rain_stn_reg_sel <- rain_stn_reg[,c(1:7, 13:15)]


## reorder columns
rain_stn_reg_sel <- rain_stn_reg_sel[, c(1,9 ,8,10,2:7 )]

str(rain_stn_reg_sel)


#####

# L-moment ratio diagram
## L-kurtosis vs L- skewness

#print plot to file
png(file = paste0("RFA_", a_name, "_RF_", dur, "h_lmrd_all.jpg"), 
    res = 300, pointsize = 8,
    width = 5, height = 3, units = "in")

#plot
## lmrd got wrong columns for axis!
lmrd(rain_stn_reg_sel)

#close file
dev.off()


#####
# L-CV vs L-skewness
## H&W (1997), pg 178, Fig 9.11
## check for outliers, which station
## plot the outlier station(s)' annual max rainfall

#print plot to file
png(file = paste0("RFA_", a_name, "_RF_", dur, "h_lmrdcv_all.jpg"), 
    res = 300, pointsize = 8,
    width = 5, height = 3, units = "in")

#plot
plot(rain_stn_reg_sel$t_3, rain_stn_reg_sel$t, 
     pch = 20, col =  "#FF0000", cex = 0.5,
     xlab = "L-skewness", ylab = "L-CV")
text(rain_stn_reg_sel$t_3, rain_stn_reg_sel$t, labels = rain_stn_reg_sel$name, cex = 0.5)

#close file
dev.off()


#####
# check outlier station AMS

stn_no <- 3711001

gg_rf_stn <- raindata_df_sel %>% 
  #filter(Stn_no == stn_no) %>% 
  ggplot(aes(x = Year, y = Depthmax)) +
  geom_point(aes(colour = factor(Stn_no)), shape = 20, size = 1.5, alpha = 0.5, na.rm = T) +
  gghighlight(Stn_no == stn_no, use_group_by = FALSE) +
  theme_bw(base_size = 10) +
  scale_x_continuous(name= "Year", 
               minor_breaks = NULL) +
  scale_y_continuous(name= paste("Annual maximum rainfall (mm)"),
                     #breaks = seq(0, 1000, by = 20),
                     limits = c(0, NA),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = paste0("Annual maximum ", dur, "h rainfall"),
       subtitle = paste0(area_name, " stations: ", stn_no))

gg_rf_stn



#print last plot to file
ggsave(paste0("RFA_", a_name, "_RF_", dur, "h_", stn_no, "_check.jpg"), dpi = 300,
       width = 10, height = 5, units = "in")




######################################
# FORMATTING STATION DATA FOR RFA

# regtst
# Computes discordancy, heterogeneity and goodness-of-fit measures for regional frequency analysis.
# These are the statistics Di, H, and Z DIST defined respectively in sections 3.2.3, 4.3.3, and 5.2.3 of
# Hosking and Wallis (1997).
# An example from Hosking (1996). Compare the output with
# the file 'cascades.out' in the LMOMENTS Fortran package at
# http://lib.stat.cmu.edu/lmoments/general (results will not
# be identical, because random-number generators are different).

# uses output from regsamlmu (regdata)
# regsamlmu output format:  It should be a data frame,
# each of whose rows contains data for one site. The first seven columns should
# contain respectively the site name, record length and L-moments and L-moment
# ratios, in the order `1 (mean), t (L-CV), t3 (L-skewness), t4 (L-kurtosis), and t5.

# reproduce same output 
## does not work
#set.seed(000)


#get regdata format
#regdata_rain


# computes regtst
regtst_rain <- regtst(regdata_rain, nsim = 1000) 


##nsim does not affect value of D, only heterogeneity H and z values
##apparently every recalculation gives different H and z values even with same nsim


# discordancy for each site
## Table 3.1
## number of sites in region >= 15, D should be less than 3
## check the site data if D > 3



# check summary
## check flagged test values
## check best probability distributions in 
## PARAMETER ESTIMATES FOR DISTRIBUTIONS ACCEPTED AT THE 0.90 LEVEL
## Z-scores: less is better
## check also LMRD


# HETEROGENEITY (pg 63)
## H < 1, acceptably homogeneous
## 1 <= H < 2, possibly heterogeneous
## H >= 2, definitely heterogenous
##H1: std dev, weighted according to record length of at-site L-CV
##H2: avg distance from site coordinates to reg. avg on a plot of L-CV vs L-skew
##H3: avg distance from site coordinates to reg. avg on a plot of L-skew vs L-kurt


regtst_rain_summary <- summary(regtst_rain)
## check H1



# save output to txt
capture.output(regtst_rain_summary, 
               file = paste0("RFA_", a_name, "_RF_", dur, "h_regtst_summary_all.txt")) 



######################################
# FIND HOMOGENEOUS REGIONS
# group regions, preferably less than 20 sites per region (pg 180)
# reformulate regions if H too heterogeneous


# trial and error
## but based on rainfall characteristics or topography

# set current set of region's name 
reg_set <- "reg1"


# check region by district
RF_region <- rain_stn_reg %>% 
  group_by(DISTRICT) %>% 
  summarise(cnt = sum(!is.na(name)))


# check ranges
max(rain_stn_reg$Elev)
min(rain_stn_reg$Elev)
max(rain_stn_reg$Max)
min(rain_stn_reg$Max)
max(rain_stn_reg$n)
min(rain_stn_reg$n)


# find max of max rainfall for each station
rain_stn_reg_sel_max <- raindata_df_sel %>% 
  group_by(Stn_no) %>% 
  summarise(Max = max(Depthmax))

# join with regdata
rain_stn_reg <- rain_stn_reg %>% 
  inner_join(rain_stn_reg_sel_max, by = c("name" = "Stn_no"))



#####
# option 1
## group according to value ranges
levels <- c(-0.1, 50, 75, 100, 200, 1300) # elevation
levels <- c(50, 70, 90, 110, 130, 160) # max rainfall
labels <- c("A", "B", "C", "D", "E")
rain_stn_reg <- rain_stn_reg %>% 
  mutate(Region = cut(Elev, levels, labels = labels))

rain_stn_reg_sel_cat <- rain_stn_reg %>% 
  group_by(Region) %>% 
  summarise(n = sum(!is.na(Max)))



# option 2
## group according to names
rain_stn_reg <- rain_stn_reg %>% 
  mutate(Region = case_when(
    DISTRICT == "Wilayah Persekutuan" ~ "A",
    DISTRICT == "Bentong (pahang)" ~ "A",
    DISTRICT == "Gombak" ~ "A",
    DISTRICT == "Hulu Selangor" ~ "A",
    DISTRICT == "Kuala Langat" ~ "B",
    DISTRICT == "Sepang" ~ "B",
    DISTRICT == "Klang" ~ "C",
    DISTRICT == "Petaling" ~ "C",
    DISTRICT == "Hulu Langat" ~ "A",
    DISTRICT == "Sabak Bernam" ~ "D",
    DISTRICT == "Kuala Selangor" ~ "D",
    TRUE ~ "G")
  )


#####
# plot map to check regions

## set map format
my_sf <- st_as_sf(rain_stn_reg, coords = c('Long', 'Lat'),
                  crs = 4326)

##plot
gg_region <- ggplot(my_sf) + 
  #geom_sf(aes(color = DISTRICT))
  geom_sf(aes(color = Region))

gg_region


#print last plot to file
ggsave(paste0("RFA_", a_name, "_RF_map_", reg_set, ".jpg"), dpi = 300,
       width = 5, height = 4, units = "in")



##check regions numbers
RF_region <- rain_stn_reg %>% 
  group_by(Region) %>% 
  summarise(cnt = sum(!is.na(name)))



#####

#get regdata format by region

# get data in regdata format
rain_stn_reg2 <- rain_stn_reg[1:7]

# split regdata according to region
## not sure if can split 2 different df
regdata_rain_reg <- split(rain_stn_reg2, rain_stn_reg$Region)


#####
# computes regtst

regtst_rain_reg = list()
regtst_rain_reg_summary = list()

i = 1

for(i in 1:length(unique(rain_stn_reg$Region))){
  
  # computes regtst
  regtst_rain_each <- regtst(regdata_rain_reg[[i]], nsim = 1000)
  
  # check summary
  regtst_summary_each <- summary(regtst_rain_each)
  
  # keep result in list
  regtst_rain_reg[[i]] <- regtst_rain_each
  regtst_rain_reg_summary[[i]] <- regtst_summary_each
  
  # save output to txt
  capture.output(regtst_summary_each, 
                 file = paste0("RFA_", a_name, "_RF_", dur, 
                               "h_regtst_summary_", reg_set, "_", names(regdata_rain_reg)[i],
                               ".txt"))
}


######################################
# FIT (SINGLE) DISTRIBUTION FOR ALL REGIONS
# Chapter 9.2.4


# L-moment ratio diagram
# Fig 9.17
# L-kurtosis vs L-skewness
# Ref: https://www.youtube.com/watch?v=hjdnlbxrje8 (HEC - Index Flood Regionalization Pt III)



#print plot to file
png(file = paste0("RFA_", a_name, "_RF_", dur, "h_lmrd_all.jpg"), 
    res = 300, pointsize = 8,
    width = 5, height = 3, units = "in")

#plot
## correct plot compared to above
lmrd(regdata_rain)

#close file
dev.off()


#####
#print plot to file
png(file = paste0("RFA_", a_name, "_RF_", dur, "h_lmrdpt_all_stn.jpg"), 
    res = 300, pointsize = 8,
    width = 5, height = 3, units = "in")

# L-kurtosis vs L-skewness
plot(regdata_rain$t_3, regdata_rain$t_4, 
     #pch = as.integer(regdata_rain$name), col = as.integer(regdata_rain$name),
     pch = 19, col= "#FF0000", cex = 0.5,
     xlab = "L-skewness", ylab = "L-kurtosis")
text(regdata_rain$t_3, regdata_rain$t_4, labels = regdata_rain$name, cex = 0.5)

#close file
dev.off()


# fit distribution
# Fit a selected distribution to the regional data 
# after checking L-moment ratio diagram, choose best fit 
# GEV in this case
# returns object rfd, which contains the specification of the regional frequency distribution: 
# the quantile function, parameters of the regional growth curve, and the 
# index-flood values (site-specific scale factors) for each site.

# probability distributions
## exp: exponential
## gam: gamma
## gev: generalized extreme value
## glo: generalized logistic
## gpa: generalized Pareto
## gno: generalized normal
## gum: Gumbel (extreme-value type I)
## kap: kappa
## ln3: lognormal
## nor: normal
## pe3: Pearson type III
## wak: Wakeby
## wei: Weibull (EV3)


# check regtst for whole region
regtst_rain_summary 

# choose probability distibution
prob_dist <- "gev"

rfit_rain <- regfit(regdata_rain, prob_dist)

# Print details of the fitted distribution
# (components 'dist' and 'para')
rfit_rain

# Index flood values (for each region)
rfit_rain$index


# Plot the regional growth curve
## Ref: https://www.youtube.com/watch?v=hjdnlbxrje8 (HEC - Index Flood Regionalization Pt III)

evplot(rfit_rain) 



######################################
# FIT DISTRIBUTION TO EACH REGION



# filter according to cluster then remove cluster column
# must have station ID column with unique ID

#create list to store regfit lists for each region
rfit_rain_list = list()


#create dataframe to store defined quantiles for each region
## define probabilities
## non-exceedence probability
## reverse of exceedence probability (1-x)
prob_v <- c(0.01, 0.1, 0.2, 0.5, 0.8, 0.9, 0.98, 0.99, 0.995, 0.998, 0.999)
#AEP (%): 99, 90, 80, 50, 20, 10, 2, 1, 0.5, 0.2, 0.1 
#ARI (yr): 1.01, 1.11, 1.25, 2, 5, 10, 50, 100, 200, 500, 1000
#prob_v <- seq(0.1, 0.9, by = 0.1)

## Define column names
col_names <- c("Region", prob_v)

## Convert matrix to a data frame with column names
quan_reg <- data.frame(matrix(nrow = 0, ncol = length(col_names))) 
colnames(quan_reg) <- col_names



#loop for each region

i = 1

for (i in 1:nrow(RF_region)) {
  
  # region name
  region_name <- as.character(RF_region[i, 1])
  
  # filter regdata for each region
  regdata_rain_each <- regdata_rain_reg[[region_name]]
  
  
  # plot L-moment ratio diagram
  ## print plot to file
  png(file = paste0("RFA_", a_name, "_RF_", dur, "h_lmrd_", reg_set, "_", region_name, ".jpg"), 
      res = 300, pointsize = 8,
      width = 5, height = 3, units = "in")
  lmrd(regdata_rain_each)
  dev.off()
  
  
  
  # fit distribution for each region
  rfit_each <- regfit(regdata_rain_each, prob_dist)
  
  # store rfit list for each region into general list
  rfit_rain_list[[i]] <- rfit_each
  
  
  # calculate quantile for each region
  quan_each <- regquant(prob_v, rfit_each)
  quan_each <- round(quan_each, 5)
  
  # add each region quantile into dataframe
  quan_reg[nrow(quan_reg) + 1, ] <- c(region_name, quan_each)
  
  
  # plot regional growth curve
  png(file = paste0("RFA_", a_name, "_RF_", dur, "h_evplot_", reg_set, "_", region_name, ".jpg"), 
      res = 300, pointsize = 8,
      width = 5, height = 3, units = "in")
  evplot(rfit_each) 
  dev.off()
  
}




# Table 9.6
## regional growth curve quantiles for regions  
quan_reg


### pivot
quan_reg_pv <- pivot_longer(quan_reg, cols = c(2:ncol(quan_reg)), 
                            names_to = "Prob", values_to = "Quantile")

str(quan_reg_pv)

## format column from character to numeric
quan_reg_pv$Quantile <- as.numeric(as.character(quan_reg_pv$Quantile))
quan_reg_pv$Prob <- as.numeric(as.character(quan_reg_pv$Prob))




# export list of regfit parameters for all regions
## dist: distribution
## para: parameters of the fitted regional distribution
## qfunc: quantile function of distribution dist
## rmom: regional average L-moments
## index: index flood values at each site


library(yaml)

# write to yaml text file
write_yaml(rfit_rain_list, fileEncoding = "UTF-8",
           paste0("RFA_", a_name, "_RF_", dur, "h_rfit_", 
                  prob_dist, "_", reg_set, ".yaml"))


# save list in rds
saveRDS(rfit_rain_list, paste0("RFA_", a_name, "_RF_", dur, "h_rfit_", 
                               prob_dist, "_", reg_set, ".rds"))

# load list from rds
#rfit_rain_list2 <- readRDS(paste0("RFA_", a_name, "_RF_", dur, "h_rfit_", 
# prob_dist, "_", reg_set, ".rds"))

######################################
# REGIONAL GROWTH CURVE FOR EVERY REGION


#for log axis
breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each = 9))


# round y axis limit
#log_upper <- 10^(ceiling(log10(max(quan_reg_pv$Quantile)*1.1)))
#log_lower <- 10^(floor(log10(min(quan_reg_pv$Quantile)*0.9)))


# y axis limit
#y_upper <- max(quan_reg_pv$Quantile)*1.1
#y_lower <- min(quan_reg_pv$Quantile)*0.9

#AEP (%): 99, 90, 80, 50, 20, 10, 2, 1, 0.5, 0.2, 0.1

# plot 

gg_rgc <- quan_reg_pv %>%
  ggplot(aes(x = (1-Prob), y = Quantile, group = Region)) +
  geom_point(aes(x = (1-Prob), y = Quantile, color = factor(Region)),
             shape = 16, size = 0.5) +
  geom_line(aes(x = (1-Prob), y = Quantile, color = factor(Region)),
            linewidth = 0.5, alpha = 0.8) +
  theme_bw(base_size = 10) +
  scale_x_continuous(name = "Annual Exceedence Probability (AEP)",
                     trans = scales::transform_compose("log10","reverse"),
                     breaks = breaks,
                     minor_breaks = minor_breaks) +
  scale_y_continuous(name = "Regional growth factor",
                     #labels = comma,
                     limits = c(0, NA) #adjust limits accordingly
  ) +
  #scale_colour_manual("Region") +
  theme(text = element_text(family = font_family, 
                          color = "grey20", 
                          size = 10),
        axis.title = element_text(size = 8, colour = "grey35"),
        legend.margin = margin(0, 0, 0, 0),
        legend.position = "right") +
  labs(title = paste0("Regional Growth Curves for ", area_name, " Regions")) +
  guides(color = guide_legend("Region"))

gg_rgc


#print last plot to file
ggsave(paste0("RFA_", a_name, "_RF_", dur, "h_rgc_", reg_set, ".jpg"), dpi = 300,
       width = 7, height = 4, units = "in")

ggsave(paste0("RFA_", a_name, "_RF_", dur, "h_rgc_", reg_set, ".svg"), dpi = 300,
       width = 7, height = 4, units = "in")


######################################
# L-MOMENT RATIO DIAGRAM
## selection of regional probability distribution


# stations L-moments
rain_stn_reg

# region average L-moments



# extract L-moment ratio diagram data for selected distribution

## add lmrd.data
lmrd_dist_data = read.csv("J:/Backup_main/2024/20211221_Banjir_2021/Analysis/Aquarius1h_202404_KLSel/RFA3/lmrd_data.csv", 
                  header = T, sep = ",")

## select distribution from lmrd.data
lmrd_dist <- data.frame(tau_3 = lmrd_dist_data[, 1],
                        dist = lmrd_dist_data[, toupper(prob_dist)])


i = 1

for (i in 1:nrow(RF_region)) {

  region_name <- as.character(RF_region[i, 1])
  
  
  gg_lmrd <- rain_stn_reg %>%
    filter(Region == region_name) %>% 
    ggplot() +
    geom_point(aes(x = t_3, y = t_4), color = "red",
               shape = 16, size = 1, alpha = 0.7) +
    #geom_point(data = lmrd_dist, aes(x = tau_3, y = dist), color = "blue",
    #           shape = 16, size = 1, alpha = 0.7) +
    stat_smooth(data = lmrd_dist, aes(x = tau_3, y = dist), geom = "line", alpha = 0.5,
                color = 'blue', linewidth = 1) +
    theme_bw(base_size = 10) +
    scale_x_continuous(name = "L-skewness",
                       limits = c(min(rain_stn_reg$t_3), max(rain_stn_reg$t_3)),
                       #breaks = "5 year",
                       #expand = c(0, 0),
                       minor_breaks = NULL) +
    scale_y_continuous(name = "L-kurtosis",
                       #breaks = seq(0, 1000, by = 20),
                       limits = c(min(rain_stn_reg$t_4), max(rain_stn_reg$t_4)),
                       minor_breaks = NULL) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    theme(text = element_text(family = font_family, 
                            color = "grey20", 
                            size = 10),
          axis.title = element_text(size = 8, colour = "gray35"),
          legend.position = "none") +
    labs(title = paste0("L-Moment Ratio Diagram"),
         subtitle = paste0(area_name, ": Region ", region_name))
  
  #gg_lmrd
  
  
  ##print last plot to file
  ggsave(paste0("RFA_", a_name, "_RF_", dur, "h_lmrdc_", reg_set, "_", region_name, ".jpg"), 
         dpi = 300, width = 7, height = 4, units = "in")

}


######################################
# EXPORT TO XLSX



# Create a blank workbook
wb <- createWorkbook()


# Convert nested list to data frame
rfit_para_reg <- data.frame()
rfit_rgc_reg <- data.frame()
rfit_index_reg <- data.frame()
regtst_D_reg <- data.frame()
regtst_Z_reg <- data.frame()


# set regtst dataframe

## heterogeneity
regtst_reg_all <- data.frame(regtst_rain_summary$H)
H_all <- c("H1", "H2", "H3")
regtst_reg_all <- cbind(H_all, regtst_reg_all)
colnames(regtst_reg_all) <- c("Heterogeneity", "Value")
regtst_reg_all$Region <- "All"


  

#loop for each region

i = 1

for (i in 1:nrow(RF_region)) {
  
  # region name
  region_name <- as.character(RF_region[i, 1])
  
  ## get the list name
  #regtst_list_each <- get(paste0("regtst_rain_", region_name, "_summary"))
  regtst_list_each <- regtst_rain_reg_summary[[i]]
  
  
  # HETEROGENEITY
  ## combine with H
  regtst_H_each <- cbind(H_all, data.frame(regtst_list_each$H))
  colnames(regtst_H_each) <- c("Heterogeneity", "Value")
  ## add region
  regtst_H_each$Region <- region_name
  ## add each region para into dataframe
  regtst_reg_all <- rbind(regtst_reg_all, regtst_H_each)
  
  
  # DISCORDANCY
  ## get dataframe
  regtst_D_each <- data.frame(regtst_list_each$D)
  regtst_D_each <- rownames_to_column(regtst_D_each, "Stn_no")
  colnames(regtst_D_each)[2] <- "Discordancy"
  ## add region
  regtst_D_each$Region <- region_name
  ## add each region para into dataframe
  regtst_D_reg <- rbind(regtst_D_reg, regtst_D_each)
  
  
  
  # Z-STATISTIC
  regtst_Z_each <- data.frame(regtst_list_each$Z)
  regtst_Z_each <- rownames_to_column(regtst_Z_each, "Dist")
  colnames(regtst_Z_each)[2] <- "Z"
  ## add region
  regtst_Z_each$Region <- region_name
  ## add each region para into dataframe
  regtst_Z_reg <- rbind(regtst_Z_reg, regtst_Z_each)
  

  # DISTRIBUTION PARAMETERS
  rfit_para_each <- as.data.frame(rfit_rain_list[[i]]$para)
  rfit_para_each <- rownames_to_column(rfit_para_each, "Para")
  colnames(rfit_para_each)[2] <- "Value"
  ## add region
  rfit_para_each$Region <- region_name
  ## add each region para into dataframe
  rfit_para_reg <- rbind(rfit_para_reg, rfit_para_each)
  
  
  
  # REGIONAL GROWTH CURVE (RGC) (regional average L-moments)
  rfit_rgc_each <- as.data.frame(rfit_rain_list[[i]]$rmom)
  rfit_rgc_each <- rownames_to_column(rfit_rgc_each, "Para")
  colnames(rfit_rgc_each)[2] <- "Value"
  ## add region
  rfit_rgc_each$Region <- region_name
  ## add each region lmom into dataframe
  rfit_rgc_reg <- rbind(rfit_rgc_reg, rfit_rgc_each)
  
  
  
  # INDEX
  rfit_index_each <- as.data.frame(rfit_rain_list[[i]]$index)
  rfit_index_each <- rownames_to_column(rfit_index_each, "Stn_no")
  colnames(rfit_index_each)[2] <- "Index"
  ## add region
  rfit_index_each$Region <- region_name
  ## add each region index into dataframe
  rfit_index_reg <- rbind(rfit_index_reg, rfit_index_each)
  
  
}


## add duration for each df
rain_stn_reg3 <- rain_stn_reg[, c(1:7, 16:17)]
rain_stn_reg3$Duration <- dur 
regtst_D_reg$Duration <- dur
regtst_reg_all$Duration <- dur
regtst_Z_reg$Duration <- dur
rfit_para_reg$Duration <- dur
rfit_rgc_reg$Duration <- dur
rfit_index_reg$Duration <- dur
quan_reg_pv$Duration <- dur



#list all dataframe
list_worksheet <- list("Region" = RF_region,
                       "Stn" = rain_stn_reg3,
                       "Disc" = regtst_D_reg,
                       "Het" = regtst_reg_all,
                       "Z" = regtst_Z_reg,
                       "Para" = rfit_para_reg,
                       "RGC" = rfit_rgc_reg,
                       "Index" = rfit_index_reg,
                       "Quantile" = quan_reg_pv,
                       "LMRD" = ""
                       )



# Loop through the list of split tables as well as their names
#   and add each one as a sheet to the workbook
Map(function(data, name){
  addWorksheet(wb, name)
  writeData(wb, name, data)
}, list_worksheet, names(list_worksheet))



#insert image
## region map
insertImage(wb, "Region", paste0("RFA_", a_name, "_RF_map_", reg_set, ".jpg"),  
            width = 5, height = 4, units = "in", 
            startRow = nrow(RF_region) + 3, startCol = 2)

## RGC all regions
insertImage(wb, "RGC", paste0("RFA_", a_name, "_RF_", dur, "h_rgc_", reg_set, ".jpg"),  
            width = 7, height = 4, units = "in", 
            startRow = 2, startCol = 15)



## L-moment ratio diagrams for each region
i = 1

for (i in 1:nrow(RF_region)) {
  
  # region name
  region_name <- as.character(RF_region[i, 1])
  
  # LMRD
  insertImage(wb, "LMRD", paste0("RFA_", a_name, "_RF_", dur, "h_lmrd_", 
                                 reg_set, "_", region_name, ".jpg"),  
              width = 5, height = 4, units = "in", 
              startRow = (i-1)*23 + 2, startCol = 2)
  
  # LMRD - single distribution
  insertImage(wb, "LMRD", paste0("RFA_", a_name, "_RF_", dur, "h_lmrdc_", 
                                 reg_set, "_", region_name, ".jpg"),  
              width = 7, height = 4, units = "in", 
              startRow = (i-1)*23 + 2, startCol = 10)
  
  # Regional growth curve
  insertImage(wb, "RGC", paste0("RFA_", a_name, "_RF_", dur, "h_evplot_", 
                                 reg_set, "_", region_name, ".jpg"),  
              width = 5, height = 4, units = "in", 
              startRow = (i-1)*23 + 2, startCol = 7)
  
}



# Save workbook to working directory
saveWorkbook(wb, file = paste0("RFA_", a_name, "_RF_", dur, "h_", 
                               prob_dist,"_", reg_set,".xlsx"), 
             overwrite = TRUE)

