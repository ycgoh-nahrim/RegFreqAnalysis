
# load packages
library(lmomRFA)
library(lmom)

library(tidyverse)

library(sf)

######################################
# INPUT DATA


# set strings as factors to false
options(stringsAsFactors = FALSE)

#set working directory
setwd('J:/Backup_main/2024/20211221_Banjir_2021/Analysis/Aquarius1h_202404_KLSel')



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


# regsalmu
raindata_lm <- regsamlmu(raindata_list)


# get regdata format
## remove stations with NA
regdata_rain <- na.omit(raindata_lm)

str(regdata_rain)

## format column from character to numeric
regdata_rain$name <- as.integer(as.character(regdata_rain$name))




#####
# join station list


### join data with station list
rain_stn_reg <- regdata_rain %>%
  left_join(stn_df, by = c("name" = "Stn_no")) 

str(rain_stn_reg)


### select relevant columns only
rain_stn_reg_sel <- rain_stn_reg[,c(1:7, 13:15)]


### reorder columns
rain_stn_reg_sel <- rain_stn_reg_sel[, c(1,9 ,8,10,2:7 )]



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



# L-moment ratio diagram
lmrd(rain_stn_reg_sel)



# L-moment ratios 
## H&W (1997), pg 178, Fig 9.11

plot(rain_stn_reg_sel$t_3, rain_stn_reg_sel$t, 
     xlab = "L-skewness", ylab = "L-CV")


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
# each of whose rows contains data for one site. The rst seven columns should
# contain respectively the site name, record length and L-moments and L-moment
# ratios, in the order `1 (mean), t (L-CV), t3 (L-skewness), t4 (L-kurtosis), and t5.

# reproduce same output 
## does not work
#set.seed(000)


#get regdata format
regdata_rain


# computes regtst
regtst_rain <- regtst(regdata_rain, nsim = 1000) 


##nsim does not affect value of D, only heterogeneity H and z values
##apparently every recalculation gives different H and z values even with same nsim


# discordancy for each site
regtst_rain_Discordancy <- data.frame(regtst_rain[["D"]]) 
regtst_rain_Discordancy <- rownames_to_column(regtst_rain_Discordancy, "Stn_no")
colnames(regtst_rain_Discordancy)[2] <- "Discordancy"




# check summary
## check flagged test values
## check best probability distributions in 
## PARAMETER ESTIMATES FOR DISTRIBUTIONS ACCEPTED AT THE 0.90 LEVEL

# HETEROGENEITY (pg 63)
## H < 1, acceptably homogeneous
## 1 <= H < 2, possibly heterogeneous
## H >= 2, definitely heterogenous
##H1: std dev, weighted according to record length of at-site L-CV
##H2: avg distance from site coordinates to reg. avg on a plot of L-CV vs L-skew
##H3: avg distance from site coordinates to reg. avg on a plot of L-skew vs L-kurt


regtst_rain_summary <- summary(regtst_rain)
##H1 = 1.85
##possibly heterogeneous


# save output to txt
capture.output(regtst_rain_summary, 
               file = paste0(source_abb, "_RF_", dur, "h_RFA_", a_name, "_regtst_summary.txt")) 



######################################
# FIND HOMOGENEOUS REGIONS
# group regions, preferably less than 20 sites per region (pg 180)


# trial and error
## but based on rainfall characteristics or topography


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


# find max for each station
rain_stn_reg_sel_max <- raindata_df_sel %>% 
  group_by(Stn_no) %>% 
  summarise(Max = max(Depthmax))

# join with regdata
rain_stn_reg <- rain_stn_reg %>% 
  inner_join(rain_stn_reg_sel_max, by = c("name" = "Stn_no"))


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
ggsave(paste0(source_abb, "_RF_RFA_", a_name, "_map_reg1.jpg"), dpi = 300,
       width = 5, height = 4, units = "in")



##check regions numbers
RF_region <- rain_stn_reg %>% 
  group_by(Region) %>% 
  summarise(cnt = sum(!is.na(name)))



# BY REGION

#get regdata format

regdata_rain_A <- rain_stn_reg %>% 
  filter(Region == "A") %>% 
  select(1:7)

regdata_rain_B <- rain_stn_reg %>% 
  filter(Region == "B") %>% 
  select(1:7)

regdata_rain_C <- rain_stn_reg %>% 
  filter(Region == "C") %>% 
  select(1:7)

regdata_rain_D <- rain_stn_reg %>% 
  filter(Region == "D") %>% 
  select(1:7)



# computes regtst
regtst_rain_A <- regtst(regdata_rain_A, nsim = 1000)
regtst_rain_B <- regtst(regdata_rain_B, nsim = 1000)
regtst_rain_C <- regtst(regdata_rain_C, nsim = 1000)
regtst_rain_D <- regtst(regdata_rain_D, nsim = 1000)



# check summary
regtst_rain_A_summary <- summary(regtst_rain_A) 
regtst_rain_B_summary <- summary(regtst_rain_B) 
regtst_rain_C_summary <- summary(regtst_rain_C) 
regtst_rain_D_summary <- summary(regtst_rain_D) 


# save output to txt
capture.output(regtst_rain_A_summary, 
               file = paste0(source_abb, "_RF_", dur, "h_RFA_", a_name, "_regtst_rain_A_summary.txt"))
capture.output(regtst_rain_B_summary, 
               file = paste0(source_abb, "_RF_", dur, "h_RFA_", a_name, "_regtst_rain_B_summary.txt"))
capture.output(regtst_rain_C_summary, 
               file = paste0(source_abb, "_RF_", dur, "h_RFA_", a_name, "_regtst_rain_C_summary.txt"))
capture.output(regtst_rain_D_summary, 
               file = paste0(source_abb, "_RF_", dur, "h_RFA_", a_name, "_regtst_rain_D_summary.txt"))



######################################
# FIT (SINGLE) DISTRIBUTION FOR ALL REGIONS
# Chapter 9.2.4


# L-moment ratio diagram
# Fig 9.17
lmrd(regdata_rain)

plot(regdata_rain$t_3, regdata_rain$t_4, 
     #pch = as.integer(regdata_rain$name), col = as.integer(regdata_rain$name),
     pch = 19, col= "blue", cex = 0.5,
     xlab = "L-skewness", ylab = "L-kurtosis")
text(regdata_rain$t_3, regdata_rain$t_4, labels = regdata_rain$name, cex = 0.7)



# fit distribution
# Fit a selected distribution to the regional data 
# after checking L-moment ratio diagram, choose best fit 
# GEV in this case
# returns object rfd, which contains the specification of the regional frequency distribution: 
# the quantile function, parameters of the regional growth curve, and the 
# index-flood values (site-specific scale factors) for  each site.
rfit_rain <- regfit(regdata_rain, "gev")

# Print details of the fitted distribution
# (components 'dist' and 'para')
rfit_rain

# Index flood values (for each region)
rfit_rain$index

# Plot the regional growth curve
evplot(rfit_rain) 



######################################
# FIT DISTRIBUTION TO EACH REGION


#select columns, include region column
regdata_rain_cluster <- rain_stn_reg[, c(1:7, 17)] 

#reorder by column index
regdata_rain_cluster <- regdata_rain_cluster[, c(8, 1:7)] 



# filter according to cluster then remove cluster column
# must have station ID column with unique ID

#create list to store regfit lists for each region
rfit_rain_list = list()


#create dataframe to store defined quantiles for each region
## define quantiles
quan_sel <- c(0.01, 0.1, 0.2, 0.5, 0.8, 0.9, 0.98, 0.99, 0.998, 0.999)
#quan_sel <- seq(0.1, 0.9, by = 0.1)

## Define column names
col_names <- c("Region", quan_sel)

## Convert matrix to a data frame with column names
quan_reg <- data.frame(matrix(nrow = 0, ncol = length(col_names))) 
colnames(quan_reg) <- col_names



#loop for each region

i = 1

for (i in 1:nrow(RF_region)) {
  
  # region name
  region_name <- as.character(RF_region[i, 1])
  
  # filter regdata for each region
  reg_cluster_df_each <- regdata_rain_cluster[regdata_rain_cluster$Region == region_name, 2:8]
  
  # fit distribution for each region
  rfit_each <- regfit(reg_cluster_df_each, "gev")
  
  # store rfit list for each region into general list
  rfit_rain_list[[i]] <- rfit_each
  
  
  # calculate quantile for each region
  quan_each <- regquant(quan_sel, rfit_rain_list[[i]])
  quan_each <- round(quan_each, 2)
  
  # add each region quantile into dataframe
  quan_reg[nrow(quan_reg) + 1, ] <- c(region_name, quan_each)
  
}



# Table 9.6
## regional growth curve quantiles for regions in RF_region 
quan_reg

### pivot
quan_reg_pv <- pivot_longer(quan_reg, cols = c(2:11), names_to = "Quantile", values_to = "Value")




# export list of regfit parameters for all regions
## dist: distribution
## para: parameters of the fitted regional distribution
## qfunc: quantile function of distribution dist
## rmom: regional average L-moments
## index: index flood values at each site


library(yaml)

# write to yaml text file
write_yaml(rfit_rain_list, fileEncoding = "UTF-8",
           paste0(source_abb, "_RF_", dur, "h_RFA_", a_name, "_rfit_gev_reg1.yaml"))


# save list in rds
saveRDS(rfit_rain_list, paste0(source_abb, "_RF_", dur, "h_RFA_", a_name, "_rfit_gev_reg1.rds"))

# load list from rds
rfit_rain_list2 <- readRDS(paste0(source_abb, "_RF_", dur, "h_RFA_", a_name, "_rfit_gev_reg1.rds"))



######################################
# EXPORT TO XLSX

library(openxlsx)

# Create a blank workbook
wb <- createWorkbook()


# Convert nested list to data frame
rfit_para_reg <- data.frame()
rfit_rmom_reg <- data.frame()
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
  regtst_list_each <- get(paste0("regtst_rain_", region_name, "_summary"))
  
  
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
  
  
  
  # L-MOMENT
  rfit_rmom_each <- as.data.frame(rfit_rain_list[[i]]$rmom)
  rfit_rmom_each <- rownames_to_column(rfit_rmom_each, "Para")
  colnames(rfit_rmom_each)[2] <- "Value"
  ## add region
  rfit_rmom_each$Region <- region_name
  ## add each region lmom into dataframe
  rfit_rmom_reg <- rbind(rfit_rmom_reg, rfit_rmom_each)
  
  
  
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
regtst_D_reg$Duration <- dur
regtst_reg_all$Duration <- dur
regtst_Z_reg$Duration <- dur
rfit_para_reg$Duration <- dur
rfit_rmom_reg$Duration <- dur
rfit_index_reg$Duration <- dur
quan_reg_pv$Duration <- dur



#list all dataframe
list_worksheet <- list("Region" = RF_region,
                       "Stn" = rain_stn_reg,
                       "Disc" = regtst_D_reg,
                       "Het" = regtst_reg_all,
                       "Z" = regtst_Z_reg,
                       "Para" = rfit_para_reg,
                       "Lmom" = rfit_rmom_reg,
                       "Index" = rfit_index_reg,
                       "Quantile" = quan_reg_pv
                       )



# Loop through the list of split tables as well as their names
#   and add each one as a sheet to the workbook
Map(function(data, name){
  addWorksheet(wb, name)
  writeData(wb, name, data)
}, list_worksheet, names(list_worksheet))



#insert image
## region map
insertImage(wb, "Region", paste0(source_abb, "_RF_RFA_", a_name, "_map_reg1.jpg"),  
            width = 5, height = 4, units = "in", 
            startRow = nrow(RF_region) + 3, startCol = 2)



# Save workbook to working directory
saveWorkbook(wb, file = paste0(source_abb, "_RF_", dur, "h_RFA_", a_name, "_rfit_gev_reg1.xlsx"), 
             overwrite = TRUE)

