setwd("~/GitHub/case_study")
library(readxl)
library(dplyr)
library(ggplot2)
library(purrr)
library(lubridate)
library(stringr)
library(tidyr)
library(data.table)

# Read the data in to R
dataset_resale <- read_excel("~/GitHub/case_study/Housing_Resale_Data_1.xlsx")


# Dimension
dim(dataset_resale) #477833     10
# View few first and last rows 
head(dataset_resale)
tail(dataset_resale)
# Variables in the dataset
names(dataset_resale) # 10 variables
# [1] "month"               "town"                "flat_type"           "block"              
# [5] "street_name"         "storey_range"        "floor_area_sqm"      "flat_model"         
# [9] "lease_commence_date" "resale_price"   

# Save response variable to target
target <- dataset_resale$resale_price
#Check for missing values
colSums(is.na(dataset_resale))# no nissing values

#View structure of data
glimpse(dataset_resale)
#summary of data
summary(dataset_resale)

#Check each variable one by one

# 1)----------month Variable-----------------
length(unique(dataset_resale$month))#215
# Seperate month variable into year and month_num
dataset_resale <- separate(data = dataset_resale, col = month, 
                           into = c("year", "month_num"), sep = "-", remove = FALSE)


#--------------Let's start our data analysis-------------------------------

# Barplot for year variable
ggplot(dataset_resale, aes(x = year , fill = factor(flat_type))) + 
  geom_bar() + theme(axis.text.x = element_text(angle = 60,hjust = 1,size = 10)) + 
  labs(x = "Year") +
  ggtitle("flat count by year by flat_type") + 
  theme(legend.direction = "horizontal", legend.position = "bottom")
#Number of flats available for sale has decreased in recent years
#Since the data is given monthly we will create a new variable with number of 
#flats by flat type for each month of the year
#----------------------------------------------------------------------------
# Calculate flat count per month for each year by flat_type
flat_cnt_month <- dataset_resale %>% 
  group_by(month, flat_type) %>% summarise(flat_cnt_month = n())
# Add flat_cnt_month to dataset
dataset_resale <- left_join(dataset_resale, flat_cnt_month, by = c("month", "flat_type"))
#------------------------------------------------------------------------------
#View dataset_resale
glimpse(dataset_resale)

# Year with resale price
ggplot(dataset_resale, aes(x = year, y  = resale_price, color = factor(flat_type), group = factor(flat_type), fill = factor(flat_type))) + 
  stat_summary(fun.y = mean, geom = "line") + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "ribbon", col = NA, alpha = 0.2) + 
  theme(axis.text.x = element_text(angle  = 70,hjust = 1,size = 6)) + 
  ggtitle("resale price by year by flat_type") + 
  theme(legend.direction = "horizontal", legend.position = "bottom")

#Prices of all flat type have increased gradually till 2013 then and 
#dropped a bit
#1 ROOM is the lowest and for Executive and Multi Generation the price 
#is highest'

#Barplot for lease commencement date 
ggplot(dataset_resale, aes(x = lease_commence_date , fill = factor(flat_type))) + 
  geom_bar() + theme(axis.text.x = element_text(angle = 60,hjust = 1,size = 10)) + 
  labs(x = "lease_commence_date") +
  ggtitle("flat count by lease_commence_date by flat_type") + 
  theme(legend.direction = "horizontal", legend.position = "bottom")
#New lease commencement has reduced from 1990 to 1995 and then from 2002 onwards
#Also lease commence date for different flat type are different most new lease 
#commence flats are larger in size.
#----------------------------------------------------------------------------
#Create a new variable of flat count by lease_commence_date by flat type
#for each month of the years 
flat_cnt_month_lease <- dataset_resale %>% 
  group_by(month, lease_commence_date, flat_type) %>% 
  summarise(flat_cnt_month_lease = n())
# Add flat_cnt_month to dataset
dataset_resale <- left_join(dataset_resale, flat_cnt_month_lease, 
                            by = c("month", "lease_commence_date", "flat_type"))
#View dataset
glimpse(dataset_resale)

#Create a categorical variable based on lease_commence_date
dataset_resale$condition <- rep(0, times = nrow(dataset_resale))
dataset_resale$condition <- ifelse(dataset_resale$lease_commence_date <= 1980, "old", dataset_resale$condition)
dataset_resale$condition <- ifelse(dataset_resale$lease_commence_date > 1980 & dataset_resale$lease_commence_date <= 2000 , "medium", dataset_resale$condition)
dataset_resale$condition <- ifelse(dataset_resale$lease_commence_date > 2000, "new", dataset_resale$condition)

#View dataset
glimpse(dataset_resale)
unique(dataset_resale$condition)
#Convert condition into factor variable
dataset_resale$condition <- as.factor(dataset_resale$condition)

#-----------------------------------------------------------------------------------
# lease_commence_date with resale price
ggplot(dataset_resale, aes(x = lease_commence_date, y  = resale_price, color = factor(flat_type), group = factor(flat_type), fill = factor(flat_type))) + 
  stat_summary(fun.y = mean, geom = "line") + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "ribbon", col = NA, alpha = 0.2) + 
  theme(axis.text.x = element_text(angle  = 70,hjust = 1,size = 6)) + 
  ggtitle("resale_price by lease_commence_date") + 
  theme(legend.direction = "horizontal", legend.position = "bottom")
#Newly leased flats have higher price

#floor_area_sqm vs year
ggplot(dataset_resale, aes(x = year, y = floor_area_sqm, color = factor(flat_type))) +
  geom_point(position = "jitter", alpha = 0.5) + 
  theme(axis.text.x = element_text(angle  = 70,hjust = 1,size = 8)) + 
  ggtitle("floor_area by year") + 
  theme(legend.direction = "horizontal", legend.position = "bottom")
#Almost all flat type have area evenly distributed except for 3 ROOM which 
#have quite a good number of outliers.

#floor_area_sqm vs lease_commence_date
ggplot(dataset_resale, aes(x = lease_commence_date, y = floor_area_sqm, color = factor(flat_type))) + 
  geom_point(position = "jitter", alpha = 0.5) + 
  theme(axis.text.x = element_text(angle  = 70,hjust = 1,size = 6)) + 
  ggtitle("floor_area by lease_commence_date") + 
  theme(legend.direction = "horizontal", legend.position = "bottom")
#There are lot of outliers in a single year for 3 ROOM . Let's plot it with 
#flat_model for that year to see which model of flats are in these outliers

dataset_resale %>% filter(lease_commence_date == "1972" & flat_type == "3 ROOM") %>%
  ggplot(aes(factor(month_num), floor_area_sqm, color = factor(flat_model))) + 
  geom_point(position = "jitter", alpha = 0.8) +
  labs(x = "month in 1972") + 
  theme(axis.text.x = element_text(angle  = 60,hjust = 1,size = 10)) + 
  ggtitle("floor_area_sqm vs lease_commence_date in 1972")
# outliers are evenly spread for the whole year 3 ROOM terrace flats are 
#outliers in 1972

# Let's find other lease commence year with Terrace model flat
dataset_resale %>% 
  filter(lease_commence_date %in% c("1968", "1972") & flat_type == "3 ROOM") %>%
  select(lease_commence_date) %>% unique()
#Similar flats are present in 1968

#plot floor area for lease years 1968 and 1972
dataset_resale %>% filter(lease_commence_date %in% c("1968", "1972") & flat_type == "3 ROOM") %>%
  ggplot(aes(factor(month_num), floor_area_sqm, color = factor(flat_model))) + 
  geom_point(position = "jitter", alpha = 0.5) +
  labs(x = "month in 1968 & 1972") + 
  theme(axis.text.x = element_text(angle  = 60,hjust = 1,size = 10)) + 
  facet_wrap(~lease_commence_date) + 
  theme(legend.direction = "horizontal", legend.position = "bottom")

# Terrace flats have way  more area in 1972 than 1968

# Let's compare the price
dataset_resale %>% filter(lease_commence_date %in% c("1968", "1972") & flat_type == "3 ROOM") %>%
  ggplot(aes(floor_area_sqm, resale_price, color = factor(flat_model))) + 
  geom_point(position = "jitter", alpha = 0.7) + 
  theme(axis.text.x = element_text(angle  = 60,hjust = 1,size = 10)) + 
  facet_wrap(~lease_commence_date) + 
  theme(legend.direction = "horizontal", legend.position = "bottom")
#Prices are almost same with few outliers

# Let's reduce the size of terrace flat to median size of terrace flats
dataset_resale %>% filter(lease_commence_date == "1972" & flat_type == "3 ROOM") %>%
  group_by(flat_model) %>% summarise(avg_size = median(floor_area_sqm))
# median size is 100 sq.m
# compute terrace to find outliers in the variable
terrace <- dataset_resale %>% 
  filter(lease_commence_date == "1972" & flat_type == "3 ROOM" & flat_model == "Terrace") %>% 
  select(flat_model, floor_area_sqm)
terrace_1968 <- dataset_resale %>% 
  filter(lease_commence_date == "1968" & flat_type == "3 ROOM" & flat_model == "Terrace") %>% 
  select(flat_model, floor_area_sqm)


#Replace the values greater than mean of Terrace flat floor area with the median of floor_area of terrace
dataset_resale$floor_area_sqm[dataset_resale$lease_commence_date == "1972" & 
                                dataset_resale$flat_type == "3 ROOM" & 
                                dataset_resale$flat_model == "Terrace"] <- ifelse(dataset_resale$floor_area_sqm[dataset_resale$lease_commence_date == "1972" & 
                                                                                                                  dataset_resale$flat_type == "3 ROOM" & 
                                                                                                                  dataset_resale$flat_model == "Terrace"] >= mean(terrace$floor_area_sqm), median(terrace$floor_area_sqm), dataset_resale$floor_area_sqm[dataset_resale$lease_commence_date == "1972" & 
                                                                                                                                                                                                                                                           dataset_resale$flat_type == "3 ROOM" & 
                                                                                                                                                                                                                                                           dataset_resale$flat_model == "Terrace"])

#Again plot floor vs lease_commencement date                                                                                                                                                                                                                                                                             
dataset_resale %>% filter(lease_commence_date %in% c("1968", "1972") & flat_type == "3 ROOM") %>%
  ggplot(aes(factor(month_num), floor_area_sqm, color = factor(flat_model))) + 
  geom_point(position = "jitter", alpha = 0.5) +
  labs(x = "month in 1968 & 1972") + 
  theme(axis.text.x = element_text(angle  = 60,hjust = 1,size = 10)) + 
  facet_wrap(~lease_commence_date) + 
  theme(legend.direction = "horizontal", legend.position = "bottom")





#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------

#--------------- flat model variable-------------------------
length(unique(dataset_resale$flat_model))#20
unique(dataset_resale$flat_model)
# [1] "New Generation"         "Improved"               "Model A"               
# [4] "Standard"               "Apartment"              "Model A-Maisonette"    
# [7] "Maisonette"             "Simplified"             "Multi Generation"      
# [10] "Adjoined flat"          "Premium Apartment"      "Terrace"               
# [13] "Improved-Maisonette"    "Premium Maisonette"     "2-room"                
# [16] "Model A2"               "DBSS"                   "Type S1"               
# [19] "Type S2"                "Premium Apartment Loft"
sort(round(prop.table(table(dataset_resale$flat_model))*100,4))
# many flat model percentage are very less they can  be grouped 

#Grouping flat_model with less number into new groups  
dataset_resale$flat_model[dataset_resale$flat_model =="2-room"] <- "others_old"
dataset_resale$flat_model[dataset_resale$flat_model =="DBSS"] <- "others_new"
dataset_resale$flat_model[dataset_resale$flat_model =="Improved-Maisonette" & 
                            dataset_resale$lease_commence_date == "1983"] <- "others_old"
dataset_resale$flat_model[dataset_resale$flat_model =="Improved-Maisonette" & 
                            dataset_resale$lease_commence_date == "1997"] <- "others_new"
dataset_resale$flat_model[dataset_resale$flat_model =="Model A-Maisonette"] <- "others_old"
dataset_resale$flat_model[dataset_resale$flat_model =="Model A2"] <- "others_new"
dataset_resale$flat_model[dataset_resale$flat_model =="Multi Generation"] <- "others_old"
dataset_resale$flat_model[dataset_resale$flat_model =="Premium Apartment Loft"] <- "others_new"
dataset_resale$flat_model[dataset_resale$flat_model =="Premium Maisonette"] <- "others_new"
dataset_resale$flat_model[dataset_resale$flat_model =="Terrace"] <- "others_old"
dataset_resale$flat_model[dataset_resale$flat_model =="Type S1"] <- "others_new"
dataset_resale$flat_model[dataset_resale$flat_model =="Type S2"] <- "others_new"
dataset_resale$flat_model[dataset_resale$flat_model =="Adjoined flat"] <- "others_old"


sort(round(prop.table(table(dataset_resale$flat_model))*100,4))


# lets see distribution of flat_model with flat_type
ggplot(dataset_resale, aes(x = flat_model, fill = factor(flat_type))) + geom_bar() + 
  theme(axis.text.x = element_text(angle  = 50,hjust = 1,size = 10)) + 
  ggtitle("Distribution of flats by Flat_model variable") + 
  theme(legend.direction = "horizontal", legend.position = "bottom")

# Histogram of resale price by flat model
ggplot(dataset_resale, aes(resale_price, fill = factor(flat_model))) + 
  geom_histogram(position = "dodge") + 
  ggtitle("resale_price by Flat_model variable") + 
  theme(legend.direction = "horizontal", legend.position = "none")
# no definite pattern by model and prices are slightly right skewed  

#-----------------------------------------------------------------------------
## Calculate flat count per month for each year by flat_model
flat_cnt_month_model <- dataset_resale %>% 
  group_by(month, flat_model) %>% summarise(flat_cnt_month_model = n())
# Add flat_cnt_month to dataset
dataset_resale <- left_join(dataset_resale, flat_cnt_month_model, by = c("month", "flat_model"))

#View dataset
glimpse(dataset_resale)

ggplot(dataset_resale, aes(resale_price, fill = factor(cut(flat_cnt_month_model, breaks = 5, right = TRUE)))) + 
  geom_histogram(position = "dodge") + 
  ggtitle("resale_price by flat_cnt_month_model") + 
  theme(legend.direction = "horizontal", legend.position = "bottom")


#-----------------------------------------------------------------------------

#----------storey_range variable--------------- 
length(unique(dataset_resale$storey_range))#25

#Split the storey variable
dataset_resale$storey_lower <- as.integer(substr(dataset_resale$storey_range, 2, 3))
dataset_resale$storey_upper <- as.integer(substr(dataset_resale$storey_range, 7, 8))

# Plot histogram of resale_price by storey
ggplot(dataset_resale, aes(resale_price, fill = factor(cut(storey_upper, breaks = 5, right = TRUE)))) + 
  geom_histogram(position = "dodge") + 
  ggtitle("resale_price by storey") + 
  theme(legend.direction = "horizontal", legend.position = "bottom")

ggplot(dataset_resale, aes(floor_area_sqm, fill = factor(cut(storey_upper, breaks = 5, right = TRUE)))) + 
  geom_histogram(position = "dodge") + 
  ggtitle("floor_area_sqm by storey") + 
  theme(legend.direction = "horizontal", legend.position = "bottom")


#6)--------------town variable-----------------------
length(unique(dataset_resale$town))#26
unique(dataset_resale$town)
# [1] "ANG MO KIO"      "BEDOK"           "BISHAN"          "BUKIT BATOK"     "BUKIT MERAH"    
# [6] "BUKIT PANJANG"   "BUKIT TIMAH"     "CENTRAL AREA"    "CHOA CHU KANG"   "CLEMENTI"       
# [11] "GEYLANG"         "HOUGANG"         "JURONG EAST"     "JURONG WEST"     "KALLANG/WHAMPOA"
# [16] "MARINE PARADE"   "PASIR RIS"       "QUEENSTOWN"      "SENGKANG"        "SERANGOON"      
# [21] "TAMPINES"        "TOA PAYOH"       "WOODLANDS"       "YISHUN"          "SEMBAWANG"      
# [26] "PUNGGOL"
sort(round(prop.table(table(dataset_resale$town)) * 100, 2))
# percentage of properties for resale vary across towns 0.25 to 9.31

#--------------------------------------------------------------------------------
#summarise number of flats available in each town for each month 
town_cnt <- dataset_resale %>% group_by(month, town) %>% 
  summarise(flat_count_town = n())

#Add town count variable to the dataset
dataset_resale <- left_join(dataset_resale, town_cnt, by = c("month", "town"))

glimpse(dataset_resale)

#resale_price by flat_count_town
ggplot(dataset_resale, aes(resale_price, fill = factor(cut(flat_count_town, breaks = 5)))) + 
  geom_histogram(position = "dodge") + 
  ggtitle("resale_price by flat_town_count") + 
  theme(legend.direction = "horizontal", legend.position = "bottom")
#--------------------------------------------------------------------------------

#8)------------variable block------------------
length(unique(dataset_resale$block)) # 2190 blocks
#-----------------------------------------------------------------------------
#number of blocks in each street for each month of every year
block <- dataset_resale %>% group_by(month, town, street_name) %>% 
  summarise(block_count = length(unique(block)))
# Add block count variable to dataset
dataset_resale <- left_join(dataset_resale, block, 
                            by = c("month", "town", "street_name"))

glimpse(dataset_resale)
unique(dataset_resale$block_count)

ggplot(dataset_resale, aes(resale_price, fill = factor(cut(block_count, breaks = 5)))) + 
  geom_histogram(position = "dodge") + 
  ggtitle("resale_price by block_count") + 
  theme(legend.direction = "horizontal", legend.position = "bottom")
#-----------------------------------------------------------------------------


#9)-----------street_name variable------------------------ 
length(unique(dataset_resale$street_name))#534

#------------------------------------------------------------------------------
#Calculate number of streets per town 
street_count <- dataset_resale %>% group_by(town) %>% summarise(street_count = length(unique(street_name)))

#Add street count to the datasset
dataset_resale <- left_join(dataset_resale, street_count, by = "town")

ggplot(dataset_resale, aes(resale_price, fill = factor(cut(street_count, breaks = 5)))) + 
  geom_histogram(position = "dodge") + 
  ggtitle("resale_price by street_count") + 
  theme(legend.direction = "horizontal", legend.position = "bottom")

#----------------------------------------------------------------------------

#----------------------------------------------------------------------------
#Calculate number of flats for each streets per town for each month 
streets <- dataset_resale %>% group_by(month, town, street_name) %>% 
  summarise(flat_count_street = n())
#Add street count to the datasset
dataset_resale <- left_join(dataset_resale, streets, 
                            by = c("month", "town", "street_name"))

ggplot(dataset_resale, aes(resale_price, fill = factor(cut(flat_count_street, breaks = 5)))) + 
  geom_histogram(position = "dodge") + 
  ggtitle("resale_price by flat_count_street") + 
  theme(legend.direction = "horizontal", legend.position = "bottom")
#----------------------------------------------------------------------------

#create a variable month_lapsed from month variable
dataset_resale$month <- ymd(paste(dataset_resale$month, "-01", sep =""))

# Calculate number of months 
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

#Add new elapsed month variable to dataset
dataset_resale$month_lapsed <- elapsed_months(Sys.Date(), dataset_resale$month)
glimpse(dataset_resale)

# Create age_flat which gives the number of years ago the lease was commenced for the flat
dataset_resale$flat_age <- as.integer(lubridate::year(Sys.Date())) - dataset_resale$lease_commence_date

glimpse(dataset_resale)


#Select Variables
dataset_resale_1 <- dataset_resale %>% 
  select(year, month_lapsed, town, flat_type, floor_area_sqm, flat_model, 
         flat_age, flat_cnt_month, flat_cnt_month_lease,
         condition, flat_cnt_month_model, storey_lower, storey_upper, 
         flat_count_town, flat_count_street, block_count, street_count, 
         street_name, resale_price)

glimpse(dataset_resale_1)

#Convert Character variables to factor
dataset_resale_1[, c(3, 4, 6, 10, 12, 13 ,17, 18)] <- map_df(dataset_resale_1[, c(3, 4, 6, 10, 12, 13, 17, 18)], as.factor)
#dataset_resale_1[, map_lgl(dataset_resale_1, is.character)] <- map_df(dataset_resale_1[, map_lgl(dataset_resale_1, is.character)], as.factor)

#Convert year to Numeric

dataset_resale_1$year <- as.numeric(dataset_resale_1$year)

glimpse(dataset_resale_1)












