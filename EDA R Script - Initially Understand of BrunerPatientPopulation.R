###Exploring the Patient Population at Bruner Clinic
##This is the R code for the EDA process of exploring the patient population at Bruner Clinic in Denver, CO 
#with a pleothra of different chronic conditions. This first set of code is just what I did to learn about the data
#I have and what I can take away from a basic EDA process with this dataset and which variables do I think would be
#interesting for further analysis.

##Load & View the data
#Can't analyze data if we don't have any, I first need to start by loading the dataset I will be analyzing.
library(readr)
bpp <- read_csv("Data Science School Documents/MSDS 692 Data Science Practicum I/BrunerPatientPopulation.csv")
head(bpp)

##Load the libaries
#There are a lot of libraries in RStudio, here are some of the ones I'll be using for this EDA process
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

##Basic EDA
#Start by just learning about the characteristics of the dataset. I was the one who built the dataset and brought in 
# what fields I believed to be useful from different sources, but what does RStudio say about what I'm bringing in.
dim(bpp)
#14424 records and 68 fields in total
colnames(bpp)

#What is the data source's structure?
str(bpp)
#It appears that I have some variables that are characters and should be integers and vice-versa.
#I also have 3 variables with seemingly only null values, Check In/Out Times and the Enc close date.
#I also appear to have nulls in my Dept_Id, Financial Class, Family size, income, and Patient Homeless Y/N
#One of the 1st things I need to do is change some of these variables to the proper data type, then from there
# I should be able to look into the variables with seemingly all null values and see if this is true or not
# and if so, I need to omit it from my dataset. 

##Change the data types of specific variables within the Bruner Data:
#Start with variables that need to be changed from intergers to character types
bpp$PAT_ENC_CSN_ID <- as.character(bpp$PAT_ENC_CSN_ID)
bpp$ENC_TYPE_C <- as.character(bpp$ENC_TYPE_C)
bpp$DEPARTMENT_ID <- as.character(bpp$DEPARTMENT_ID)
bpp$LINE <- as.character(bpp$LINE)
bpp$SERV_AREA_ID <- as.character(bpp$SERV_AREA_ID)
bpp$STATE_C <- as.character(bpp$STATE_C)
bpp$COUNTY_C <- as.character(bpp$COUNTY_C)
bpp$PAT_MRN_ID <- as.character(bpp$PAT_MRN_ID)
bpp$CUR_PCP_PROV_ID <- as.character(bpp$CUR_PCP_PROV_ID)
bpp$CUR_PRIM_LOC_ID <- as.character(bpp$CUR_PRIM_LOC_ID)
bpp$DX_ID <- as.character(bpp$DX_ID)

#Now the variables that are characters and need to change to numeric variables, there are fortunately only family income and size for these that fields that need changed.
bpp$FAMILY_INCOME <- as.numeric(bpp$FAMILY_INCOME)
bpp$FAMILY_SIZE <- as.integer(bpp$FAMILY_SIZE)

#Setting variables to factors
bpp$ICD_Group <- as.factor(bpp$ICD_Group)
bpp$LOC_NAME <- as.factor(bpp$LOC_NAME)
bpp$ZIP_CODES <- as.factor(bpp$ZIP_CODES)
bpp$SEX_C <- as.factor(bpp$SEX_C)
bpp$ETHNIC_GROUP_C <- as.factor(bpp$ETHNIC_GROUP_C)
bpp$MARITAL_STATUS_C <- as.factor(bpp$MARITAL_STATUS_C)
bpp$LANGUAGE_C <- as.factor(bpp$LANGUAGE_C)
bpp$TOBACCO_USE_VRFY_YN <- as.factor(bpp$TOBACCO_USE_VRFY_YN)

#I want to start by looking at my data and making sure I made all the requisite changes to the data types.
str(bpp)
#My data types look good!

#Now I want to make sure my data was uploaded into Rstudio without any concerns so lets check that using the head & tail funcitons
head(bpp)
tail(bpp)
#I have noticed some columns with what appears to be a lot of null values, let's take a deeper look
# and try to figure out if there are only null values or if there is some useable values in these variables.

##Explore the variables that are seemingly filled with only nulls:
#DEPARTMENT_ID
sum(is.na(bpp$DEPARTMENT_ID))
plot(bpp$DEPARTMENT_ID)
#Though we have null values, it appears there are indeed some records with actual department ID's, at SCL these are numbered so plotting them is plausable when showing 
# if you have any of these values or not.

#CHECKIN_TIME
sum(is.na(bpp$CHECKIN_TIME))
table(bpp$CHECKIN_TIME)
#Creating a table for check-in times showed that there are indeed times recorded for when patient's were checked into the hospital but just not all records have this recorded.

#CHECKOUT_TIME
sum(is.na(bpp$CHECKOUT_TIME))
table(bpp$CHECKOUT_TIME)
#The same strategy for check-in time was used here for check-out time, but when comparing the tables, it's evident that there isn't many times where check-out time was recorded.

#ENC_CLOSE_DATE
sum(is.na(bpp$ENC_CLOSE_DATE))
table(bpp$ENC_CLOSE_DATE)
#Again, same strategy can be used since ENC_CLOSE_DATE is a date-time, there isn't many but there appears to be about 22 times where this value was recorded.

#PAT_PAIN_SCORE_C
sum(is.na(bpp$PAT_PAIN_SCORE_C))
bpp$PAT_PAIN_SCORE_C <- as.factor(bpp$PAT_PAIN_SCORE_C)
table(bpp$PAT_PAIN_SCORE_C)
plot(bpp$PAT_PAIN_SCORE_C)
#Unfortunately, the PAT_PAIN_SCORE_C didn't yield many non-null values, only 14 in total. It is likely I will omit this from being used in my analysis.

#FAMILY_SIZE
sum(is.na(bpp$FAMILY_SIZE))
table(bpp$FAMILY_SIZE)
plot(bpp$FAMILY_SIZE)
#Unfortunately family size only yields null results for the dataset that I have to analsis. I will be omitting this from my dataset later.

#FAMILY_INCOME
sum(is.na(bpp$FAMILY_INCOME))
table(bpp$FAMILY_INCOME)
plot(bpp$FAMILY_INCOME)
#Same thing with family income, no non-null variables so this variable will be omitted.

#PAT_HOMELESS_YN
sum(is.na(bpp$PAT_HOMELESS_YN))
table(bpp$PAT_HOMELESS_YN)
#Isn't null, but with only fourteen values in the variable, all of which are null it makes for a kind of pointless variable for this set of patients.

#DEF_FIN_CLASS_C
sum(is.na(bpp$DEF_FIN_CLASS_C))
table(bpp$DEF_FIN_CLASS_C)
plot(bpp$DEF_FIN_CLASS_C)
#Only null values, so we can get rid of this variable as well.

#After looking into some of my variables that I suspected to be strictly null values I came back with some encouraging and some not so much, it looks like there are a number
# of variables that it would be best for me to get out of my dataset before moving on with my analysis, I luckily can do that here with RStudio.

##Omitting Null variables from my dataset
#The columns that I am going to omit from my dataset are FAMILY_SIZE, FAMILY_INCOME, PAT_HOMELESS_YN, DEF_FIN_CLASS_C, PAT_PAIN_SCORE_C & ENC_CLOSE_DATE
#To do this I need to make sure of the location for each of these variables within the dataset, so I don't omit anything I want to keep in there.
#I can find the location just by looking at the names of all my variables.
names(bpp)
#From the looks of my data columns here is the locations ENC_CLOSE_DATE = 14; PAT_PAIN_SCORE_C = 26; FAMILY_SIZE = 27; FAMILY_INCOME = 28; PAT_HOMELESS_YN = 29; DEF_FIN_CLASS_C = 45
#Let's make a subset of this data before omitting it
osubset <- select(bpp, DEPARTMENT_ID, CHECKIN_TIME, CHECKOUT_TIME, ENC_CLOSE_DATE, PAT_PAIN_SCORE_C, FAMILY_SIZE, FAMILY_INCOME, PAT_HOMELESS_YN, DEF_FIN_CLASS_C)
head(osubset)
str(osubset)
#It truly appears none of the data from the osubset will be helpful for my analysis going further so it is time to get rid of it.
bpp <- select(bpp, -(DEPARTMENT_ID), -(CHECKIN_TIME), -(CHECKOUT_TIME),-(ENC_CLOSE_DATE), -(PAT_PAIN_SCORE_C), -(FAMILY_SIZE), -(FAMILY_INCOME), -(PAT_HOMELESS_YN), -(DEF_FIN_CLASS_C))
str(bpp)
head(bpp)
#Now as we can see our new Bruner Patient Population has only 59 variables instead of the original 68

#Then with our remaining variables that are null were just goin to omit the null values.
#I would like to make a subset of some of the data I have left now, singling out perhaps which variables I want to measure and which I want to aggregate upon.
#I will start with a subset of my variables that are factors, I want to make sure I know which ones these are so I can then compare my different factors across different measures and eventually use 
# these to help with my visualizations and cluster analysis.
#I want to start again by looking at my column names, I always find this to be an effect way to point out different variables I want to subset.
names(bpp)
#Some of these variables are pointless for my subset, even though they aren't measures. The reason is because they were good for connecting my data, and great for primary keys for possible counting 
# purposes, but are unnecessary for what I'm doing here, it could be another situation where I can omit them from my entire analysis, but first let me make my two subsets.
#The variables I'd like to look into are: CONTACT_DATE, LOC_NAME, CURRENT_ICD10_LIST, ICD Group, TOBACCO_USE_VRFY_YN, CITY, State, Zip Codes Clean, County, BIRTH_DATE,
# SEX_C, ETHNIC_GROUP_C, MARITAL_STATUS_C, LANGUAGE_C

#Now I'm curious to see what some individual records look like for our bpp dataset, we can do this pretty simply.
bpp[c(1,3,5,7,9)]
#The command above is great for looking at data rows, but unfortunately I can't do that effectively because of the amount of variables 
# so what might be the best course of action is to split them into 2 subsets, one for dimensional variables, and one for measures

#The subset of these variables:
dim_subset <- select(bpp, ICD_Group, CONTACT_DATE, LOC_NAME, CURRENT_ICD10_LIST, TOBACCO_USE_VRFY_YN, CITY, State, ZIP_CODES, County, BIRTH_DATE, SEX_C, ETHNIC_GROUP_C, MARITAL_STATUS_C, LANGUAGE_C)
head(dim_subset)
str(dim_subset)
#Looking into the structure of this subset, it looks like we have a mix of variables that are character based and structure based, The ones that are factor based range from just one level with the Tobacco field
# to 750 with the zip code field. Ultimately, the variables which are factors will be what I'm most interested in when it comes to variables within this subset, the reason why is it appears they will be what will 
# help me better understand the patient population at Bruner Clinic and the chronic conditions which are the ICD_Group is one that we really want to highlight to understand if some of these variables are correlated to 
# why patients are getting these conditions.

#Now let's take a look at some of these dimensions 
colnames(dim_subset)
#There appears to be only 14 variables within this subset, only a few that I think are important going forward, let's take a quick of those individually.
dim_subset[,1]
table(dim_subset$ICD_Group)
#Looking ath the first few samples of the dim_subset you'd assume that there is a lot for hypertension, but when tabled, it is apparent that Arthritis is 
# far and away the highest numbered response for the chronic condition these patients come in with per our population. 
dim_subset[,8]
table(dim_subset$ZIP_CODES)
#Our zip codes are very wide ranging, which means our patients are coming from more than just places within Colorado, a little surprising 
# with how vast but the majority are within Colorado area codes starting with 80 and 81 so that makes sense for the data collected.
dim_subset[,11]
#The 11th column is primarily 1 and 2 which is 1 for female and 2 for male. 
table(dim_subset$SEX_C)
#The only values I'm actually seeing here are 1's and 2's nothing else for the gender of the patients, with almost 2000 more females than males

#Now let's make a subset of the measures, 
meas_subset <- select(bpp, BP_SYSTOLIC:Uninsured_Tract)
head(meas_subset)
#Looks like I have some measures I still need to disgard.
meas_subset <- select(meas_subset, -(HEIGHT), -(PHYS_BP), -(TOBACCO_USE_VRFY_YN))
meas_subset <- select(meas_subset, -(PAT_NAME:LANGUAGE_C))
meas_subset <- select(meas_subset, -(CUR_PCP_PROV_ID:PAT_MRN_ID))
str(meas_subset)
#There is a total of 28 variables of interest within this subset from the Bruner Patient data. It is mix of numerical and binary data which gives insight on the patient's health based on their encounter and a number of 
# variables which are tied to the zip codes and counties these patients are from to help pull in insights into the environments patients are from and what that could be contributing to their overall health.

##Some of the measure columns are different in how they are counted within our measures subset, I would like to take a look at some of these and 
# assess how they're different and how I can use them later on in my analysis.
colnames(meas_subset)
#I have a total of 28 measures here, varing from vital measurements from a patient encounter to metrics about a social environment. Let's look at what is the type of variables I can see from these 
# measures though
meas_subset[,3]
#Column 3 was body temperature, and though we appear to have some null values from what I'm seeing from the small set of them, they all seem to hoover around
# what is a normal body temp. which is 98 degrees
meas_subset[,25]
#Column 25 is the measure pertaining to the percentage of uninsured for the county the patient comes from. These are generally low numbers which 
# is a good thing for our society, but if our patient has a higher probability to fall into these unemployment numbers that can be an issue for their health.
#There are also variables which are binary that determine if the patient is from a high risk area or not for a specific societal metric, let's see one of those.
meas_subset[,18]
#Here column 18 is the Family Income Tract, which determines if the Median family income for an area is above or below the national average, this has no 
# barring on the patient themselves, just where they come from based on their county 

#That is a good look into the types of measures were going to see within our dataset but what do the measures look like in a single row?
meas_subset[c(1, 3, 5, 7,9),]
#Definitely curious to look at this, to see if I can see anything that sticks out, nothing really but there are so many measures that it is hard for just one to stick out.

#Looking through both the dimensional and measurable variables it is easy to see I can still take out some more variables before I get deeper into my analysis.

#Since ICD_Group is one of my more important dimension variables, I'd like to take a look into it and see the kinds of insight I can find out about each of its factors.
#Before doing so, I'm going to omit a few more variables so I can declutter my dataset for better analysis
str(bpp)
bpp_final <- select(bpp, ICD_Group, CONTACT_DATE, CITY, State, ZIP_CODES, County, SEX_C, ETHNIC_GROUP_C, MARITAL_STATUS_C, LANGUAGE_C, TOBACCO_USE_VRFY_YN, 
                    BIRTH_DATE, BP_SYSTOLIC, BP_DIASTOLIC, TEMPERATURE, PULSE, WEIGHT, RESPIRATIONS, BMI, BSA,Crime_Rate, Urban_or_Rural, Population, Low_Access_Tract_1and10, 
                    Vehicle_Access, Low_Income_Tract, Poverty_Rate, Poverty_Tract, Median_Family_Income, Family_Income_Tract, Low_Access_to_Supermarket, Supermarket_Tract, 
                    Access_to_Exercise, Access_to_Exercise_Tract, High_School_Graduation_Rate, Graduation_Tract, Percentage_Unemployed, Unemployed_Tract, Percentage_Uninsured, 
                    Uninsured_Tract)
head(bpp_final)
#I now have a mix of my dimensions and measures that will be ideal for analysis going forward
str(bpp_final)
summary(bpp_final)
#Our bpp_final dataset still has quite a few variables but it has cut down a substantial 27 variables in total 

bpp_final[c(1,3,5,7,9),]
#It is still hard to see the entire data set unfortunately, but we have all the variables we would want to use moving forward so let's look deeper into understanding our dataset.

#We can start by looking at some charts which will help us understand the distribution of the data.
cc_dist <- table(bpp_final$ICD_Group)
cc_dist
barplot(cc_dist, col = "blue")
#From the looks of our chronic conditions arthritis and cancer are overwhelmingly what we see patients being diagnosed with 
#We can also look and see where these people are coming from by state, city and county as well. Let's create simple bar charts for each of those 
state_dist <- table(bpp_final$State)
state_dist
barplot(state_dist, col = "red")
#As anticipated, Colorado is where a majority of our patients reside. 
city_dist <- table(bpp_final$CITY)
city_dist
barplot(city_dist, col = "black")
#The spread on our cities is much more varied, which isn't a surprise because we have a lot of different cities within our patient population
#What is surprising to me isn't the that places like Denver and Arvada are some of the higher areas were seeing places, but places like Topeka, KS
# have such a high population of patients residing from that specific part of the country.

county_dist <- table(bpp_final$County)
county_dist
barplot(county_dist)
#No real surprise here, seeing Jefferson far and away the highest populated county from our patient population is what is expected, same with Denver
# county being up there as well. I'd like to look a little bit more into the different variables though because those are going to be how we understand what 
# variables are the biggest indicator for a specific disease or not.

#I'm going to go a little scatterplot crazy comparing these variables on a 2-dimensional plane but later on we will look at these on different 
# dimensions and split between the different chronic conditions.
pairs(meas_subset)
#There are so many variables to look at it is a bit overwhelming for even the pairs command in RStudio. I think it is best to just look at some of these, two at a time
with(bpp_final, {plot(BP_SYSTOLIC~BP_DIASTOLIC)
  lines(lowess(BP_SYSTOLIC~BP_DIASTOLIC), col = "red")})
#There is definitely a positive correlation between the two blood pressures, but that is expected.
with(bpp_final, {plot(TEMPERATURE~PULSE)
  lines(lowess(TEMPERATURE~PULSE), col = "red")})
#Surprisingly to me, it appears that our patients tend to have the same general body temperature, which makes it difficult to judge what kind of 
# correlation that could have to any variables, that's disappointing, let's look at pulse with another variable
with(bpp_final, {plot(PULSE~RESPIRATIONS)
     lines(lowess(PULSE~RESPIRATIONS), col= "red")})
#Again, two variables to find difficult to find a trend to, the variances are different so it is harder to notice any trend
#It might be easier to understand our data more if we look at the distributions of our variables see what we should be expecting and the best way to do
# this is by looking at histograms
##Histograms
#BP_Systolic
hist(bpp_final$BP_SYSTOLIC, breaks = 10)

#BP_Diastolic
hist(bpp_final$BP_DIASTOLIC, breaks = 10)
#The distribution of these two variables are almost carbon copies of one another, only difference is systolic has higher numbers to diastolic

#Temperature
hist(bpp_final$TEMPERATURE, breaks = 10)
#This is showing again that the range in temperatures is quite stuck within that normal 98 degree area

#Pulse
hist(bpp_final$PULSE, breaks = 10)
#The pulse of these patients appear to in a range that is a little higher than 60 beats/min

#Weight
hist(bpp_final$WEIGHT, breaks = 10)
#The distribution says its in the 1000s but its really in the 100s and the majority of the patients look to be over 200 lbs

#Respirations
hist(bpp_final$RESPIRATIONS, breaks = 10)
#Respirations look to all be in the same general area, just under 20.

#BMI
hist(bpp_final$BMI, breaks = 10)
#Each of these BMI's appear to be on the high side of this measure typically, this doesn't mean that our population is necessarily oveweight but 
# we can deduce that is likely

#BSA
hist(bpp_final$BSA, breaks = 10)
#The body surface area appears to be prominant around 2.0 and range heavily between 1.5 and 2.5

#Population
hist(bpp$Population, breaks = 10)
#It appears a majority of our patients come from an area with a population around 4,000 people

#Poverty Rate
hist(bpp$Poverty_Rate, breaks = 10)

#Median Family Income
hist(bpp$Median_Family_Income, breaks = 10)
#Our patient population appears to come from households that are just under $100K for median earnings, which is higher than expected 
# but this doesn't mean our patients are at this level its just based on where they live not actually what they make.

#SuperMarket Access
hist(bpp_final$Low_Access_to_Supermarket, breaks = 10)
#It appears for the most part our patients have good access to a super market

#Access to Exercise
hist(bpp_final$Access_to_Exercise, breaks = 10)
#It would appear our patients have no issues being able to exercise within their environment

#High School graduation rate
hist(bpp_final$High_School_Graduation_Rate, breaks = 10)
# the high school graduation in the US is at 84.6% which is around where a majority of patients are from, more appear to be below that 
# though then noticeably ahead

#Unemployment rate
hist(bpp_final$Percentage_Unemployed, breaks = 10)
#Unemployment around the country is very low, that is why our patients appear to come from low unemployment areas, but that doesn't mean
# this is true for the individual patient

#Uninsured patients
hist(bpp_final$Percentage_Uninsured, breaks = 10)
#Uninsured rate in America falls just over 8%, It would seem we have a lot of patients fall under that, but there is probably a lot hovering 
# around or right above it, we would need a more refined chart to truly know.

#With the rest of then numbers being binary, I will move onto looking at these variables when compared across dimensions. The one in 
# particular that I have a lot of interest in is the ICD_Group because it houses what type of chronic condition the record is.
#For these comparisons I will start by looking at box plots

##Medical Measures - Vitals
#BP_Diastolic
boxplot(bpp_final$BP_DIASTOLIC ~ bpp_final$ICD_Group, ylab = "Blood Pressure Diastolic",
        xlab = "Chronic Conditions", main = "BP Diastolic vs. Chronic Condition", cex.lab=1.5)
#BP_Systolic
boxplot(bpp_final$BP_SYSTOLIC ~ bpp_final$ICD_Group, ylab = "Blood Pressure - Systolic",
        col = rep(c(1,2,3, 4, 5, 6,7, 8, 9)),xlab = "Chronic Conditions", main = "BP Systolic vs. Chronic Condition", 
        cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)

#This does a great job showing the differences within the different types of patients we see per chronic condition. Both blood pressures look quite similar.
#Arthritis and cancer appear to have a lot of patients who's outliers are higher than the range of their individual boxplot, while CHD has a 
# surprising number of patients who are lower outliers where they fall below that quartile range. The variance of diabetes is the one that has the biggest spread.

#Temperature
boxplot(bpp_final$TEMPERATURE ~ bpp_final$ICD_Group, ylab = "Body Temp.",
        xlab = "Chronic Conditions", main = "Body Temp. vs. Chronic Conditions", col = rep(c(1,2,3,4,5,6,7,8,9)), 
        cex.lab = 1.5, cex.axis = 1.5, cex.main =2)
#The body temperatures look to be in the same for each chronic condition.  

#Pulse
boxplot(bpp_final$PULSE ~ bpp_final$ICD_Group, ylab = "Pulse",
        xlab = "Chronic Conditions", main = "Pulse vs. Chronic Conditions", col = rep(c(1,2,3,4,5,6,7,8,9)),
        cex.lab = 1.5, cex.axis = 1.5, cex.main= 2)
#A high amount of outliers that are above the quartile rang for many of the chronic conditions but these quartiles and medians are close to the same 
# area between 50 and 100 bpm.

#Weight
#Before doing the box plot for weight, I'd like to change this measure because it is multipled by 10 for some reason and makes it seem like each patient
# weighs over 1000 lbs. 
bpp_final$WEIGHT <- bpp_final$WEIGHT /10
head(bpp_final$WEIGHT)
#Now that the weights look good, let's make the box plot
boxplot(bpp_final$WEIGHT ~ bpp_final$ICD_Group, ylab = "Weight", xlab = "Chronic Conditions",
        main = "Weight vs. Chronic Conditions", col = rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9)),
        cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
#Surprised to see here that the patients with hepatitis appear to have one of the higher quartile ranges, I wouldn't have expected that

#Respirations
boxplot(bpp_final$RESPIRATIONS ~ bpp_final$ICD_Group, ylab = "Respirations", xlab = "Chronic Conditions",
        main = "Respirations vs. Chronic Conditions", col = rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9)),
        cex.lab = 1.5, cex.axis = 1.5, cex.main =2)
#All the respiration box plots look highly similar in their median level and their quartile range, I'm interested in some of the very high outliers 
# in diabetes and obesity though. Those are almost to 120 respirations.

#BMI
boxplot(bpp_final$BMI ~ bpp_final$ICD_Group, ylab = "BMI", xlab = "Chronic Conditions",
        main = "BMI vs. Chronic Conditions", col = rep(c(1,2,3,4,5,6,7,8,9)),
        cex.lab = 1.5, cex.axis = 1.5, cex.main =2)
#No surprise Obese has the highest quartile range, what is surprising to me is the number of hepatitis patients whe have that allow that quartile range to 
# be so high.

#BSA
boxplot(bpp_final$BSA ~ bpp_final$ICD_Group, ylab = "BSA", xlab = "Chronic Conditions",
        main = "BSA vs. Chronic Conditions", col = rep(c(1,2,3,4,5,6,7,8,9)),
        cex.lab = 1.5, cex.axis = 1.5, cex.main =2)

#There was some variation to the different chronic conditions per each vital measure, there was no unique surprises on contrasting difference between
# the conditions but in the social measures that could be different.

##Social determinants
#Population
boxplot(bpp_final$Population ~ bpp_final$ICD_Group, ylab = "Population", xlab = "Chronic Conditions",
        main = "Population vs. Chronic Conditions", col = rep(c(1,2,3,4,5,6,7,8,9)),
        cex.lab = 1.5, cex.axis = 1.5, cex.main =2)
#Population doesn't have much variance betweeen the chronic conditions

#Poverty Rate
boxplot(bpp_final$Poverty_Rate ~ bpp_final$ICD_Group, ylab = "Poverty Rate vs. Chronic Conditions",
        main = "Poverty Rate vs. Chronic Conditions", col = rep(c(1,2,3,4, 5,6,7,8,9)),
        cex.lab=1.5, cex.axis = 1.5, cex.main = 2)

#Median Family Income
boxplot(bpp_final$Median_Family_Income ~ bpp_final$ICD_Group, ylab = "Median Family Income", xlab = "Chronic Conditions",
        main = "Median Family Income vs. Chronic Conditions", col = rep(c(1,2,3,4,5,6,7,8,9)),
        cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
#The median family incomes for a lot of these chronic conditions are similar. They are anywhere between $50k and $100k

#Access to Supermarket
boxplot(bpp_final$Low_Access_to_Supermarket ~ bpp_final$ICD_Group, ylab = "Access to Supermarket", xlab = "Chronic Conditions",
        main = "Low Access to Supermarket vs. Chronic Conditions", col = rep(c(1,2,3,4,5,6,7,8,9)),
        cex.lab = 1.5, cex.axis = 1.5, cex.main =2)

#Lack of Exercise
boxplot(bpp_final$Access_to_Exercise ~ bpp_final$ICD_Group, ylab = "Access to Exercise", xlab = "Chronic Conditions",
        main = "Access to Exercise vs. Chronic Conditions", col = rep(c(1,2,3,4,5,6,7,8,9)),
        cex.lab = 1.5, cex.axis = 1.5, cex.main =2)

#High School Graduation Rate
boxplot(bpp_final$High_School_Graduation_Rate ~ bpp_final$ICD_Group, ylab = "High School Grad Rate", xlab = "Chronic Conditions",
        main = "High School Grad Rate vs. Chronic Condition", col = rep(c(1,2,3,4,5,6,7,8,9)),
        cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)

#Percentage Unemployed 
boxplot(bpp_final$Percentage_Unemployed ~ bpp_final$ICD_Group, ylab = "Percent Unemployed", xlab = "Chronic Conditions",
        main = "Percentage Unemployed vs. Chronic Conditions", col = rep(c(1,2,3,4,5,6,7,8,9)),
        cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)

#Percentage Uninsured
boxplot(bpp_final$Percentage_Uninsured ~ bpp_final$ICD_Group, ylab = "Percent Uninsured", xlab = "Chronic Condition",
        main = "Percentage Uninsured vs. Chronic Conditions", col = rep(c(1,2,3,4,5,6,7,8,9)),
        cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)

#Those box plots were great for understanding if there was any difference in the distribution of the individual variables, but it doesn't give 
# any indication on any correlating variables possible, just what might be different from one chronic condition to another.
plot(bpp_final$BP_DIASTOLIC, bpp_final$BP_SYSTOLIC, pch =19, col = bpp_final$ICD_Group, main = "BP-Diastolic vs. BP-Systolic",
     xlab = "BP-Diastolic", ylab = "BP - Systolic")
#There is a definite upward trend between the two, but no obvious clustered relationship between the different chronic conditions.

plot(bpp_final$TEMPERATURE, bpp_final$PULSE, pch = 19, col = bpp_final$ICD_Group, main = "Body Temp. vs. Pulse",
     xlab = "Body Temperature", ylab = "Pulse")
#Body temperature is very stagnant, but pulse is something that has some variance, no obvious clusters or relationship.

plot(bpp_final$WEIGHT, bpp_final$RESPIRATIONS, pch = 19, col = bpp_final$ICD_Group, main = "Weight vs. Respirations",
     xlab = "Weight", ylab = "Respirations")
#Here there doesn't appear to be much of a relationship between respirations and weight, with weight having a much wider variance than respirations.

plot(bpp_final$BMI, bpp_final$BSA, pch = 19, col = bpp_final$ICD_Group, main = "BMI vs. BSA",
     xlab = "BMI", ylab = "BSA")
#As anticipated there is a positive relationship between BSA and BMI. Also, it appears the obesity patients are towards the upper right corner

plot(bpp_final$Population, bpp_final$Poverty_Rate, pch = 19, col = bpp_final$ICD_Group, main = "Population vs. Poverty Rate",
     xlab = "Population", ylab = "Poverty Rate")

plot(bpp_final$Median_Family_Income, bpp_final$High_School_Graduation_Rate, pch = 19, col = bpp_final$ICD_Group, main = "Median Family Income vs. Grad Rate",
     xlab = "Median Family Income", ylab = "High School Grad Rate")

plot(bpp_final$Low_Access_to_Supermarket, bpp_final$Access_to_Exercise, pch = 19, col = bpp_final$ICD_Group, main = "Access to Supermarket vs Access to Exercise",
     xlab = "Access to Supermarket vs Access to Exercise")

plot(bpp_final$Percentage_Unemployed, bpp_final$Percentage_Uninsured, pch = 19, col = bpp_final$ICD_Group, main = "Unemployed vs Uninsured",
     xlab = "Unemployed", ylab= "Uninsured")

#It was good to plot this data to see the relationship between pairs of variables, but there was hard to see if the data was clustered together, it was all combined together.
# So the last plot I want to use for this EDA process is qplots to see if that would be beneficial for looking at this data.
#Blood Pressures
qplot(BP_SYSTOLIC, BP_DIASTOLIC, data = bpp_final, facets = .~ ICD_Group) + geom_smooth()

#Temp vs. Respirations
qplot(TEMPERATURE, RESPIRATIONS, data = bpp_final, facets = .~ ICD_Group) + geom_smooth()
#Here this relationship appears to have quite a variation depending on the chronic disease

#Pulse vs. Weight
qplot(PULSE, WEIGHT, data = bpp_final, facets = .~ ICD_Group) + geom_smooth()
#Interesting variation per chronic condition between the two here.

#BMI vs BSA
qplot(BMI, BSA, data = bpp_final, facets = .~ ICD_Group) + geom_smooth()
#Each line is positive, but the tail of each one almost does something different, either dipping negative, still going positive
# or moving in several different ways

#Population vs. Poverty Rate
qplot(Population, Poverty_Rate, data = bpp_final,facets = .~ ICD_Group) + geom_smooth()

#Median Family Income vs Low Access to Supermarket
qplot(Median_Family_Income, Low_Access_to_Supermarket, data = bpp_final, facets = .~ ICD_Group) + geom_smooth()

#Access to Exercise vs. High School Grad Rate
qplot(Access_to_Exercise, High_School_Graduation_Rate, data = bpp_final, facets = .~ICD_Group) + geom_smooth()
#These two appear to have a negative correlation, as the grad rates get lower, the access to exercise appears to rise and that is for almost all the 
# chronic conditions

#Unemployed vs. Uninsured
qplot(Percentage_Unemployed, Percentage_Uninsured, data = bpp_final, facets = .~ICD_Group) + geom_smooth()
#Another pair of variables which move so differently from condition to condition.

#After going through the qplots it is apparent to me that it would be wise when I move into my cluster analysis to 
# do a cluster analysis per chronic condition, and not do all of the data points together in one analysis.
#The reason is because these data points don't appear to cluster togehter, the variance between chronic conditions are 
# fairly similar between variables, but when split out in a qplot the data points appear to trend differently 
#depending on the chronic condition and the variables.

#I continued my EDA process within Tableau where I took a look at the data through interactive dashboards.

#################################################################################################################

##Beginning the Cluster Analysis

#Before I get into my HCA and K-means clustering I need to change the look of my dataset one last time. I will need to make it 
# so I only have the one variable I'm clustering on and then all my different measures. The variable I want to 
# cluster upon to start is ICD_Group (Chronic Condition) because I want to be able to see how different these are from one another.
colnames(bpp_final)

bpp_final1 <- select(bpp_final, ICD_Group, BP_SYSTOLIC, BP_DIASTOLIC, TEMPERATURE, PULSE, WEIGHT, RESPIRATIONS, BMI, BSA, Population,
                     Poverty_Rate, Median_Family_Income, Low_Access_to_Supermarket, Access_to_Exercise, High_School_Graduation_Rate,
                     Percentage_Unemployed, Percentage_Uninsured)
head(bpp_final1)
str(bpp_final1)
bpp_final1$BP_DIASTOLIC <- as.numeric(bpp_final1$BP_DIASTOLIC)
bpp_final1$BP_SYSTOLIC <- as.numeric(bpp_final1$BP_SYSTOLIC)
bpp_final1$PULSE <- as.numeric(bpp_final1$PULSE)
bpp_final1$RESPIRATIONS <- as.numeric(bpp_final$RESPIRATIONS)
bpp_final1$Population <- as.numeric(bpp_final1$Population)
str(bpp_final1)

#Before looking into the cluster analysis, I want to look at a chart which will help me see the correlation of my variables.
# A correlogram will help me compare and measure the correlation of each measure.
install.packages("ggcorrplot")
library(ggcorrplot)
bpp_final1 <- na.omit(bpp_final1)
cc_corr <- select(bpp_final1, -ICD_Group)
#I need to remove the last variable that isn't numeric from my data
corr <- round(cor(cc_corr), 1)

ggcorrplot(corr, hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method = "circle",
           colors = c("tomato2", "white","springgreen3"),
           title = "Correlogram of Chronic Conditions",
           ggtheme = theme_bw)

#To validate what were seeing from the correlogram, we can do the same thing but with a correlation matrix with pie charts:
install.packages("corrplot")
library(corrplot)
corrplot(corr, "pie","lower")

#The chart indicates only a few correlations between the data, and that's ok. There is 16 total variables so it's not uncommon to 
# not see a lot of variables that don't correlate with its given pair. The ones that do have a positive correlation appear to be:
# Weight vs. BSA, BMI vs BSA, BMI vs. Weight, Systolic vs. Diastolic (BP), Poverty rate vs. Uninsured, Unemployed vs uninsured, 
# population vs. supermarket access, median family income vs. supermarket access and Exercise access to median family income.
#The negative correlations I'm seeing are from: Exercise access vs. High school grad rate, Exercise access vs. Unemployed, MFI vs. Poverty Rate,
# MFI vs. Uninsured, Exercise Access vs. Uninsured, Supermarket access vs. poverty rate, and supermarket access vs uninsured.

#It is great to see an indication of what variables correlate before doing the cluster analysis because it helps with understanding how the 
# data points should plot when looking at these data points 

colnames(bpp_final1)
#It looks like I will have a total of 17 columns going into my cluster analysis, with ICD_Group and 16 numeric variables.
str(bpp_final1)
#There are two types of data types, ICD_Group is the only non-numeric variable, and it is of type factor.
summary(bpp_final1)

#That should help get us the desired result for K-means.

#HCA

library(colorspace)
head(bpp_final1)
bpp2 <- bpp_final1[,-1]
chronic_conditions_labels <- bpp_final1[,1]
chronic_conditions_labels <- unlist(chronic_conditions_labels)
cc_color <- rev(rainbow_hcl(10))[as.numeric(chronic_conditions_labels)]

pairs(bpp2, col = cc_color,
      lower.panel = NULL,
      cex.labels = 1, pch=19, cex= 1.2)

par(xpd = TRUE)
legend("topleft", x = 0.025, y = 0.2, cex = 2,
legend = as.character(levels(chronic_conditions_labels)),
fill = unique(cc_color))
par(xpd = NA)

#It is hard to find any distinction between any of the variables and chronic conditions. There are 16 variables & 10 Chronic conditions that can happen
#We can see conclusions as well with a parallel coordinates plot of the data:
par(las = 1, mar = c(4.5,3,3,2) + 0.1, cex =0.8)
MASS::parcoord(bpp2, col = cc_color, var.label = TRUE, lwd= 2)


#Add title
title("Parellel coordinate plot of Chronic Conditions")
#Legend
par(xpd = TRUE)
legend(x =1.75, y = -.25, cex = 1,
       legend = as.character(levels(chronic_conditions_labels)),
       fill = unique(cc_color), horiz = TRUE)
par(xpd = NA)
#These two charts are really really hard to see with 10 different chronic conditions, let's see if when we 
# limit to just 1 chronic condition per chart:
##Arthritis
patients_arthritis <- filter(bpp_final1, ICD_Group == "Arthritis")
head(patients_arthritis)
bpp2_arthritis <- patients_arthritis[,-1]
arthritis_labels <- patients_arthritis[,1]
arthritis_labels <- unlist(arthritis_labels)
arthritis_color <- rev(rainbow_hcl(3))[as.numeric(arthritis_labels)]
table(arthritis_labels)
pairs(bpp2_arthritis, col = arthritis_color,
      lower.panel = NULL,
      cex.labels = 2, pch = 19, cex = 1.2)

#Now let's try parallel coordinates with this Chronic condition:
par(las = 1, mar = c(4.5, 3, 3, 2) + 0.1, cex = 0.8)
MASS::parcoord(bpp2_arthritis)
#Add Title
title("Parallel coordinates plot of Arthritis")
#Add a legend
par(xpd = TRUE)
legend(x = 1.75, y = -.25, cex = 1,
       legend = as.character(levels(arthritis_labels)),
       fill = unique(arthritis_color), horiz = TRUE)
par(xpd = NA)

##Hypertension
patients_hypertension <- filter(bpp_final1, ICD_Group == "Hypertension")
head(patients_hypertension)
bpp2_hypertension <- patients_hypertension[,-1]
hypertension_labels <- patients_hypertension[,1]
hypertension_labels <- unlist(hypertension_labels)
hypertension_color <- rev(rainbow_hcl(3))[as.numeric(hypertension_labels)]
pairs(bpp2_hypertension, col = hypertension_color,
      lower.panel = NULL,
      cex.labels = 2, pch = 19, cex = 1.2)
par(xpd = TRUE)
legend(x = 0.05, y = 0.4, cex = 2,
       legend = as.character(levels(hypertension_labels)),
       fill = unique(hypertension_color))
par(xpd = NA)

#Now let's try parallel coordinates with this Chronic condition:
par(las = 1, mar = c(4.5, 3, 3, 2) + 0.1, cex = 0.8)
MASS::parcoord(bpp2_hypertension)
#Add Title
title("Parallel coordinates plot of Arthritis")

par(xpd = TRUE)

##Cancer
patients_cancer <- filter(bpp_final1, ICD_Group == "Cancer")
head(patients_cancer)
bpp2_cancer <- patients_cancer[,-1]
cancer_labels <- patients_cancer[,1]
cancer_labels <- unlist(cancer_labels)
cancer_color <- rev(rainbow_hcl(3))[as.numeric(cancer_labels)]
pairs(bpp2_cancer, col = cancer_color,
      lower.panel = NULL,
      cex.labels = 2, pch = 19, cex = 1.2)
par(xpd = TRUE)

#Now let's try parallel coordinates with this Chronic condition:
par(las = 1, mar = c(4.5, 3, 3, 2) + 0.1, cex = 0.8)
MASS::parcoord(bpp2_cancer)


##CHD
patients_chd <- filter(bpp_final1, ICD_Group == "Coronary Heart Disease (CHD)")
head(patients_chd)
bpp2_chd <- patients_chd[,-1]
chd_labels <- patients_chd[,1]
chd_labels <- unlist(chd_labels)
chd_color <- rev(rainbow_hcl(3))[as.numeric(chd_labels)]
pairs(bpp2_chd, col = chd_color,
      lower.panel = NULL,
      cex.labels = 2, pch = 19, cex = 1.2)
par(xpd = TRUE)

#Now let's try parallel coordinates with this Chronic condition:
par(las = 1, mar = c(4.5, 3, 3, 2) + 0.1, cex = 0.8)
MASS::parcoord(bpp2_chd)

##Failing Kidneys
patients_wfkidneys <- filter(bpp_final1, ICD_Group == "Weak or Failing Kidneys")
head(patients_wfkidneys)
bpp2_wfkidneys <- patients_wfkidneys[,-1]
wfkidneys_labels <- patients_wfkidneys[,1]
wfkidneys_labels <- unlist(wfkidneys_labels)
wfkidneys_color <- rev(rainbow_hcl(3))[as.numeric(wfkidneys_labels)]
pairs(bpp2_wfkidneys, col = wfkidneys_color,
      lower.panel = NULL,
      cex.labels = 2, pch = 19, cex = 1.2)
par(xpd = TRUE)

#Now let's try parallel coordinates with this Chronic condition:
par(las = 1, mar = c(4.5, 3, 3, 2) + 0.1, cex = 0.8)
MASS::parcoord(bpp2_wfkidneys)
title("Parallel Coordinates plot for Kidney Patients")
par(xpd = TRUE)


##Hepatitis
patients_hepatitis <- filter(bpp_final1, ICD_Group == "Hepatitis")
head(patients_hepatitis)
bpp2_hepatitis <- patients_hepatitis[,-1]
hepatitis_labels <- patients_hepatitis[,1]
hepatitis_labels <- unlist(hepatitis_labels)
hepatitis_color <- rev(rainbow_hcl(3))[as.numeric(hepatitis_labels)]
pairs(bpp2_hepatitis, col = hepatitis_color,
      lower.panel = NULL,
      cex.labels = 2, pch = 19, cex = 1.2)
par(xpd = TRUE)

#Now let's try parallel coordinates with this Chronic condition:
par(las = 1, mar = c(4.5, 3, 3, 2) + 0.1, cex = 0.8)
MASS::parcoord(bpp2_hepatitis)
title("Parellel Coordinates plot of the Hepatitis Data")
par(xpd = TRUE)

##Diabetes
patients_diabetes <- filter(bpp_final1, ICD_Group == "Diabetes")
head(patients_diabetes)
bpp2_diabetes <- patients_diabetes[,-1]
diabetes_labels <- patients_diabetes[,1]
diabetes_labels <- unlist(diabetes_labels)
diabetes_color <- rev(rainbow_hcl(3))[as.numeric(diabetes_labels)]
pairs(bpp2_diabetes, col = diabetes_color,
      lower.panel = NULL,
      cex.labels = 2, pch = 19, cex = 1.2)
par(xpd = TRUE)

#Now let's try parallel coordinates with this Chronic condition:
par(las = 1, mar = c(4.5, 3, 3, 2) + 0.1, cex = 0.8)
MASS::parcoord(bpp2_diabetes)
title("Parellel Coordinates plot of the Diabetes Data")
par(xpd = TRUE)

##Asthma
patients_asthma <- filter(bpp_final1, ICD_Group == "Asthma")
head(patients_asthma)
bpp2_asthma <- patients_asthma[,-1]
asthma_labels <- patients_asthma[,1]
asthma_labels <- unlist(asthma_labels)
asthma_color <- rev(rainbow_hcl(3))[as.numeric(asthma_labels)]
pairs(bpp2_asthma, col = asthma_color,
      lower.panel = NULL,
      cex.labels = 2, pch = 19, cex = 1.2)
par(xpd = TRUE)

#Now let's try parallel coordinates with this Chronic condition:
par(las = 1, mar = c(4.5, 3, 3, 2) + 0.1, cex = 0.8)
MASS::parcoord(bpp2_asthma)
title("Parellel Coordinates plot of the Diabetes Data")
par(xpd = TRUE)

##COPD
patients_copd <- filter(bpp_final1, ICD_Group == "Chronic Obstructive Pulmonary Disease")
head(patients_copd)
bpp2_copd <- patients_copd[,-1]
copd_labels <- patients_copd[,1]
copd_labels <- unlist(copd_labels)
copd_color <- rev(rainbow_hcl(3))[as.numeric(copd_labels)]
pairs(bpp2_asthma, col = copd_color,
      lower.panel = NULL,
      cex.labels = 2, pch = 19, cex = 1.2)
par(xpd = TRUE)

#Now let's try parallel coordinates with this Chronic condition:
par(las = 1, mar = c(4.5, 3, 3, 2) + 0.1, cex = 0.8)
MASS::parcoord(bpp2_copd)
title("Parellel Coordinates plot of the Diabetes Data")
par(xpd = TRUE)

##HCA will help us now look at these chronic conditions and see how to cluster these based on these variables
dist_cc <- dist(bpp2)
hc_cc <- hclust(dist_cc, method = "complete")
cc_types <- rev(levels(bpp_final1[,1]))

library(dendextend)
dend <- as.dendrogram(hc_cc)
#order it by 
dend <- rotate(dend, 1:13923)

#Color the branches based on the clusters:
dend <- color_branches(dend, k=10)
#This will group by the chronic condition

#The bpp_final1 needs to be unlisted so it can be used as sort level values
unlist(bpp_final1)

unlist(dend)
#Manually match the labels, to the real classification of the chronic conditions:
labels_colors(dend) <-
  rainbow_hcl(3)[sort_levels_values(
    as.numeric(bpp_final1[,1])[order.dendrogram(dend)]
  )]

#Add the Chronic condition to the labels:
labels(dend) <- paste(as.character(bpp_final1[,1])[order.dendrogram(dend)],
                      "(",labels(dend),")",
                      sep = "")
#We hang the dendrogram a bit:
dend <- hang.dendrogram(dend, hang_height = 0.1)
#Reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", 0.5)
#Plot it:
par(mar = c(1,1,1,1))
plot(dend,
     main = "Clustered Chronic Conditions
     (the labels give the true chronic conditions)",
     horiz = TRUE, nodePar = list(cex = 0.007))
legend("left", 
       x = 0.025, y = 0.2, cex = 1,
       legend = as.character(levels(chronic_conditions_labels)),
       fill = unique(cc_color))
par(xpd = NA)

install.packages("circlize")
library(circlize)
par(mar = rep(0,6))
circlize_dendrogram(dend)
legend("left", 
       x = 0.025, y = 0.2, cex = 1,
       legend = as.character(levels(chronic_conditions_labels)),
       fill = unique(cc_color))
par(xpd = NA)

#Now let's see what adding the effects of a heat map can do for finding how the data clusters
install.packages("gplots")
library(gplots)
some_col_func <- function(n) rev(colorspace:: heat_hcl(n, c = c(80, 30), l = c(30,90),
                                                       power= c(1/5, 1.5)))
#Not going to be able to properly see these variables impact if I use them all together because 
# of the different scales they are counting by. Grouping them by like scales should do a better job
# showing their impact per cluster.
bpp.g1 <- select(bpp2, BP_SYSTOLIC, BP_DIASTOLIC, TEMPERATURE, PULSE)
gplots:: heatmap.2(as.matrix(bpp.g1),
                   main = "Heatmap for the Chronic Conditions",
                   strCol = 20,
                   dendrogram = "row",
                   Rowv = dend,
                   Colv = "NA", #makes sure the columns are not ordered
                   trace = "none",
                   margins = c(5, 0.1),
                   key.xlab = "Measure Value",
                   denscol = "grey",
                   density.info = "density",
                   #RowSideColors = rev(labels_colors(dend)), #adds the colored strips
                   col = some_col_func
)
bpp.g2 <- select(bpp2, RESPIRATIONS, BMI, BSA, Poverty_Rate)
gplots:: heatmap.2(as.matrix(bpp.g2),
                   main = "Heatmap for the Chronic Conditions",
                   strCol = 20,
                   dendrogram = "row",
                   Rowv = dend,
                   Colv = "NA", #makes sure the columns are not ordered
                   trace = "none",
                   margins = c(5, 0.1),
                   key.xlab = "Measure Value",
                   denscol = "grey",
                   density.info = "density",
                   #RowSideColors = rev(labels_colors(dend)), #adds the colored strips
                   col = some_col_func
)
bpp.g3 <- select(bpp2, Population, Low_Access_to_Supermarket)
gplots:: heatmap.2(as.matrix(bpp.g3),
                   main = "Heatmap for the Chronic Conditions",
                   strCol = 20,
                   dendrogram = "row",
                   Rowv = dend,
                   Colv = "NA", #makes sure the columns are not ordered
                   trace = "none",
                   margins = c(5, 0.1),
                   key.xlab = "Measure Value",
                   denscol = "grey",
                   density.info = "density",
                   #RowSideColors = rev(labels_colors(dend)), #adds the colored strips
                   col = some_col_func
)
bpp.g4 <- select(bpp2, Access_to_Exercise, High_School_Graduation_Rate, Percentage_Unemployed, Percentage_Uninsured)
gplots:: heatmap.2(as.matrix(bpp.g4),
                   main = "Heatmap for the Chronic Conditions",
                   strCol = 20,
                   dendrogram = "row",
                   Rowv = dend,
                   Colv = "NA", #makes sure the columns are not ordered
                   trace = "none",
                   margins = c(5, 0.1),
                   key.xlab = "Measure Value",
                   denscol = "grey",
                   density.info = "density",
                   #RowSideColors = rev(labels_colors(dend)), #adds the colored strips
                   col = some_col_func
                   )
#Let's show the entire variable group together to prove the scales are off. 
d3heatmap::d3heatmap(as.matrix(bpp2),
                     dendrogram = "row",
                     Rowv = dend,
                     colors = "Greens",
                     width = 600,
                     show_grid = FALSE)
#The variables are difficult to get information from because they are so inconsistent when ran across each cluster
#None of them appear to trend towards a chronic condition unfortunately. This could possibly be the type of HCA method
# being used though.

#We need to ask ourselves if this was the best method of HCA for our data, there are 8 different algorithm methods 
# that can be possibly implemented so we can test them against one another.
hclust_methods <- c("ward.D", "single","complete","average", "mcquitty","median", "centroid", "ward.D2")
cc_dendlist <- dendlist()
for(i in seq_along(hclust_methods)) {
  hc_cc <- hclust(dist_cc, method = hclust_methods[i])
  cc_dendlist <- dendlist(cc_dendlist, as.dendrogram(hc_cc))
}
names(cc_dendlist) <- hclust_methods
cc_dendlist
#The method I used originally was the complete method, which we are seeing is actually our lowest based on height.
#We can create a correlation of these as well and see which methods relate more to each other. 

#Comparing ward.D and ward.D2 methods 
cc_dendlist %>% dendlist(which = c(1, 8)) %>% ladderize %>%
  set("branches_k_color", k = 10) %>%
  tanglegram(faster = TRUE)
#Comparing ward.D and the average
cc_dendlist %>% dendlist(which = c(1,4)) %>% ladderize %>%
  set("branches_k_color", k = 10) %>%
  tanglegram(faster = TRUE)

#Comparing all 8 methods
par(mfrow = c(4,2))
for(i in 1:8) {
  cc_dendlist[[i]] %>% set("branches_k_color", k = 10) %>% plot(axes = FALSE, horiz = TRUE)
  title(names(cc_dendlist)[i])
}

#Test the correlation of the 8 types
cc_dendlist_cor <- cor.dendlist(cc_dendlist, method = "common")
cc_dendlist_cor

#Measuring the performance
get_ordered_10_clusters <- function(dend) {
  cutree(dend, k = 10)[order.dendrogram(dend)]
}

dend_10_clusters <- lapply(cc_dendlist, get_ordered_10_clusters)

compare_clusters_to_cc <- function(clus) {FM_index(clus, rep(1:10, each = 1200), assume_sorted_vectors = 
                                                     TRUE)}
clusters_performance <- sapply(dend_10_clusters, compare_clusters_to_cc)
dotchart(sort(clusters_performance), xlim = c(0.7, 1),
         xlab = "Fowlkes-Mallow Index (from 0 to 1)",
         main = "Performance of Clustering algorithms \n in detecting the 10 chronic conditons",
         pch = 18)
#The dot analysis would show that none of our dendrograms did a great job with matching our values to their specific chronic 
# conditions but based on our overall nodes we did a decent job. 


##K-means analysis
#K-means cluster analysis is great for great way to take high-dimensional data and group data points together based on the number of factors
# within the dataset. K-means are highly algorithmic and iterative and the basic idea is you try to find the centroids of a fixed # of clusters
# of points in  a high-dimensional space. This is a perfect method for our final data is great because we have so many measures to look at and it will
# be interesting if this method will help cluster this data based on the chronic conditions.

#Before performing a k-means analysis, it is smart to find what is the best number of clusters you should use. To do this what you need to
# do is plot the group sum of squares. Before doing that though, it is smart to take out any non-numeric variables. Here that means our chronic conditions
cc_numeric <- select(bpp_final1, -ICD_Group)
wss <- (nrow(cc_numeric)-1)*sum(apply(cc_numeric,2,var))
for (i in 2:16) wss[i] <- sum(kmeans(cc_numeric,
                                     nstart=10,
                                     centers=i)$withinss)
wss_data <- data.frame(centers = 1:16, wss)
ggplot(wss_data, aes(x=centers, y = wss)) + geom_point() + geom_line() + xlab("# of Clusters") +
  ylab("Within Groups Sum of Squares")
#From the appearance of my group sum of squares plot I need to set k=9 for my analysis.


##Clustering at k = 9, the elbow method suggested amount
bpp_final1 %>% select(-ICD_Group) %>% kmeans(centers = 9, nstart = 16) -> cc
cc_clustered <- data.frame(bpp_final1, cluster=factor(cc$cluster))
ggplot(cc_clustered, aes(x=BP_SYSTOLIC, y=BP_DIASTOLIC, color=cluster, shape=ICD_Group)) + geom_point()
#The 9 clusters seem really joined together, I'd also like to try this with the number of clusters as I have Chronic Conditions.
centroids1 <- data.frame(cc$centers)
centroids1
centroids1 <- data.frame(centroids1, cluster = factor(1:9))
ggplot(cc_clustered, aes(x=BP_SYSTOLIC, y=BP_DIASTOLIC, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The 9 clusters don't appear to be different for the two blood pressure measurements, all 9 clusters are in the same general area.
#Let's try and see what all the other combinations of variables yields for four clusters.

#BP_SYSTOLIC vs. Temperature
ggplot(cc_clustered, aes(x=BP_SYSTOLIC, y=TEMPERATURE, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#All 9 clusters center in the same general area.

#BP_Systolic vs. Pulse
ggplot(cc_clustered, aes(x=BP_SYSTOLIC, y=PULSE, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#All centroids conjuer in the same part of the chart

#BP_Systolic vs. Weight
ggplot(cc_clustered, aes(x=BP_SYSTOLIC, y=WEIGHT, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Cluster a couple of clusters are noticeably lower than the majority of centroids.

#BP_Systolic vs. Respirations
ggplot(cc_clustered, aes(x=BP_SYSTOLIC, y=RESPIRATIONS, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#All centers are close, nothing to truly make note of here.

#BP_Systolic vs. BMI
ggplot(cc_clustered, aes(x=BP_SYSTOLIC, y=BMI, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids are a little spread out here, but not really of much note.

#BP Systolic vs. BSA
ggplot(cc_clustered, aes(x=BP_SYSTOLIC, y=BSA, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#These centorids are very clustered together and so are data points.

#BP Systolic vs. Population
ggplot(cc_clustered, aes(x=BP_SYSTOLIC, y=Population, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Cluster 1 is distinctly lower than the other centroids but the data points are blended together a lot with 
# some obvious outliers but nothing that moves the centroids enough.

#BP Systolic vs. Median Family Income
ggplot(cc_clustered, aes(x=BP_SYSTOLIC, y=Median_Family_Income, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#There is a distinct cluster with these points. There isn't a change in the BP_systolic but by the MFI it is obviously
# clustered.

#BP Systolic vs. Supermarket Access
ggplot(cc_clustered, aes(x=BP_SYSTOLIC, y=Low_Access_to_Supermarket, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#These centroids have a lot of distance with only minimal overlapping there, the data points aren't clustered from each other
# they are right around each other but the variance is large. 

#BP Systolic vs. Access to Exercise
ggplot(cc_clustered, aes(x=BP_SYSTOLIC, y=Access_to_Exercise, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Here we can see some distance between the centroids again.The centroids have minimal overlapping but no distance apart 
# with the data points.

#BP Systolic vs. High School Graduation Rate
ggplot(cc_clustered, aes(x=BP_SYSTOLIC, y=High_School_Graduation_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The clusters appear fairly close to one another. Only a couple that don't overlap. They also don't have any noticeable distance
# from the clusters.

#BP Systolic vs. Unemployed
ggplot(cc_clustered, aes(x=BP_SYSTOLIC, y=Percentage_Unemployed, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids are really close to each other, but we do see some outliers primarily from cluster 8 and 7 which is why those 
#2 clusters have a noticeable separation from the others.

#BP Systolic vs. Uninsured
ggplot(cc_clustered, aes(x=BP_SYSTOLIC, y=Percentage_Uninsured, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#There appears to be a a little break out between these centroids, none of the data point clusters are noticeably separated.

#BP Diastolic vs.  Temperature
ggplot(cc_clustered, aes(x=BP_DIASTOLIC, y=TEMPERATURE, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#All 9 centroids overlap upon one another.

#BP Diastolic vs. Pulse
ggplot(cc_clustered, aes(x=BP_DIASTOLIC, y=PULSE, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#All 9 centroids are clustered together tightly

#BP Diastolic vs. Weight
ggplot(cc_clustered, aes(x=BP_DIASTOLIC, y=WEIGHT, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Cluster 4 is centered noticeably below the other clusters, but not enough to suggest any sort of correlation.

#BP Diastolic vs. Respirations
ggplot(cc_clustered, aes(x=BP_DIASTOLIC, y=RESPIRATIONS, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#All 9 centroids are clustered together tightly

#BP Diastolic vs. BMI
ggplot(cc_clustered, aes(x=BP_DIASTOLIC, y=BMI, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#THe data looks very clustered together with nothing distinct of note.

#BP Diastolic vs. BSA
ggplot(cc_clustered, aes(x=BP_DIASTOLIC, y=BSA, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Similar to Diastolic vs. BMI no data points were very distinct or broken out from the others.

#BP Diastolic vs. Population
ggplot(cc_clustered, aes(x=BP_DIASTOLIC, y=Population, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Cluster 1 is centered significantly below the others but the data points are blended together still quite a bit.

#BP Diastolic vs. Poverty Rate
ggplot(cc_clustered, aes(x=BP_DIASTOLIC, y=Poverty_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The clusters appear to have a bit of separation from each other. Some centroids are still overlapping but the data is broken out quite 
# a bit by poverty rate.

#BP Diastolic vs. Median Family Income
ggplot(cc_clustered, aes(x=BP_DIASTOLIC, y=Median_Family_Income, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Based on MFI the clusters are staunchly different, but the BP Diastolic levels are all centered around 70.

#BP Diastolic vs. Access to supermarket
ggplot(cc_clustered, aes(x=BP_DIASTOLIC, y=Low_Access_to_Supermarket, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Supermarket access is definitely a metric that we some variation to here again, with noticeable distance between centroids 
# and a minote amount of cluster separation.

#BP Diastolic vs. Access to Exercise
ggplot(cc_clustered, aes(x=BP_DIASTOLIC, y=Access_to_Exercise, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids appear to be nearly paired off with distinct distance but the outliers to exercise rate could be a bit 
# of the reasons why.

#BP Diastolic vs. High School Grad Rate
ggplot(cc_clustered, aes(x=BP_DIASTOLIC, y=High_School_Graduation_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Besides clusters 1 and 8, the centroids are huddled together. 

#BP Diastolic vs. Unemployed
ggplot(cc_clustered, aes(x=BP_DIASTOLIC, y=Percentage_Unemployed, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The clusters are really mashed together, centroid for cluster 8 is distanced from the others but that is due to the outliers from
# the cluster that are highly noticeable.

#BP Diastolic vs. Uninsured
ggplot(cc_clustered, aes(x=BP_DIASTOLIC, y=Percentage_Uninsured, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#THe clusters have some distance from each other by centroid but the data is still really blended together

#Temp vs. Pulse
ggplot(cc_clustered, aes(x=TEMPERATURE, y=PULSE, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#All of the clusters seem to be the exact same location on the plot.

#Temp vs. Weight
ggplot(cc_clustered, aes(x=TEMPERATURE, y=PULSE, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#All centroids land in seemingly the same location

#Temp vs. Respirations
ggplot(cc_clustered, aes(x=TEMPERATURE, y=RESPIRATIONS, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#All centroids are in the same location

#Temp vs BMI
ggplot(cc_clustered, aes(x=TEMPERATURE, y=BMI, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#All in the same general area, but the centroids are starting to have separation compared to other variables vs.
# body temp.

#Temp vs. BSA
ggplot(cc_clustered, aes(x=TEMPERATURE, y=BSA, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#BSA is similar to BMI in how it spread the centroids, 

#Temp vs. Population
ggplot(cc_clustered, aes(x=TEMPERATURE, y=Population, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids have some separation, none more than cluster 1. The data points aren't separated at all by cluster though.

#Temp vs. Poverty Rate
ggplot(cc_clustered, aes(x=TEMPERATURE, y=Poverty_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids are seemingly paired off based on the poverty rate, the data points are lightly clustered separately where
# there is only 2 or 3 types of data points per poverty rate.

#Temp vs. Median Family Income
ggplot(cc_clustered, aes(x=TEMPERATURE, y=Median_Family_Income, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Here we are seeing distinct clusters, Each centroid is at a certain level of MFI, and though they're on the same level of
# body temperature, the data points aren't mingled with the others.

#Temp vs. Supermarket access
ggplot(cc_clustered, aes(x=TEMPERATURE, y=Low_Access_to_Supermarket, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The clusters appear to be distanced by center but besides a few spots there aren't distinct clusters

#Temp vs. Access to Exercise
ggplot(cc_clustered, aes(x=TEMPERATURE, y=Access_to_Exercise, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids are split out noticeably into 4 different areas and the data points in those areas appear to correlate with
# the centroid in the area.

#Temp vs. High School Grad Rate
ggplot(cc_clustered, aes(x=TEMPERATURE, y=High_School_Graduation_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Most centroids are in the same general area but cluster 1 and 8 are noticeably below that.

#Temp vs. Unemployed
ggplot(cc_clustered, aes(x=TEMPERATURE, y=Percentage_Unemployed, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#All these centroids are in the same general area, with all data points blended together.

#Temp vs. Uninsured
ggplot(cc_clustered, aes(x=TEMPERATURE, y=Percentage_Uninsured, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids are broken out a little bit, but the data is really blended together but the variance is significant.

#Pulse vs. Weight
ggplot(cc_clustered, aes(x=PULSE, y=WEIGHT, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#These centroids come together in the same general area

#Pulse vs. Respirations
ggplot(cc_clustered, aes(x=PULSE, y=RESPIRATIONS, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#All the centroids cluster together 

#Pulse vs. BMI
ggplot(cc_clustered, aes(x=PULSE, y=BMI, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The data is really blended together, there are a couple of clusters that are lower but not of any siginficant distance for 
# either centroid of 4 and 5. 

#Pulse vs. BSA
ggplot(cc_clustered, aes(x=PULSE, y=BSA, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids are all seemingly clustered together, with no considerable distance.

#Pulse vs. Population
ggplot(cc_clustered, aes(x=PULSE, y=Population, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Centroid 1 is noticeably lower than the other centroids but the data points are blended together quite a bit.

#Pulse vs. Poverty Rate
ggplot(cc_clustered, aes(x=PULSE, y=Poverty_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids have some distance from one another, but some do have pairs within a close proximity. But because of the 
#distance in centroid they have some distinct clusters in the data.

#Pulse vs. Median Family Income
ggplot(cc_clustered, aes(x=PULSE, y=Median_Family_Income, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Here there is a noticeable clustering of data points, what is less noticeable but still true the data points as they 
# go up in Median Family Income, the pulse slightly drops.

#Pulse vs. Supermarket access
ggplot(cc_clustered, aes(x=PULSE, y=Low_Access_to_Supermarket, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The data has some considerable distance from each other, but the data is blended together quite a bit 

#Pulse vs. Access to Exercise
ggplot(cc_clustered, aes(x=PULSE, y=Access_to_Exercise, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The points for cluster 1 appear to all be centered up near the top, though blended with some of the other clusters, 
# the centroids though seem quite spread out.

#Pulse vs. High School Graduation Rate
ggplot(cc_clustered, aes(x=PULSE, y=High_School_Graduation_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids for cluster 1 and 8 have some distance but no distinguishable clusters.

#Pulse vs. Unemployed
ggplot(cc_clustered, aes(x=PULSE, y=Percentage_Unemployed, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#These clusters are generally close to one another, and also the centroids overlap except for the centroid for cluster 8 whihc 
# is a little higher

#Pulse vs. Uninsured
ggplot(cc_clustered, aes(x=PULSE, y=Percentage_Uninsured, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids are distinct but the data points are blended together making for no distinct clusters.

#Weight vs. Respirations
ggplot(cc_clustered, aes(x=WEIGHT, y=RESPIRATIONS, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids form around the same point on the chart, and the data points are blended together

#Weight vs. BMI
ggplot(cc_clustered, aes(x=WEIGHT, y=BMI, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The data is appears correlated, as BMI rises, so does weight, but the data is blended and the centroids are overlapping

#Weight vs. BSA
ggplot(cc_clustered, aes(x=WEIGHT, y=BSA, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#There is a noticeable positive trend here with these 2 variables, but all centroids and data points are blended in the same area.

#Weight vs. Population
ggplot(cc_clustered, aes(x=WEIGHT, y=Population, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Cluster 1 has a significant distance below the other centroids, but all the data points are blended together.

#Weight vs. Poverty Rate
ggplot(cc_clustered, aes(x=WEIGHT, y=Poverty_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The data appears to have some distance from other clusters and that is because the centroids have some separation of note.

#Weight vs. Median Family Income
ggplot(cc_clustered, aes(x=WEIGHT, y=Median_Family_Income, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Here the data is clustered, by MFI, but for the weight that is less distinguishable because each centroid appears to line
# up vertically

#Weight vs. Supermarket Access
ggplot(cc_clustered, aes(x=WEIGHT, y=Low_Access_to_Supermarket, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#A lot of the centroids are separated but the data is still for the most part very blended together.

#Weight vs. Exercise Access
ggplot(cc_clustered, aes(x=WEIGHT, y=Access_to_Exercise, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids aren't all huddled together and some are spread out because of the wide variance between access to exercise 
# variable

#Weight vs. HS Grad Rate
ggplot(cc_clustered, aes(x=WEIGHT, y=High_School_Graduation_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids are fairly clustered together and the data is blended together. Only centroids 1 and 8 are away from the
# formed cluster of centroids and that is because the data for those clusters is definitely lower.

#Weight vs. Unemployed
ggplot(cc_clustered, aes(x=WEIGHT, y=Percentage_Unemployed, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Besides some outliers the data appears rather smashed together with the centroids being close together too with only 
#centroid 8 having some distance from the other centroid

#Weight vs. Uninsured
ggplot(cc_clustered, aes(x=WEIGHT, y=Percentage_Uninsured, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The data is blended together, but the centroids are a solid distance apart from each other.

#Respirations vs. BMI
ggplot(cc_clustered, aes(x=RESPIRATIONS, y=BMI, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids and data are mixed in together

#Respirations vs. BSA
ggplot(cc_clustered, aes(x=RESPIRATIONS, y=BSA, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Similar to BMI vs Respirations, data blended together and so are centroids.

#Respirations vs. Population
ggplot(cc_clustered, aes(x=RESPIRATIONS, y=Population, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The data appears to be really mixed togehter but centroid 1 is significantly lower than the other 8.

#Respirations vs. Poverty Rate
ggplot(cc_clustered, aes(x=RESPIRATIONS, y=Poverty_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The data appears to be noticeably different where the data points are spread out by centroids so much so that the 
# data appears to cluster in these areas.

#Respirations vs. Median Family Income
ggplot(cc_clustered, aes(x=RESPIRATIONS, y=Median_Family_Income, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Distinct cluster of the 9, but they all appear to be in the same area with respirations.

#Respirations vs. Supermarket access
ggplot(cc_clustered, aes(x=RESPIRATIONS, y=Low_Access_to_Supermarket, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids really have some distance from each other, where the data is spread but the data clusters are blended together 

#Respirations vs. Access to Exercise
ggplot(cc_clustered, aes(x=RESPIRATIONS, y=Access_to_Exercise, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids have been separated in about 4 groups, with no noticeable clusters but a wide variance of data for access
# to exercise

#Respirations vs. HS Grad Rates
ggplot(cc_clustered, aes(x=RESPIRATIONS, y=High_School_Graduation_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The data is blended together but centroids for cluster 1 and 8 do have some distance 

#Respirations vs. Unemployed
ggplot(cc_clustered, aes(x=RESPIRATIONS, y=Percentage_Unemployed, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Besides some outliers, the data looks generally clustered together

#Respirations vs. Uninsured
ggplot(cc_clustered, aes(x=RESPIRATIONS, y=Percentage_Uninsured, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids are distinctly separtate but the data is quite spread out, while still being blended together.

#BMI vs. BSA
ggplot(cc_clustered, aes(x=BMI, y=BSA, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The data appears to correlate positively, but the centroids are very close and the data all blends together.

#BMI vs. Population
ggplot(cc_clustered, aes(x=BMI, y=Population, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The data and centroids are blended together quite a bit but centroid 1 which is quite a bit lower than all other centroids.

#BMI vs. Poverty Rate
ggplot(cc_clustered, aes(x=BMI, y=Poverty_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The poverty rate gives us some distinct groups where centroids are almost paired in their areas along with the data points 
# surrounding them.

#BMI vs. Median Family Income
ggplot(cc_clustered, aes(x=BMI, y=Median_Family_Income, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#By MFI the data is distinctly different but all generated around the same level of BMI

#BMI vs. Supermarket access
ggplot(cc_clustered, aes(x=BMI, y=Low_Access_to_Supermarket, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids look a little spread out here, but most of the data points are still blended together.

#BMI vs. Exercise Access
ggplot(cc_clustered, aes(x=BMI, y=Access_to_Exercise, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids are spread out by the access to exercise but not as much by the BMI

#BMI vs. HS Grad Rate
ggplot(cc_clustered, aes(x=BMI, y=High_School_Graduation_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Centroids for cluster 1 and 8 are pretty low compared to the other centroids but the data points are very blended together

#BMI vs. Unemployed
ggplot(cc_clustered, aes(x=BMI, y=Percentage_Unemployed, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroid for cluster 8 is higher than the others but they are generally really close in distance, cluster 8 does have 
#some very high outliers which could have caused the centroid being higher than the others.

#BMI vs. Uninsured
ggplot(cc_clustered, aes(x=BMI, y=Percentage_Uninsured, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids are separated, but the data is very blended together

#BSA vs. Population
ggplot(cc_clustered, aes(x=BSA, y=Population, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Besides the centroid for cluster 1 the centroids are in the same location 

#BSA vs. Poverty Rate
ggplot(cc_clustered, aes(x=BSA, y=Poverty_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#THe clusters are separated out a little based around their centroids. Some of the centroids are near each other 
#and that is the only true blending seen.

#BSA vs. Median Family Income
ggplot(cc_clustered, aes(x=BSA, y=Median_Family_Income, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#This data is clustered with the centroids in their own area. There is a slight chance to interpret that as 
#we go down the MFI scale the BSA slowly goes up, but not signficantly

#BSA vs. Supermarket Access
ggplot(cc_clustered, aes(x=BSA, y=Low_Access_to_Supermarket, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The data is spread out quite a bit and that allows some centroids to form for each cluster with decent distance 
# from the others.

#BSA vs. Exercise Access
ggplot(cc_clustered, aes(x=BSA, y=Access_to_Exercise, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The clusters are blended quite a bit, even with the wide variance. The centroids aren't all in huddled in 1 spot but they 
# have other centroids they are securely overlapping.

#BSA vs. HS Grad Rate
ggplot(cc_clustered, aes(x=BSA, y=High_School_Graduation_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Data points and centroids appear a bit mashed together besides the ones for clusters 1 and 8 which are lower

#BSA vs. Unemployed
ggplot(cc_clustered, aes(x=BSA, y=Percentage_Unemployed, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Data generally clustered together, nothing really distinct

#BSA vs. Uninsured
ggplot(cc_clustered, aes(x=BSA, y=Percentage_Uninsured, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids are spread out somewhat but the data points are not, they're quite blended together

#Population vs. Poverty Rate
ggplot(cc_clustered, aes(x=Population, y=Poverty_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The distance between the centroids is distinct in both directions of the plane. Making it apparent that patients
# from cluster 1 are from smaller populated areas but higher poverty rates

#Population vs. Median Family Income
ggplot(cc_clustered, aes(x=Population, y=Median_Family_Income, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Clustering clear here, the patients from cluster 1 appear to be from lower MFI families 

#Population vs. Supermarket Access
ggplot(cc_clustered, aes(x=Population, y=Low_Access_to_Supermarket, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The clusters are quite spread out here as well. Cluster 9 appears to have the highest population with poor supermarket access.

#Population vs. Access to Exercise
ggplot(cc_clustered, aes(x=Population, y=Access_to_Exercise, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Though there is quite a blend to the data, there is some decent distance between centroids.

#Population vs. High School Grad Rate
ggplot(cc_clustered, aes(x=Population, y=High_School_Graduation_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#All centroids are clustered together besides cluster 1 where the HS grad rate and population are the lowest group

#Population vs. Unemployed
ggplot(cc_clustered, aes(x=Population, y=Percentage_Unemployed, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#A lot of data clustered close together here, causing also to have the centroids clustered together mostly.

#Population vs. Uninsured
ggplot(cc_clustered, aes(x=Population, y=Percentage_Uninsured, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The data points are blended together, but the centroids are pretty spread out a little bit.

#Poverty Rate vs. Median Family Income
ggplot(cc_clustered, aes(x=Poverty_Rate, y=Median_Family_Income, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#This is a very fascinating plot, all clusters are separte, and the data is noticeably trended negatively, as poverty rate rises,
# the MFI falls

#Poverty Rate vs. Supermarket Access
ggplot(cc_clustered, aes(x=Poverty_Rate, y=Low_Access_to_Supermarket, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Clusters have centroids that with a lot of distance amongst them causing some separation in the clusters.

#Poverty Rate vs. Access to Exercise
ggplot(cc_clustered, aes(x=Poverty_Rate, y=Access_to_Exercise, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The data is quite spread out, causing some interesting centroid locations and some noticeable clusters on the plane

#Poverty Rate vs. HS Grad Rate
ggplot(cc_clustered, aes(x=Poverty_Rate, y=High_School_Graduation_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Centroids are in several interesting areas, it is obvious too that as grad rate falls, poverty rate rises

#Poverty Rate vs. Unemployed
ggplot(cc_clustered, aes(x=Poverty_Rate, y=Percentage_Unemployed, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The clusters are spread out mainly by poverty rate and less so the unemployment percentage but it allows distinct clustering to form.

#Poverty Rate vs. Uninsured
ggplot(cc_clustered, aes(x=Poverty_Rate, y=Percentage_Uninsured, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Positive trend forming where as uninsured rises so does the poverty rate, this allows distinct clusters to form in parts 
# of the plot.

#Medain Family Income vs. Supemarket Access
ggplot(cc_clustered, aes(x=Median_Family_Income, y=Low_Access_to_Supermarket, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Surprising trend forming where the higher the MFI the more of a population is far away from a supermarket. 

#Median Family Income vs. Access to Exercise
ggplot(cc_clustered, aes(x=Median_Family_Income, y=Access_to_Exercise, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#This is an interesting plot becasue o fthe position of the centroids and the trend of the data looks like it could 
# be trending towards the better access to exercise the higher MFI.

#Median Family Income vs. HS Grad Rate
ggplot(cc_clustered, aes(x=Median_Family_Income, y=High_School_Graduation_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The high school grad rates are close together, but the clusters are separated, from each other.

#Median Family Income vs. Unemployed
ggplot(cc_clustered, aes(x=Median_Family_Income, y=Percentage_Unemployed, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The data isn't noticeably different by unemployed rate, but it is by MFI, allowing the clusters to be noticeably distant from each other

#Median Family Income vs. Uninsured
ggplot(cc_clustered, aes(x=Median_Family_Income, y=Percentage_Uninsured, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids are on different levels, and the data clusters are noticeably separated

#Supermarket Access vs. Exercise Access
ggplot(cc_clustered, aes(x=Low_Access_to_Supermarket, y=Access_to_Exercise, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The data appears to be blended together, though the centroids are quite spread out.

#Supermarket Access vs. HS grad rate
ggplot(cc_clustered, aes(x=Low_Access_to_Supermarket, y=High_School_Graduation_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The data appears to be fairly similar based on HS grad rate except for clusters 1 and 8 are lower and with the distinct 
# difference in the MFI then it will allow for clustering and separation to take place like this.

#Supermarket vs. Unemployed
ggplot(cc_clustered, aes(x=Low_Access_to_Supermarket, y=Percentage_Unemployed, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#Centroids all around the same value for unemployed, with their distance for supermarket access being staunchly different but
# the data is bunched togeterh enough to make it hard to see any specific group clustering.

#Supermarket vs. Uninsured
ggplot(cc_clustered, aes(x=Low_Access_to_Supermarket, y=Percentage_Uninsured, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The centroids look to have a trend where as access to supermarkets by population rises the # of uninsured drops

#Exercise Access vs. HS grad rate
ggplot(cc_clustered, aes(x=Access_to_Exercise, y=High_School_Graduation_Rate, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#the data points are spread out quite a bit which allows some space between the centroids but no noticeable clusters.

#Exercise Access vs. Unemployed
ggplot(cc_clustered, aes(x=Access_to_Exercise, y=Percentage_Unemployed, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The access to exercise variance being so high is the only reason these centroids are as far apart as they are.

#Exercise Access vs. Uninsured
ggplot(cc_clustered, aes(x=Access_to_Exercise, y=Percentage_Uninsured, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The data pointa are really blended together, the centroids do have a lot of distance between each other though becasue
# of the access to exercise variable.

#HS Grad Rate vs. Unemployed
ggplot(cc_clustered, aes(x=High_School_Graduation_Rate, y=Percentage_Unemployed, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The data points and centroids are very clustered together here, only cluster 1 & 8 have centroids not overlapping other centroids.

#HS Grad Rate vs. Uninsured
ggplot(cc_clustered, aes(x=High_School_Graduation_Rate, y=Percentage_Uninsured, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
#The data points are blended together quite a bit but the centroids are separated out quite a bit. 

#Unemployed vs. Uninsured
ggplot(cc_clustered, aes(x=Percentage_Unemployed, y=Percentage_Uninsured, color=cluster)) + geom_point(aes(shape=ICD_Group))+
  geom_point(data=centroids1, aes(fill=cluster), shape =21, color= "black", size=4, stroke = 1)
# The data is very blended together, but the centroids have a noticeable distance from each other.

#The k-means charts had a lot of consistency with some variables where it forced the clusters into the same general areas 
# but there were a few variables that really spread out the data causing for some good insight into the clusters and the variables 
# making them react because of it.




