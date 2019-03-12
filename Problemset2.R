# this code is for the problem set of topic in macro and Labor course
# code is beased on the following two papers
# 1. Acemoglu, D. and Autor, D. (2011). Handbook of Labor Economics, 4b:1043–1171
# 2. Katz, L. F. and Murphy, K. M. (1992). The Quarterly Journal of Economics
# clear environment
rm(list = ls())
# laad libaries
install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(lubridate)
# read the downloaded
data_00 <- read_fwf(file="data_00.dat",                     
                    fwf_cols(year      = c(1, 4),
                             serial    = c(5,9),
                             month     = c(10,11),
                            # hwtfinl   = c(12,21),
                             cpsid     = c(12,25),
                             asecflag  = c(26,26),
                             hflag     = c(27,27),
                             asecwth   = c(28,37),
                             pernum    = c(38,39),
                            # wtfinl    = c(50,63),
                             cpsidp    = c(40,53),
                             asecwt    = c(54,63),
                             age       = c(64,65),
                             sex       = c(66,66),
                             race      = c(67,69),
                             educ      = c(70,72),
                             schlcoll  = c(73,73),
                             indly     = c(74,77),
                             classwly  = c(78,79),
                             wkswork1  = c(80,81),
                             wkswork2  = c(82,82),
                             fullpart  = c(83,83),
                             incwage   = c(84,90)),
                    col_types = cols(year       = "i",
                                     serial     = "n",
                                     month      = "i",
                                   #  hwtfinl    = "d",
                                     cpsid      = "d",
                                     asecflag   = "i",
                                     hflag      = "i",
                                     asecwth    = "d",
                                     pernum     = "i",
                                    # wtfinl     = "d",
                                     cpsidp     = "d",
                                     asecwt     = "d",                    
                                     age        = "i",
                                     sex        = "i",
                                     race       = "i",
                                     educ       = "i",
                                     schlcoll   = "i",
                                     indly      = "i",
                                     classwly   = "i",
                                     wkswork1   = "i",
                                     wkswork2   = "i",
                                     fullpart   = "i",
                                     incwage    = "n"))
#data_00$hwtfinl = data_00$hwtfinl/10000
#data_00$wtfinl = data_00$wtfinl/10000
data_00$asecwt = data_00$asecwt/10000
# merge cpi data (see Acemoglu and Autor's Data Appendix)
data_cpi <- read_csv(file = "data_cpi.csv", col_names = c("year","cpi"), col_types=cols(year = "D", cpi = "d"), skip = 1)
data_cpi$year <- year(data_cpi$year)
data_cpi <- data_cpi %>%
  mutate(price_1982 = ifelse(year == 1982, cpi, 0)) %>% # the base year is 1982 (see Acemoglu and Autor's Data Appendix)
  mutate(price_1982 = max(price_1982)) %>%
  mutate(cpi = cpi/price_1982) %>%
  select(year, cpi)
data_00 <- data_00 %>%
  left_join(data_cpi, by = "year")
# replace missing values
data_00 <- data_00 %>%
  mutate(educ = ifelse(educ == 999, NA, educ)) %>%
  mutate(classwly = ifelse(classwly == 99, NA, classwly)) %>%  
  mutate(wkswork2 = ifelse(wkswork2 == 999, NA, wkswork2)) %>%  
  mutate(incwage = ifelse(incwage == 9999999 | incwage == 9999998, NA, incwage)) %>%
  mutate(race = ifelse(race == 999, NA, race))
# create wrkswork variable: worked weeks are in brackets before 1976 see Katz and Murphy (1992)
data_00 <- data_00 %>%
  mutate(wkswork = ifelse(year >= 1976, wkswork1, NA)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 1, 7, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 2, 20, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 3, 33, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 4, 43.5, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 5, 48.5, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 6, 51, wkswork))
# handle the top coding issue for income see Katz and Murphy (1992)'s Data section
data_00 <- data_00 %>%
  group_by(year) %>%
  mutate(top_incwage = max(incwage, na.rm = TRUE)) %>%
  mutate(incwage = ifelse(incwage == top_incwage, 1.45*incwage, incwage)) %>%
  ungroup()
# calculate log real wages
data_00 <- data_00 %>%
  mutate(rwage = incwage/cpi/wkswork) %>%
  mutate(lrwage = log(rwage))
# create education duammies
data_00 <- data_00 %>%
  mutate(dfemale = (sex == 2)) # female
data_00 <- data_00 %>%      
  mutate(deduc_1 = ifelse(educ < 70, 1, 0)) %>%                # highshool dropout
  mutate(deduc_2 = ifelse(educ >= 80 & educ < 110, 1, 0)) %>%  # some college
  mutate(deduc_3 = ifelse(educ >= 110 & educ < 123, 1, 0)) %>% # 4 years college 
  mutate(deduc_4 = ifelse(educ >= 123, 1, 0))                  # more than college
data_00 <- data_00 %>%
  mutate(drace_1 = ifelse(race == 200,1,0)) %>% # black
  mutate(drace_2 = ifelse(race > 200,1,0)) # nonwhite other
# create experience variable: check the IPUMS website for variable definition
data_00 <- data_00 %>%
  mutate(exp = ifelse(educ == 10, age - 8.5, NA)) %>%
  mutate(exp = ifelse(educ == 11, age - 7, exp)) %>%
  mutate(exp = ifelse(educ == 12, age - 8, exp)) %>%
  mutate(exp = ifelse(educ == 13, age - 9, exp)) %>%
  mutate(exp = ifelse(educ == 14, age - 10, exp)) %>%
  mutate(exp = ifelse(educ == 20, age - 11.5, exp)) %>%
  mutate(exp = ifelse(educ == 21, age - 11, exp)) %>%
  mutate(exp = ifelse(educ == 22, age - 12, exp)) %>%
  mutate(exp = ifelse(educ == 30, age - 13.5, exp)) %>%
  mutate(exp = ifelse(educ == 31, age - 13, exp)) %>%
  mutate(exp = ifelse(educ == 32, age - 14, exp)) %>%
  mutate(exp = ifelse(educ == 40, age - 15, exp)) %>%
  mutate(exp = ifelse(educ == 50, age - 16, exp)) %>%
  mutate(exp = ifelse(educ == 60, age - 17, exp)) %>%
  mutate(exp = ifelse(educ == 70, age - 18, exp)) %>%
  mutate(exp = ifelse(educ == 71, age - 18, exp)) %>%
  mutate(exp = ifelse(educ == 72, age - 18, exp)) %>%
  mutate(exp = ifelse(educ == 73, age - 18, exp)) %>%
  mutate(exp = ifelse(educ == 80, age - 19, exp)) %>%
  mutate(exp = ifelse(educ == 81, age - 19, exp)) %>%
  mutate(exp = ifelse(educ == 90, age - 20, exp)) %>%
  mutate(exp = ifelse(educ == 91, age - 20, exp)) %>%
  mutate(exp = ifelse(educ == 92, age - 20, exp)) %>%
  mutate(exp = ifelse(educ == 100, age - 21, exp)) %>%
  mutate(exp = ifelse(educ == 110, age - 22, exp)) %>%
  mutate(exp = ifelse(educ == 111, age - 22, exp)) %>%
  mutate(exp = ifelse(educ == 120, age - 23.5, exp)) %>%
  mutate(exp = ifelse(educ == 121, age - 23, exp)) %>%
  mutate(exp = ifelse(educ == 122, age - 24, exp)) %>%
  mutate(exp = ifelse(educ == 123, age - 23, exp)) %>%
  mutate(exp = ifelse(educ == 124, age - 23, exp)) %>%
  mutate(exp = ifelse(educ == 125, age - 27, exp))
# sample selection (see Katz and Murphy (1992) and Acemoglu and Autor (2011)'s Data Appendix)
data_00 <- data_00 %>%
  filter(rwage >= 67) %>%                                                       # real wage more than 67 dollars in the 1987 dollar
  filter(age >= 16 & age <= 64) %>%                                             # age equal or above 16 and equal or less than 64
  filter(fullpart == 1) %>%                                                     # work more than 35 hours
  filter(wkswork >= 40) %>%                                                     # work more than 40 weeks
  filter(classwly != 10 | classwly != 13 | classwly != 14) %>%                  # not self-employed
  filter(!((year >= 1992 & year <= 2002) & (indly >= 940 & indly <= 960))) %>%  # not in military
  filter(!(year >= 2003 & indly == 9890)) %>%
  filter(schlcoll == 5 | year < 1986) %>%                                       # no school attendance
  filter(exp >= 0)                                                              # get rid of negative experience


#####################################################################################################################
#Export data

write_csv(data_00, "data_00.csv")

#Attach the data

attach(data_00)

###################################Figure 1####################################################

#Create matrices to store predicted wages in each year and weights of each group

# Since values of variables in 1963 are N/A, I start from the year 1964

predict_wage <- matrix(0,54,50)
weight <- matrix(0,1,50)

# Predict lrwage for each group in each year

for (i in 1964:2017){
  
  #Run regression for male and femal separately
  
  #Female
  
  rfemale <- lm(lrwage~(deduc_1+deduc_2+deduc_3+deduc_4)*(exp+I(exp^2)+I(exp^3)+I(exp^4))+drace_1+drace_2+(deduc_1+deduc_2+deduc_3+deduc_4):exp:(drace_1+drace_2),subset = (dfemale=="TRUE" & year==i))
  
  #Male
  
  rmale <- lm(lrwage~(deduc_1+deduc_2+deduc_3+deduc_4)*(exp+I(exp^2)+I(exp^3)+I(exp^4))+drace_1+drace_2+(deduc_1+deduc_2+deduc_3+deduc_4):exp:(drace_1+drace_2),subset = (dfemale=="FALSE" & year==i))
  
  #Predict for each group
  
  predict_wage[i-1963,1] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=0,exp=5)) #Female, 5 year experience, high school graduated, white
  predict_wage[i-1963,2] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=1,deduc_2=0,deduc_3=0,deduc_4=0,exp=5)) #Female, 5 year experience, high school dropout, white
  predict_wage[i-1963,3] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=1,deduc_3=0,deduc_4=0,exp=5)) #Female, 5 year experience, some college, white
  predict_wage[i-1963,4] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=1,deduc_4=0,exp=5)) #Female, 5 year experience, college graduated, white
  predict_wage[i-1963,5] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=1,exp=5)) #Female, 5 year experience, grater than college, white
  predict_wage[i-1963,6] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=0,exp=15)) #Female, 15 year experience, high school graduated, white
  predict_wage[i-1963,7] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=1,deduc_2=0,deduc_3=0,deduc_4=0,exp=15)) #Female, 15 year experience, high school dropout, white
  predict_wage[i-1963,8] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=1,deduc_3=0,deduc_4=0,exp=15)) #Female, 15 year experience, some college, white
  predict_wage[i-1963,9] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=1,deduc_4=0,exp=15)) #Female, 15 year experience, college graduated, white
  predict_wage[i-1963,10] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=1,exp=15)) #Female, 15 year experience, grater than college, white
  predict_wage[i-1963,11] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=0,exp=25)) #Female, 25 year experience, high school graduated, white
  predict_wage[i-1963,12] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=1,deduc_2=0,deduc_3=0,deduc_4=0,exp=25)) #Female, 25 year experience, high school dropout, white
  predict_wage[i-1963,13] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=1,deduc_3=0,deduc_4=0,exp=25)) #Female, 25 year experience, some college, white
  predict_wage[i-1963,14] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=1,deduc_4=0,exp=25)) #Female, 25 year experience, college graduated, white
  predict_wage[i-1963,15] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=1,exp=25)) #Female, 25 year experience, grater than college, white
  predict_wage[i-1963,16] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=0,exp=35)) #Female, 35 year experience, high school graduated, white
  predict_wage[i-1963,17] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=1,deduc_2=0,deduc_3=0,deduc_4=0,exp=35)) #Female, 35 year experience, high school dropout, white
  predict_wage[i-1963,18] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=1,deduc_3=0,deduc_4=0,exp=35)) #Female, 35 year experience, some college, white
  predict_wage[i-1963,19] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=1,deduc_4=0,exp=35)) #Female, 35 year experience, college graduated, white
  predict_wage[i-1963,20] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=1,exp=35)) #Female, 35 year experience, grater than college, white
  predict_wage[i-1963,21] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=0,exp=45)) #Female, 5 year experience, high school graduated, white
  predict_wage[i-1963,22] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=1,deduc_2=0,deduc_3=0,deduc_4=0,exp=45)) #Female, 5 year experience, high school dropout, white
  predict_wage[i-1963,23] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=1,deduc_3=0,deduc_4=0,exp=45)) #Female, 5 year experience, some college, white
  predict_wage[i-1963,24] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=1,deduc_4=0,exp=45)) #Female, 5 year experience, college graduated, white
  predict_wage[i-1963,25] <-predict(rfemale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=1,exp=45)) #Female, 5 year experience, grater than college, white
  predict_wage[i-1963,26] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=0,exp=5)) #male, 5 year experience, high school graduated, white
  predict_wage[i-1963,27] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=1,deduc_2=0,deduc_3=0,deduc_4=0,exp=5)) #male, 5 year experience, high school dropout, white
  predict_wage[i-1963,28] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=1,deduc_3=0,deduc_4=0,exp=5)) #male, 5 year experience, some college, white
  predict_wage[i-1963,29] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=1,deduc_4=0,exp=5)) #male, 5 year experience, college graduated, white
  predict_wage[i-1963,30] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=1,exp=5)) #male, 5 year experience, grater than college, white
  predict_wage[i-1963,31] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=0,exp=15)) #male, 15 year experience, high school graduated, white
  predict_wage[i-1963,32] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=1,deduc_2=0,deduc_3=0,deduc_4=0,exp=15)) #male, 15 year experience, high school dropout, white
  predict_wage[i-1963,33] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=1,deduc_3=0,deduc_4=0,exp=15)) #male, 15 year experience, some college, white
  predict_wage[i-1963,34] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=1,deduc_4=0,exp=15)) #male, 15 year experience, college graduated, white
  predict_wage[i-1963,35] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=1,exp=15)) #male, 15 year experience, grater than college, white
  predict_wage[i-1963,36] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=0,exp=25)) #male, 25 year experience, high school graduated, white
  predict_wage[i-1963,37] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=1,deduc_2=0,deduc_3=0,deduc_4=0,exp=25)) #male, 25 year experience, high school dropout, white
  predict_wage[i-1963,38] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=1,deduc_3=0,deduc_4=0,exp=25)) #male, 25 year experience, some college, white
  predict_wage[i-1963,39] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=1,deduc_4=0,exp=25)) #male, 25 year experience, college graduated, white
  predict_wage[i-1963,40] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=1,exp=25)) #male, 25 year experience, grater than college, white
  predict_wage[i-1963,41] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=0,exp=35)) #male, 35 year experience, high school graduated, white
  predict_wage[i-1963,42] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=1,deduc_2=0,deduc_3=0,deduc_4=0,exp=35)) #male, 35 year experience, high school dropout, white
  predict_wage[i-1963,43] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=1,deduc_3=0,deduc_4=0,exp=35)) #male, 35 year experience, some college, white
  predict_wage[i-1963,44] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=1,deduc_4=0,exp=35)) #male, 35 year experience, college graduated, white
  predict_wage[i-1963,45] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=1,exp=35)) #male, 35 year experience, grater than college, white
  predict_wage[i-1963,46] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=0,exp=45)) #male, 5 year experience, high school graduated, white
  predict_wage[i-1963,47] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=1,deduc_2=0,deduc_3=0,deduc_4=0,exp=45)) #male, 5 year experience, high school dropout, white
  predict_wage[i-1963,48] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=1,deduc_3=0,deduc_4=0,exp=45)) #male, 5 year experience, some college, white
  predict_wage[i-1963,49] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=1,deduc_4=0,exp=45)) #male, 5 year experience, college graduated, white
  predict_wage[i-1963,50] <-predict(rmale, newdata=data.frame(drace_1=0,drace_2=0,deduc_1=0,deduc_2=0,deduc_3=0,deduc_4=1,exp=45)) #male, 5 year experience, grater than college, white
}

# Calculate weights for each group (i.e. sum wkswork of each group for all years)

weight[,1]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==5 )) #Female, 5 year experience, high school graduated, white
weight[,2]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==1 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==5 )) #Female, 5 year experience, high school dropout, white
weight[,3]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==1 & deduc_3==0 & deduc_4==0 & exp==5 )) #Female, 5 year experience, some college, white
weight[,4]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==1 & deduc_4==0 & exp==5 )) #Female, 5 year experience, college graduated, white
weight[,5]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==1 & exp==5 )) #Female, 5 year experience, greater than college, white
weight[,6]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==15 )) #Female, 15 year experience, high school graduated, white
weight[,7]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==1 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==15 )) #Female, 15 year experience, high school dropout, white
weight[,8]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==1 & deduc_3==0 & deduc_4==0 & exp==15 )) #Female, 15 year experience, some college, white
weight[,9]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==1 & deduc_4==0 & exp==15 )) #Female, 15 year experience, college graduated, white
weight[,10]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==1 & exp==15 )) #Female, 15 year experience, greater than college, white
weight[,11]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==25 )) #Female, 25 year experience, high school graduated, white
weight[,12]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==1 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==25 )) #Female, 25 year experience, high school dropout, white
weight[,13]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==1 & deduc_3==0 & deduc_4==0 & exp==25 )) #Female, 25 year experience, some college, white
weight[,14]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==1 & deduc_4==0 & exp==25 )) #Female, 25 year experience, college graduated, white
weight[,15]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==1 & exp==25 )) #Female, 25 year experience, greater than college, white
weight[,16]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==35 )) #Female, 35 year experience, high school graduated, white
weight[,17]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==1 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==35 )) #Female, 35 year experience, high school dropout, white
weight[,18]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==1 & deduc_3==0 & deduc_4==0 & exp==35 )) #Female, 35 year experience, some college, white
weight[,19]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==1 & deduc_4==0 & exp==35 )) #Female, 35 year experience, college graduated, white
weight[,20]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==1 & exp==35 )) #Female, 35 year experience, greater than college, white
weight[,21]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==45 )) #Female, 45 year experience, high school graduated, white
weight[,22]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==1 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==45 )) #Female, 45 year experience, high school dropout, white
weight[,23]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==1 & deduc_3==0 & deduc_4==0 & exp==45 )) #Female, 45 year experience, some college, white
weight[,24]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==1 & deduc_4==0 & exp==45 )) #Female, 45 year experience, college graduated, white
weight[,25]<-sum(wkswork,subset = (dfemale=="TRUE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==1 & exp==45 )) #Female, 45 year experience, greater than college, white
weight[,26]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==5 )) #Male, 5 year experience, high school graduated, white
weight[,27]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==1 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==5 )) #Male, 5 year experience, high school dropout, white
weight[,28]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==1 & deduc_3==0 & deduc_4==0 & exp==5 )) #Male, 5 year experience, some college, white
weight[,29]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==1 & deduc_4==0 & exp==5 )) #Male, 5 year experience, college graduated, white
weight[,30]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==1 & exp==5 )) #Male, 5 year experience, greater than college, white
weight[,31]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==15 )) #Male, 15 year experience, high school graduated, white
weight[,32]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==1 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==15 )) #Male, 15 year experience, high school dropout, white
weight[,33]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==1 & deduc_3==0 & deduc_4==0 & exp==15 )) #Male, 15 year experience, some college, white
weight[,34]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==1 & deduc_4==0 & exp==15 )) #Male, 15 year experience, college graduated, white
weight[,35]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==1 & exp==15 )) #Male, 15 year experience, greater than college, white
weight[,36]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==25 )) #Male, 25 year experience, high school graduated, white
weight[,37]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==1 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==25 )) #Male, 25 year experience, high school dropout, white
weight[,38]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==1 & deduc_3==0 & deduc_4==0 & exp==25 )) #Male, 25 year experience, some college, white
weight[,39]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==1 & deduc_4==0 & exp==25 )) #Male, 25 year experience, college graduated, white
weight[,40]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==1 & exp==25 )) #Male, 25 year experience, greater than college, white
weight[,41]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==35 )) #Male, 35 year experience, high school graduated, white
weight[,42]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==1 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==35 )) #Male, 35 year experience, high school dropout, white
weight[,43]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==1 & deduc_3==0 & deduc_4==0 & exp==35 )) #Male, 35 year experience, some college, white
weight[,44]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==1 & deduc_4==0 & exp==35 )) #Male, 35 year experience, college graduated, white
weight[,45]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==1 & exp==35 )) #Male, 35 year experience, greater than college, white
weight[,46]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==45 )) #Male, 45 year experience, high school graduated, white
weight[,47]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==1 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==45 )) #Male, 45 year experience, high school dropout, white
weight[,48]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==1 & deduc_3==0 & deduc_4==0 & exp==45 )) #Male, 45 year experience, some college, white
weight[,49]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==1 & deduc_4==0 & exp==45 )) #Male, 45 year experience, college graduated, white
weight[,50]<-sum(wkswork,subset = (dfemale=="FALSE" & drace_1==0 & drace_2==0 & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==1 & exp==45 )) #Male, 45 year experience, greater than college, white

#  Aggregate adjusted wages for highschool

# Create vector to store value

highschool <- matrix(0,54,1)
sumhighschoolweight <- 0

# Aggregate (i.e. weighted average of highschool graduate and highschool dropout)

for (i in 0:9){
  highschool <- highschool + (predict_wage[,1+5*i]*weight[1+5*i]+predict_wage[,2+5*i]*weight[2+5*i])
  sumhighschoolweight <- sumhighschoolweight + weight[1+5*i] + weight[2+5*i]
  }
avg_highschool <- highschool/sumhighschoolweight

#  Aggregate adjusted wages for graduate

# Create vector to store value

college <- matrix(0,54,1)
sumcollegeweight <- 0

# Aggregate (i.e. weighted average of some college, college graduated and greater than college)

for (i in 0:9){
  college <- college + (predict_wage[,3+5*i]*weight[3+5*i]+predict_wage[,4+5*i]*weight[4+5*i]+predict_wage[,5+5*i]*weight[5+5*i])
  sumcollegeweight <- sumcollegeweight + weight[3+5*i] + weight[4+5*i] + weight[5+5*i]
}
avg_college <- college/sumcollegeweight

# Calculate ratio of mean log wage for college and high school (adjusted by 1 to be consistent with those of Figure 1 )

ratio <- avg_college/avg_highschool - 1

# Plot

# Since the values of ratio from 1986 on look very abnormal, I only draw graph from the year 1964 to 1985

yearplot <- 1964:1985
plot(yearplot,ratio[1:22],
     main="Composition adjusted college/high-school log weekly ratio, 1964-1985",
     xlab = "",
     ylab="Log wage gap",
     pch = 16,
     type="b",
     col="black")
grid(nx = 0,ny = 6, lty = "dotted")
abline(v = 1982, col = "gray60")

###################################Figure 2###################################################

## Count the number of workers in each group

# Create maxtrix to store counted value (each matrix  has 49 rows for 49 experience groups and 54 colums for 54 years)

female_edu0 <- matrix(0,49,54)
female_edu1 <- matrix(0,49,54)
female_edu2 <- matrix(0,49,54)
female_edu3 <- matrix(0,49,54)
female_edu4 <- matrix(0,49,54)
male_edu0 <- matrix(0,49,54)
male_edu1 <- matrix(0,49,54)
male_edu2 <- matrix(0,49,54)
male_edu3 <- matrix(0,49,54)
male_edu4 <- matrix(0,49,54)

# Count

for (i in 1:49){
  for (j in 1964:2017){
    female_edu0[i,j-1963] <- nrow(data_00 %>% filter(dfemale=="TRUE" & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==i-1&year==j))
    female_edu1[i,j-1963] <- nrow(data_00 %>% filter(dfemale=="TRUE" & deduc_1==1 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==i-1&year==j))
    female_edu2[i,j-1963] <- nrow(data_00 %>% filter(dfemale=="TRUE" & deduc_1==0 & deduc_2==1 & deduc_3==0 & deduc_4==0 & exp==i-1&year==j))
    female_edu3[i,j-1963] <- nrow(data_00 %>% filter(dfemale=="TRUE" & deduc_1==0 & deduc_2==0 & deduc_3==1 & deduc_4==0 & exp==i-1&year==j))
    female_edu4[i,j-1963] <- nrow(data_00 %>% filter(dfemale=="TRUE" & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==1 & exp==i-1&year==j))
    male_edu0[i,j-1963] <- nrow(data_00 %>% filter(dfemale=="FALSE" & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==i-1&year==j))
    male_edu1[i,j-1963] <- nrow(data_00 %>% filter(dfemale=="FALSE" & deduc_1==1 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==i-1&year==j))
    male_edu2[i,j-1963] <- nrow(data_00 %>% filter(dfemale=="FALSE" & deduc_1==0 & deduc_2==1 & deduc_3==0 & deduc_4==0 & exp==i-1&year==j))
    male_edu3[i,j-1963] <- nrow(data_00 %>% filter(dfemale=="FALSE" & deduc_1==0 & deduc_2==0 & deduc_3==1 & deduc_4==0 & exp==i-1&year==j))
    male_edu4[i,j-1963] <- nrow(data_00 %>% filter(dfemale=="FALSE" & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==1 & exp==i-1&year==j))
  }
}
## Weights for each group

# Create vector to store value

w_female_edu0 <- as.vector(matrix(0,49,1))
W_female_edu1 <- as.vector(matrix(0,49,1))
w_female_edu2 <- as.vector(matrix(0,49,1))
w_female_edu3 <- as.vector(matrix(0,49,1))
w_female_edu4 <- as.vector(matrix(0,49,1))
w_male_edu0 <- as.vector(matrix(0,49,1))
w_male_edu1 <- as.vector(matrix(0,49,1))
w_male_edu2 <- as.vector(matrix(0,49,1))
w_male_edu3 <- as.vector(matrix(0,49,1))
w_male_edu4 <- as.vector(matrix(0,49,1))

# Assign weights
for (i in 1:49){
  w_female_edu0[i] <- sum(rwage, subset=(dfemale=="TRUE" & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==i-1))
  W_female_edu1[i] <- sum(rwage, subset=(dfemale=="TRUE" & deduc_1==1 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==i-1))
  w_female_edu2[i] <- sum(rwage, subset=(dfemale=="TRUE" & deduc_1==0 & deduc_2==1 & deduc_3==0 & deduc_4==0 & exp==i-1))
  w_female_edu3[i] <- sum(rwage, subset=(dfemale=="TRUE" & deduc_1==0 & deduc_2==0 & deduc_3==1 & deduc_4==0 & exp==i-1))
  w_female_edu4[i] <- sum(rwage, subset=(dfemale=="TRUE" & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==1 & exp==i-1))
  w_male_edu0[i] <- sum(rwage, subset=(dfemale=="FALSE" & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==i-1))
  w_male_edu1[i] <- sum(rwage, subset=(dfemale=="FALSE" & deduc_1==1 & deduc_2==0 & deduc_3==0 & deduc_4==0 & exp==i-1))
  w_male_edu2[i] <- sum(rwage, subset=(dfemale=="FALSE" & deduc_1==0 & deduc_2==1 & deduc_3==0 & deduc_4==0 & exp==i-1))
  w_male_edu3[i] <- sum(rwage, subset=(dfemale=="FALSE" & deduc_1==0 & deduc_2==0 & deduc_3==1 & deduc_4==0 & exp==i-1))
  w_male_edu4[i] <- sum(rwage, subset=(dfemale=="FALSE" & deduc_1==0 & deduc_2==0 & deduc_3==0 & deduc_4==1 & exp==i-1)) 
}

## Aggregate

highschool_supply = .colSums(w_female_edu0*female_edu0+W_female_edu1*female_edu1+w_female_edu2/2*female_edu2/2+w_male_edu0*male_edu0+w_male_edu1*male_edu1+w_male_edu2/2*male_edu2,49,54)/sum(w_female_edu0+W_female_edu1+w_female_edu2/2+w_male_edu0+w_male_edu1+w_male_edu2/2)
graduate_supply = .colSums(w_female_edu3*female_edu3+w_female_edu4*female_edu4+w_female_edu2/2*female_edu2/2+w_male_edu3*male_edu3+w_male_edu4*male_edu4+w_male_edu2/2*male_edu2,49,54)/sum(w_female_edu3+w_female_edu4+w_female_edu2/2+w_male_edu3+w_male_edu4+w_male_edu2/2)
supply_ratio =as.vector(log(graduate_supply)/log(highschool_supply))

## Plot
yearplot <- 1964:2017
plot(yearplot,supply_ratio,
     main="College/high-school log relative supply, 1964-2017",
     xlab = "",
     ylab="Log relative supply index",
     pch = 16,
     type="b",
     col="black")
grid(nx = 0,ny = 6, lty = "dotted")
abline(v = 1982, col = "gray60")

