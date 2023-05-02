
#Library Installation
library(dplyr)
library(ggplot2)

#Data Import
main_data<-read.csv(file="C:/Users/User/Documents/APU/Year 2 Semester 3/Programming for Data Analysis/Assignment/employee_attrition.csv",header=TRUE,sep=",")

#Data Cleaning
#1. checking duplicates
unique_main_data <- unique(main_data)
rm(unique_main_data) #since the purpose of checking duplicate has been done, the dataframe can be dropped

#2. Cleaning the recorddate_key timestamp
main_data$recorddate_key <- sub(" 0:00","",main_data$recorddate_key)

#3. Removing the terminationdate_key variables that has the value of '1/1/1900'
main_data$terminationdate_key[main_data$terminationdate_key == '1900-01-01'] <- NA

#Data Pre-Processing
#1. Parsing all dates variables from String into Date values
main_data$recorddate_key <- as.Date(as.character(main_data$recorddate_key), format = "%m/%d/%Y")
main_data$birthdate_key <- as.Date(as.character(main_data$birthdate_key), format = "%m/%d/%Y")
main_data$orighiredate_key <- as.Date(as.character(main_data$orighiredate_key), format = "%m/%d/%Y")
main_data$terminationdate_key <- as.Date(as.character(main_data$terminationdate_key), format = "%m/%d/%Y")

#2. fixing termination reason typo
main_data$termreason_desc[main_data$termreason_desc == 'Resignaton'] <- 'Resignation'

#3. fixing city name typo
main_data$city_name[main_data$city_name == 'New Westminister'] <- 'New Westminster'

#4. Parsing the store names into character
main_data$store_name <- as.character(main_data$store_name)

#Data Transformation
#1. Removing gender_short variable
main_data <- select (main_data,-c(gender_short))

#Question 1 - Why people leaving their jobs?
  #Analysis 1 - Termination Reason - Age, separated by gender
  main_data%>%
    filter(termtype_desc == "Voluntary")%>%
    ggplot(aes(y= age, x=termreason_desc))+
    geom_boxplot(alpha = 0)+
    geom_jitter(alpha = 0.5, aes(colour = "orange"))+
    facet_wrap(~gender_full)+
    stat_summary(fun=mean, geom="point", shape=20, size=3, color="blue")+
    labs(title = "Correlation between Termination Reason and Age, separated by Gender",
         x='Termination Reason', y = 'Age')
  
  #Analysis 2 - Termination Reason - Job Title
  main_data%>%
    filter(termreason_desc == "Resignation")%>%
    ggplot(aes(x=department_name, fill = job_title))+
    geom_bar()+
    labs(title = "Correlation between Termination Reason and Job Title", x='Termination Reason', y = 'Count', fill = "Job Title")
  
  #Analysis 3 - Termination Reason - City
  main_data%>%
    filter(termreason_desc == "Resignation")%>%
    ggplot(aes(x= termreason_desc, fill = job_title))+
    geom_bar()+
    facet_wrap(~city_name)+
    labs(title = "Correlation between Termination Reason and City", x='Termination Reason', y = 'Count', fill = "Job Title")

#Question 2 - How is Company Current Condition?
  #Analysis 4 - Comparing the workforce between 2006 and 2015
  
  data41<- main_data%>%
    group_by(department_name)%>%
    filter(STATUS == "ACTIVE")%>%
    filter(STATUS_YEAR == "2006")%>%
    count()
  data41['year']='2006'
  data41
  
  data42<- main_data%>%
    group_by(department_name)%>%
    filter(STATUS == "ACTIVE")%>%
    filter(STATUS_YEAR == "2015")%>%
    count()
  data42['year']='2015'
  data42
  
  data43 = rbind(data41,data42)
  rm(data41)
  rm(data42)
  
  data43%>%
    ggplot(aes(fill=year, y=n, x=department_name))+
    geom_bar(position="dodge", stat="identity")+
    labs(title = "Comparing the workforce between 2006 and 2015",
         x="Department Name", y="Employee Count", fill="Year")
  
  #Analysis 5 - Comparing store condition between 2006 and 2015
  
  data51<- main_data%>%
    group_by(store_name)%>%
    filter(STATUS == "ACTIVE")%>%
    filter(STATUS_YEAR == "2006")%>%
    count()
  data51['year']='2006'
  data51
  
  data52 <- main_data%>%
    group_by(store_name)%>%
    filter(STATUS == "ACTIVE")%>%
    filter(STATUS_YEAR == "2015")%>%
    count()
  data52['year']='2015'
  data52
  
  data53 = rbind(data51,data52)
  rm(data51)
  rm(data52)
  
  data53%>%
    ggplot(aes(fill=year, y=n, x=store_name))+
    geom_bar(position="dodge", stat="identity")+
    labs(title = "Comparing the store condition between 2006 and 2015",
         x="Store Name", y="Employee Count", fill="Year")
  
  #Analysis 6 - Comparing City Presence between 2006 and 2015
  data61<- main_data%>%
    group_by(city_name)%>%
    filter(STATUS == "ACTIVE")%>%
    filter(STATUS_YEAR == "2006")%>%
    count()
  data61['year']='2006'
  data61
  
  data62 <- main_data%>%
    group_by(city_name)%>%
    filter(STATUS == "ACTIVE")%>%
    filter(STATUS_YEAR == "2015")%>%
    count()
  data62['year']='2015'
  data62
  
  data63 = rbind(data61,data62)
  rm(data61)
  rm(data62)
  
  data63%>%
    ggplot(aes(fill=year, y=n, x=city_name))+
    geom_bar(position="dodge", stat="identity")+
    labs(title = "Comparing the City Presence between 2006 and 2015",
         x="City Name", y="Employee Count", fill="Year")
  
  #Analysis 7 - Counting Layoff Rate between 2006-2015
  main_data%>%
    group_by(STATUS_YEAR)%>%
    filter(STATUS=="TERMINATED")%>%
    ggplot(aes(x=as.character(STATUS_YEAR),fill = termreason_desc))+
    geom_bar(position="dodge")+
    labs(title = "Comparing the Layoff Rate between 2006 and 2015",
         x="Year", y="Employee Count", fill="Termination Reason")
  
  
#Question 3 - Which Store has the highest turnover rate and why?
  #Analysis 8 - Finding the store with the highest turnover rate
  main_data%>%
    filter(STATUS == "TERMINATED")%>%
    filter(termreason_desc != "Retirement")%>%
    arrange(store_name)%>%
    ggplot(aes(x=store_name, fill = termreason_desc))+
    geom_bar()+
    labs(title = "Finding the store with the highest turnover rate", x="Store name", y="Count", fill = "Termination Reason")
  
  #Analysis 9 - Finding each store's jobs distribution (Store 11)
  main_data%>%
    filter(store_name == "11")%>%
    filter(termreason_desc != "Retirement")%>%
    ggplot(aes(x=job_title, fill = termreason_desc))+
    geom_bar()+
    labs(title = "Finding the store with the highest turnover rate", subtitle = "Store 11 - Fort Nelson", x="Job Title", y="Count",
         fill = "Termination Reason")
  
  #Supporting analysis for Analysis 9 - Checking the number of active employee in store 11 by each year
  main_data%>%
    filter(store_name == "11")%>%
    filter(STATUS == "ACTIVE")%>%
    group_by(STATUS_YEAR)%>%
    count()
  
  #Analysis 10 - Finding each store's jobs distribution (Store 46)
  main_data%>%
    filter(STATUS == "TERMINATED")%>%
    filter(store_name == "46")%>%
    filter(termreason_desc != "Retirement")%>%
    ggplot(aes(x=job_title, fill = termreason_desc))+
    geom_bar()+
    labs(title = "Finding the store with the highest turnover rate", subtitle = "Store 46 - Victoria", x="Job Title", y="Count",
         fill = "Termination Reason")
  
  #Supporting analysis for Analysis 10 - Checking the number of active employee in store 46 by each year
  main_data%>%
    filter(store_name == "46")%>%
    filter(STATUS == "ACTIVE")%>%
    group_by(STATUS_YEAR)%>%
    count()
  
#Question 4 - When is the best time for hiring spree?
  #Analysis 11 - Resignation Count - Per Month (Per Year)
  main_data%>%
    filter(termreason_desc == "Resignation")%>%
    mutate(termination_month = format(format(terminationdate_key, format = "%m")))%>%
    mutate(termination_year = format(format(terminationdate_key, format = "%Y")))%>%
    ggplot(aes(x= termination_month))+
    geom_bar()+
    facet_wrap(~termination_year)+
    labs(title = "Amount of Resignation per Month (Per Year)", x='Month', y = 'Count')
  
  #Analysis 12 - Resignation Count - Per Month (Average)
  main_data%>%
    filter(termreason_desc == "Resignation")%>%
    mutate(termination_month = format(format(terminationdate_key, format = "%m")))%>%
    ggplot(aes(x= termination_month))+
    geom_bar()+
    labs(title = "Amount of Resignation per Month (Average)", x='Month', y = 'Count')
  
  
  
  
  