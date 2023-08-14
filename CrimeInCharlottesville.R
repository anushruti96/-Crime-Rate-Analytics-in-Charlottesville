library(UsingR)
library(stringr)
library(lubridate) 
library(dbplyr)
library(tidyverse)
library(tidygeocoder)
library(plotrix)
#Loading The Data
arrest.data <- read.csv("/Users/anushruti/Desktop/Stat For Engineers/Project/Crime_Data.csv")

#Format the Data
arrest.data$Year <- as.numeric(substr(arrest.data$DateReported,1,4))
arrest.data$Month <- as.numeric(str_sub(arrest.data$DateReported,6,7))
arrest.data$Day <-as.numeric(str_sub(arrest.data$DateReported,9,10))
arrest.data$Date <- as.Date(str_sub(arrest.data$DateReported,1,10))
arrest.data$BlockNumber[is.na(arrest.data$BlockNumber)]<-1
arrest.data$Time <- as.numeric(str_sub(arrest.data$DateReported,12,13))
head(arrest.data)
describe(arrest.data)
view(arrest.data)

#Finding the average number of crimes, month wise 

#Year Wise Crimes in Charlottesville
#here we have created dataframe having number of crimes in that year and month irrespective of area to see the crime trend in Charlottesville
arrest_counts = arrest.data %>%
  count(year = Year, month = Month) %>% # aggregate number of arrests by month, year
  arrange(year, month) %>%
  mutate(
    date = lubridate::make_date(year, month, day = 1),
    index = 1:n()
  )

arrest_counts %>%
  ggplot(aes(x=date, n)) +
  geom_point() + geom_line() +
  labs(title="Charlottesville Crimes", x = "Year", y = "Number of Crimes")

#Separately Year Wise Crimes 
arrest_counts %>%
  # add yearly average
  group_by(year) %>% mutate(n_avg = mean(n)) %>% ungroup() %>%
  ggplot(aes(month, n)) +
  geom_point() + geom_line() +
  geom_hline(aes(yintercept = n_avg), color="orange") +
  theme_light() +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~year) +
  labs(y = "Number of Crimes")


#In single plot, all the years separately

arrest_counts %>% group_by(year) %>% mutate(n_avg = mean(n)) %>% ungroup() %>%
  mutate(year = factor(year)) %>% # make discrete for ggplot
  ggplot(aes(month, n, color=year)) +
  geom_point() + geom_line() +
  scale_x_continuous(breaks = 1:12) +
  labs(y = "Number of Crimes")

#Average Daily number of crimes
crime_rate = arrest_counts %>%
  mutate(
    days_in_month = lubridate::days_in_month(date),
    daily_avg = n / days_in_month
  )
crime_rate %>%
  mutate(
    days_in_year = ifelse(year %% 4 == 0, 366, 365)
  ) %>%
  group_by(year) %>% mutate(yearly_avg = sum(n) / days_in_year) %>% ungroup() %>%
  ggplot(aes(year, daily_avg, color = factor(month))) +
  geom_point(aes(y = yearly_avg), color="black", shape=18, size = 3) +
  geom_line(aes(y = yearly_avg), color="black", alpha = .25, linewidth = 1) +
  geom_point() + geom_line() +
  labs(y = "average daily number of crimes", color="month",
       caption = "black diamonds represent the yearly average")


#Fitting a quadratic model to find the daily crime rate.

#--------------------------------------------------------------------------------------------------------------------------
#Moving on to finding the areas where the crime rate is high. 
#Here we are trying to find which area has the highest crime rate
crime_area = arrest.data %>%
  count(area=StreetName) %>% # aggregate number of arrests by month, year
  arrange(area) %>%
  filter(n>50)


ggplot(crime_area, aes(x=area,y=n,group = 1)) +
  geom_point(colour="darkblue",aes(size=n)) +
  geom_line() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
#We found that E MARKET ST,EMMET ST N,W MAIN ST has maximum crime rate. 

#Here, we have found the number of crimes per year, per month for that area. 
crime_area_year_wise = arrest.data %>%
  count(year = Year, month = Month,area=StreetName) %>% 
  arrange(year, month,area)

#Based on that, we filter year wise and see the trends for these 3 areas with high crime rates separately.
crime_area_year_wise %>%
  filter(area %in% c("E MARKET ST","EMMET ST N","W MAIN ST","5TH ST SW","E MAIN ST","UNIVERSITY AVE")) %>%
  group_by(year,area) %>% summarise(n_avg = mean(n))  %>%
  # aggregate number of arrests by month, year
  ggplot(aes(year, n_avg)) +
  geom_point() + 
  geom_line() +
  theme_light() +
  facet_wrap(~area) +
  labs(y = "Number of Crimes")

#Comparing crime rate for these 3 areas on a single graph.
crime_area_year_wise %>%
  filter(area %in% c("E MARKET ST","EMMET ST N","W MAIN ST","5TH ST SW","E MAIN ST","UNIVERSITY AVE")) %>%
  group_by(year,area) %>% summarise(n_avg = mean(n)) %>%
  ggplot(aes(x=year,y=n_avg,colour=area)) +
  geom_point() + 
  geom_line() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="Year",y="Monthly Average crimes for the year")

#Month Wise Trend to see which month needs more attention.
crime_area_year_wise %>%
  filter(area %in% c("E MARKET ST","EMMET ST N","W MAIN ST","5TH ST SW","E MAIN ST","UNIVERSITY AVE")) %>%
  group_by(month,area) %>% summarise(n_avg = mean(n)) %>%
  group_by(month) %>% mutate(month_avg = mean(n_avg)) %>% ungroup() %>%
  ggplot(aes(x=month,y=n_avg,colour=area)) +
  geom_point() + 
  geom_line() +
  theme_light() +
  geom_point(aes(y = month_avg), color="black", shape=18, size = 3) +
  geom_line(aes(y = month_avg), color="black", alpha = .25, linewidth = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = 1:12) +
  labs(x="Year",y="Monthly Average crimes for the year")

view(crime_area_year_wise)
crime_area_year_wise$area <- as.factor(crime_area_year_wise$area)
head(crime_area_year_wise)

#``````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````

#TYPE OF CRIME 


arrest.data %>%
  group_by(Offense)  %>%
  summarize(cn=n()) %>%
  filter(cn>100) %>% 
  ggplot(aes(x=Offense,y=cn,group=1)) +
  geom_col(colour="darkblue",) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

names_of_popular_crimes <- arrest.data %>% group_by(Offense)  %>%
  summarize(cn=n()) %>%
  filter(cn>700) %>% 
  select(Offense)

arrest.data %>%
  filter(Offense %in% c("Assault Simple","Assist Citizen - Mental/TDO/ECO","Hit and Run","Larceny - All Other","Larceny - From Motor Vehicle","Larceny - Theft from Building","Lost/FoundProperty","Suspicious Activity","Vandalism")) %>%
  group_by(Offense,StreetName) %>%
  summarize(cn=n()) %>%
  filter(cn>70) %>%
  ungroup() %>%
  ggplot(aes(x=StreetName,y=cn,group=1)) +
  geom_col(colour="darkblue",) +
  theme_light() +
  facet_wrap(~Offense) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

arrest.data %>%
  filter(Offense %in% c("Assault Simple","Assist Citizen - Mental/TDO/ECO","Hit and Run","Larceny - All Other","Larceny - From Motor Vehicle","Larceny - Theft from Building","Lost/FoundProperty","Suspicious Activity","Vandalism")) %>%
  filter(Year %in% c(2018,2019,2020,2021,2022)) %>%
  group_by(Offense,Year) %>%
  summarize(cn=n()) %>%
  filter(cn>70) %>%
  ungroup() %>%
  ggplot(aes(x=Year,y=cn,group=1)) +
  geom_col(colour="darkblue",) +
  theme_light() +
  facet_wrap(~Offense) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

#``````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
#Time of Crime analysis
 arrest.data %>%
  filter(Offense %in% c("Assault Simple","Assist Citizen - Mental/TDO/ECO","Hit and Run","Larceny - All Other","Larceny - From Motor Vehicle","Larceny - Theft from Building","Lost/FoundProperty","Suspicious Activity","Vandalism")) %>%
  filter(Year %in% c(2018,2019,2020,2021,2022)) %>%
  group_by(Offense,Time) %>%
  summarize(time_Avg=n()) %>%
   ggplot(aes(x=Time,y=time_Avg,group=1)) +
   geom_col(colour="darkblue",) +
   facet_wrap(~Offense) +
   theme_light() +
   scale_x_continuous(breaks = 0:23) +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

 
 fit.data <- arrest.data %>%
   group_by(Offense,Time) %>%
   summarize(time_Avg=n()) %>%
   ggplot(aes(x=Time,y=time_Avg,group=1)) +
   geom_col(colour="darkblue",) +
   facet_wrap(~Offense) +
   theme_light() +
   scale_x_continuous(breaks = 0:23) +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
 

#Fitting 1
#We have tried to fit the model based on the crime rate ( Year Month Area ) to predict the future crimes rate in a particular area. This will predict the monthly average for that month.
crime_year_month_area <- arrest.data %>%
  group_by(Year, Month, StreetName) %>% 
    summarise(total_count=n())
year_month_area_wise_pred = lm(total_count ~ ., data = crime_year_month_area)
summary(year_month_area_wise_pred)
y_test<-predict(year_month_area_wise_pred, newdata = crime_year_month_area)
mse1 <- sqrt(sum((y_test - crime_year_month_area$total_count)^2))
#Here we are trying to print the MSE. We have used the same data for training and testing purposes.
cat("The MSE IS %f",mse1)

linear_model_deg_2 = lm(total_count ~ poly(Year,2)+poly(Month,2)+StreetName, data = crime_year_month_area)
summary(linear_model_deg_2)
y_test<-predict(linear_model_deg_2,newdata = crime_year_month_area)
mse <- sqrt(sum((y_test-crime_year_month_area$total_count)^2))
cat("The MSE IS %f",mse)

linear_model_deg_3 = lm(total_count ~ poly(Year,3)+poly(Month,3)+StreetName, data = crime_year_month_area)
summary(linear_model_deg_2)
y_test<-predict(linear_model_deg_2,newdata = crime_year_month_area)
mse <- sqrt(sum((y_test-crime_year_month_area$total_count)^2))
cat("The MSE IS %f",mse)


#Fitting 2
#We have tried to fit the model based on the crime rate ( Year Month Time Offence ) to predict the future crimes rate for a particular offence. This will predict the monthly average for that month.
#Fitting First Degree Polynomial
crime_year_month_time_offence <- arrest.data %>%
  group_by(Year, Month, Offense) %>% 
  summarise(total_count=n())
view(crime_year_month_time_offence)
year_month_time_offence_wise_pred = lm(total_count ~ ., data = crime_year_month_time_offence)
summary(year_month_time_offence_wise_pred)
y_test<-predict(year_month_time_offence_wise_pred, newdata = crime_year_month_time_offence)
mse2 <- sqrt(sum((y_test - crime_year_month_time_offence$total_count)^2))
#Here we are trying to print the MSE. We have used the same data for training and testing purposes.
cat("The MSE IS %f",mse2)
#Fitting Second Degree Polynomial
year_month_time_offence_wise_pred = lm(total_count ~ poly(Year,2)+poly(Month,2)+Offense, data = crime_year_month_time_offence)
summary(year_month_time_offence_wise_pred)
y_test<-predict(year_month_time_offence_wise_pred, newdata = crime_year_month_time_offence)
mse2 <- sqrt(sum((y_test - crime_year_month_time_offence$total_count)^2))
#Here we are trying to print the MSE. We have used the same data for training and testing purposes.
cat("The MSE IS %f",mse2)
#Fitting third degree polynomial
year_month_time_offence_wise_pred = lm(total_count ~ poly(Year,3)+poly(Month,3)+Offense, data = crime_year_month_time_offence)
summary(year_month_time_offence_wise_pred)
y_test<-predict(year_month_time_offence_wise_pred, newdata = crime_year_month_time_offence)
mse2 <- sqrt(sum((y_test - crime_year_month_time_offence$total_count)^2))
#Here we are trying to print the MSE. We have used the same data for training and testing purposes.
cat("The MSE IS %f",mse2)

