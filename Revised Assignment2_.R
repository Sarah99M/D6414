##Step1: Read CSV file 
data1 <- read.csv("Set1_A1_FINdata.csv", stringsAsFactors = FALSE)
data2 <- read.csv("Set1_A1_HRdata.csv", stringsAsFactors = FALSE)


##Step2: Load necessary libraries
require(dplyr)               #data manipulation
require(tidyr)               #data tidying
require(lubridate)           #simplifies working with date & time
require(ggplot2)             #for complex graphic


##Step3:Data Cleaning
#Examine the structure data
str(data1)
str(data2)

#Remove duplicated rows
data1<-distinct(data1)
data2<-distinct(data2)

#Verify no duplicates remain
sum(duplicated(data1))
sum(duplicated(data2))

#Check for consistency of Salary in Data1 
summary(data1$salary)

#Convert the character of height & weight column (from integer to numeric) in Data2
data2$height<-as.numeric(data2$height)
data2$weight<-as.numeric(data2$weight)

#Check for unique values for categorical columns in Data2
unique(data2$gender)   
unique(data2$country)
unique(data2$state)
unique(data2$deptid)

#Replace incorrect gender values (Replace "-M" with "M")
data2<-data2%>%mutate(gender = case_when(gender == "-M" ~ "M",TRUE ~ gender))  

#Check logical errors in numerical Data2
range(data2$weight)      
range(data2$height) 
range(data2$birthyear)  
range(data2$birthmonth) 
range(data2$birthday)    

#Standardize date 
data2$enterdate<-dmy(data2$enterdate)
data2$exitdate<-dmy(data2$exitdate)

#Identify missing values
colSums(is.na(data1))
colSums(is.na(data2))

#Strategies to handle missing values
data1<-data1%>%filter(!is.na(staffid))           #Remove NA values in staffid rows
data2$state[is.na(data2$state)]<-"Unknown"       #Replace NA values in state column with Unknown
data2$deptid[is.na(data2$deptid)]<-"Unknown"     #Replace NA values in deptid column with Unknown


##Step4: Data Integration
#Filter active employees(exitdate is NA)
active_data2<-data2%>%filter(is.na(exitdate))

#Recent salary information 
recent_data1<-data1%>%arrange(staffid,desc(yearid))%>%distinct(staffid,.keep_all = TRUE)

#Merge both datasets(recent salary information with exitdate NA)
merged_data<-active_data2%>%inner_join(recent_data1,by = "staffid")


##Step5: Data Transformation
#Calculate & create age column
merged_data <- merged_data %>% mutate(
  # If birth month is February and day is 29, and the year is not a leap year,
  # adjust the date to February 28
  birthdate = if_else(
    (birthmonth == 2 & birthday == 29) & !(lubridate::leap_year(birthyear)), 
    make_date(birthyear, 2, 28),
    make_date(birthyear, birthmonth, birthday)
  ),
  age = as.integer(difftime(Sys.Date(), birthdate, units = "weeks") / 52.25),
  birthdate = format(birthdate, "%Y-%m-%d")
)

#Calculate & create length of service column 
merged_data<-merged_data%>%mutate(length_of_service=as.integer(difftime(Sys.Date(),enterdate,units = "weeks")/52.25))

#Calculate & create BMI column - change the unit of weight(pounds to kg) & height(inches to m)
merged_data <- merged_data %>%mutate(
  BMI = weight * 0.4534 / ((height * 0.0254)^2),
  BMI_Category = case_when(
    BMI < 18.5 ~ "Underweight",
    BMI >= 18.5 & BMI < 25 ~ "Normal weight",
    BMI >= 25 & BMI < 30 ~ "Overweight",
    BMI >= 30 ~ "Obesity"))

#Create fullname column
merged_data<-merged_data%>%mutate(fullname=paste(namefirst,namelast))

#Remove unnecessary column
merged_data<-merged_data%>%select(-birthyear,-birthmonth,-birthday,-namefirst,-namelast)

#Reorder column name accordingly in latest data
head(merged_data)
merged_data<-merged_data%>%select(.,1,fullname,everything())          #Specify the desired order of columns

##Export new CSV file
write.csv(merged_data,"New_Employee_data.csv",row.names=FALSE)


##Step5: Data Visualization
##Visualization 1: BMI Distribution
barplot(table(merged_data$BMI_Category), 
        main = "BMI Distribution", 
        xlab = "BMI Category", 
        ylab = "Frequency", 
        col = c("lightgreen", "lightblue", "orange", "red"))

#Visualization 2: Distribution of Salary
ggplot(merged_data,aes(x = salary)) +
  geom_histogram(binwidth = 4000, fill = "blue", color = "black") +
  labs(title = "Distribution of Salary",
       x = "Salary",
       y = "Frequency")+
  theme_minimal()

#Visualization 3: Salary vs Length of Service
average_salary <- merged_data %>%
  group_by(length_of_service) %>%
  summarize(mean_salary = mean(salary, na.rm = TRUE))

ggplot(average_salary, aes(x = length_of_service, y = mean_salary)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Average Salary by Length of Service",
       x = "Length of Service",
       y = "Average Salary") +
  theme_minimal()

#Visualization 4: Salary vs Department
ggplot(merged_data,aes(x=deptid,y=salary))+
  geom_boxplot(fill="blue")+
  labs(title = "Salary Distribution vs Department",
       x= "Department ID",
       y= "Salary")+
  theme_minimal()

#Visualization 5: Salary vs Age
average_salary_by_age <- merged_data %>%
  group_by(age) %>%
  summarize(mean_salary = mean(salary, na.rm = TRUE))

ggplot(average_salary_by_age, aes(x = age, y = mean_salary)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Average Salary by Age",
       x = "Age",
       y = "Average Salary") +
  theme_minimal()