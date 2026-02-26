# Create dataframe of 5 students, determine number of rows and columns in a data frame

Name = c("Rahul", "Himanshu", "Gautam","Manas","Piyush")
Age = c(22,23,22,23,24)
height = c(175,173,176,180,170)
weight = c(70,75,70,65,73)

students = data.frame(Name,Age,height,weight)
students

dim(students)


#dataframe of 8 students R programming

Student_Names = c("Student A","Student B","Student C","Student D","Student E","Student F","Student G","Student H")
Program = c("B.Tech EEE", "B. Com", "MBA", "B. Com", "MBA", "B. Com","B.Tech EEE","B.Tech EEE")
RollNo = c(51,24,71,22,92,2,8,10)
Semester = c(1,3,1,5,3,1,5,3)
Email_IDs = c("StA@gmail.com","StB@gmail.com","StC@gmail.com","StD@gmail.com","StE@gmail.com","StF@gmail.com","StG@gmail.com", "StH@gmail.com")
R_Programming = data.frame(Student_Names,Program,RollNo,Semester,Email_IDs)

subset(R_Programming,Program=="B.Tech EEE")
subset(R_Programming,Program=="MBA")
dim(R_Programming)


#Merging two dataframes
IDs = c(1243,4513,2081,6510,2122)
AGE = c(22,30,27,20,31)
df1 = data.frame(IDs,AGE)

IDs = c(123,212,999,888,234)
AGE=c(18,19,18,25,30)
CITY = c("Delhi","Ahemdabad","Kolkata","Mumbai","Delhi")
df2 = data.frame(IDs,AGE,CITY)
merged_df = merge(df1, df2, by = "IDs",all= TRUE)
merged_df

# finding unique values in df with duplicate values 
colours = c("Red","Green","Blue","Yellow","Blue","Yellow","Blue","Yellow","Green","Blue","Green")
amount = c(4,2,6,1,6,1,6,1,2,6,2)

duplicatedf = data.frame(colours,amount)
unique(duplicatedf$colours)
unique(duplicatedf$amount)
duplicatedf

# change column name 
names(duplicatedf)[names(duplicatedf)=="amount"] <- "Value"
duplicatedf
