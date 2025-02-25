library(readxl)
library(dplyr)

dsdata <- read_excel('C:/Users/Sudipta/Documents/Data/Dataset.xlsx')

head(dsdata)

str(dsdata)

dsdata %>% count(Gender)

sum(is.na(dsdata$Gender))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

genderMode <- getmode(dsdata$Gender)
print(genderMode)

dsdata$Gender[is.na(dsdata$Gender)] <- genderMode
sum(is.na(dsdata$Gender))

dsdata$Gender <- factor(dsdata$Gender, levels = c("Male","Female"),labels = c(1,2))
head(dsdata$Gender)

View(dsdata)

dsdata %>% count(`Dietary Habits`)

sum(is.na(dsdata$`Dietary Habits`))
dsdata$`Dietary Habits` <- factor(dsdata$`Dietary Habits`, levels = c("Unhealthy","Moderate","Healthy"),labels = c(1,2,3))
head(dsdata$`Dietary Habits`)

dsdata %>% count(`Have you ever had suicidal thoughts ?`)

dsdata <- dsdata %>% rename(`Suicidal thoughts ?` = `Have you ever had suicidal thoughts ?`)

dsdata <- dsdata %>% mutate(`Suicidal thoughts ?` = recode(`Suicidal thoughts ?`, "Yess" = "Yes", "Noo" = "No"))
dsdata %>% count(`Suicidal thoughts ?`)

dsdata$`Suicidal thoughts ?` <- factor(dsdata$`Suicidal thoughts ?`, levels = c("Yes","No"),labels = c(1,0))
head(dsdata$`Suicidal thoughts ?`)

dsdata %>% count(`Family History of Mental Illness`)

dsdata$`Family History of Mental Illness` <- factor(dsdata$`Family History of Mental Illness`, levels = c("Yes","No"),labels = c(1,0))
head(dsdata$`Family History of Mental Illness`)

dsdata %>% count(Depression)
sum(is.na(dsdata$Depression))

sum(is.na(dsdata))

c_dsdata <- na.omit(dsdata)
sum(is.na(c_dsdata))
nrow(c_dsdata)

Yes <- which(c_dsdata$Depression == "Yes")
No <- which(c_dsdata$Depression == "No")
length(Yes)
length(No)

yes.up <- sample(Yes, length(No), replace = TRUE)
u_dsdata <- c_dsdata[c(yes.up, No),]
u_dsdata %>% count(Depression)


u_dsdata$Depression <- factor(u_dsdata$Depression, levels = c("Yes","No"),labels = c(1,0))
head(u_dsdata$Depression)

u_dsdata %>% count(`Sleep Duration`)

sleep_mapping <- c("Less than 5 hours" = 4, 
                   "5-6 hours" = 5.5, 
                   "7-8 hours" = 7.5, 
                   "More than 8 hours" = 9)

u_dsdata$`Sleep Duration`<- sleep_mapping[u_dsdata$`Sleep Duration`]
head(u_dsdata)

write.csv(u_dsdata, "C:/Users/Sudipta/Documents/Data/prepdata.csv", row.names = FALSE)

dsdata$`Sleep Duration`<- sleep_mapping[dsdata$`Sleep Duration`]


sum(is.na(dsdata$`Sleep Duration`))
sdmode <- ceiling(mean(dsdata$`Sleep Duration`, na.rm = TRUE))
sdmode

mdsdata <- dsdata

mdsdata$`Sleep Duration`[is.na(dsdata$`Sleep Duration`)] <- sdmode
sum(is.na(mdsdata$`Sleep Duration`))


sum(is.na(dsdata$`Sleep Duration`))
sdmedian <- median(dsdata$`Sleep Duration`, na.rm = TRUE)
sdmedian

medsdata <- dsdata

medsdata$`Sleep Duration`[is.na(dsdata$`Sleep Duration`)] <- sdmedian
sum(is.na(medsdata$`Sleep Duration`))

str(u_dsdata$Gender)
u_dsdata %>% count(Gender)

u_dsdata$Gender <- as.numeric(as.character(u_dsdata$Gender))

Male <- which(u_dsdata$Gender == 1)
Female <- which(u_dsdata$Gender == 2)
length(Male)
length(Female)

Male.dw <- sample(Male, length(Female), replace = TRUE)
d_dsdata <- u_dsdata[c(Male.dw, Female),]
d_dsdata %>% count(Gender)

head(u_dsdata$Age)
summary(u_dsdata$Age)
str(u_dsdata$Age)

Q1 <- quantile(u_dsdata$Age, 0.25)
Q3 <- quantile(u_dsdata$Age, 0.75)

IQR_v <- IQR(u_dsdata$Age)

lower_bound <- Q1 - 1.5 * IQR_v
upper_bound <- Q3 + 1.5 * IQR_v

f_data <- u_dsdata[u_dsdata$Age >= lower_bound & u_dsdata$Age <= upper_bound, ]
summary(f_data$Age)
View(f_data)

f_data_n <- f_data
f_data_n$Age <- (f_data_n$Age - min(f_data_n$Age)) / (max(f_data_n$Age) - min(f_data_n$Age))
head(f_data_n$Age)
View(f_data_n)

filter(f_data, Age<20)

write.csv(f_data_n, "C:/Users/Sudipta/Documents/Data/finaldata.csv", row.names = FALSE)

datanew <- read.csv("C:/Users/Sudipta/Documents/Data/finaldata.csv",header = TRUE,sep = ",")
str(datanew)
summary(datanew)

length(unique(datanew$Age))
distinct(datanew, Age)