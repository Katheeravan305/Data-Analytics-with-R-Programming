#loading dataset
library("xlsx")
setwd("C:/Users/alien/OneDrive - Universiti Sains Malaysia/Desktop/CPC351/Assignment 2")
schooldata <- read.xlsx("Government_Schools_Pupils_Teachers_2017-2018.xlsx",sheetName = "Sheet1",colNames=TRUE)
summary(schooldata)

######################################################################################################################################################################################################
#Question 1
#changing variables data type Question 1a
schooldata$Sex <- as.factor(schooldata$Sex)
schooldata$State <- as.factor(schooldata$State)
schooldata$District.Education.Office<- as.factor(schooldata$District.Education.Office)
schooldata$School.type <- as.factor(schooldata$School.type)
schooldata$School.stage <- as.factor(schooldata$School.stage)
schooldata$Number.of.pupils <- as.numeric(schooldata$Number.of.pupils)
schooldata$Number.of.teachers <- as.numeric(schooldata$Number.of.teachers)
summary(schooldata)


#removing all rows with missing values Question 1b
schooldata <- na.omit(schooldata)

summary(schooldata)


#renaming variables in dataset Question 1c
names(schooldata) [1] <- 'school_stage'
names(schooldata) [2] <- 'state'
names(schooldata) [3] <- 'district'
names(schooldata) [4] <- 'year'
names(schooldata) [5] <- 'school_type'
names(schooldata) [6] <- 'gender'
names(schooldata) [7] <- 'number_of_pupils'
names(schooldata) [8] <- 'number_of_teachers'

summary(schooldata)

#######################################################################################################################################################################################################
#Question 2
#primary school
#state vector which will be used for bar graphs as x value
states <- c('Johor','Kedah','Kelantan','Melaka','Negeri Sembilan','Pahang','Perak','Perlis','Pulau Pinang','Sabah','Sarawak','Selangor','Terengganu','Kuala Lumpur','Labuan','Putrajaya ')

#primary_pupil_2017 and primary_pupil_2018 takes all the primary school dataset by year 2017 and 2018 respectively
primary_pupil_2017 <- schooldata[schooldata$year == 2017 & schooldata$school_stage == "Primary school",]
primary_pupil_2018 <- schooldata[schooldata$year == 2018 & schooldata$school_stage == "Primary school",]
labels1 <- c(sum(primary_pupil_2017$number_of_pupils),sum(primary_pupil_2018$number_of_pupils))

#pie chart for total number of primary school students by year
pie(c(sum(primary_pupil_2017$number_of_pupils),sum(primary_pupil_2018$number_of_pupils)),labels1,main="Pie Chart for Number of Primary School Pupils in Year 2017 and 2018",col = rainbow((length(labels1))))
legend("topright", c('Number of Primary School Pupils 2017','Number of Primary School Pupils 2018'), cex = 0.8,
       fill = rainbow(length(labels1)))

#bar chart for total number of primary school students by state in 2017
primary2017.1<- aggregate(number_of_pupils~state,data=primary_pupil_2017,sum)
primary2017.2<-c(primary2017.1[1,2],primary2017.1[2,2],primary2017.1[3,2],primary2017.1[4,2],primary2017.1[5,2],primary2017.1[6,2],primary2017.1[7,2],primary2017.1[8,2],primary2017.1[9,2],primary2017.1[10,2],primary2017.1[11,2],primary2017.1[12,2],primary2017.1[13,2],primary2017.1[14,2],primary2017.1[15,2],primary2017.1[16,2])
print(primary2017.2)
par(mar=c(4, 4, 4, 0))
bar1 <- barplot(primary2017.2, names.arg=states,
        xlab="States",
        ylab="Number of Primary School Student in Year 2017",
        main="Number of Primary School Student in Year 2017 by States",
        ylim=c(0,600000),
        col = rainbow(length(states))
)
text(bar1,primary2017.2+10000,primary2017.2,cex=1)
legend("topright",
       cex = 1,
       fill = rainbow(length(states)),
       legend = states
)

#bar chart for total number of primary school students by state in 2018
primary2018.1<- aggregate(number_of_pupils~state,data=primary_pupil_2018,sum)
primary2018.2<-c(primary2018.1[1,2],primary2018.1[2,2],primary2018.1[3,2],primary2018.1[4,2],primary2018.1[5,2],primary2018.1[6,2],primary2018.1[7,2],primary2018.1[8,2],primary2018.1[9,2],primary2018.1[10,2],primary2018.1[11,2],primary2018.1[12,2],primary2018.1[13,2],primary2018.1[14,2],primary2018.1[15,2],primary2018.1[16,2])
print(primary2018.2)
par(mar=c(4, 4, 4, 0))
bar2 <- barplot(primary2018.2, names.arg=states,
        xlab="States",
        ylab="Number of Primary School Student in Year 2018",
        main="Number of Primary School Student in Year 2018 by States",
        ylim=c(0,600000),
        col=rainbow(length(states))
)
text(bar2,primary2018.2+10000,primary2018.2,cex=1)
legend("topright",
       cex = 1,
       fill = rainbow(length(states)),
       legend = states
)


#Secondary school
#secondary_pupil_2017 and secondary_pupil_2018 takes all the secondary school data set by year 2017 and 2018 respectively
secondary_pupil_2017 <- schooldata[schooldata$year == 2017 & schooldata$school_stage == "Secondary school",]
secondary_pupil_2018 <- schooldata[schooldata$year == 2018 & schooldata$school_stage == "Secondary school",]
labels2 <- c(sum(secondary_pupil_2017$number_of_pupils),sum(secondary_pupil_2018$number_of_pupils))


#pie chart for total number of secondary school students by year
pie(c(sum(secondary_pupil_2017$number_of_pupils),sum(secondary_pupil_2018$number_of_pupils)),labels2,main="Pie Chart for Number of Secondary School Pupils in Year 2017 and 2018",col = rainbow((length(labels2))))
legend("topright", c('Number of Secondary School Pupils 2017','Number of Secondary School Pupils 2018'), cex = 0.8,
       fill = rainbow(length(labels2)))

#bar chart for total number of secondary school students by state in 2017
secondary2017.1<- aggregate(number_of_pupils~state,data=secondary_pupil_2017,sum)
secondary2017.2<-c(secondary2017.1[1,2],secondary2017.1[2,2],secondary2017.1[3,2],secondary2017.1[4,2],secondary2017.1[5,2],secondary2017.1[6,2],secondary2017.1[7,2],secondary2017.1[8,2],secondary2017.1[9,2],secondary2017.1[10,2],secondary2017.1[11,2],secondary2017.1[12,2],secondary2017.1[13,2],secondary2017.1[14,2],secondary2017.1[15,2],secondary2017.1[16,2])
print(secondary2017.2)
par(mar=c(4, 4, 4, 0))
bar3 <- barplot(secondary2017.2, names.arg=states,
        xlab="States",
        ylab="Number of Secondary School Student in Year 2017",
        main="Number of Secondary School Student in Year 2017 by States",
        ylim=c(0,400000),
        col=rainbow(length(states))
)
text(bar3,secondary2017.2+10000,secondary2017.2,cex=1)
legend("topright",
       cex = 1,
       fill = rainbow(length(states)),
       legend = states
)


#bar chart for total number of secondary school students by state in 2018
secondary2018.1<- aggregate(number_of_pupils~state,data=secondary_pupil_2018,sum)
secondary2018.2<-c(secondary2018.1[1,2],secondary2018.1[2,2],secondary2018.1[3,2],secondary2018.1[4,2],secondary2018.1[5,2],secondary2018.1[6,2],secondary2018.1[7,2],secondary2018.1[8,2],secondary2018.1[9,2],secondary2018.1[10,2],secondary2018.1[11,2],secondary2018.1[12,2],secondary2018.1[13,2],secondary2018.1[14,2],secondary2018.1[15,2],secondary2018.1[16,2])
print(secondary2018.2)
par(mar=c(4, 4, 4, 0))
bar4 <- barplot(secondary2018.2, names.arg=states,
        xlab="States",
        ylab="Number of Secondary School Student in Year 2018",
        main="Number of Secondary School Student in Year 2018 by States",
        ylim=c(0,400000),
        col=rainbow(length(states))
)
text(bar4,secondary2018.2+10000,secondary2018.2,cex=1)
legend("topright",
       cex = 1,
       fill = rainbow(length(states)),
       legend = states
)


##############################################################################################################################################################################################################################################
#Question 3
library("ggplot2")
pupils_2017.1 <- schooldata[schooldata$year == "2017",]
pupils_2018.1 <- schooldata[schooldata$year == "2018",]

pupils_2017.2<- aggregate(number_of_pupils~state + gender,data = pupils_2017.1,sum)
pupils_2018.2<- aggregate(number_of_pupils~state + gender,data = pupils_2018.1,sum)
print(pupils_2017.2)
pupils_2017.3 <- c(length=32)
for(i in 1:32)
{
  pupils_2017.3[i] = pupils_2017.2[i,3]
}
ggplot(pupils_2017.2, aes(state,number_of_pupils, fill =gender)) + 
  geom_bar(stat="identity", position = "stack") + ylim(0,1000000) +
  scale_fill_brewer(palette = "Set2") + labs(title= "Number of Male and Female Pupils by States in 2017",y="Number of Pupils in 2017", x = "States", fill="Gender") + 
  geom_text(aes(label = pupils_2017.3), position = position_stack(), vjust=1) +   theme(plot.title = element_text(hjust = 0.5))


pupils_2018.3 <- c(length=32)
for(i in 1:32)
{
  pupils_2018.3[i] = pupils_2018.2[i,3]
}
ggplot(pupils_2018.2, aes(state, number_of_pupils, fill =gender)) + 
  geom_bar(stat="identity", position = "stack") + ylim(0,1000000) + 
  scale_fill_brewer(palette = "Set2") + labs(title= "Number of Male and Female Pupils by States in 2018",y="Number of Pupils in 2018", x = "States", fill="Gender") + 
  geom_text(aes(label = pupils_2018.3), position = position_stack(), vjust=1) + theme(plot.title = element_text(hjust = 0.5))

##############################################################################################################################################################################################
#Question 4
# Group data frame by Perak state
perak_pupil <- schooldata[schooldata$state == "Perak",]

# Group gender and year using number of pupils in Perak
perak_2017_2018 <- aggregate(number_of_pupils~gender + year,data = perak_pupil,sum)

# Create new data frame to plot graph
total_perak_pupil <- data.frame(year = factor(c("2017","2017","2018","2018")),
                                gender = factor(c("Female","Male","Female","Male")),
                                number_of_pupils = c(perak_2017_2018[1,3],
                                                     perak_2017_2018[2,3],
                                                     perak_2017_2018[3,3],
                                                     perak_2017_2018[4,3]))

# Visualization of bar plot
ggplot(total_perak_pupil, aes(year, number_of_pupils, fill = gender)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Dark2") +
  labs(title= "Number of Perak Pupils by Year According to Gender",y="Number of Pupils", x = "Year", fill="Gender") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = number_of_pupils), position = position_dodge(width =1), vjust=1.8)


################################################################################################################################################################################################
# Question 5
# Group data frame by year of 2018
pupils_from_2018 <- schooldata[schooldata$year == "2018",]

# Group the districts according to total number of pupils in each district
group_district <- aggregate(number_of_pupils~district,data=pupils_from_2018,sum)

# Arrange the top_10 data frame from highest number of pupils in each district
top_10 <- group_district[order(group_district$number_of_pupils,decreasing=TRUE),]

# Extract first 10 districts from top_10 data frame
high_pupil_district <- head(top_10,n=10)

# Increase bottom margin of bar graph
par(mar=c(6,4,4,4))

# Visualize the bar plot graph
bar_chart <- barplot(high_pupil_district$number_of_pupils,names.arg = high_pupil_district$district,
                     ylim=c(0,250000),
                     las=3,
                     xlab = "Districts",
                     ylab = "Number of pupils",
                     cex.names = 0.7,
                     main = "Top 10 districts of high number of pupils in 2018",
                     col = rainbow(length(high_pupil_district$number_of_pupils))
)

# Add data value which is number of pupils for each districts
text(bar_chart,high_pupil_district$number_of_pupils-5000,high_pupil_district$number_of_pupils,cex=1)

# Add legend at top right of bar graph
legend("topright",
       cex = 1.0,
       fill = rainbow(length(high_pupil_district$number_of_pupils)),
       legend = high_pupil_district$district
)


