#Loading the stroke dataset
stroke_data <- read.csv("healthcare-dataset-stroke-data.csv")

#6 a

summary(stroke_data)
#Getting all the column names in stroke dataset using colnames function
cols = colnames(stroke_data)


#Displaying the column names along with their data type by
#concatenating them together
for(i in 2:length(cols)){
  cat(cols[i], ":", class(stroke_data[,i]), "\n")
}

#All variable with character data type changed to factors
stroke_data$gender <- as.factor(stroke_data$gender)
stroke_data$ever_married <- as.factor(stroke_data$ever_married)
stroke_data$work_type <- as.factor(stroke_data$work_type)
stroke_data$Residence_type <- as.factor(stroke_data$Residence_type)
stroke_data$smoking_status <- as.factor(stroke_data$smoking_status)
#BMI was converted to numeric from characters as the values in it are double values
stroke_data$bmi <- as.numeric(stroke_data$bmi)
#Variables with character value 0 and 1 changed to factors
stroke_data$hypertension <- as.factor(stroke_data$hypertension)
stroke_data$heart_disease <- as.factor(stroke_data$heart_disease)
stroke_data$stroke <- as.factor(stroke_data$stroke)

summary(stroke_data)

#6 b
#Removed the row with gender value other
stroke_data <- stroke_data[stroke_data$gender != 'Other',]

#Computed the mean of BMI for all rows expect for the ones that are missing
meanBMI <- mean(stroke_data$bmi, na.rm=T)
#Replace all missing BMI values with mean value of BMI
stroke_data$bmi.fix <- ifelse(is.na(stroke_data$bmi), meanBMI, stroke_data$bmi)
summary(stroke_data$bmi.fix)
summary(stroke_data)



# 7
library(ggplot2)
#Configuring theme for plots. The themes include the font type , font size for titles and text
plottheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (18), hjust = 0.5),
                   legend.title = element_text(colour = "#656568",  face = "bold.italic", family = "Helvetica"), 
                   legend.text = element_text(face = "bold.italic",family = "Helvetica"), 
                   axis.title = element_text(family = "Helvetica", size = (14)),
                   axis.text = element_text(family = "Courier", size = (14)))


#Plotting a boxplot to visualize the relationship between age and stroke
#The colour and labels for boxes was manually configured using scale_fill_manual function
#The y labels was replaced with values such as No Stroke and Stroke using scale_y_discrete function
ggplot(stroke_data, aes(x=age, y=stroke, fill=stroke)) + 
  geom_boxplot(outlier.size = 3) + 
  scale_fill_manual(values=c("#00B3B3", "#B30000"), labels=c("No Stroke", "Stroke")) + 
  scale_y_discrete(
    "Stroke disease",
    labels = c(
      "0" = "No Stroke",
      "1" = "Stroke"
    )
  ) + labs(title= "Boxplot visualizing the relationship between Age and Stroke", 
           y="Stroke Disease", x = "Age") + plottheme

#Plotting a boxplot to visualize the relationship between BMI and stroke
ggplot(stroke_data, aes(x=bmi.fix, y=stroke, fill=stroke)) + 
  geom_boxplot(outlier.size = 3) + 
  scale_fill_manual(values=c("#00B3B3", "#B30000"), labels=c("No Stroke", "Stroke")) + 
  scale_y_discrete(
    "Stroke disease",
    labels = c(
      "0" = "No Stroke",
      "1" = "Stroke"
    )
  ) + labs(title= "Boxplot visualizing the relationship between BMI and Stroke", 
         y="Stroke Disease", x = "BMI (kg/m^2)") + plottheme


#8
#Plotting grouped bar plot to visualize number of patients with heart disease based on gender
#The colour for bars was manually configured using scale_fill_manual function
#Changed the factor values 0 and 1 to appropriate strings using scale_x_discrete() function
#The count value for each bar is displayed on top of the bar using geom_text function
ggplot(stroke_data, aes(x=heart_disease, fill=gender)) +
  geom_bar(position="dodge") + plottheme + 
  labs(title= "Grouped Barplot for number of patients with heart disease based on gender", 
       y="Number of Patients", x = "Stroke Disease", fill="Gender") + 
  scale_fill_manual(values = c("#eda692", "#5c6df5")) + scale_x_discrete(
    "Heart Disease",
    labels = c(
      "0" = "No Heart Disease",
      "1" = "Heart Disease"
    )
  ) + ylim(0, 3000) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(0.9))

#Plotting grouped bar plot to visualize number of patients with hypertension based on gender
ggplot(stroke_data, aes(x=hypertension, fill=gender)) +
  geom_bar(position="dodge") + plottheme + 
  labs(title= "Grouped Barplot for number of patients with hypertension based on gender", 
       y="Number of patients", x = "Hypertension Disease") + 
  scale_fill_manual(values = c("#eda692", "#5c6df5")) + scale_x_discrete(
    "Hypertension Disease",
    labels = c(
      "0" = "No Hypertension",
      "1" = "Hypertension"
    )
  ) + ylim(0, 3000) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(0.9))

#Plotting grouped bar plot to visualize number of patients with stroke based on gender
ggplot(stroke_data, aes(x=stroke, fill=gender)) +
  geom_bar(position="dodge") + plottheme + 
  labs(title= "Grouped Barplot for number of patients with stroke based on gender", 
       y="Number of Patients", x = "Stroke Disease") + 
  scale_fill_manual(values = c("#eda692", "#5c6df5")) + scale_x_discrete(
    "Stroke Disease",
    labels = c(
      "0" = "No Stroke",
      "1" = "Stroke"
    )
  ) + ylim(0, 3000) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(0.9))

#9
#Plotting stacked barplot to visualize patients work types by gender
#stat_count() function is used to show the count of the bar in it
ggplot(stroke_data, aes(y=work_type, fill=gender)) +
  geom_bar(position="stack") + plottheme + 
  labs(title= "Graph of patients work types by gender", y="Work types", x = "Number of patients") + 
  scale_fill_manual(values = c("#eda692", "#5c6df5"))+stat_count(geom = "text", aes(label = ..count..), hjust = 1.8)

#10
#Plotting scatter plot to create an analysis in visualizing the
#relationship between average glucose level and age
ggplot(stroke_data, aes(x=age, y=avg_glucose_level)) +
  geom_point(colour = "#ADD8E6", size = 3) + geom_smooth(colour = "#1d2739") +ylim(55, 273) + labs(title= "Relationship between Average Glucose Level and Age", y="Average Glucose Level (mmol/L)", x = "Age (years)") + plottheme
