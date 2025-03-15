#install the pacman package to install multiple packages at once
install.packages('pacman')

#load the library to import multiple packages at once
library(pacman)

#call the function to import multiple packages
p_load(pacman, tidyverse, readxl, gridExtra,ggpmisc)


#get current working directory
getwd()

#load working dataset into R
dataset <- read_excel('student_performance.xlsx')

#view imported dataset
view(dataset)

#get structure of the dataset
str(dataset)

head(dataset, 20)


########### (1) DATA CLEANING

#check if any missing values
view(is.na(dataset))

sum(is.na(dataset))

#sort missing values in increasing order
view(sort.int(colSums(is.na(dataset)), decreasing = TRUE))

#working on missing values, removing outliers & consistent Gender values
c_dataset <- dataset |> mutate(
  Math_Score  = if_else(is.na(Math_Score), round(mean(Math_Score, na.rm = TRUE), 2), Math_Score),
  Science_Score = if_else(is.na(Science_Score), round(mean(Science_Score, na.rm = TRUE), 2), Science_Score)
) |>
  filter(Math_Score <= 100) |>
  mutate(Gender = tolower(Gender))
  
#check if all corrections effected - NEW DATASET
view(c_dataset)


############### EXPLORATORY DATA ANALYSIS (EDA)

#Calculate summary statistics for each score
#mean
summary_stats_mean <- c_dataset |>
  summarise(
    Average_Maths_Score = round(mean(Math_Score, na.rm = TRUE), 2),
    Average_English_Score = round(mean(English_Score, na.rm = TRUE), 2),
    Average_Science_Score = round(mean(Science_Score, na.rm = TRUE), 2),
    )
view(summary_stats_mean)

#median
summary_stats_median <- c_dataset |>
  summarise(
    Median_Maths_Score = round(median(Math_Score, na.rm = TRUE), 2),
    Median_English_Score = round(median(English_Score, na.rm = TRUE), 2),
    Median_Science_Score = round(median(Science_Score, na.rm = TRUE), 2),
  )
view(summary_stats_median)

#standard deviation
summary_stats_std <- c_dataset |>
  summarise(
    STD_Maths_Score = round(sd(Math_Score, na.rm = TRUE), 2),
    STD_English_Score = round(sd(English_Score, na.rm = TRUE), 2),
    Median_Science_Score = round(sd(Science_Score, na.rm = TRUE), 2),
  )
view(summary_stats_std)

#Group by Gender and calculate the average scores
grp_by_gender <- c_dataset |>
  group_by(Gender) |>
  summarise(
    Average_Maths_Score = round(mean(Math_Score, na.rm = TRUE), 2),
    Average_English_Score = round(mean(English_Score, na.rm = TRUE), 2),
    Average_Science_Score = round(mean(Science_Score, na.rm = TRUE), 2),
  )
view(grp_by_gender)


################# (3) VISUALIZATIONS

#Create a bar chart comparing average scores by gender
piv_longer_gen <- grp_by_gender |> pivot_longer(2:4, names_to = 'Subject', values_to = 'Mean_Exam_Score')
view(piv_longer_gen)


plt_by_gender <- piv_longer_gen |> ggplot(aes(Gender, Mean_Exam_Score, fill = Gender)) +
  geom_bar(stat = 'identity')+
  ylim(0, 82) +
  geom_text(aes(Gender, Mean_Exam_Score, label = Mean_Exam_Score), 
            vjust=-.5, color="black", position = position_dodge(.9)) + 
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))+
  ggtitle('Gender vs Mean Exam score')+
  facet_wrap(~Subject) +
  scale_fill_manual(values = c("#C4961A", "#293352"))+
  theme_bw()

plt_by_gender


#Create a scatter plot of Study_Hours vs. Math_Score, colouring by Gender
plt_sdy_mathscore <- c_dataset |> ggplot(aes(Study_Hours, Math_Score, colour = Gender, shape = Gender)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("adj.R2", "f", "p"))) +
  geom_point(stat = 'identity') +
  ggtitle('Study hours vs Maths score') +
  scale_y_continuous(limits = c(0,120, by = 10)) +
  scale_x_continuous(breaks = seq(0,30, by = 1))+
  facet_wrap(~Gender)+
  theme_bw()

plt_sdy_mathscore


#Create a histogram of Math_Score distribution
hist_mathscore <- c_dataset |> ggplot(aes(Math_Score)) +
  geom_histogram(binwidth = 1, fill = 'white', colour='black') +
  ggtitle('Maths score Distribution') +
  scale_y_continuous(breaks = seq(0,13, by = 1)) +
  scale_x_continuous(breaks = seq(0,100, by = 4)) +
  theme_classic() +
  annotate("text", x=67, y=11, label= "64.00")+
  geom_vline(aes(xintercept=mean(Math_Score)),
             color="blue", linetype="dashed", linewidth=1)

hist_mathscore


################EXTRAS

#group by type of lunch

grp_by_lunch <- c_dataset |>
  group_by(Lunch) |>
  summarise(
    Average_Maths_Score = round(mean(Math_Score, na.rm = TRUE), 2),
    Average_English_Score = round(mean(English_Score, na.rm = TRUE), 2),
    Average_Science_Score = round(mean(Science_Score, na.rm = TRUE), 2),
  )

view(grp_by_lunch)

#group by type of age

grp_by_age <- c_dataset |>
  group_by(Age) |>
  summarise(
    Average_Maths_Score = round(mean(Math_Score, na.rm = TRUE), 2),
    Average_English_Score = round(mean(English_Score, na.rm = TRUE), 2),
    Average_Science_Score = round(mean(Science_Score, na.rm = TRUE), 2),
  )

view(grp_by_age)

#plot by age

plt_by_age <- grp_by_age |> pivot_longer(2:4, names_to = 'Subject', values_to = 'Mean_Exam_Scores')

view(plt_by_age)

plt_by_age |> ggplot(aes(Age, Mean_Exam_Scores, fill = Age)) +
geom_bar(stat = 'identity') +
  ggtitle('Age vs Mean Exam scores', subtitle = waiver()) +
  facet_wrap(~Subject) +
  theme_bw()
  

view(plt_by_age)
