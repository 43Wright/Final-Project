---
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Analysis of Student Spending
#### Gabriel Wright and Brennan Sauser

## Introduction
The goal of the project is to explore the dataset and to be able to visualize student spending habits. The dataset contains information such as monthly income, financial aid, gender, major, and other school related expenses such as tuition. Analysis on this topic can help increase students looking to go to college understanding of the costs associated with going to college and look at what affects each variable.

Questions and topics our group explored in our project:
1. Does different majors affect student's monthly income?

2. Which category has the highest expenses (excluding school affiliated costs: tuition)?

3. Which Majors have the highest Tuition

4. School Expenses based on Major

5. Does a student's gender have any indication of their major (what majors each gender typically go into)

6. Difference in student's expenses based on gender

7. student spending based on year

8. Major distribution by year and gender

9. Distribution of financial aid by major, year and gender

10. Display of financial aid granted vs school related costs

## Data
### Structure
The link to the dataset: "https://www.kaggle.com/datasets/sumanthnimmagadda/student-spending-dataset/data". This dataset contains 18 columns which house different student related expenses as well as other factors such as age and gender with 1,000 rows of data.
### Variables Inlcuded in the dataset:
1. Age: age of the student
2. Gender: Student's gender (Male, Female, Non-binary)
3. Year in School: Year classification (Freshman, Sophmore, Junior, Senior)
4. Major: Students Major (Psychology, Engineering, Economics, Computer Science, Biology)
5. Montly Income: Monthly income of the student (in dollars)
6. Financial Aid: Financial aid recieved by the student (in dollars)
7. Tuition: Expenses for tuition (in dollars)
8. Housing: Expenses for Housing (in dollars)
9. Food: Expenses for food (in dollars)
10. Transportation: Expenses for transportation
11. Book and Supplies: Expenses for books and supplies (in dollars)
12. Entertainment: Expenses for Entertainment (in dollars)
13. Personal Care: Expenses for personal care items (in dollars)
14. Technology: Expenses for technology (in dollars)
15. Health & Wellnesss: Expenses for health and wellness (in dollars)
16. Miscellaneous: Miscellaneous expenses (in dollars)
17. Preferred Payment Method: Method of payment (Cash, Credit/Debit card, Moblie Payment App)


### Cleaning
```{r, echo=FALSE, warning=FALSE, message=FALSE}
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
```
First we read the data:
```{r, warning=FALSE,message=FALSE}
spending <- read_csv('student_spending.csv')
```
After reading the data, we check for null values in our dataset. Forcunty our data contains no null values and doesn't require any adjustments to be made:
```{r, warning=FALSE,message=FALSE}
sum(is.na(spending))
```
After checking for nulls values we look at the summary of the data as well as looking at the dimensions of the dataset.
```{r, warning=FALSE,message=FALSE}
#Summary of data
summary(spending)

#Checking dimensions of the data
dim(spending)
```
The dataset contains 1000 rows and 18 columns.

After viewing the summary of the data and its dimensions we remove the column preferred_payment_method due to it not having any relevancy to the topics/questions that we want to answer. It also doesn't provide much insight as it doesn't directly imply exactly what was paid and how much was paid.
```{r, warning=FALSE,message=FALSE}
spending <- spending %>% select(-preferred_payment_method)
```

Once we removed preferred_pay_method we created two new columns total_expenses which sums up all expenses in each row and other_expenses column which sums up all non-school related expenses.
```{r, warning=FALSE,message=FALSE}
spending <- spending %>% mutate(total_expenses = rowSums(pick(tuition:miscellaneous)), other_expenses = rowSums(pick(food, transportation, entertainment, personal_care, technology, health_wellness, miscellaneous)))
```

## Statistical Information
Before we started looking into our questions and topics we wanted to look more into the data. To understand any possible bias and information that would aid in making conclusions about our results.

First, we looked into the number of people in the dataset grouped by their gender:
``` {r}
spending %>% group_by(gender) %>%
  summarize(n = n()) %>% ungroup()
```

This shows that the dataset contains more male students in the data than the other two genders.

Secondly, we looked into the number of people in the dataset grouped by their major:
``` {r}
spending %>% group_by(major) %>%
  summarize(n = n()) %>% ungroup()
```

This shows that there are far more students taking biology than any other major in the dataset.

Thirdly, we looked into the number of people in each year:
``` {r}
spending %>% group_by(year_in_school) %>%
  summarize(n = n()) %>% ungroup()
```

This shows a pretty equal distribution of students for each year classification.

## Results
### Distribution of majors based on their income
``` {r}
expenses_df <- spending %>% select(housing, books_supplies, food, transportation, entertainment, personal_care, technology, health_wellness, miscellaneous)
summary(expenses_df)

average_income <- spending %>% group_by(major) %>% summarise(avg_monthly_income = mean(monthly_income, na.rm = TRUE))

average_income %>% ggplot(aes(x = major, y = avg_monthly_income)) + geom_col(fill = "black") + coord_flip() + labs(title = "Distribution of Majors Based on Their Average Income", x = "Majors", y = "Monthly Income ($)") + theme(legend.position = "none")
```

This graphic displays the average monthly income of the students grouped by their major, looking at the graph each major have very similar averages. However Engineering majors on average have the highest average monthly income. Displaying that engineering majors while in school on average have the highest monthly income. 

### Which category produces the highest expenses (excluding tuition, and Housing)
``` {r}
avg_expenses <- c( mean(expenses_df$food), mean(expenses_df$transportation), mean(expenses_df$entertainment), mean(expenses_df$personal_care), mean(expenses_df$technology), mean(expenses_df$health_wellness), mean(expenses_df$miscellaneous), mean(expenses_df$books_supplies) )
names_avg <- c('Food', 'Transportation', 'Entertainment', 'Personal Care', 'Technology', 'Health', 'Misc', 'Books')

avg_df <- data.frame(names_avg, avg_expenses)
ggplot(data = avg_df, aes(x = names_avg, y = avg_expenses)) + geom_col(fill = "black") + coord_flip() + labs(title = "Average Expenses by Category", x = "Category", y = "Expense ($)") + theme(legend.position = "none")
```

This graphic displays each expenses excluding tuition and housing due to them on average having exponentially larger costs then those presented in the graph. If included tuition would be the largest expense with housing being the second largest on average. Excluding these two variables, food on average seems to have the highest expenses.

### Which Majors have the highest tuition
``` {r}
average_tuition <- spending %>% group_by(major) %>% summarise(avg_tuition = mean(tuition, na.rm = TRUE))

average_tuition %>% ggplot(aes(x = major, y = avg_tuition)) + geom_col(fill = "black") + coord_flip() + labs(title = "Average Tuition for Each Major", x = "Major", y = "Average Tuition") + theme(legend.position = "none")
```

This graphic shows that on average psychology majors tend to have the highest tuition compared to the other majors with computer science having the second most. However each major have very similar costs in terms of their tuition.

### School Expenses based on major
``` {r}
grouped_expenses <- spending %>% group_by(major)
ggplot(data = grouped_expenses, aes(x = major, y = total_expenses)) + geom_col(fill = "black") + coord_flip() + labs(title = "Total Expenses by Major", x = "Major", y = "Expenses ($)") + theme(legend.position = "none")
```

This graphic displays an overview of each major's total expenses (cumulative). It displays that overall Biology majors have the highest total expenses.

When we break this down to look at the average of each majors total expenses:
``` {r}
ggplot(data = grouped_expenses, aes(x = major, y = mean(total_expenses))) + geom_col(fill = "black") + coord_flip() + labs(title = "Avg Total Expenses by Major", x = "Major", y = "Expenses ($)") + theme(legend.position = "none")
```

We see that the graphic remains about the same in terms of its looks with Biology majors on average having the highest total expenses

### Distribution of Gender and Majors
``` {r}
grouped_spending <- spending %>% group_by(major, gender)
color_palette <- c("darkred", "red", "coral1")
ggplot(data = grouped_spending, aes(x = major, fill = gender)) + geom_bar(position = position_dodge(preserve = 'single')) + scale_fill_manual(values = color_palette) + labs(title = "Gender Distribution across Majors", x = "Majors", y = "Count", fill = "Gender")
```

This graphic displays the distribution of each gender within each major, showing that biology has the highest number of non-binary students, with biology and economics having the highest number of male students, and with Engineering having the highest number of female students.

### Difference in students expenses and income based on gender
```{r}
spending %>% group_by(gender) %>%
  summarize(avg_income = mean(monthly_income), avg_expenses = mean(total_expenses), n = n()) %>% ungroup()
```

Here we look at the differences in students expenses and income based on their gender and we found that female and non-binary students on average have a higher income than male students. However male students on average have higher total expenses than the other two genders.

### Student spending based on year in school
``` {r}
spending %>%
    mutate(year_in_school = factor(year_in_school, levels = c("Freshman", "Sophomore", "Junior", "Senior"))) %>%
    group_by(year_in_school) %>%
    summarise(avg_total_expenses = mean(total_expenses, na.rm = TRUE)) %>%
    ggplot(aes(x = year_in_school, y = avg_total_expenses)) +
    geom_col(fill = "black") +
    labs(
        title = "Average Total Monthly Expenses based on Year in School",
        x = "Year in School",
        y = "Average Total Expenses ($)"
    ) +
    theme(legend.position="none", plot.title.position = "panel")
```

This graphic displays the average total expenses based on year in school. However each year displays very similar values in their total expenses. Upon breaking this down in several other plots to look at things such as non-school affiliated costs the graph doesn't display any significant change when compared to this graph.


## Average Student income based on year
``` {r}
spending %>%
    mutate(year_in_school = factor(year_in_school, levels = c("Freshman", "Sophomore", "Junior", "Senior"))) %>%
    group_by(year_in_school) %>%
    summarize(avg_monthly_income = mean(monthly_income, na.rm = TRUE)) %>%
    ggplot(aes(x = year_in_school, y = avg_monthly_income)) +
    geom_col(fill="black") +
    labs(
        title = "Average Student Monthly Income Based on Year",
        x = "Year in School",
        y = "Income ($)",
    )
```

This graphic displays the average monthly income of each student based on their year and it displays that on average Freshman typically make more per month than the other three.


###Distribution of financial aid by major, gender and year
##Average Students financial aid based on major
``` {r}
spending %>% group_by(major) %>% ggplot(aes(x = major, y = mean(financial_aid))) + geom_col(fill = 'black') + labs(
        title = "Average Student Financial Aid based on major",
        x = "Major",
        y = "Financial Aid ($)",
    )
```

This graphic displays the average financial aid received by students based on their major. It shows that Biology students on average recieve the most financial aid compared to the other majors, with economics receiving the second highest.

##Average Students financial aid based on gender
``` {r}
spending %>% group_by(gender) %>% ggplot(aes(x = gender, y = mean(financial_aid))) + geom_col(fill = 'black') + labs(
        title = "Average Student Financial Aid based on gender",
        x = "Gender",
        y = "Financial Aid ($)",
    )
```

This graphic displays the average financial aid received by each student based on their gender. It shows that male students on average receive the highest financial aid, with female and non-binary students receiving very similar values on average.

## Student financial aid based on year
``` {r}
spending %>%
    mutate(year_in_school = factor(year_in_school, levels = c("Freshman", "Sophomore", "Junior", "Senior"))) %>%
    group_by(year_in_school) %>%
    ggplot(aes(x = year_in_school, y = mean(financial_aid))) +
    geom_col(fill = "black") +
    labs(
        title = "Average Total Financial aid based on Year in School",
        x = "Year in School",
        y = "Average Financial Aid ($)"
    ) +
    theme(legend.position="none", plot.title.position = "panel")
```

This graphic displays the average financial aid received by students based on their year. It shows that each year on average receives very similar amounts of aid, however Seniors typically receive the most financial aid on average.

## Financial aid vs tuition
``` {r}
spending %>%
  summarize(avg_tuition = mean(tuition), avg_financial_aid = mean(financial_aid), tuition_covered = (avg_financial_aid/avg_tuition * 100))
```

This looks at the average financial aid is received by students with the average tuition costs. It shows that the average aid that is received is able to help students cover around 11% of their tuition costs on average.

## Conclusion
In conclusion, our analysis of student spending displays the differences between each students expenses, income, and financial aid (our target variables), based on their gender, year classification, and their major. As well as displaying what their total expenses are when they are in college. Students largest expenses on average when going to college are their costs of tuition and housing. When this is broken down into gender our data displays that man on average have higher total expenses than non-binary and female students. However men typically receive on average more financial aid than the other two genders, but they have the highest total expenses on average when compared to the other two. Year classification doesn't display much impact on our desired target variables. While Majors like biology on average have the highest total expenses.

Further research into looking at data containing tuition rates from school across the country would aid in the process of understanding the costs that are associated with going to college, as well as being able to know the average tuition rates of each school to aid in the design making of where to pursue further education, which would be the next step.
