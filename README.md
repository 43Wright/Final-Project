
\#Analysis of Student Spending \#### Gabriel Wright and Brennan Sauser

## Introduction

The goal of the project is to explore the dataset and to be able to
visualize student spending habits. The dataset contains information such
as monthly income, financial aid, gender, major, and other school
related expenses such as tuition. Analysis on this topic can help
increase students looking to go to college understanding of the costs
associated with going to college and look at what affects each variable.

Questions and topics our group explored in our project: 1. Does
different majors affect student’s monthly income?

2.  Which category has the highest expenses (excluding school affiliated
    costs: tuition)?

3.  Which Majors have the highest Tuition

4.  School Expenses based on Major

5.  Does a student’s gender have any indication of their major (what
    majors each gender typically go into)

6.  Difference in student’s expenses based on gender

7.  student spending based on year

8.  Major distribution by year and gender

9.  Distribution of financial aid by major, year and gender

10. Display of financial aid granted vs school related costs

## Data

### Structure

The link to the dataset:
“<https://www.kaggle.com/datasets/sumanthnimmagadda/student-spending-dataset/data>”.
This dataset contains 18 columns which house different student related
expenses as well as other factors such as age and gender with 1,000 rows
of data. \### Variables Inlcuded in the dataset: 1. Age: age of the
student 2. Gender: Student’s gender (Male, Female, Non-binary) 3. Year
in School: Year classification (Freshman, Sophmore, Junior, Senior) 4.
Major: Students Major (Psychology, Engineering, Economics, Computer
Science, Biology) 5. Montly Income: Monthly income of the student (in
dollars) 6. Financial Aid: Financial aid recieved by the student (in
dollars) 7. Tuition: Expenses for tuition (in dollars) 8. Housing:
Expenses for Housing (in dollars) 9. Food: Expenses for food (in
dollars) 10. Transportation: Expenses for transportation 11. Book and
Supplies: Expenses for books and supplies (in dollars) 12.
Entertainment: Expenses for Entertainment (in dollars) 13. Personal
Care: Expenses for personal care items (in dollars) 14. Technology:
Expenses for technology (in dollars) 15. Health & Wellnesss: Expenses
for health and wellness (in dollars) 16. Miscellaneous: Miscellaneous
expenses (in dollars) 17. Preferred Payment Method: Method of payment
(Cash, Credit/Debit card, Moblie Payment App)

### Cleaning

First we read the data:

``` r
spending <- read_csv('student_spending.csv')
```

After reading the data, we check for null values in our dataset.
Forcunty our data contains no null values and doesn’t require any
adjustments to be made:

``` r
sum(is.na(spending))
```

    ## [1] 0

After checking for nulls values we look at the summary of the data as
well as looking at the dimensions of the dataset.

``` r
#Summary of data
summary(spending)
```

    ##       ...1            age           gender          year_in_school    
    ##  Min.   :  0.0   Min.   :18.00   Length:1000        Length:1000       
    ##  1st Qu.:249.8   1st Qu.:20.00   Class :character   Class :character  
    ##  Median :499.5   Median :22.00   Mode  :character   Mode  :character  
    ##  Mean   :499.5   Mean   :21.68                                        
    ##  3rd Qu.:749.2   3rd Qu.:24.00                                        
    ##  Max.   :999.0   Max.   :25.00                                        
    ##     major           monthly_income   financial_aid       tuition    
    ##  Length:1000        Min.   : 501.0   Min.   :   0.0   Min.   :3003  
    ##  Class :character   1st Qu.: 770.8   1st Qu.: 261.0   1st Qu.:3780  
    ##  Mode  :character   Median :1021.0   Median : 513.0   Median :4548  
    ##                     Mean   :1020.6   Mean   : 504.8   Mean   :4520  
    ##                     3rd Qu.:1288.2   3rd Qu.: 751.5   3rd Qu.:5285  
    ##                     Max.   :1500.0   Max.   :1000.0   Max.   :6000  
    ##     housing            food       transportation  books_supplies 
    ##  Min.   : 401.0   Min.   :100.0   Min.   : 50.0   Min.   : 50.0  
    ##  1st Qu.: 538.8   1st Qu.:175.0   1st Qu.: 88.0   1st Qu.:112.0  
    ##  Median : 704.5   Median :255.0   Median :123.0   Median :175.0  
    ##  Mean   : 696.0   Mean   :252.6   Mean   :124.6   Mean   :174.8  
    ##  3rd Qu.: 837.2   3rd Qu.:330.0   3rd Qu.:162.2   3rd Qu.:238.0  
    ##  Max.   :1000.0   Max.   :400.0   Max.   :200.0   Max.   :300.0  
    ##  entertainment    personal_care     technology    health_wellness
    ##  Min.   : 20.00   Min.   : 20.0   Min.   : 50.0   Min.   : 30.0  
    ##  1st Qu.: 54.00   1st Qu.: 41.0   1st Qu.:114.0   1st Qu.: 73.0  
    ##  Median : 86.00   Median : 62.0   Median :178.0   Median :115.0  
    ##  Mean   : 84.81   Mean   : 60.7   Mean   :178.3   Mean   :114.3  
    ##  3rd Qu.:116.00   3rd Qu.: 80.0   3rd Qu.:241.0   3rd Qu.:158.0  
    ##  Max.   :150.00   Max.   :100.0   Max.   :300.0   Max.   :200.0  
    ##  miscellaneous    preferred_payment_method
    ##  Min.   : 20.00   Length:1000             
    ##  1st Qu.: 63.75   Class :character        
    ##  Median :110.00   Mode  :character        
    ##  Mean   :108.91                           
    ##  3rd Qu.:153.00                           
    ##  Max.   :200.00

``` r
#Checking dimensions of the data
dim(spending)
```

    ## [1] 1000   18

The dataset contains 1000 rows and 18 columns.

After viewing the summary of the data and its dimensions we remove the
column preferred_payment_method due to it not having any relevancy to
the topics/questions that we want to answer. It also doesn’t provide
much insight as it doesn’t directly imply exactly what was paid and how
much was paid.

``` r
spending <- spending %>% select(-preferred_payment_method)
```

Once we removed preferred_pay_method we created two new columns
total_expenses which sums up all expenses in each row and other_expenses
column which sums up all non-school related expenses.

``` r
spending <- spending %>% mutate(total_expenses = rowSums(pick(tuition:miscellaneous)), other_expenses = rowSums(pick(food, transportation, entertainment, personal_care, technology, health_wellness, miscellaneous)))
```

## Statistical Information

Before we started looking into our questions and topics we wanted to
look more into the data. To understand any possible bias and information
that would aid in making conclusions about our results.

First, we looked into the number of people in the dataset grouped by
their gender:

``` r
spending %>% group_by(gender) %>%
  summarize(n = n()) %>% ungroup()
```

    ## # A tibble: 3 × 2
    ##   gender         n
    ##   <chr>      <int>
    ## 1 Female       323
    ## 2 Male         356
    ## 3 Non-binary   321

This shows that the dataset contains more male students in the data than
the other two genders.

Secondly, we looked into the number of people in the dataset grouped by
their major:

``` r
spending %>% group_by(major) %>%
  summarize(n = n()) %>% ungroup()
```

    ## # A tibble: 5 × 2
    ##   major                n
    ##   <chr>            <int>
    ## 1 Biology            228
    ## 2 Computer Science   192
    ## 3 Economics          204
    ## 4 Engineering        192
    ## 5 Psychology         184

This shows that there are far more students taking biology than any
other major in the dataset.

Thirdly, we looked into the number of people in each year:

``` r
spending %>% group_by(year_in_school) %>%
  summarize(n = n()) %>% ungroup()
```

    ## # A tibble: 4 × 2
    ##   year_in_school     n
    ##   <chr>          <int>
    ## 1 Freshman         253
    ## 2 Junior           247
    ## 3 Senior           254
    ## 4 Sophomore        246

This shows a pretty equal distribution of students for each year
classification.

## Results

### Distribution of majors based on their income

``` r
expenses_df <- spending %>% select(housing, books_supplies, food, transportation, entertainment, personal_care, technology, health_wellness, miscellaneous)
summary(expenses_df)
```

    ##     housing       books_supplies       food       transportation 
    ##  Min.   : 401.0   Min.   : 50.0   Min.   :100.0   Min.   : 50.0  
    ##  1st Qu.: 538.8   1st Qu.:112.0   1st Qu.:175.0   1st Qu.: 88.0  
    ##  Median : 704.5   Median :175.0   Median :255.0   Median :123.0  
    ##  Mean   : 696.0   Mean   :174.8   Mean   :252.6   Mean   :124.6  
    ##  3rd Qu.: 837.2   3rd Qu.:238.0   3rd Qu.:330.0   3rd Qu.:162.2  
    ##  Max.   :1000.0   Max.   :300.0   Max.   :400.0   Max.   :200.0  
    ##  entertainment    personal_care     technology    health_wellness
    ##  Min.   : 20.00   Min.   : 20.0   Min.   : 50.0   Min.   : 30.0  
    ##  1st Qu.: 54.00   1st Qu.: 41.0   1st Qu.:114.0   1st Qu.: 73.0  
    ##  Median : 86.00   Median : 62.0   Median :178.0   Median :115.0  
    ##  Mean   : 84.81   Mean   : 60.7   Mean   :178.3   Mean   :114.3  
    ##  3rd Qu.:116.00   3rd Qu.: 80.0   3rd Qu.:241.0   3rd Qu.:158.0  
    ##  Max.   :150.00   Max.   :100.0   Max.   :300.0   Max.   :200.0  
    ##  miscellaneous   
    ##  Min.   : 20.00  
    ##  1st Qu.: 63.75  
    ##  Median :110.00  
    ##  Mean   :108.91  
    ##  3rd Qu.:153.00  
    ##  Max.   :200.00

``` r
average_income <- spending %>% group_by(major) %>% summarise(avg_monthly_income = mean(monthly_income, na.rm = TRUE))

average_income %>% ggplot(aes(x = major, y = avg_monthly_income)) + geom_col(fill = "black") + coord_flip() + labs(title = "Distribution of Majors Based on Their Average Income", x = "Majors", y = "Monthly Income ($)") + theme(legend.position = "none")
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

This graphic displays the average monthly income of the students grouped
by their major, looking at the graph each major have very similar
averages. However Engineering majors on average have the highest average
monthly income. Displaying that engineering majors while in school on
average have the highest monthly income.

### Which category produces the highest expenses (excluding tuition, and Housing)

``` r
avg_expenses <- c( mean(expenses_df$food), mean(expenses_df$transportation), mean(expenses_df$entertainment), mean(expenses_df$personal_care), mean(expenses_df$technology), mean(expenses_df$health_wellness), mean(expenses_df$miscellaneous), mean(expenses_df$books_supplies) )
names_avg <- c('Food', 'Transportation', 'Entertainment', 'Personal Care', 'Technology', 'Health', 'Misc', 'Books')

avg_df <- data.frame(names_avg, avg_expenses)
ggplot(data = avg_df, aes(x = names_avg, y = avg_expenses)) + geom_col(fill = "black") + coord_flip() + labs(title = "Average Expenses by Category", x = "Category", y = "Expense ($)") + theme(legend.position = "none")
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

This graphic displays each expenses excluding tuition and housing due to
them on average having exponentially larger costs then those presented
in the graph. If included tuition would be the largest expense with
housing being the second largest on average. Excluding these two
variables, food on average seems to have the highest expenses.

### Which Majors have the highest tuition

``` r
average_tuition <- spending %>% group_by(major) %>% summarise(avg_tuition = mean(tuition, na.rm = TRUE))

average_tuition %>% ggplot(aes(x = major, y = avg_tuition)) + geom_col(fill = "black") + coord_flip() + labs(title = "Average Tuition for Each Major", x = "Major", y = "Average Tuition") + theme(legend.position = "none")
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

This graphic shows that on average psychology majors tend to have the
highest tuition compared to the other majors with computer science
having the second most. However each major have very similar costs in
terms of their tuition.

### School Expenses based on major

``` r
grouped_expenses <- spending %>% group_by(major)
ggplot(data = grouped_expenses, aes(x = major, y = total_expenses)) + geom_col(fill = "black") + coord_flip() + labs(title = "Total Expenses by Major", x = "Major", y = "Expenses ($)") + theme(legend.position = "none")
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

This graphic displays an overview of each major’s total expenses
(cumulative). It displays that overall Biology majors have the highest
total expenses.

When we break this down to look at the average of each majors total
expenses:

``` r
ggplot(data = grouped_expenses, aes(x = major, y = mean(total_expenses))) + geom_col(fill = "black") + coord_flip() + labs(title = "Avg Total Expenses by Major", x = "Major", y = "Expenses ($)") + theme(legend.position = "none")
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

We see that the graphic remains about the same in terms of its looks
with Biology majors on average having the highest total expenses

### Distribution of Gender and Majors

``` r
grouped_spending <- spending %>% group_by(major, gender)
color_palette <- c("darkred", "red", "coral1")
ggplot(data = grouped_spending, aes(x = major, fill = gender)) + geom_bar(position = position_dodge(preserve = 'single')) + scale_fill_manual(values = color_palette) + labs(title = "Gender Distribution across Majors", x = "Majors", y = "Count", fill = "Gender")
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

This graphic displays the distribution of each gender within each major,
showing that biology has the highest number of non-binary students, with
biology and economics having the highest number of male students, and
with Engineering having the highest number of female students.

### Difference in students expenses and income based on gender

``` r
spending %>% group_by(gender) %>%
  summarize(avg_income = mean(monthly_income), avg_expenses = mean(total_expenses), n = n()) %>% ungroup()
```

    ## # A tibble: 3 × 4
    ##   gender     avg_income avg_expenses     n
    ##   <chr>           <dbl>        <dbl> <int>
    ## 1 Female          1024.        6279.   323
    ## 2 Male            1008.        6341.   356
    ## 3 Non-binary      1031.        6324.   321

Here we look at the differences in students expenses and income based on
their gender and we found that female and non-binary students on average
have a higher income than male students. However male students on
average have higher total expenses than the other two genders.

### Student spending based on year in school

``` r
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

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

This graphic displays the average total expenses based on year in
school. However each year displays very similar values in their total
expenses. Upon breaking this down in several other plots to look at
things such as non-school affiliated costs the graph doesn’t display any
significant change when compared to this graph.

## Average Student income based on year

``` r
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

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

This graphic displays the average monthly income of each student based
on their year and it displays that on average Freshman typically make
more per month than the other three.

\###Distribution of financial aid by major, gender and year \##Average
Students financial aid based on major

``` r
spending %>% group_by(major) %>% ggplot(aes(x = major, y = mean(financial_aid))) + geom_col(fill = 'black') + labs(
        title = "Average Student Financial Aid based on major",
        x = "Major",
        y = "Financial Aid ($)",
    )
```

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

This graphic displays the average financial aid received by students
based on their major. It shows that Biology students on average recieve
the most financial aid compared to the other majors, with economics
receiving the second highest.

\##Average Students financial aid based on gender

``` r
spending %>% group_by(gender) %>% ggplot(aes(x = gender, y = mean(financial_aid))) + geom_col(fill = 'black') + labs(
        title = "Average Student Financial Aid based on gender",
        x = "Gender",
        y = "Financial Aid ($)",
    )
```

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

This graphic displays the average financial aid received by each student
based on their gender. It shows that male students on average receive
the highest financial aid, with female and non-binary students receiving
very similar values on average.

## Student financial aid based on year

``` r
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

![](README_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

This graphic displays the average financial aid received by students
based on their year. It shows that each year on average receives very
similar amounts of aid, however Seniors typically receive the most
financial aid on average.

## Financial aid vs tuition

``` r
spending %>%
  summarize(avg_tuition = mean(tuition), avg_financial_aid = mean(financial_aid), tuition_covered = (avg_financial_aid/avg_tuition * 100))
```

    ## # A tibble: 1 × 3
    ##   avg_tuition avg_financial_aid tuition_covered
    ##         <dbl>             <dbl>           <dbl>
    ## 1       4520.              505.            11.2

This looks at the average financial aid is received by students with the
average tuition costs. It shows that the average aid that is received is
able to help students cover around 11% of their tuition costs on
average.

## Conclusion

In conclusion, our analysis of student spending displays the differences
between each students expenses, income, and financial aid (our target
variables), based on their gender, year classification, and their major.
As well as displaying what their total expenses are when they are in
college. Students largest expenses on average when going to college are
their costs of tuition and housing. When this is broken down into gender
our data displays that man on average have higher total expenses than
non-binary and female students. However men typically receive on average
more financial aid than the other two genders, but they have the highest
total expenses on average when compared to the other two. Year
classification doesn’t display much impact on our desired target
variables. While Majors like biology on average have the highest total
expenses.

Further research into looking at data containing tuition rates from
school across the country would aid in the process of understanding the
costs that are associated with going to college, as well as being able
to know the average tuition rates of each school to aid in the design
making of where to pursue further education, which would be the next
step.
